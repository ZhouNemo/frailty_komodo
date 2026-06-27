library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual shared CFI scoring
# Author: Nemo Zhou
# Date started: 2026-06-27
# Date last updated: 2026-06-27
#
# ---- Purpose ----
# Calculate annual Claims-Based Frailty Index (CFI) scores from the shared
# matched-event tables created by Code/5.1 and Code/5.2. This script does not
# rescan raw KRD event tables. It combines CFI diagnosis and procedure feature
# matches, keeps each disease feature once per patient-year, joins the validated
# CFI weight lookup, adds the 0.10288 model intercept, and writes one row per
# eligible patient-year to:
#   - annual_cfi_scores
#
# The default configuration processes 2016 only for validation. Additional years
# can be supplied later with options("frailty.clinical_metrics.config").
# Patient-level scores remain in Redshift. Only aggregate QA is printed.

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

default_config <- list(
  analysis_years = 2016L,
  ids_table = "2_annual_metric_ids",
  diagnosis_matches_table = "2_annual_diagnosis_matches",
  procedure_matches_table = "2_annual_procedure_matches",
  cfi_scores_table = "annual_cfi_scores",
  lookup_dir = file.path(getwd(), "Documents", "Clinical Metric Look Up Tables"),
  model_intercept = 0.10288
)

config <- utils::modifyList(
  default_config,
  getOption("frailty.clinical_metrics.config", list())
)

analysis_years <- sort(unique(as.integer(config$analysis_years)))
if (
  length(analysis_years) == 0L ||
    any(is.na(analysis_years)) ||
    any(analysis_years < 2016L | analysis_years > 2025L)
) {
  stop("analysis_years must contain years from 2016 through 2025.")
}

ids_table <- config$ids_table
diagnosis_matches_table <- config$diagnosis_matches_table
procedure_matches_table <- config$procedure_matches_table
cfi_scores_table <- config$cfi_scores_table
cfi_weight_lookup_path <- file.path(config$lookup_dir, "0.6_cfi_weight_lookup.csv")
model_intercept <- as.numeric(config$model_intercept)

con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

qualified_identifier <- function(schema, table) {
  paste(quote_identifier(schema), quote_identifier(table), sep = ".")
}

sql_string <- function(value) {
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

sql_values <- function(values) {
  paste(values, collapse = ", ")
}

print_query <- function(label, sql) {
  message(label)
  result <- DBI::dbGetQuery(con, sql)
  print(result)
  invisible(result)
}

table_exists <- function(schema, table) {
  table_identifier <- qualified_identifier(schema, table)
  isTRUE(
    tryCatch(
      {
        DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_identifier, " LIMIT 0"))
        TRUE
      },
      error = function(e) FALSE
    )
  )
}

table_has_columns <- function(schema, table, columns) {
  table_identifier <- qualified_identifier(schema, table)
  result <- tryCatch(
    DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_identifier, " LIMIT 0")),
    error = function(e) {
      stop(
        "Required table was not accessible: ",
        schema,
        ".",
        table,
        ". ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  available_columns <- tolower(names(result))
  missing_columns <- setdiff(tolower(columns), available_columns)
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns in ",
      schema,
      ".",
      table,
      ": ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

read_lookup_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

execute_insert_batches <- function(table_identifier, columns, data, chunk_size = 1000L) {
  if (nrow(data) == 0L) {
    return(invisible(NULL))
  }

  column_sql <- paste(quote_identifier(columns), collapse = ", ")
  starts <- seq.int(1L, nrow(data), by = chunk_size)

  for (start_row in starts) {
    end_row <- min(start_row + chunk_size - 1L, nrow(data))
    chunk <- data[start_row:end_row, columns, drop = FALSE]
    values <- apply(
      chunk,
      1L,
      function(row) {
        paste0(
          "(",
          paste(
            vapply(
              names(row),
              function(column) {
                value <- row[[column]]
                if (is.na(value)) {
                  "NULL"
                } else if (column %in% c("disease_number", "weight")) {
                  value
                } else {
                  sql_string(value)
                }
              },
              character(1)
            ),
            collapse = ", "
          ),
          ")"
        )
      }
    )

    DatabaseConnector::executeSql(
      con,
      paste0(
        "INSERT INTO ",
        table_identifier,
        " (",
        column_sql,
        ") VALUES ",
        paste(values, collapse = ", "),
        ";"
      ),
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  }
}

if (!file.exists(cfi_weight_lookup_path)) {
  stop("Missing CFI weight lookup: ", cfi_weight_lookup_path)
}

for (table in c(ids_table, diagnosis_matches_table, procedure_matches_table)) {
  if (!table_exists(write_schema, table)) {
    stop("Required table was not found: ", write_schema, ".", table)
  }
}

table_has_columns(
  write_schema,
  ids_table,
  c("patid", "patient_id", "analysis_year")
)
table_has_columns(
  write_schema,
  diagnosis_matches_table,
  c("patid", "analysis_year", "metric", "feature_id", "lookup_version")
)
table_has_columns(
  write_schema,
  procedure_matches_table,
  c("patid", "analysis_year", "metric", "feature_id", "lookup_version")
)

cfi_weight_lookup <- read_lookup_csv(cfi_weight_lookup_path)
required_weight_columns <- c(
  "lookup_version",
  "metric",
  "disease_number",
  "feature_id",
  "weight"
)
missing_weight_columns <- setdiff(required_weight_columns, names(cfi_weight_lookup))
if (length(missing_weight_columns) > 0L) {
  stop(
    "CFI weight lookup is missing columns: ",
    paste(missing_weight_columns, collapse = ", ")
  )
}

cfi_weight_lookup <- cfi_weight_lookup[
  toupper(cfi_weight_lookup$metric) == "CFI",
  required_weight_columns
]
cfi_weight_lookup$disease_number <- as.integer(cfi_weight_lookup$disease_number)
cfi_weight_lookup$weight <- as.numeric(cfi_weight_lookup$weight)
cfi_weight_lookup <- cfi_weight_lookup[
  !is.na(cfi_weight_lookup$disease_number) &
    !is.na(cfi_weight_lookup$weight) &
    !is.na(cfi_weight_lookup$feature_id) &
    cfi_weight_lookup$feature_id != "",
]

if (nrow(cfi_weight_lookup) == 0L) {
  stop("No usable CFI weight lookup rows were found.")
}

ids_identifier <- qualified_identifier(write_schema, ids_table)
diagnosis_matches_identifier <- qualified_identifier(
  write_schema,
  diagnosis_matches_table
)
procedure_matches_identifier <- qualified_identifier(
  write_schema,
  procedure_matches_table
)
cfi_scores_identifier <- qualified_identifier(write_schema, cfi_scores_table)
cfi_weight_stage_table <- "clinical_metric_cfi_weight_stage"
cfi_weight_stage_identifier <- quote_identifier(cfi_weight_stage_table)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", cfi_weight_stage_identifier, ";
     CREATE TEMP TABLE ", cfi_weight_stage_identifier, " (
       lookup_version VARCHAR(128) NOT NULL,
       disease_number INTEGER NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       weight DOUBLE PRECISION NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(feature_id);"
  )
)

cfi_weight_lookup$disease_number <- as.character(cfi_weight_lookup$disease_number)
cfi_weight_lookup$weight <- as.character(cfi_weight_lookup$weight)
execute_insert_batches(
  cfi_weight_stage_identifier,
  c("lookup_version", "disease_number", "feature_id", "weight"),
  cfi_weight_lookup
)

if (!table_exists(write_schema, cfi_scores_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", cfi_scores_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         cfi_score DOUBLE PRECISION NOT NULL,
         cfi_feature_count INTEGER NOT NULL,
         cfi_intercept_only_flag INTEGER NOT NULL,
         cfi_weight_sum DOUBLE PRECISION NOT NULL,
         lookup_version VARCHAR(128) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid);"
    )
  )
}

table_has_columns(
  write_schema,
  cfi_scores_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "cfi_score",
    "cfi_feature_count",
    "cfi_intercept_only_flag",
    "cfi_weight_sum",
    "lookup_version"
  )
)

# QA: every matched CFI feature must have a weight. Otherwise the INNER JOIN to
# the weight stage below would silently omit that feature from the score.
feature_weight_qa <- print_query(
  "Checking that every matched CFI feature has a weight.",
  paste0(
    "SELECT COUNT(*)::BIGINT AS cfi_features_without_weight
     FROM (
       SELECT DISTINCT feature_id
       FROM ", diagnosis_matches_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ")
         AND metric = 'CFI'
       UNION
       SELECT DISTINCT feature_id
       FROM ", procedure_matches_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ")
         AND metric = 'CFI'
     ) mf
     LEFT JOIN ", cfi_weight_stage_identifier, " w
       ON mf.feature_id = w.feature_id
     WHERE w.feature_id IS NULL"
  )
)

if (feature_weight_qa$cfi_features_without_weight[[1]] != 0) {
  stop(
    "Some matched CFI features have no weight in the CFI weight lookup; ",
    "their contribution would be omitted from the score."
  )
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", cfi_scores_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ");

     INSERT INTO ", cfi_scores_identifier, " (
       patid,
       patient_id,
       analysis_year,
       cfi_score,
       cfi_feature_count,
       cfi_intercept_only_flag,
       cfi_weight_sum,
       lookup_version
     )
     WITH matched_features AS (
       SELECT DISTINCT patid, analysis_year, feature_id, lookup_version
       FROM ", diagnosis_matches_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ")
         AND metric = 'CFI'
       UNION
       SELECT DISTINCT patid, analysis_year, feature_id, lookup_version
       FROM ", procedure_matches_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ")
         AND metric = 'CFI'
     ),
     weighted_features AS (
       SELECT DISTINCT
         mf.patid,
         mf.analysis_year,
         weight.disease_number,
         weight.weight,
         weight.lookup_version
       FROM matched_features mf
       INNER JOIN ", cfi_weight_stage_identifier, " weight
         ON mf.feature_id = weight.feature_id
     ),
     weighted_scores AS (
       SELECT
         patid,
         analysis_year,
         COUNT(DISTINCT disease_number)::INTEGER AS cfi_feature_count,
         SUM(weight)::DOUBLE PRECISION AS cfi_weight_sum,
         MAX(lookup_version) AS lookup_version
       FROM weighted_features
       GROUP BY patid, analysis_year
     )
     SELECT
       ids.patid,
       ids.patient_id,
       ids.analysis_year,
       (", model_intercept, " + COALESCE(score.cfi_weight_sum, 0))
         ::DOUBLE PRECISION AS cfi_score,
       COALESCE(score.cfi_feature_count, 0)::INTEGER AS cfi_feature_count,
       CASE
         WHEN COALESCE(score.cfi_feature_count, 0) = 0 THEN 1
         ELSE 0
       END::INTEGER AS cfi_intercept_only_flag,
       COALESCE(score.cfi_weight_sum, 0)::DOUBLE PRECISION AS cfi_weight_sum,
       COALESCE(
         score.lookup_version,
         (SELECT MAX(lookup_version) FROM ", cfi_weight_stage_identifier, ")
       ) AS lookup_version
     FROM ", ids_identifier, " ids
     LEFT JOIN weighted_scores score
       ON ids.patid = score.patid
      AND ids.analysis_year = score.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ");"
  )
)

score_qa <- print_query(
  "Checking annual CFI score integrity.",
  paste0(
    "SELECT
       ids.analysis_year,
       COUNT(ids.patid)::BIGINT AS denominator_rows,
       COUNT(score.patid)::BIGINT AS score_rows,
       SUM(CASE WHEN score.cfi_score IS NULL THEN 1 ELSE 0 END)::BIGINT
         AS missing_score_rows,
       SUM(CASE WHEN score.cfi_intercept_only_flag = 1 THEN 1 ELSE 0 END)::BIGINT
         AS intercept_only_rows,
       MIN(score.cfi_score) AS min_cfi_score,
       MAX(score.cfi_score) AS max_cfi_score
     FROM ", ids_identifier, " ids
     LEFT JOIN ", cfi_scores_identifier, " score
       ON ids.patid = score.patid
      AND ids.analysis_year = score.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY ids.analysis_year
     ORDER BY ids.analysis_year"
  )
)

if (
  any(score_qa$denominator_rows != score_qa$score_rows) ||
    any(score_qa$missing_score_rows != 0)
) {
  stop("Annual CFI score integrity checks failed.")
}

duplicate_qa <- print_query(
  "Checking annual CFI duplicate rows.",
  paste0(
    "SELECT
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
         AS duplicate_score_rows
     FROM ", cfi_scores_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")"
  )
)

if (duplicate_qa$duplicate_score_rows[[1]] != 0) {
  stop("annual_cfi_scores contains duplicate selected-year rows.")
}

message(
  "Annual CFI scoring complete. Table updated in ",
  write_schema,
  ": ",
  cfi_scores_table,
  "."
)
