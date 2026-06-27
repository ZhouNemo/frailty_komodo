library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual Gagne combined comorbidity score
# Author: Nemo Zhou
# Date started: 2026-06-27
# Date last updated: 2026-06-27
#
# ---- Purpose ----
# Calculate annual Gagne combined comorbidity scores from the shared diagnosis
# matched-event table. This script does not rescan raw KRD claims. It maps
# matched Gagne diagnosis features to the converted Gagne groups, retains each
# group once per patient-year, applies the supplied group weights, and writes one
# row per eligible patient-year to:
#   - annual_gagne_scores
#
# Patients with no matched Gagne group receive score zero, group count zero, and
# all group indicators equal to zero. Only aggregate QA is printed.

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

default_config <- list(
  analysis_years = 2016L,
  ids_table = "2_annual_metric_ids",
  diagnosis_matches_table = "2_annual_diagnosis_matches",
  gagne_scores_table = "annual_gagne_scores",
  lookup_dir = file.path(getwd(), "Documents", "Clinical Metric Look Up Tables")
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
gagne_scores_table <- config$gagne_scores_table
gagne_dx_lookup_path <- file.path(config$lookup_dir, "0.6_gagne_diagnosis_lookup.csv")
gagne_weight_lookup_path <- file.path(
  config$lookup_dir,
  "0.6_gagne_weight_lookup.csv"
)

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

sanitize_identifier <- function(value, prefix) {
  cleaned <- gsub("[^a-z0-9]+", "_", tolower(value))
  cleaned <- gsub("^_+|_+$", "", cleaned)
  paste0(prefix, cleaned)
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

execute_insert_batches <- function(
  table_identifier,
  columns,
  data,
  numeric_columns = character(),
  chunk_size = 1000L
) {
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
                } else if (column %in% numeric_columns) {
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

missing_lookup_files <- c(
  gagne_dx_lookup_path,
  gagne_weight_lookup_path
)[!file.exists(c(gagne_dx_lookup_path, gagne_weight_lookup_path))]
if (length(missing_lookup_files) > 0L) {
  stop(
    "Missing Gagne lookup files:\n",
    paste(" -", missing_lookup_files, collapse = "\n")
  )
}

for (table in c(ids_table, diagnosis_matches_table)) {
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

gagne_dx_lookup <- read_lookup_csv(gagne_dx_lookup_path)
gagne_weight_lookup <- read_lookup_csv(gagne_weight_lookup_path)

required_dx_columns <- c(
  "lookup_version",
  "feature_id",
  "gagne_group",
  "gagne_group_desc"
)
required_weight_columns <- c(
  "lookup_version",
  "gagne_group",
  "gagne_group_desc",
  "weight"
)

missing_dx_columns <- setdiff(required_dx_columns, names(gagne_dx_lookup))
missing_weight_columns <- setdiff(required_weight_columns, names(gagne_weight_lookup))
if (length(missing_dx_columns) > 0L) {
  stop(
    "Gagne diagnosis lookup is missing columns: ",
    paste(missing_dx_columns, collapse = ", ")
  )
}
if (length(missing_weight_columns) > 0L) {
  stop(
    "Gagne weight lookup is missing columns: ",
    paste(missing_weight_columns, collapse = ", ")
  )
}

gagne_groups <- unique(gagne_weight_lookup[, required_weight_columns])
gagne_groups$gagne_group <- as.integer(gagne_groups$gagne_group)
gagne_groups$weight <- as.numeric(gagne_groups$weight)
gagne_groups <- gagne_groups[
  !is.na(gagne_groups$gagne_group) &
    !is.na(gagne_groups$weight) &
    !is.na(gagne_groups$gagne_group_desc) &
    gagne_groups$gagne_group_desc != "",
]
gagne_groups <- gagne_groups[order(gagne_groups$gagne_group), ]
gagne_groups$indicator_column <- sprintf(
  "gagne_group_%02d_%s",
  gagne_groups$gagne_group,
  gsub(
    "^gagne_",
    "",
    vapply(
      gagne_groups$gagne_group_desc,
      sanitize_identifier,
      character(1),
      prefix = ""
    )
  )
)

if (nrow(gagne_groups) == 0L) {
  stop("No usable Gagne weight lookup rows were found.")
}
if (any(duplicated(gagne_groups$gagne_group))) {
  stop("Gagne weight lookup contains duplicate group rows.")
}
if (any(duplicated(gagne_groups$indicator_column))) {
  stop("Gagne indicator column names are not unique.")
}

gagne_feature_groups <- unique(gagne_dx_lookup[, required_dx_columns])
gagne_feature_groups$gagne_group <- as.integer(gagne_feature_groups$gagne_group)
gagne_feature_groups <- merge(
  gagne_feature_groups[, c("feature_id", "gagne_group")],
  gagne_groups[, c("lookup_version", "gagne_group", "gagne_group_desc", "weight")],
  by = "gagne_group",
  all.x = TRUE
)
gagne_feature_groups <- gagne_feature_groups[
  !is.na(gagne_feature_groups$feature_id) &
    gagne_feature_groups$feature_id != "" &
    !is.na(gagne_feature_groups$gagne_group) &
    !is.na(gagne_feature_groups$weight),
]

if (nrow(gagne_feature_groups) == 0L) {
  stop("No usable Gagne feature-to-group lookup rows were found.")
}

ids_identifier <- qualified_identifier(write_schema, ids_table)
diagnosis_matches_identifier <- qualified_identifier(
  write_schema,
  diagnosis_matches_table
)
gagne_scores_identifier <- qualified_identifier(write_schema, gagne_scores_table)
gagne_stage_table <- "clinical_metric_gagne_group_stage"
gagne_stage_identifier <- quote_identifier(gagne_stage_table)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", gagne_stage_identifier, ";
     CREATE TEMP TABLE ", gagne_stage_identifier, " (
       lookup_version VARCHAR(128) NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       gagne_group INTEGER NOT NULL,
       gagne_group_desc VARCHAR(256) NOT NULL,
       weight DOUBLE PRECISION NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(feature_id);"
  )
)

gagne_feature_groups$gagne_group <- as.character(gagne_feature_groups$gagne_group)
gagne_feature_groups$weight <- as.character(gagne_feature_groups$weight)
execute_insert_batches(
  gagne_stage_identifier,
  c("lookup_version", "feature_id", "gagne_group", "gagne_group_desc", "weight"),
  gagne_feature_groups,
  numeric_columns = c("gagne_group", "weight")
)

indicator_definitions <- paste(
  paste0(quote_identifier(gagne_groups$indicator_column), " INTEGER NOT NULL"),
  collapse = ",\n         "
)
indicator_selects <- paste(
  paste0(
    "COALESCE(MAX(CASE WHEN observed.gagne_group = ",
    gagne_groups$gagne_group,
    " THEN 1 ELSE 0 END), 0)::INTEGER AS ",
    quote_identifier(gagne_groups$indicator_column)
  ),
  collapse = ",\n       "
)
indicator_insert_columns <- paste(
  quote_identifier(gagne_groups$indicator_column),
  collapse = ",\n       "
)

if (!table_exists(write_schema, gagne_scores_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", gagne_scores_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         ",
      indicator_definitions,
      ",
         gagne_group_count INTEGER NOT NULL,
         gagne_score DOUBLE PRECISION NOT NULL,
         lookup_version VARCHAR(128) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid);"
    )
  )
}

table_has_columns(
  write_schema,
  gagne_scores_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    gagne_groups$indicator_column,
    "gagne_group_count",
    "gagne_score",
    "lookup_version"
  )
)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", gagne_scores_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ");

     INSERT INTO ", gagne_scores_identifier, " (
       patid,
       patient_id,
       analysis_year,
       ",
    indicator_insert_columns,
    ",
       gagne_group_count,
       gagne_score,
       lookup_version
     )
     WITH observed AS (
       SELECT DISTINCT
         d.patid,
         d.analysis_year,
         stage.gagne_group,
         stage.weight,
         stage.lookup_version
       FROM ", diagnosis_matches_identifier, " d
       INNER JOIN ", gagne_stage_identifier, " stage
         ON d.feature_id = stage.feature_id
       WHERE d.analysis_year IN (", sql_values(analysis_years), ")
         AND d.metric = 'GAGNE'
     ),
     weighted_scores AS (
       SELECT
         patid,
         analysis_year,
         COUNT(DISTINCT gagne_group)::INTEGER AS gagne_group_count,
         SUM(weight)::DOUBLE PRECISION AS gagne_score,
         MAX(lookup_version) AS lookup_version
       FROM observed
       GROUP BY patid, analysis_year
     )
     SELECT
       ids.patid,
       ids.patient_id,
       ids.analysis_year,
       ",
    indicator_selects,
    ",
       COALESCE(weighted_scores.gagne_group_count, 0)::INTEGER
         AS gagne_group_count,
       COALESCE(weighted_scores.gagne_score, 0)::DOUBLE PRECISION
         AS gagne_score,
       COALESCE(
         weighted_scores.lookup_version,
         (SELECT MAX(lookup_version) FROM ", gagne_stage_identifier, ")
       ) AS lookup_version
     FROM ", ids_identifier, " ids
     LEFT JOIN observed
       ON ids.patid = observed.patid
      AND ids.analysis_year = observed.analysis_year
     LEFT JOIN weighted_scores
       ON ids.patid = weighted_scores.patid
      AND ids.analysis_year = weighted_scores.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY
       ids.patid,
       ids.patient_id,
       ids.analysis_year,
       weighted_scores.gagne_group_count,
       weighted_scores.gagne_score,
       weighted_scores.lookup_version;"
  )
)

gagne_qa <- print_query(
  "Checking annual Gagne score integrity.",
  paste0(
    "SELECT
       ids.analysis_year,
       COUNT(ids.patid)::BIGINT AS denominator_rows,
       COUNT(score.patid)::BIGINT AS score_rows,
       SUM(CASE WHEN score.gagne_group_count = 0 THEN 1 ELSE 0 END)::BIGINT
         AS zero_group_rows,
       MIN(score.gagne_score) AS min_gagne_score,
       MAX(score.gagne_score) AS max_gagne_score
     FROM ", ids_identifier, " ids
     LEFT JOIN ", gagne_scores_identifier, " score
       ON ids.patid = score.patid
      AND ids.analysis_year = score.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY ids.analysis_year
     ORDER BY ids.analysis_year"
  )
)

if (any(gagne_qa$denominator_rows != gagne_qa$score_rows)) {
  stop("Annual Gagne score integrity checks failed.")
}

duplicate_qa <- print_query(
  "Checking annual Gagne duplicate rows.",
  paste0(
    "SELECT
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
         AS duplicate_score_rows
     FROM ", gagne_scores_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")"
  )
)

if (duplicate_qa$duplicate_score_rows[[1]] != 0) {
  stop("annual_gagne_scores contains duplicate selected-year rows.")
}

message(
  "Annual Gagne scoring complete. Table updated in ",
  write_schema,
  ": ",
  gagne_scores_table,
  "."
)
