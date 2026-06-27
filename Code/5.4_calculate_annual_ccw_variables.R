library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual shared CCW variables
# Author: Nemo Zhou
# Date started: 2026-06-27
# Date last updated: 2026-06-27
#
# ---- Purpose ----
# Calculate annual Chronic Conditions Data Warehouse (CCW)-based variables from
# the shared diagnosis matched-event table. This script does not rescan raw KRD
# claims. It assigns a condition when any reviewed CCW diagnosis match exists in
# the patient-year, then writes:
#   - annual_ccw_conditions_long
#   - annual_ccw_condition_indicators
#   - annual_ccw_group_counts
#
# The default configuration processes 2016 only for validation. Patients with no
# CCW evidence receive zero condition indicators and zero group counts. Only
# aggregate QA is printed.

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

default_config <- list(
  analysis_years = 2016L,
  ids_table = "2_annual_metric_ids",
  diagnosis_matches_table = "2_annual_diagnosis_matches",
  ccw_conditions_long_table = "annual_ccw_conditions_long",
  ccw_condition_indicators_table = "annual_ccw_condition_indicators",
  ccw_group_counts_table = "annual_ccw_group_counts",
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
ccw_conditions_long_table <- config$ccw_conditions_long_table
ccw_condition_indicators_table <- config$ccw_condition_indicators_table
ccw_group_counts_table <- config$ccw_group_counts_table
ccw_lookup_path <- file.path(config$lookup_dir, "0.6_ccw_diagnosis_lookup.csv")

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
                if (is.na(value)) "NULL" else sql_string(value)
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

if (!file.exists(ccw_lookup_path)) {
  stop("Missing CCW lookup: ", ccw_lookup_path)
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
  c(
    "patid",
    "analysis_year",
    "metric",
    "feature_id",
    "feature_name",
    "match_type",
    "lookup_version"
  )
)

ccw_lookup <- read_lookup_csv(ccw_lookup_path)
required_lookup_columns <- c(
  "lookup_version",
  "feature_id",
  "ccw_condition_id",
  "ccw_condition_name",
  "ccw_group"
)
missing_lookup_columns <- setdiff(required_lookup_columns, names(ccw_lookup))
if (length(missing_lookup_columns) > 0L) {
  stop(
    "CCW lookup is missing columns: ",
    paste(missing_lookup_columns, collapse = ", ")
  )
}

ccw_conditions <- unique(ccw_lookup[, required_lookup_columns])
ccw_conditions <- ccw_conditions[
  !is.na(ccw_conditions$ccw_condition_id) &
    ccw_conditions$ccw_condition_id != "" &
    !is.na(ccw_conditions$ccw_group) &
    ccw_conditions$ccw_group != "",
]
ccw_conditions$indicator_column <- vapply(
  ccw_conditions$ccw_condition_id,
  sanitize_identifier,
  character(1),
  prefix = "ccw_"
)

if (nrow(ccw_conditions) == 0L) {
  stop("No usable CCW condition lookup rows were found.")
}

if (any(duplicated(ccw_conditions$indicator_column))) {
  stop("CCW condition indicator column names are not unique.")
}

ccw_groups <- sort(unique(ccw_conditions$ccw_group))
group_columns <- paste0("index_", tolower(ccw_groups))

ids_identifier <- qualified_identifier(write_schema, ids_table)
diagnosis_matches_identifier <- qualified_identifier(
  write_schema,
  diagnosis_matches_table
)
conditions_long_identifier <- qualified_identifier(
  write_schema,
  ccw_conditions_long_table
)
condition_indicators_identifier <- qualified_identifier(
  write_schema,
  ccw_condition_indicators_table
)
group_counts_identifier <- qualified_identifier(write_schema, ccw_group_counts_table)
ccw_lookup_stage_table <- "clinical_metric_ccw_condition_stage"
ccw_lookup_stage_identifier <- quote_identifier(ccw_lookup_stage_table)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", ccw_lookup_stage_identifier, ";
     CREATE TEMP TABLE ", ccw_lookup_stage_identifier, " (
       lookup_version VARCHAR(128) NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       ccw_condition_id VARCHAR(128) NOT NULL,
       ccw_condition_name VARCHAR(256) NOT NULL,
       ccw_group VARCHAR(32) NOT NULL,
       indicator_column VARCHAR(128) NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(feature_id);"
  )
)

execute_insert_batches(
  ccw_lookup_stage_identifier,
  c(
    "lookup_version",
    "feature_id",
    "ccw_condition_id",
    "ccw_condition_name",
    "ccw_group",
    "indicator_column"
  ),
  ccw_conditions
)

indicator_definitions <- paste(
  paste0(quote_identifier(ccw_conditions$indicator_column), " INTEGER NOT NULL"),
  collapse = ",\n         "
)
indicator_selects <- paste(
  paste0(
    "COALESCE(MAX(CASE WHEN observed.ccw_condition_id = ",
    sql_string(ccw_conditions$ccw_condition_id),
    " THEN 1 ELSE 0 END), 0)::INTEGER AS ",
    quote_identifier(ccw_conditions$indicator_column)
  ),
  collapse = ",\n       "
)
group_definitions <- paste(
  paste0(quote_identifier(group_columns), " INTEGER NOT NULL"),
  collapse = ",\n         "
)
group_selects <- paste(
  paste0(
    "COALESCE(MAX(CASE WHEN gc.ccw_group = ",
    sql_string(ccw_groups),
    " THEN gc.n_conditions END), 0)::INTEGER AS ",
    quote_identifier(group_columns)
  ),
  collapse = ",\n       "
)

if (!table_exists(write_schema, ccw_conditions_long_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", conditions_long_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         ccw_condition_id VARCHAR(128) NOT NULL,
         ccw_condition_name VARCHAR(256) NOT NULL,
         ccw_group VARCHAR(32) NOT NULL,
         match_type VARCHAR(32) NOT NULL,
         lookup_version VARCHAR(128) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid, ccw_condition_id);"
    )
  )
}

table_has_columns(
  write_schema,
  ccw_conditions_long_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "ccw_condition_id",
    "ccw_condition_name",
    "ccw_group",
    "match_type",
    "lookup_version"
  )
)

if (!table_exists(write_schema, ccw_condition_indicators_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", condition_indicators_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         ",
      indicator_definitions,
      ",
         ccw_condition_count INTEGER NOT NULL,
         lookup_version VARCHAR(128) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid);"
    )
  )
}

if (!table_exists(write_schema, ccw_group_counts_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", group_counts_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         ",
      group_definitions,
      ",
         ccw_total_condition_count INTEGER NOT NULL,
         lookup_version VARCHAR(128) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid);"
    )
  )
}

table_has_columns(
  write_schema,
  ccw_condition_indicators_table,
  c("patid", "patient_id", "analysis_year", ccw_conditions$indicator_column)
)
table_has_columns(
  write_schema,
  ccw_group_counts_table,
  c("patid", "patient_id", "analysis_year", group_columns)
)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", conditions_long_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ");
     DELETE FROM ", condition_indicators_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ");
     DELETE FROM ", group_counts_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ");

     INSERT INTO ", conditions_long_identifier, " (
       patid,
       patient_id,
       analysis_year,
       ccw_condition_id,
       ccw_condition_name,
       ccw_group,
       match_type,
       lookup_version
     )
     SELECT DISTINCT
       d.patid,
       ids.patient_id,
       d.analysis_year,
       lkp.ccw_condition_id,
       lkp.ccw_condition_name,
       lkp.ccw_group,
       d.match_type,
       lkp.lookup_version
     FROM ", diagnosis_matches_identifier, " d
     INNER JOIN ", ids_identifier, " ids
       ON d.patid = ids.patid
      AND d.analysis_year = ids.analysis_year
     INNER JOIN ", ccw_lookup_stage_identifier, " lkp
       ON d.feature_id = lkp.feature_id
     WHERE d.analysis_year IN (", sql_values(analysis_years), ")
       AND d.metric = 'CCW';

     INSERT INTO ", condition_indicators_identifier, " (
       patid,
       patient_id,
       analysis_year,
       ",
    paste(quote_identifier(ccw_conditions$indicator_column), collapse = ",\n       "),
    ",
       ccw_condition_count,
       lookup_version
     )
     WITH observed AS (
       SELECT DISTINCT
         patid,
         analysis_year,
         ccw_condition_id
       FROM ", conditions_long_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ")
     )
     SELECT
       ids.patid,
       ids.patient_id,
       ids.analysis_year,
       ",
    indicator_selects,
    ",
       COUNT(DISTINCT observed.ccw_condition_id)::INTEGER
         AS ccw_condition_count,
       (SELECT MAX(lookup_version) FROM ", ccw_lookup_stage_identifier, ")
         AS lookup_version
     FROM ", ids_identifier, " ids
     LEFT JOIN observed
       ON ids.patid = observed.patid
      AND ids.analysis_year = observed.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY ids.patid, ids.patient_id, ids.analysis_year;

     INSERT INTO ", group_counts_identifier, " (
       patid,
       patient_id,
       analysis_year,
       ",
    paste(quote_identifier(group_columns), collapse = ",\n       "),
    ",
       ccw_total_condition_count,
       lookup_version
     )
     WITH group_counts AS (
       SELECT
         patid,
         analysis_year,
         ccw_group,
         COUNT(DISTINCT ccw_condition_id)::INTEGER AS n_conditions
       FROM ", conditions_long_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ")
       GROUP BY patid, analysis_year, ccw_group
     ),
     total_counts AS (
       SELECT
         patid,
         analysis_year,
         COUNT(DISTINCT ccw_condition_id)::INTEGER AS n_conditions
       FROM ", conditions_long_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ")
       GROUP BY patid, analysis_year
     )
     SELECT
       ids.patid,
       ids.patient_id,
       ids.analysis_year,
       ",
    group_selects,
    ",
       COALESCE(total_counts.n_conditions, 0)::INTEGER
         AS ccw_total_condition_count,
       (SELECT MAX(lookup_version) FROM ", ccw_lookup_stage_identifier, ")
         AS lookup_version
     FROM ", ids_identifier, " ids
     LEFT JOIN group_counts gc
       ON ids.patid = gc.patid
      AND ids.analysis_year = gc.analysis_year
     LEFT JOIN total_counts
       ON ids.patid = total_counts.patid
      AND ids.analysis_year = total_counts.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY
       ids.patid,
       ids.patient_id,
       ids.analysis_year,
       total_counts.n_conditions;"
  )
)

ccw_qa <- print_query(
  "Checking annual CCW output integrity.",
  paste0(
    "SELECT
       ids.analysis_year,
       COUNT(ids.patid)::BIGINT AS denominator_rows,
       COUNT(ind.patid)::BIGINT AS indicator_rows,
       COUNT(gc.patid)::BIGINT AS group_count_rows,
       SUM(CASE WHEN ind.ccw_condition_count = 0 THEN 1 ELSE 0 END)::BIGINT
         AS zero_condition_rows,
       SUM(CASE WHEN gc.ccw_total_condition_count = 0 THEN 1 ELSE 0 END)::BIGINT
         AS zero_group_count_rows
     FROM ", ids_identifier, " ids
     LEFT JOIN ", condition_indicators_identifier, " ind
       ON ids.patid = ind.patid
      AND ids.analysis_year = ind.analysis_year
     LEFT JOIN ", group_counts_identifier, " gc
       ON ids.patid = gc.patid
      AND ids.analysis_year = gc.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY ids.analysis_year
     ORDER BY ids.analysis_year"
  )
)

if (
  any(ccw_qa$denominator_rows != ccw_qa$indicator_rows) ||
    any(ccw_qa$denominator_rows != ccw_qa$group_count_rows)
) {
  stop("Annual CCW output integrity checks failed.")
}

print_query(
  "Summarizing annual CCW condition rows.",
  paste0(
    "SELECT
       analysis_year,
       ccw_group,
       COUNT(DISTINCT patid)::BIGINT AS patient_years,
       COUNT(DISTINCT ccw_condition_id)::INTEGER AS represented_conditions
     FROM ", conditions_long_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY analysis_year, ccw_group
     ORDER BY analysis_year, ccw_group"
  )
)

message(
  "Annual CCW variables complete. Tables updated in ",
  write_schema,
  ": ",
  paste(
    c(
      ccw_conditions_long_table,
      ccw_condition_indicators_table,
      ccw_group_counts_table
    ),
    collapse = ", "
  ),
  "."
)
