library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual HIV status
# Author: Nemo Zhou
# Date started: 2026-06-27
# Date last updated: 2026-06-27
#
# ---- Purpose ----
# Calculate annual HIV status from the shared diagnosis matched-event table.
# This script does not rescan raw KRD claims and does not use pharmacy evidence.
# The annual-only confirmation rule is:
#   - HIV status = 1 with at least one inpatient HIV diagnosis match in the
#     same patient-year; or
#   - HIV status = 1 with at least two distinct non-inpatient HIV diagnosis
#     dates in the same patient-year.
#
# A single non-inpatient HIV diagnosis match is not sufficient. HIV status is
# not carried forward from prior years. The script writes one row per eligible
# patient-year to:
#   - annual_hiv_status
#
# Only aggregate QA is printed.

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

default_config <- list(
  analysis_years = 2016L,
  ids_table = "2_annual_metric_ids",
  diagnosis_matches_table = "2_annual_diagnosis_matches",
  hiv_status_table = "annual_hiv_status",
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
hiv_status_table <- config$hiv_status_table
hiv_lookup_path <- file.path(config$lookup_dir, "0.6_hiv_diagnosis_lookup.csv")

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

if (!file.exists(hiv_lookup_path)) {
  stop("Missing HIV diagnosis lookup: ", hiv_lookup_path)
}

hiv_lookup <- utils::read.csv(
  hiv_lookup_path,
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE,
  na.strings = c("", "NA")
)

if (!"lookup_version" %in% names(hiv_lookup)) {
  stop("HIV diagnosis lookup is missing lookup_version.")
}
hiv_lookup_version <- max(hiv_lookup$lookup_version, na.rm = TRUE)

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
    "diagnosis_date",
    "claim_setting",
    "metric",
    "lookup_version"
  )
)

ids_identifier <- qualified_identifier(write_schema, ids_table)
diagnosis_matches_identifier <- qualified_identifier(
  write_schema,
  diagnosis_matches_table
)
hiv_status_identifier <- qualified_identifier(write_schema, hiv_status_table)

if (!table_exists(write_schema, hiv_status_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", hiv_status_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         hiv_status INTEGER NOT NULL,
         hiv_inpatient_evidence INTEGER NOT NULL,
         hiv_non_inpatient_distinct_dates INTEGER NOT NULL,
         hiv_non_inpatient_second_date DATE,
         hiv_first_observed_date DATE,
         hiv_evidence_count INTEGER NOT NULL,
         lookup_version VARCHAR(128) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid);"
    )
  )
}

table_has_columns(
  write_schema,
  hiv_status_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "hiv_status",
    "hiv_inpatient_evidence",
    "hiv_non_inpatient_distinct_dates",
    "hiv_non_inpatient_second_date",
    "hiv_first_observed_date",
    "hiv_evidence_count",
    "lookup_version"
  )
)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", hiv_status_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ");

     INSERT INTO ", hiv_status_identifier, " (
       patid,
       patient_id,
       analysis_year,
       hiv_status,
       hiv_inpatient_evidence,
       hiv_non_inpatient_distinct_dates,
       hiv_non_inpatient_second_date,
       hiv_first_observed_date,
       hiv_evidence_count,
       lookup_version
     )
     WITH hiv_matches AS (
       SELECT DISTINCT
         patid,
         analysis_year,
         diagnosis_date,
         claim_setting,
         lookup_version
       FROM ", diagnosis_matches_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ")
         AND metric = 'HIV'
     ),
     inpatient_evidence AS (
       SELECT
         patid,
         analysis_year,
         1::INTEGER AS hiv_inpatient_evidence
       FROM hiv_matches
       WHERE claim_setting = 'inpatient'
       GROUP BY patid, analysis_year
     ),
     non_inpatient_dates AS (
       SELECT DISTINCT
         patid,
         analysis_year,
         diagnosis_date
       FROM hiv_matches
       WHERE claim_setting = 'non_inpatient'
     ),
     ranked_non_inpatient_dates AS (
       SELECT
         patid,
         analysis_year,
         diagnosis_date,
         ROW_NUMBER() OVER (
           PARTITION BY patid, analysis_year
           ORDER BY diagnosis_date
         ) AS date_rank
       FROM non_inpatient_dates
     ),
     non_inpatient_evidence AS (
       SELECT
         patid,
         analysis_year,
         COUNT(*)::INTEGER AS hiv_non_inpatient_distinct_dates,
         MIN(CASE WHEN date_rank = 2 THEN diagnosis_date ELSE NULL END)
           AS hiv_non_inpatient_second_date
       FROM ranked_non_inpatient_dates
       GROUP BY patid, analysis_year
     ),
     all_evidence AS (
       SELECT
         patid,
         analysis_year,
         MIN(diagnosis_date) AS hiv_first_observed_date,
         COUNT(*)::INTEGER AS hiv_evidence_count,
         MAX(lookup_version) AS lookup_version
       FROM hiv_matches
       GROUP BY patid, analysis_year
     )
     SELECT
       ids.patid,
       ids.patient_id,
       ids.analysis_year,
       CASE
         WHEN COALESCE(inpatient.hiv_inpatient_evidence, 0) = 1
           OR non_inpatient.hiv_non_inpatient_second_date IS NOT NULL
         THEN 1
         ELSE 0
       END::INTEGER AS hiv_status,
       COALESCE(inpatient.hiv_inpatient_evidence, 0)::INTEGER
         AS hiv_inpatient_evidence,
       COALESCE(non_inpatient.hiv_non_inpatient_distinct_dates, 0)::INTEGER
         AS hiv_non_inpatient_distinct_dates,
       non_inpatient.hiv_non_inpatient_second_date,
       all_evidence.hiv_first_observed_date,
       COALESCE(all_evidence.hiv_evidence_count, 0)::INTEGER
         AS hiv_evidence_count,
       COALESCE(
         all_evidence.lookup_version,
         ", sql_string(hiv_lookup_version), "
       ) AS lookup_version
     FROM ", ids_identifier, " ids
     LEFT JOIN inpatient_evidence inpatient
       ON ids.patid = inpatient.patid
      AND ids.analysis_year = inpatient.analysis_year
     LEFT JOIN non_inpatient_evidence non_inpatient
       ON ids.patid = non_inpatient.patid
      AND ids.analysis_year = non_inpatient.analysis_year
     LEFT JOIN all_evidence
       ON ids.patid = all_evidence.patid
      AND ids.analysis_year = all_evidence.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ");"
  )
)

hiv_qa <- print_query(
  "Checking annual HIV status integrity.",
  paste0(
    "SELECT
       ids.analysis_year,
       COUNT(ids.patid)::BIGINT AS denominator_rows,
       COUNT(hiv.patid)::BIGINT AS hiv_rows,
       SUM(CASE WHEN hiv.hiv_status = 1 THEN 1 ELSE 0 END)::BIGINT
         AS hiv_status_rows,
       SUM(CASE
         WHEN hiv.hiv_status = 0
          AND hiv.hiv_inpatient_evidence = 0
          AND hiv.hiv_non_inpatient_distinct_dates = 1
         THEN 1 ELSE 0 END)::BIGINT AS single_non_inpatient_evidence_rows
     FROM ", ids_identifier, " ids
     LEFT JOIN ", hiv_status_identifier, " hiv
       ON ids.patid = hiv.patid
      AND ids.analysis_year = hiv.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY ids.analysis_year
     ORDER BY ids.analysis_year"
  )
)

if (any(hiv_qa$denominator_rows != hiv_qa$hiv_rows)) {
  stop("Annual HIV status integrity checks failed.")
}

duplicate_qa <- print_query(
  "Checking annual HIV duplicate rows.",
  paste0(
    "SELECT
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
         AS duplicate_hiv_rows
     FROM ", hiv_status_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")"
  )
)

if (duplicate_qa$duplicate_hiv_rows[[1]] != 0) {
  stop("annual_hiv_status contains duplicate selected-year rows.")
}

message(
  "Annual HIV status complete. Table updated in ",
  write_schema,
  ": ",
  hiv_status_table,
  "."
)
