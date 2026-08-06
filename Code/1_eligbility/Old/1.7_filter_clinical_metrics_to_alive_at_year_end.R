# Project: Frailty_Komoto annual clinical-metrics eligibility restriction
# Author: Nemo Zhou
# Date started: 2026-07-16
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Apply the annual mortality eligibility criterion to the completed normalized
# clinical-metrics table. A patient-year is retained only when the patient has
# no nonmissing PATIENT_MORTALITY.patient_death_date on or before December 31
# of that analysis year. Because the KRD death date is truncated to the first
# day of the death month, a death date of 2022-12-01 excludes the patient from
# the 2022 year-end-survivor analysis.
#
# The default 2022 run reads:
#   - 6_annual_clinical_metrics_shared
#
# and writes:
#   - 6_annual_clinical_metrics_shared_alive_at_year_end
#
# This is a post-final-table restriction. It does not rebuild the annual
# denominator or recalculate CFI, CCW, Gagne, or HIV metric values.
#
# Run this after:
#   - Code/2_variable construction/3.11_build_normalized_annual_clinical_metrics.R
#   - Code/2_variable construction/3.12_check_normalized_annual_clinical_metrics.R

source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

default_alive_at_year_end_filter_config <- list(
  analysis_years = 2022L,
  source_final_schema = NULL,
  source_final_table = "6_annual_clinical_metrics_shared",
  restricted_final_schema = NULL,
  restricted_final_table =
    "6_annual_clinical_metrics_shared_alive_at_year_end",
  mortality_table = "patient_mortality"
)

filter_config <- utils::modifyList(
  default_alive_at_year_end_filter_config,
  getOption("frailty.alive_at_year_end_filter.config", list())
)

analysis_years <- sort(unique(as.integer(filter_config$analysis_years)))
if (
  length(analysis_years) == 0L ||
    any(is.na(analysis_years)) ||
    any(analysis_years < 2016L | analysis_years > 2025L)
) {
  stop("analysis_years must contain years from 2016 through 2025.")
}

source_final_table <- filter_config$source_final_table
if (is.null(source_final_table) || !nzchar(source_final_table)) {
  stop("source_final_table must be a nonempty table name.")
}

source_final_schema <- filter_config$source_final_schema
if (is.null(source_final_schema) || !nzchar(source_final_schema)) {
  source_final_schema <- write_schema
}

restricted_final_table <- filter_config$restricted_final_table
if (is.null(restricted_final_table) || !nzchar(restricted_final_table)) {
  stop("restricted_final_table must be a nonempty table name.")
}

restricted_final_schema <- filter_config$restricted_final_schema
if (is.null(restricted_final_schema) || !nzchar(restricted_final_schema)) {
  restricted_final_schema <- write_schema
}

mortality_table <- filter_config$mortality_table
if (is.null(mortality_table) || !nzchar(mortality_table)) {
  stop("mortality_table must be a nonempty table name.")
}

con <- connect_komodo()
# Do not register on.exit(disconnect_komodo(con)) here. This file is sourced at
# the top level, where that handler can close the Redshift connection before
# the first query runs. Disconnect explicitly after aggregate QA completes.

source_final_identifier <- qualified_identifier(
  source_final_schema,
  source_final_table
)
restricted_final_identifier <- qualified_identifier(
  restricted_final_schema,
  restricted_final_table
)
mortality_identifier <- qualified_identifier(komodo_schema, mortality_table)

if (!table_exists(con, source_final_schema, source_final_table)) {
  stop(
    "Required source clinical-metrics table was not found: ",
    source_final_schema,
    ".",
    source_final_table,
    "."
  )
}
if (!table_exists(con, komodo_schema, mortality_table)) {
  stop(
    "Required KRD mortality table was not found: ",
    komodo_schema,
    ".",
    mortality_table,
    "."
  )
}

table_has_columns(
  con,
  source_final_schema,
  source_final_table,
  c("patient_id", "analysis_year")
)
table_has_columns(
  con,
  komodo_schema,
  mortality_table,
  c("patient_id", "patient_death_date")
)

source_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", source_final_identifier, " LIMIT 0")
)))
source_column_sql <- paste(
  paste0("clinical.", quote_identifier(source_columns)),
  collapse = ",\n       "
)
insert_column_sql <- paste(quote_identifier(source_columns), collapse = ",\n         ")
selected_year_sql <- sql_values(analysis_years)

restricted_select_sql <- paste0(
  "SELECT
       ",
  source_column_sql,
  "
     FROM ",
  source_final_identifier,
  " clinical
     WHERE clinical.analysis_year IN (",
  selected_year_sql,
  ")
       AND NOT EXISTS (
         SELECT 1
         FROM ",
  mortality_identifier,
  " mortality
         WHERE mortality.patient_id = clinical.patient_id
           AND mortality.patient_death_date IS NOT NULL
           AND mortality.patient_death_date <=
             TO_DATE(clinical.analysis_year::VARCHAR || '-12-31', 'YYYY-MM-DD')
       )"
)

message(
  "Applying alive-at-year-end eligibility criterion to selected years: ",
  paste(analysis_years, collapse = ", "),
  "."
)
message("Source clinical-metrics table: ", source_final_schema, ".", source_final_table)
message(
  "Year-end-survivor table: ",
  restricted_final_schema,
  ".",
  restricted_final_table
)

start_time <- Sys.time()
message(
  format(start_time, "[%Y-%m-%d %H:%M:%S] "),
  "START: Build alive-at-year-end clinical-metrics table."
)

if (!table_exists(con, restricted_final_schema, restricted_final_table)) {
  execute_sql_with_retry(
    con,
    paste0(
      "CREATE TABLE ",
      restricted_final_identifier,
      "
       DISTKEY(patient_id)
       SORTKEY(analysis_year, patient_id) AS
       ",
      restricted_select_sql,
      ";"
    ),
    label = "create alive-at-year-end clinical-metrics table"
  )
} else {
  table_has_columns(
    con,
    restricted_final_schema,
    restricted_final_table,
    source_columns
  )

  execute_sql_with_retry(
    con,
    paste0(
      "DELETE FROM ",
      restricted_final_identifier,
      "
       WHERE analysis_year IN (",
      selected_year_sql,
      ");

       INSERT INTO ",
      restricted_final_identifier,
      " (
         ",
      insert_column_sql,
      "
       )
       ",
      restricted_select_sql,
      ";"
    ),
    label = "refresh alive-at-year-end clinical-metrics table"
  )
}

end_time <- Sys.time()
message(
  format(end_time, "[%Y-%m-%d %H:%M:%S] "),
  "DONE: Build alive-at-year-end clinical-metrics table. Elapsed minutes: ",
  round(as.numeric(difftime(end_time, start_time, units = "mins")), 2),
  "."
)

retention_qa <- print_query(
  con,
  "Checking alive-at-year-end retention by year.",
  paste0(
    "SELECT
       source.analysis_year,
       source.n_source_patient_years,
       COALESCE(restricted.n_restricted_patient_years, 0)::BIGINT
         AS n_restricted_patient_years,
       source.n_source_patient_years -
         COALESCE(restricted.n_restricted_patient_years, 0)::BIGINT
         AS n_excluded_patient_years,
       ROUND(
         100.0 * COALESCE(restricted.n_restricted_patient_years, 0) /
           NULLIF(source.n_source_patient_years, 0),
         2
       ) AS retained_percent
     FROM (
       SELECT analysis_year, COUNT(*)::BIGINT AS n_source_patient_years
       FROM ",
    source_final_identifier,
    "
       WHERE analysis_year IN (",
    selected_year_sql,
    ")
       GROUP BY analysis_year
     ) source
     LEFT JOIN (
       SELECT analysis_year, COUNT(*)::BIGINT AS n_restricted_patient_years
       FROM ",
    restricted_final_identifier,
    "
       WHERE analysis_year IN (",
    selected_year_sql,
    ")
       GROUP BY analysis_year
     ) restricted
       ON source.analysis_year = restricted.analysis_year
     ORDER BY source.analysis_year"
  )
)

if (
  nrow(retention_qa) != length(analysis_years) ||
    any(is.na(retention_qa$n_restricted_patient_years)) ||
    any(retention_qa$n_restricted_patient_years >
      retention_qa$n_source_patient_years)
) {
  stop("Alive-at-year-end retention QA failed.")
}

duplicate_qa <- print_query(
  con,
  "Checking alive-at-year-end restricted-table duplicate rows.",
  paste0(
    "SELECT
       COUNT(*) - COUNT(DISTINCT
         COALESCE(patient_id::VARCHAR, '<NULL>') || '|' ||
         COALESCE(analysis_year::VARCHAR, '<NULL>')
       ) AS duplicate_restricted_rows
     FROM ",
    restricted_final_identifier,
    "
     WHERE analysis_year IN (",
    selected_year_sql,
    ")"
  )
)

if (as.numeric(duplicate_qa$duplicate_restricted_rows[[1]]) != 0) {
  stop("Alive-at-year-end restricted table contains duplicate selected-year rows.")
}

message(
  "Alive-at-year-end clinical-metrics filter complete: ",
  restricted_final_schema,
  ".",
  restricted_final_table,
  "."
)

disconnect_komodo(con)
