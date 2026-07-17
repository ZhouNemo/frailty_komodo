# Project: Frailty_Komoto 2022 clinical-metrics analysis preparation
# Author: Nemo Zhou
# Date started: 2026-07-16
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Restrict the completed year-end-survivor clinical-metrics table to the
# project's fixed 2022 analysis year. The script reads:
#   - 6_annual_clinical_metrics_shared_alive_at_year_end
#
# and writes:
#   - 6_annual_clinical_metrics_shared_2022
#
# This is a post-final-table filter. It does not rebuild the annual denominator
# or recalculate CFI, CCW, Gagne, or HIV metric values. The output table is
# refreshed as a 2022-only table on each run.
#
# Run this after:
#   - Code/1_eligbility/1.7_filter_clinical_metrics_to_alive_at_year_end.R

source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

default_2022_filter_config <- list(
  analysis_year = 2022L,
  source_final_schema = NULL,
  source_final_table = "6_annual_clinical_metrics_shared_alive_at_year_end",
  restricted_final_schema = NULL,
  restricted_final_table = "6_annual_clinical_metrics_shared_2022"
)

filter_config <- utils::modifyList(
  default_2022_filter_config,
  getOption("frailty.clinical_metrics_2022_filter.config", list())
)

analysis_year <- as.integer(filter_config$analysis_year)
if (length(analysis_year) != 1L || is.na(analysis_year) || analysis_year != 2022L) {
  stop(
    "This script is fixed to the project's 2022 analysis year. Set ",
    "analysis_year to 2022."
  )
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

if (!table_exists(con, source_final_schema, source_final_table)) {
  stop(
    "Required source table was not found: ",
    source_final_schema,
    ".",
    source_final_table,
    "."
  )
}

table_has_columns(
  con,
  source_final_schema,
  source_final_table,
  c("patient_id", "analysis_year")
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

restricted_select_sql <- paste0(
  "SELECT
       ",
  source_column_sql,
  "
     FROM ",
  source_final_identifier,
  " clinical
     WHERE clinical.analysis_year = ",
  sql_values(analysis_year),
  ";"
)

message("Filtering year-end-survivor clinical metrics to analysis year 2022.")
message("Source clinical-metrics table: ", source_final_schema, ".", source_final_table)
message(
  "2022 clinical-metrics table: ",
  restricted_final_schema,
  ".",
  restricted_final_table
)

start_time <- Sys.time()
message(
  format(start_time, "[%Y-%m-%d %H:%M:%S] "),
  "START: Build 2022 clinical-metrics table."
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
      restricted_select_sql
    ),
    label = "create 2022 clinical-metrics table"
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
      ";

       INSERT INTO ",
      restricted_final_identifier,
      " (
         ",
      insert_column_sql,
      "
       )
       ",
      restricted_select_sql
    ),
    label = "refresh 2022 clinical-metrics table"
  )
}

end_time <- Sys.time()
message(
  format(end_time, "[%Y-%m-%d %H:%M:%S] "),
  "DONE: Build 2022 clinical-metrics table. Elapsed minutes: ",
  round(as.numeric(difftime(end_time, start_time, units = "mins")), 2),
  "."
)

source_qa <- print_query(
  con,
  "Checking source rows for analysis year 2022.",
  paste0(
    "SELECT COUNT(*)::BIGINT AS n_source_rows
     FROM ",
    source_final_identifier,
    "
     WHERE analysis_year = ",
    sql_values(analysis_year)
  )
)

output_qa <- print_query(
  con,
  "Checking 2022 output rows.",
  paste0(
    "SELECT COUNT(*)::BIGINT AS n_output_rows
     FROM ",
    restricted_final_identifier
  )
)

year_qa <- print_query(
  con,
  "Checking that the output contains only analysis year 2022.",
  paste0(
    "SELECT
       SUM(CASE WHEN analysis_year <> 2022 THEN 1 ELSE 0 END)::BIGINT
         AS n_non_2022_rows
     FROM ",
    restricted_final_identifier
  )
)

duplicate_qa <- print_query(
  con,
  "Checking 2022 output patient-year duplicate rows.",
  paste0(
    "SELECT
       COUNT(*) - COUNT(DISTINCT
         COALESCE(patient_id::VARCHAR, '<NULL>') || '|' ||
         COALESCE(analysis_year::VARCHAR, '<NULL>')
       ) AS duplicate_patient_year_rows
     FROM ",
    restricted_final_identifier
  )
)

if (
  as.numeric(source_qa$n_source_rows[[1]]) !=
    as.numeric(output_qa$n_output_rows[[1]])
) {
  stop("2022 output row count does not match the selected source row count.")
}

if (
  as.numeric(year_qa$n_non_2022_rows[[1]]) != 0 ||
    as.numeric(duplicate_qa$duplicate_patient_year_rows[[1]]) != 0
) {
  stop("2022 clinical-metrics output QA failed.")
}

message(
  "2022 clinical-metrics filter complete: ",
  restricted_final_schema,
  ".",
  restricted_final_table,
  "."
)

disconnect_komodo(con)
