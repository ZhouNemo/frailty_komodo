source("Code/2_variable construction/6.0_annual_healthcare_utilization_helpers.R")

# Project: Frailty_Komoto healthcare utilization
# Author: Nemo Zhou
# Date started: 2026-07-17
# Date last updated: 2026-08-04
#
# ---- Purpose ----
# Append the completed 2022 healthcare-utilization patient-year variables to a
# new derivative of the current clinical-metrics table for every eligible
# patient-year. The source clinical-metrics table is never modified.

config <- get_annual_healthcare_utilization_config()
con <- connect_komodo()

for (item in list(
  list(schema = write_schema, table = config$cohort_table, columns = c(
    "patient_id", "analysis_year", "mx_insurance_group", "mx_insurance_segment",
    "age", "patient_gender", "patient_race_ethnicity"
  )),
  list(schema = write_schema, table = config$metrics_table, columns = c(
    "patient_id", "analysis_year", utilization_metric_columns()
  ))
)) {
  if (!table_exists(con, item$schema, item$table)) {
    stop("Required table was not found: ", item$schema, ".", item$table)
  }
  table_has_columns(con, item$schema, item$table, item$columns)
}

cohort_identifier <- qualified_identifier(write_schema, config$cohort_table)
metrics_identifier <- qualified_identifier(write_schema, config$metrics_table)
combined_identifier <- qualified_identifier(write_schema, config$combined_table)
metric_columns <- utilization_metric_columns()
main_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", cohort_identifier, " LIMIT 0")
)))

overlapping_metric_columns <- intersect(main_columns, metric_columns)
if (length(overlapping_metric_columns) > 0L) {
  stop(
    "The source clinical-metrics table already contains utilization columns: ",
    paste(overlapping_metric_columns, collapse = ", "), "."
  )
}

cohort_sql <- utilization_cohort_sql(config)
cohort_count <- DBI::dbGetQuery(
  con,
  paste0("SELECT COUNT(*)::BIGINT AS n_cohort_rows FROM (", cohort_sql, ") cohort")
)$n_cohort_rows[[1]]
metric_completeness <- DBI::dbGetQuery(
  con,
  paste0(
    "WITH cohort AS (\n", cohort_sql, "\n)\n",
    "SELECT\n",
    "  COUNT(*)::BIGINT AS n_cohort_rows,\n",
    "  COUNT(metrics.patient_id)::BIGINT AS n_metric_rows,\n",
    "  SUM(CASE WHEN metrics.patient_id IS NULL THEN 1 ELSE 0 END)::BIGINT AS missing_metric_rows\n",
    "FROM cohort\n",
    "LEFT JOIN ", metrics_identifier, " metrics\n",
    "  ON cohort.patient_id = metrics.patient_id\n",
    " AND cohort.analysis_year = metrics.analysis_year"
  )
)
if (metric_completeness$missing_metric_rows[[1]] != 0) {
  stop("Healthcare utilization metrics are incomplete for the eligible 2022 cohort.")
}

main_select <- paste0("main.", quote_identifier(main_columns), collapse = ",\n       ")
metric_select <- paste0("metrics.", quote_identifier(metric_columns), collapse = ",\n       ")
combined_select_sql <- paste0(
  "SELECT\n       ", main_select, ",\n       ", metric_select, "\n",
  "FROM ", cohort_identifier, " main\n",
  "INNER JOIN ", metrics_identifier, " metrics\n",
  "  ON main.patient_id = metrics.patient_id\n",
  " AND main.analysis_year = metrics.analysis_year\n",
  "WHERE main.analysis_year = ", config$analysis_year
)

if (table_exists(con, write_schema, config$combined_table)) {
  existing_combined_columns <- tolower(names(DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", combined_identifier, " LIMIT 0")
  )))
  expected_combined_columns <- c("patient_id", "analysis_year", main_columns, metric_columns)
  legacy_combined_columns <- intersect(
    c(
      "ed_any_visit", "ed_n_visits", "ed_total_duration_days",
      "ed_duration_n", "ed_duration_missing_n"
    ),
    existing_combined_columns
  )
  missing_combined_columns <- setdiff(
    expected_combined_columns,
    existing_combined_columns
  )
  if (length(legacy_combined_columns) > 0L || length(missing_combined_columns) > 0L) {
    message(
      "Recreating active combined table for the three-definition ED schema."
    )
    DatabaseConnector::executeSql(
      con,
      paste0("DROP TABLE IF EXISTS ", combined_identifier, ";")
    )
  }
}

if (!table_exists(con, write_schema, config$combined_table)) {
  utilization_run_sql_stage(
    con,
    "create combined 2022 clinical metrics and utilization table",
    paste0(
      "CREATE TABLE ", combined_identifier,
      " DISTKEY(patient_id) SORTKEY(analysis_year, patient_id) AS\n",
      combined_select_sql, "\nAND 1 = 0;"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$combined_table,
  c("patient_id", "analysis_year", metric_columns)
)

combined_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", combined_identifier, " LIMIT 0")
)))
insert_columns <- c(main_columns, metric_columns)
missing_combined_columns <- setdiff(insert_columns, combined_columns)
if (length(missing_combined_columns) > 0L) {
  stop(
    "Combined utilization table is missing expected columns: ",
    paste(missing_combined_columns, collapse = ", ")
  )
}

utilization_run_sql_stage(
  con,
  "refresh combined 2022 clinical metrics and utilization table",
  paste0(
    "DELETE FROM ", combined_identifier, "\n",
    "WHERE analysis_year = ", config$analysis_year, ";\n\n",
    "INSERT INTO ", combined_identifier, " (\n  ",
    paste(quote_identifier(insert_columns), collapse = ",\n  "), "\n)\n",
    combined_select_sql, ";"
  )
)

combined_check <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT\n",
    "  COUNT(*)::BIGINT AS n_rows,\n",
    "  COUNT(*) - COUNT(DISTINCT patient_id || '|' || analysis_year::VARCHAR)\n",
    "    AS duplicate_patient_year_rows\n",
    "FROM ", combined_identifier, "\n",
    "WHERE analysis_year = ", config$analysis_year
  )
)
if (combined_check$duplicate_patient_year_rows[[1]] != 0) {
  stop("Combined clinical metrics and utilization table contains duplicate patient-year keys.")
}
if (combined_check$n_rows[[1]] != cohort_count) {
  stop("Combined table row count does not equal the eligible 2022 cohort.")
}

message("Combined 2022 clinical metrics and utilization table complete.")
disconnect_komodo(con)
