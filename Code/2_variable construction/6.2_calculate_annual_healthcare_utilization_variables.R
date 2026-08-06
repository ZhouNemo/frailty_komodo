source("Code/2_variable construction/6.0_annual_healthcare_utilization_helpers.R")

# Project: Frailty_Komoto healthcare utilization
# Author: Nemo Zhou
# Date started: 2026-07-17
# Date last updated: 2026-08-04
#
# ---- Purpose ----
# Construct one row of 2022 healthcare-utilization variables per eligible
# patient-year. Visit counts use the prepared event
# table; duration totals use only nonmissing, nonnegative prepared durations.

config <- get_annual_healthcare_utilization_config()
con <- connect_komodo()

for (item in list(
  list(schema = write_schema, table = config$cohort_table, columns = c(
    "patient_id", "analysis_year", "mx_insurance_group", "mx_insurance_segment"
  )),
  list(schema = write_schema, table = config$events_table, columns = c(
    "patient_id", "analysis_year", "utilization_category", "duration_days"
  ))
)) {
  if (!table_exists(con, item$schema, item$table)) {
    stop("Required table was not found: ", item$schema, ".", item$table)
  }
  table_has_columns(con, item$schema, item$table, item$columns)
}

metrics_identifier <- qualified_identifier(write_schema, config$metrics_table)
events_identifier <- qualified_identifier(write_schema, config$events_table)
cohort_sql <- utilization_cohort_sql(config)
metric_definitions <- paste(utilization_metric_column_definitions(), collapse = ",\n  ")
metric_select <- utilization_metric_select_sql("events")
metric_columns <- utilization_metric_columns()
insert_columns <- c("patient_id", "analysis_year", metric_columns)

if (table_exists(con, write_schema, config$metrics_table)) {
  existing_metric_columns <- tolower(names(DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", metrics_identifier, " LIMIT 0")
  )))
  legacy_metric_columns <- intersect(
    c(
      "ed_any_visit", "ed_n_visits", "ed_total_duration_days",
      "ed_duration_n", "ed_duration_missing_n"
    ),
    existing_metric_columns
  )
  missing_metric_columns <- setdiff(insert_columns, existing_metric_columns)
  if (length(legacy_metric_columns) > 0L || length(missing_metric_columns) > 0L) {
    message(
      "Recreating active utilization metrics table for the three-definition ED schema."
    )
    DatabaseConnector::executeSql(
      con,
      paste0("DROP TABLE IF EXISTS ", metrics_identifier, ";")
    )
  }
}

if (!table_exists(con, write_schema, config$metrics_table)) {
  utilization_run_sql_stage(
    con,
    "create annual healthcare utilization metrics table",
    paste0(
      "CREATE TABLE ", metrics_identifier, " (\n",
      "  patient_id VARCHAR(256) NOT NULL,\n",
      "  analysis_year INTEGER NOT NULL,\n  ", metric_definitions, "\n",
      ") DISTKEY(patient_id) SORTKEY(analysis_year, patient_id);"
    )
  )
}

table_has_columns(con, write_schema, config$metrics_table, insert_columns)

utilization_run_sql_stage(
  con,
  "refresh 2022 healthcare utilization patient-year variables",
  paste0(
    "DELETE FROM ", metrics_identifier, "\n",
    "WHERE analysis_year = ", config$analysis_year, ";\n\n",
    "INSERT INTO ", metrics_identifier, " (\n  ",
    paste(quote_identifier(insert_columns), collapse = ",\n  "), "\n)\n",
    "WITH cohort AS (\n", cohort_sql, "\n)\n",
    "SELECT\n",
    "  cohort.patient_id,\n",
    "  cohort.analysis_year,\n",
    "  ", metric_select, "\n",
    "FROM cohort\n",
    "LEFT JOIN ", events_identifier, " events\n",
    "  ON cohort.patient_id = events.patient_id\n",
    " AND cohort.analysis_year = events.analysis_year\n",
    "GROUP BY cohort.patient_id, cohort.analysis_year;"
  )
)

metric_key_check <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) - COUNT(DISTINCT patient_id || '|' || analysis_year::VARCHAR)\n",
    "  AS duplicate_patient_year_rows\n",
    "FROM ", metrics_identifier, "\n",
    "WHERE analysis_year = ", config$analysis_year
  )
)
if (metric_key_check$duplicate_patient_year_rows[[1]] != 0) {
  stop("Healthcare utilization metrics contain duplicate patient-year keys.")
}

message("Healthcare utilization patient-year variables complete.")
disconnect_komodo(con)
