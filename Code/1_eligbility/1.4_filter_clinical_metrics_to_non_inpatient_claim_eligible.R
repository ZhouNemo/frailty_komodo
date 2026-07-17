source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto upstream annual eligibility restriction
# Author: Nemo Zhou
# Date started: 2026-07-10
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Apply the annual non-inpatient claim criterion before the normalized 3.x
# clinical-metrics pipeline runs. The script reads the selected years from the
# annual eligible cohort, keeps patient-years with at least one same-year row in
# `komodo_ext.non_inpatient_events`, and writes a restricted upstream
# eligibility table for `3.1_prepare_annual_metric_ids.R` to consume.
#
# The default output table is:
#   - 1_annual_eligible_cohort_non_inpatient_claim_eligible
#
# Run this after:
#   - Code/1_eligbility/1.1_build_annual_eligible_population.R
#   - Code/1_eligbility/1.2_check_annual_eligible_population.R
#   - Code/1_eligbility/1.3_join_race_ethnicity_to_eligible_cohort.R

default_non_inpatient_claim_filter_config <- list(
  analysis_years = NULL,
  source_eligibility_schema = NULL,
  source_eligibility_table = NULL,
  restricted_eligibility_schema = NULL,
  restricted_eligibility_table =
    "1_annual_eligible_cohort_non_inpatient_claim_eligible",
  non_inpatient_table = "non_inpatient_events"
)

base_config <- get_normalized_clinical_metrics_config()
filter_config <- utils::modifyList(
  default_non_inpatient_claim_filter_config,
  getOption("frailty.non_inpatient_claim_filter.config", list())
)

analysis_years <- filter_config$analysis_years
if (is.null(analysis_years)) {
  analysis_years <- base_config$analysis_years
}
analysis_years <- sort(unique(as.integer(analysis_years)))
if (
  length(analysis_years) == 0L ||
    any(is.na(analysis_years)) ||
    any(analysis_years < 2016L | analysis_years > 2025L)
) {
  stop("analysis_years must contain years from 2016 through 2025.")
}

source_eligibility_table <- filter_config$source_eligibility_table
if (is.null(source_eligibility_table) || !nzchar(source_eligibility_table)) {
  source_eligibility_table <- base_config$eligibility_table
}

source_eligibility_schema <- filter_config$source_eligibility_schema
if (is.null(source_eligibility_schema) || !nzchar(source_eligibility_schema)) {
  source_eligibility_schema <- write_schema
}

restricted_eligibility_table <- filter_config$restricted_eligibility_table
if (is.null(restricted_eligibility_table) || !nzchar(restricted_eligibility_table)) {
  stop("restricted_eligibility_table must be a nonempty table name.")
}

restricted_eligibility_schema <- filter_config$restricted_eligibility_schema
if (is.null(restricted_eligibility_schema) || !nzchar(restricted_eligibility_schema)) {
  restricted_eligibility_schema <- write_schema
}

non_inpatient_table <- filter_config$non_inpatient_table
if (is.null(non_inpatient_table) || !nzchar(non_inpatient_table)) {
  stop("non_inpatient_table must be a nonempty table name.")
}

con <- connect_komodo()

source_eligibility_identifier <- qualified_identifier(
  source_eligibility_schema,
  source_eligibility_table
)
restricted_eligibility_identifier <- qualified_identifier(
  restricted_eligibility_schema,
  restricted_eligibility_table
)
non_inpatient_identifier <- qualified_identifier(komodo_schema, non_inpatient_table)

if (!table_exists(con, source_eligibility_schema, source_eligibility_table)) {
  stop(
    "Required source eligibility table was not found: ",
    source_eligibility_schema,
    ".",
    source_eligibility_table,
    "."
  )
}
if (!table_exists(con, komodo_schema, non_inpatient_table)) {
  stop(
    "Required KRD non-inpatient source table was not found: ",
    komodo_schema,
    ".",
    non_inpatient_table,
    "."
  )
}

table_has_columns(
  con,
  source_eligibility_schema,
  source_eligibility_table,
  c("patient_id", "analysis_year")
)
table_has_columns(
  con,
  komodo_schema,
  non_inpatient_table,
  c("patient_id", "service_date")
)

source_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", source_eligibility_identifier, " LIMIT 0")
)))
source_column_sql <- paste(
  paste0("eligible.", quote_identifier(source_columns)),
  collapse = ",\n       "
)
insert_column_sql <- paste(quote_identifier(source_columns), collapse = ",\n         ")
selected_year_sql <- sql_values(analysis_years)

restricted_select_sql <- paste0(
  "SELECT
       ",
  source_column_sql,
  "
     FROM ", source_eligibility_identifier, " eligible
     WHERE eligible.analysis_year IN (", selected_year_sql, ")
       AND EXISTS (
         SELECT 1
         FROM ", non_inpatient_identifier, " nie
         WHERE nie.patient_id = eligible.patient_id
           AND nie.service_date IS NOT NULL
           AND nie.service_date >=
             TO_DATE(eligible.analysis_year::VARCHAR || '-01-01', 'YYYY-MM-DD')
           AND nie.service_date <
             TO_DATE((eligible.analysis_year + 1)::VARCHAR || '-01-01', 'YYYY-MM-DD')
       )"
)

message(
  "Applying upstream non-inpatient claim eligibility criterion to selected years: ",
  paste(analysis_years, collapse = ", "),
  "."
)
message("Source eligibility table: ", source_eligibility_schema, ".", source_eligibility_table)
message("Restricted eligibility table: ", restricted_eligibility_schema, ".", restricted_eligibility_table)

start_time <- Sys.time()
message(format(start_time, "[%Y-%m-%d %H:%M:%S] "), "START: Build restricted eligibility table.")

if (!table_exists(con, restricted_eligibility_schema, restricted_eligibility_table)) {
  execute_sql_with_retry(
    con,
    paste0(
      "CREATE TABLE ", restricted_eligibility_identifier, "
       DISTKEY(patient_id)
       SORTKEY(analysis_year, patient_id) AS
       ",
      restricted_select_sql,
      ";"
    ),
    label = "create restricted non-inpatient-claim eligibility table"
  )
} else {
  table_has_columns(
    con,
    restricted_eligibility_schema,
    restricted_eligibility_table,
    source_columns
  )

  execute_sql_with_retry(
    con,
    paste0(
      "DELETE FROM ", restricted_eligibility_identifier, "
       WHERE analysis_year IN (", selected_year_sql, ");

       INSERT INTO ", restricted_eligibility_identifier, " (
         ",
      insert_column_sql,
      "
       )
       ",
      restricted_select_sql,
      ";"
    ),
    label = "refresh restricted non-inpatient-claim eligibility table"
  )
}

end_time <- Sys.time()
message(
  format(end_time, "[%Y-%m-%d %H:%M:%S] "),
  "DONE: Build restricted eligibility table. Elapsed minutes: ",
  round(as.numeric(difftime(end_time, start_time, units = "mins")), 2),
  "."
)

retention_qa <- print_query(
  con,
  "Checking non-inpatient claim filter retention by year.",
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
       FROM ", source_eligibility_identifier, "
       WHERE analysis_year IN (", selected_year_sql, ")
       GROUP BY analysis_year
     ) source
     LEFT JOIN (
       SELECT analysis_year, COUNT(*)::BIGINT AS n_restricted_patient_years
       FROM ", restricted_eligibility_identifier, "
       WHERE analysis_year IN (", selected_year_sql, ")
       GROUP BY analysis_year
     ) restricted
       ON source.analysis_year = restricted.analysis_year
     ORDER BY source.analysis_year"
  )
)

if (
  nrow(retention_qa) != length(analysis_years) ||
    any(is.na(retention_qa$n_restricted_patient_years)) ||
    any(retention_qa$n_restricted_patient_years > retention_qa$n_source_patient_years)
) {
  stop("Restricted eligibility retention QA failed.")
}

duplicate_qa <- print_query(
  con,
  "Checking restricted eligibility duplicate rows.",
  paste0(
    "SELECT
       COUNT(*) - COUNT(DISTINCT patient_id || '|' || analysis_year::VARCHAR)
         AS duplicate_restricted_rows
     FROM ", restricted_eligibility_identifier, "
     WHERE analysis_year IN (", selected_year_sql, ")"
  )
)

if (duplicate_qa$duplicate_restricted_rows[[1]] != 0) {
  stop("Restricted eligibility table contains duplicate selected-year rows.")
}

message(
  "Upstream non-inpatient claim eligibility filter complete: ",
  restricted_eligibility_schema,
  ".",
  restricted_eligibility_table,
  "."
)

disconnect_komodo(con)
