source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto upstream annual geography attribution
# Author: Nemo Zhou
# Date started: 2026-07-10
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Join annual patient geography onto the upstream restricted annual eligibility
# table before the normalized 3.x clinical-metrics pipeline runs. For each
# patient-year, this script selects the `patient_zip` from
# `komodo_ext.patient_geography` where the patient spent the longest clipped
# time inside the calendar analysis year. Ties are resolved deterministically by
# the latest observed span end date, earliest span start date, ZIP, and state.
#
# The default target table updated in place is:
#   - 1_annual_eligible_cohort_non_inpatient_claim_eligible
#
# Run this after:
#   - Code/1_eligbility/1.4_filter_clinical_metrics_to_non_inpatient_claim_eligible.R

default_patient_geography_join_config <- list(
  analysis_years = 2016L,
  source_eligibility_table =
    "1_annual_eligible_cohort_non_inpatient_claim_eligible",
  source_eligibility_schema = NULL,
  geography_table = "patient_geography"
)

geography_config <- utils::modifyList(
  default_patient_geography_join_config,
  getOption("frailty.patient_geography_join.config", list())
)

analysis_years <- sort(unique(as.integer(geography_config$analysis_years)))
if (
  length(analysis_years) == 0L ||
    any(is.na(analysis_years)) ||
    any(analysis_years < 2016L | analysis_years > 2025L)
) {
  stop("analysis_years must contain years from 2016 through 2025.")
}

source_eligibility_table <- geography_config$source_eligibility_table
if (is.null(source_eligibility_table) || !nzchar(source_eligibility_table)) {
  stop("source_eligibility_table must be a nonempty table name.")
}

source_eligibility_schema <- geography_config$source_eligibility_schema
if (is.null(source_eligibility_schema) || !nzchar(source_eligibility_schema)) {
  source_eligibility_schema <- write_schema
}

geography_table <- geography_config$geography_table
if (is.null(geography_table) || !nzchar(geography_table)) {
  stop("geography_table must be a nonempty table name.")
}

con <- connect_komodo()

source_eligibility_identifier <- qualified_identifier(
  source_eligibility_schema,
  source_eligibility_table
)
geography_identifier <- qualified_identifier(komodo_schema, geography_table)

if (!table_exists(con, source_eligibility_schema, source_eligibility_table)) {
  stop(
    "Required source eligibility table was not found: ",
    source_eligibility_schema,
    ".",
    source_eligibility_table,
    ". Run Code/1_eligbility/1.4_filter_clinical_metrics_to_non_inpatient_claim_eligible.R first."
  )
}

table_has_columns(
  con,
  source_eligibility_schema,
  source_eligibility_table,
  c("patient_id", "analysis_year")
)

if (!table_exists(con, komodo_schema, geography_table)) {
  stop(
    "Required KRD geography table was not found: ",
    komodo_schema,
    ".",
    geography_table,
    "."
  )
}

table_has_columns(
  con,
  komodo_schema,
  geography_table,
  c("patient_id", "valid_from_date", "valid_to_date", "patient_state", "patient_zip")
)

geography_column_types <- c(
  patient_state = "VARCHAR(32)",
  patient_zip = "VARCHAR(32)",
  geography_days_in_year = "INTEGER",
  geography_valid_from_date = "DATE",
  geography_valid_to_date = "DATE"
)

source_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", source_eligibility_identifier, " LIMIT 0")
)))

missing_geography_columns <- setdiff(names(geography_column_types), source_columns)
if (length(missing_geography_columns) > 0L) {
  message(
    "Adding geography columns to ",
    source_eligibility_schema,
    ".",
    source_eligibility_table,
    ": ",
    paste(missing_geography_columns, collapse = ", "),
    "."
  )
  for (column in missing_geography_columns) {
    DatabaseConnector::executeSql(
      con,
      paste0(
        "ALTER TABLE ",
        source_eligibility_identifier,
        " ADD COLUMN ",
        quote_identifier(column),
        " ",
        geography_column_types[[column]],
        ";"
      ),
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  }
}

table_has_columns(
  con,
  source_eligibility_schema,
  source_eligibility_table,
  names(geography_column_types)
)

selected_year_sql <- sql_values(analysis_years)

selected_count_before <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT analysis_year, COUNT(*)::BIGINT AS n_patient_years
     FROM ", source_eligibility_identifier, "
     WHERE analysis_year IN (", selected_year_sql, ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

geography_update_sql <- paste0(
  "UPDATE ", source_eligibility_identifier, " AS target
   SET
     patient_state = geography.patient_state,
     patient_zip = geography.patient_zip,
     geography_days_in_year = geography.geography_days_in_year,
     geography_valid_from_date = geography.geography_valid_from_date,
     geography_valid_to_date = geography.geography_valid_to_date
   FROM (
     WITH source_rows AS (
       SELECT
         patient_id,
         analysis_year,
         TO_DATE(analysis_year::VARCHAR || '-01-01', 'YYYY-MM-DD')
           AS analysis_start_date,
         TO_DATE((analysis_year + 1)::VARCHAR || '-01-01', 'YYYY-MM-DD') - 1
           AS analysis_end_date
       FROM ", source_eligibility_identifier, "
       WHERE analysis_year IN (", selected_year_sql, ")
     ),
     geography_overlaps AS (
       SELECT
         eligible.patient_id,
         eligible.analysis_year,
         NULLIF(TRIM(pg.patient_state), '') AS patient_state,
         NULLIF(TRIM(pg.patient_zip), '') AS patient_zip,
         GREATEST(CAST(pg.valid_from_date AS DATE), eligible.analysis_start_date)
           AS clipped_start_date,
         LEAST(
           CAST(COALESCE(pg.valid_to_date, eligible.analysis_end_date) AS DATE),
           eligible.analysis_end_date
         ) AS clipped_end_date
       FROM source_rows eligible
       INNER JOIN ", geography_identifier, " pg
         ON pg.patient_id = eligible.patient_id
        AND pg.valid_from_date IS NOT NULL
        AND pg.valid_from_date <= eligible.analysis_end_date
        AND COALESCE(pg.valid_to_date, eligible.analysis_end_date) >=
          eligible.analysis_start_date
        AND NULLIF(TRIM(pg.patient_zip), '') IS NOT NULL
     ),
     geography_days AS (
       SELECT
         patient_id,
         analysis_year,
         patient_state,
         patient_zip,
         MIN(clipped_start_date) AS geography_valid_from_date,
         MAX(clipped_end_date) AS geography_valid_to_date,
         SUM(DATEDIFF(day, clipped_start_date, clipped_end_date) + 1)::INTEGER
           AS geography_days_in_year
       FROM geography_overlaps
       WHERE clipped_start_date <= clipped_end_date
       GROUP BY patient_id, analysis_year, patient_state, patient_zip
     ),
     geography_ranked AS (
       SELECT
         *,
         ROW_NUMBER() OVER (
           PARTITION BY patient_id, analysis_year
           ORDER BY
             geography_days_in_year DESC,
             geography_valid_to_date DESC,
             geography_valid_from_date ASC,
             patient_zip ASC,
             patient_state ASC
         ) AS geography_rank
       FROM geography_days
     )
     SELECT
       eligible.patient_id,
       eligible.analysis_year,
       geography.patient_state,
       geography.patient_zip,
       geography.geography_days_in_year,
       geography.geography_valid_from_date,
       geography.geography_valid_to_date
     FROM source_rows eligible
     LEFT JOIN geography_ranked geography
       ON eligible.patient_id = geography.patient_id
      AND eligible.analysis_year = geography.analysis_year
      AND geography.geography_rank = 1
   ) geography
   WHERE target.patient_id = geography.patient_id
     AND target.analysis_year = geography.analysis_year
     AND target.analysis_year IN (", selected_year_sql, ");"
)

geography_match_count_sql <- paste0(
  "WITH source_rows AS (
       SELECT
         patient_id,
         analysis_year,
         TO_DATE(analysis_year::VARCHAR || '-01-01', 'YYYY-MM-DD')
           AS analysis_start_date,
         TO_DATE((analysis_year + 1)::VARCHAR || '-01-01', 'YYYY-MM-DD') - 1
           AS analysis_end_date
       FROM ", source_eligibility_identifier, "
       WHERE analysis_year IN (", selected_year_sql, ")
     ),
     geography_overlaps AS (
       SELECT
         eligible.patient_id,
         eligible.analysis_year,
         NULLIF(TRIM(pg.patient_state), '') AS patient_state,
         NULLIF(TRIM(pg.patient_zip), '') AS patient_zip,
         GREATEST(CAST(pg.valid_from_date AS DATE), eligible.analysis_start_date)
           AS clipped_start_date,
         LEAST(
           CAST(COALESCE(pg.valid_to_date, eligible.analysis_end_date) AS DATE),
           eligible.analysis_end_date
         ) AS clipped_end_date
       FROM source_rows eligible
       INNER JOIN ", geography_identifier, " pg
         ON pg.patient_id = eligible.patient_id
        AND pg.valid_from_date IS NOT NULL
        AND pg.valid_from_date <= eligible.analysis_end_date
        AND COALESCE(pg.valid_to_date, eligible.analysis_end_date) >=
          eligible.analysis_start_date
        AND NULLIF(TRIM(pg.patient_zip), '') IS NOT NULL
     ),
     geography_days AS (
       SELECT
         patient_id,
         analysis_year,
         patient_state,
         patient_zip,
         MIN(clipped_start_date) AS geography_valid_from_date,
         MAX(clipped_end_date) AS geography_valid_to_date,
         SUM(DATEDIFF(day, clipped_start_date, clipped_end_date) + 1)::INTEGER
           AS geography_days_in_year
       FROM geography_overlaps
       WHERE clipped_start_date <= clipped_end_date
       GROUP BY patient_id, analysis_year, patient_state, patient_zip
     ),
     geography_ranked AS (
       SELECT
         *,
         ROW_NUMBER() OVER (
           PARTITION BY patient_id, analysis_year
           ORDER BY
             geography_days_in_year DESC,
             geography_valid_to_date DESC,
             geography_valid_from_date ASC,
             patient_zip ASC,
             patient_state ASC
         ) AS geography_rank
       FROM geography_days
     )
     SELECT
       eligible.analysis_year,
       COUNT(*)::BIGINT AS n_patient_years,
       SUM(CASE WHEN geography.patient_zip IS NOT NULL THEN 1 ELSE 0 END)::BIGINT
         AS n_with_patient_zip,
       SUM(CASE WHEN geography.patient_zip IS NULL THEN 1 ELSE 0 END)::BIGINT
         AS n_missing_patient_zip
     FROM source_rows eligible
     LEFT JOIN geography_ranked geography
       ON eligible.patient_id = geography.patient_id
      AND eligible.analysis_year = geography.analysis_year
      AND geography.geography_rank = 1
     GROUP BY eligible.analysis_year
     ORDER BY eligible.analysis_year"
)

message(
  "Joining patient geography to selected years: ",
  paste(analysis_years, collapse = ", "),
  "."
)
message("Source eligibility table: ", source_eligibility_schema, ".", source_eligibility_table)

start_time <- Sys.time()
message(format(start_time, "[%Y-%m-%d %H:%M:%S] "), "START: Update patient geography columns.")

execute_sql_with_retry(
  con,
  geography_update_sql,
  label = "update annual patient geography columns"
)

end_time <- Sys.time()
message(
  format(end_time, "[%Y-%m-%d %H:%M:%S] "),
  "DONE: Update patient geography columns. Elapsed minutes: ",
  round(as.numeric(difftime(end_time, start_time, units = "mins")), 2),
  "."
)

row_count_qa <- print_query(
  con,
  "Checking geography update row counts.",
  paste0(
    "SELECT
       analysis_year,
       COUNT(*)::BIGINT AS n_patient_years
     FROM ", source_eligibility_identifier, "
     WHERE analysis_year IN (", selected_year_sql, ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

if (
  nrow(row_count_qa) != length(analysis_years) ||
    any(is.na(row_count_qa$n_patient_years)) ||
    !identical(
      as.integer(row_count_qa$n_patient_years),
      as.integer(selected_count_before$n_patient_years)
    )
) {
  stop("Geography update row-count QA failed.")
}

duplicate_qa <- print_query(
  con,
  "Checking geography-updated eligibility duplicate rows.",
  paste0(
    "SELECT
       COUNT(*) - COUNT(DISTINCT patient_id || '|' || analysis_year::VARCHAR)
         AS duplicate_geography_rows
     FROM ", source_eligibility_identifier, "
     WHERE analysis_year IN (", selected_year_sql, ")"
  )
)

if (duplicate_qa$duplicate_geography_rows[[1]] != 0) {
  stop("Geography-updated eligibility table contains duplicate selected-year rows.")
}

expected_zip_qa <- print_query(
  con,
  "Checking expected patient ZIP coverage by year.",
  geography_match_count_sql
)

actual_zip_qa <- print_query(
  con,
  "Checking updated patient ZIP coverage by year.",
  paste0(
    "SELECT
       analysis_year,
       COUNT(*)::BIGINT AS n_patient_years,
       SUM(CASE WHEN patient_zip IS NOT NULL THEN 1 ELSE 0 END)::BIGINT
         AS n_with_patient_zip,
       SUM(CASE WHEN patient_zip IS NULL THEN 1 ELSE 0 END)::BIGINT
         AS n_missing_patient_zip
     FROM ", source_eligibility_identifier, "
     WHERE analysis_year IN (", selected_year_sql, ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

if (
  !identical(
    as.integer(actual_zip_qa$n_with_patient_zip),
    as.integer(expected_zip_qa$n_with_patient_zip)
  )
) {
  stop("Updated patient ZIP coverage does not match expected geography attribution.")
}

message(
  "Patient geography columns updated in ",
  source_eligibility_schema,
  ".",
  source_eligibility_table,
  "."
)

disconnect_komodo(con)
