source("Code/6.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual clinical metrics
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Prepare or validate the shared patient-year denominator for the normalized
# clinical-metrics pipeline. The script writes selected years to:
#   - 2_annual_metric_ids
#
# This denominator is the only patient-year entry point for normalized diagnosis
# and procedure event matching. Patient-level rows remain in Redshift and only
# aggregate integrity checks are printed.

config <- get_normalized_clinical_metrics_config()
con <- connect_komodo()

eligibility_identifier <- qualified_identifier(write_schema, config$eligibility_table)
ids_identifier <- qualified_identifier(write_schema, config$ids_table)

if (!table_exists(con, write_schema, config$eligibility_table)) {
  stop(
    "Required eligibility table was not found: ",
    write_schema,
    ".",
    config$eligibility_table
  )
}

table_has_columns(
  con,
  write_schema,
  config$eligibility_table,
  c(
    "patient_id",
    "analysis_year",
    "index_date",
    "age",
    "patient_gender",
    "mx_insurance_group",
    "mx_insurance_segment",
    "mx_secondary_insurance_group",
    "mx_secondary_insurance_segment",
    "rx_insurance_group",
    "rx_insurance_segment",
    "rx_secondary_insurance_group",
    "rx_secondary_insurance_segment"
  )
)

eligibility_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", eligibility_identifier, " LIMIT 0")
)))

race_select <- if ("patient_race_ethnicity" %in% eligibility_columns) {
  "e.patient_race_ethnicity"
} else {
  "CAST(NULL AS VARCHAR(128))"
}

if (!table_exists(con, write_schema, config$ids_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", ids_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         eligibility_index_date DATE,
         analysis_start_date DATE NOT NULL,
         analysis_end_date DATE NOT NULL,
         age INTEGER,
         patient_gender VARCHAR(64),
         patient_race_ethnicity VARCHAR(128),
         mx_insurance_group VARCHAR(128),
         mx_insurance_segment VARCHAR(128),
         mx_secondary_insurance_group VARCHAR(128),
         mx_secondary_insurance_segment VARCHAR(128),
         rx_insurance_group VARCHAR(128),
         rx_insurance_segment VARCHAR(128),
         rx_secondary_insurance_group VARCHAR(128),
         rx_secondary_insurance_segment VARCHAR(128)
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid);"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$ids_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "eligibility_index_date",
    "analysis_start_date",
    "analysis_end_date",
    "age",
    "patient_gender",
    "patient_race_ethnicity"
  )
)

eligible_year_counts <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT analysis_year, COUNT(*)::BIGINT AS n_person_years
     FROM ", eligibility_identifier, "
     WHERE analysis_year IN (", sql_values(config$id_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

missing_years <- setdiff(config$id_years, eligible_year_counts$analysis_year)
if (length(missing_years) > 0L) {
  stop(
    "No eligible patient-years were found for: ",
    paste(missing_years, collapse = ", "),
    "."
  )
}

print(eligible_year_counts)

if (isTRUE(config$refresh_metric_ids)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "DELETE FROM ", ids_identifier, "
       WHERE analysis_year IN (", sql_values(config$id_years), ");

       INSERT INTO ", ids_identifier, " (
         patid,
         patient_id,
         analysis_year,
         eligibility_index_date,
         analysis_start_date,
         analysis_end_date,
         age,
         patient_gender,
         patient_race_ethnicity,
         mx_insurance_group,
         mx_insurance_segment,
         mx_secondary_insurance_group,
         mx_secondary_insurance_segment,
         rx_insurance_group,
         rx_insurance_segment,
         rx_secondary_insurance_group,
         rx_secondary_insurance_segment
       )
       SELECT DISTINCT
         e.patient_id || '_' || e.analysis_year::VARCHAR AS patid,
         e.patient_id,
         e.analysis_year,
         CAST(e.index_date AS DATE) AS eligibility_index_date,
         TO_DATE(e.analysis_year::VARCHAR || '-01-01', 'YYYY-MM-DD')
           AS analysis_start_date,
         TO_DATE((e.analysis_year + 1)::VARCHAR || '-01-01', 'YYYY-MM-DD') - 1
           AS analysis_end_date,
         e.age,
         e.patient_gender,
         ", race_select, " AS patient_race_ethnicity,
         e.mx_insurance_group,
         e.mx_insurance_segment,
         e.mx_secondary_insurance_group,
         e.mx_secondary_insurance_segment,
         e.rx_insurance_group,
         e.rx_insurance_segment,
         e.rx_secondary_insurance_group,
         e.rx_secondary_insurance_segment
       FROM ", eligibility_identifier, " e
       WHERE e.analysis_year IN (", sql_values(config$id_years), ")
         AND e.patient_id IS NOT NULL;"
    )
  )
} else {
  message(
    "Reusing existing ",
    config$ids_table,
    " rows for selected years: ",
    paste(config$id_years, collapse = ", "),
    "."
  )
}

id_integrity <- print_query(
  con,
  "Checking normalized metric ID integrity.",
  paste0(
    "SELECT
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_distinct_patid,
       SUM(CASE WHEN patid IS NULL OR patid = ''
         THEN 1 ELSE 0 END)::BIGINT AS missing_patid
     FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(config$id_years), ")"
  )
)

if (
  id_integrity$n_rows[[1]] == 0 ||
    id_integrity$n_rows[[1]] != id_integrity$n_distinct_patid[[1]] ||
    id_integrity$missing_patid[[1]] != 0
) {
  stop("The normalized metric ID table failed its integrity check.")
}

print_query(
  con,
  "Checking normalized metric ID year coverage.",
  paste0(
    "SELECT analysis_year, COUNT(*)::BIGINT AS n_rows
     FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(config$id_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

message(
  config$workflow_label,
  " metric ID preparation complete: ",
  write_schema,
  ".",
  config$ids_table,
  "."
)
