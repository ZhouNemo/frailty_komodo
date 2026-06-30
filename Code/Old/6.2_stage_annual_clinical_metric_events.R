source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual event staging
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-29
#
# ---- Purpose ----
# Persist eligible annual inpatient and non-inpatient event stages for the
# optimized 6.x clinical-metrics pipeline. This pays the raw KRD event join once
# per selected year so later flattening and matching steps can be restarted
# without rescanning raw event tables. The script writes selected years to:
#   - 2_annual_inpatient_event_stage
#   - 2_annual_non_inpatient_event_stage

config <- get_optimized_clinical_metrics_config()
con <- connect_komodo()

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
inpatient_identifier <- qualified_identifier(write_schema, config$inpatient_stage_table)
non_inpatient_identifier <- qualified_identifier(
  write_schema,
  config$non_inpatient_stage_table
)

for (table in c(config$ids_table)) {
  if (!table_exists(con, write_schema, table)) {
    stop("Required table was not found: ", write_schema, ".", table)
  }
}

table_has_columns(
  con,
  write_schema,
  config$ids_table,
  c("patid", "patient_id", "analysis_year")
)
table_has_columns(
  con,
  komodo_schema,
  "inpatient_events",
  c(
    "patient_id",
    "claim_from_date",
    "admission_diagnosis_code",
    "primary_diagnosis_code",
    "secondary_diagnosis_codes",
    "cpt_hcpcs_codes"
  )
)
table_has_columns(
  con,
  komodo_schema,
  "non_inpatient_events",
  c("patient_id", "service_date", "diagnosis_codes", "procedure_code")
)

if (!table_exists(con, write_schema, config$inpatient_stage_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", inpatient_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         event_date DATE NOT NULL,
         admission_diagnosis_code VARCHAR(64),
         primary_diagnosis_code VARCHAR(64),
         secondary_diagnosis_codes VARCHAR(65535),
         cpt_hcpcs_codes VARCHAR(65535)
       )
       DISTKEY(patient_id)
       SORTKEY(analysis_year, event_date);"
    )
  )
}

if (!table_exists(con, write_schema, config$non_inpatient_stage_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", non_inpatient_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         event_date DATE NOT NULL,
         diagnosis_codes VARCHAR(65535),
         procedure_code VARCHAR(64)
       )
       DISTKEY(patient_id)
       SORTKEY(analysis_year, event_date);"
    )
  )
}

for (analysis_year in config$analysis_years) {
  event_window <- event_window_for_year(config, analysis_year)

  message(
    "Staging optimized clinical metric events for ",
    analysis_year,
    " with event window [",
    event_window$start,
    ", ",
    event_window$end,
    ")."
  )

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DELETE FROM ", inpatient_identifier, "
       WHERE analysis_year = ", analysis_year, ";
       DELETE FROM ", non_inpatient_identifier, "
       WHERE analysis_year = ", analysis_year, ";

       INSERT INTO ", inpatient_identifier, " (
         patid,
         analysis_year,
         patient_id,
         event_date,
         admission_diagnosis_code,
         primary_diagnosis_code,
         secondary_diagnosis_codes,
         cpt_hcpcs_codes
       )
       SELECT
         ids.patid,
         ids.analysis_year,
         i.patient_id,
         i.claim_from_date AS event_date,
         i.admission_diagnosis_code,
         i.primary_diagnosis_code,
         i.secondary_diagnosis_codes,
         i.cpt_hcpcs_codes
       FROM (
         SELECT patid, analysis_year, patient_id
         FROM ", ids_identifier, "
         WHERE analysis_year = ", analysis_year, "
       ) ids
       INNER JOIN ", qualified_identifier(komodo_schema, "inpatient_events"), " i
         ON ids.patient_id = i.patient_id
       WHERE i.claim_from_date >= ", sql_string(event_window$start), "::DATE
         AND i.claim_from_date < ", sql_string(event_window$end), "::DATE;

       INSERT INTO ", non_inpatient_identifier, " (
         patid,
         analysis_year,
         patient_id,
         event_date,
         diagnosis_codes,
         procedure_code
       )
       SELECT
         ids.patid,
         ids.analysis_year,
         n.patient_id,
         n.service_date AS event_date,
         n.diagnosis_codes,
         n.procedure_code
       FROM (
         SELECT patid, analysis_year, patient_id
         FROM ", ids_identifier, "
         WHERE analysis_year = ", analysis_year, "
       ) ids
       INNER JOIN ", qualified_identifier(komodo_schema, "non_inpatient_events"), " n
         ON ids.patient_id = n.patient_id
       WHERE n.service_date >= ", sql_string(event_window$start), "::DATE
         AND n.service_date < ", sql_string(event_window$end), "::DATE;"
    )
  )

  print_query(
    con,
    paste0("Checking ", analysis_year, " optimized staged event counts."),
    paste0(
      "SELECT
         'inpatient' AS source_table,
         COUNT(*)::BIGINT AS staged_rows,
         SUM(CASE WHEN event_date IS NULL THEN 1 ELSE 0 END)::BIGINT
           AS missing_event_date
       FROM ", inpatient_identifier, "
       WHERE analysis_year = ", analysis_year, "
       UNION ALL
       SELECT
         'non_inpatient' AS source_table,
         COUNT(*)::BIGINT AS staged_rows,
         SUM(CASE WHEN event_date IS NULL THEN 1 ELSE 0 END)::BIGINT
           AS missing_event_date
       FROM ", non_inpatient_identifier, "
       WHERE analysis_year = ", analysis_year, "
       ORDER BY source_table"
    )
  )
}

message(
  config$workflow_label,
  " event staging complete: ",
  paste(
    paste0(write_schema, ".", c(
      config$inpatient_stage_table,
      config$non_inpatient_stage_table
    )),
    collapse = ", "
  ),
  "."
)

