source("Code/6.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual clinical metrics
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Build compact patient-year diagnosis and procedure code-presence tables from
# the cleaned normalized Komodo event tables:
#   - komodo_ext.normalized_dx_events
#   - komodo_ext.normalized_procedure_events
#
# This script does not read raw inpatient/non-inpatient event tables, does not
# flatten arrays, and does not apply the older first-25-array-elements rule. It
# writes selected years to:
#   - 2_annual_diagnosis_code_presence
#   - 2_annual_procedure_code_presence

config <- get_normalized_clinical_metrics_config()
con <- connect_komodo()

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
dx_identifier <- qualified_identifier(komodo_schema, config$normalized_dx_table)
procedure_identifier <- qualified_identifier(
  komodo_schema,
  config$normalized_procedure_table
)
diagnosis_presence_identifier <- qualified_identifier(
  write_schema,
  config$diagnosis_presence_table
)
procedure_presence_identifier <- qualified_identifier(
  write_schema,
  config$procedure_presence_table
)

if (!table_exists(con, write_schema, config$ids_table)) {
  stop("Required denominator table was not found: ", write_schema, ".", config$ids_table)
}
if (!table_exists(con, komodo_schema, config$normalized_dx_table)) {
  stop("Required normalized diagnosis table was not found: ", komodo_schema, ".", config$normalized_dx_table)
}
if (!table_exists(con, komodo_schema, config$normalized_procedure_table)) {
  stop("Required normalized procedure table was not found: ", komodo_schema, ".", config$normalized_procedure_table)
}

table_has_columns(
  con,
  write_schema,
  config$ids_table,
  c("patid", "patient_id", "analysis_year", "analysis_start_date", "analysis_end_date")
)
table_has_columns(
  con,
  komodo_schema,
  config$normalized_dx_table,
  c("patient_id", "event_date", "visit_id", "source_table", "source_field", "dx_code")
)
table_has_columns(
  con,
  komodo_schema,
  config$normalized_procedure_table,
  c("patient_id", "event_date", "visit_id", "source_table", "source_field", "procedure_code")
)

if (!table_exists(con, write_schema, config$diagnosis_presence_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", diagnosis_presence_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         dx_code VARCHAR(64) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, dx_code, patid);"
    )
  )
}

if (!table_exists(con, write_schema, config$procedure_presence_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", procedure_presence_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         procedure_code VARCHAR(64) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, procedure_code, patid);"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$diagnosis_presence_table,
  c("patid", "analysis_year", "dx_code")
)
table_has_columns(
  con,
  write_schema,
  config$procedure_presence_table,
  c("patid", "analysis_year", "procedure_code")
)

dx_window_sql <- gsub(
  "event_date",
  "CAST(dx.event_date AS DATE)",
  event_window_sql(config, "ids"),
  fixed = TRUE
)
procedure_window_sql <- gsub(
  "event_date",
  "CAST(px.event_date AS DATE)",
  event_window_sql(config, "ids"),
  fixed = TRUE
)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", diagnosis_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");
     DELETE FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");

     INSERT INTO ", diagnosis_presence_identifier, " (
       patid,
       analysis_year,
       dx_code
     )
     SELECT DISTINCT
       ids.patid,
       ids.analysis_year,
       dx.dx_code
     FROM ", ids_identifier, " ids
     INNER JOIN ", dx_identifier, " dx
       ON dx.patient_id = ids.patient_id
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
       AND ", dx_window_sql, "
       AND dx.dx_code IS NOT NULL
       AND dx.dx_code <> '';

     INSERT INTO ", procedure_presence_identifier, " (
       patid,
       analysis_year,
       procedure_code
     )
     SELECT DISTINCT
       ids.patid,
       ids.analysis_year,
       px.procedure_code
     FROM ", ids_identifier, " ids
     INNER JOIN ", procedure_identifier, " px
       ON px.patient_id = ids.patient_id
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
       AND ", procedure_window_sql, "
       AND px.procedure_code IS NOT NULL
       AND px.procedure_code <> '';"
  )
)

print_query(
  con,
  "Checking normalized code-presence row counts.",
  paste0(
    "SELECT 'diagnosis' AS code_layer, analysis_year,
       COUNT(*)::BIGINT AS rows,
       COUNT(DISTINCT patid)::BIGINT AS patient_years,
       COUNT(DISTINCT dx_code)::BIGINT AS distinct_codes
     FROM ", diagnosis_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT 'procedure' AS code_layer, analysis_year,
       COUNT(*)::BIGINT AS rows,
       COUNT(DISTINCT patid)::BIGINT AS patient_years,
       COUNT(DISTINCT procedure_code)::BIGINT AS distinct_codes
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year, code_layer"
  )
)

duplicate_qa <- print_query(
  con,
  "Checking normalized code-presence duplicate rows.",
  paste0(
    "SELECT 'diagnosis' AS code_layer,
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR || '|' || dx_code)
         AS duplicate_rows
     FROM ", diagnosis_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT 'procedure' AS code_layer,
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR || '|' || procedure_code)
         AS duplicate_rows
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")"
  )
)

if (any(duplicate_qa$duplicate_rows != 0)) {
  stop("Normalized code-presence tables contain duplicate selected-year rows.")
}

message(
  config$workflow_label,
  " code-presence preparation complete: ",
  paste(
    paste0(write_schema, ".", c(
      config$diagnosis_presence_table,
      config$procedure_presence_table
    )),
    collapse = ", "
  ),
  "."
)
