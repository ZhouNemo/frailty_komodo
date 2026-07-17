source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual clinical metrics
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Build compact annual HIV diagnosis evidence from
# komodo_ext.normalized_dx_events. HIV is the only current clinical metric that
# needs event date and inpatient/non-inpatient setting after code matching. This
# script retains only HIV diagnosis evidence and writes selected years to:
#   - 2_annual_hiv_diagnosis_evidence

config <- get_normalized_clinical_metrics_config()
con <- connect_komodo()
# Do NOT register on.exit(disconnect_komodo(con)) here. At the top level of a
# source()d script, on.exit() fires early and closes the connection before the
# script can query it. The connection is disconnected explicitly at the end.

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
dx_identifier <- qualified_identifier(komodo_schema, config$normalized_dx_table)
hiv_evidence_identifier <- qualified_identifier(
  write_schema,
  config$hiv_evidence_table
)
hiv_lookup_path <- file.path(config$lookup_dir, "0.6_hiv_diagnosis_lookup.csv")
hiv_stage_table <- "clinical_metric_normalized_hiv_lookup_stage"
hiv_stage_identifier <- quote_identifier(hiv_stage_table)

if (!file.exists(hiv_lookup_path)) {
  stop("Missing HIV diagnosis lookup: ", hiv_lookup_path)
}

if (!table_exists(con, write_schema, config$ids_table)) {
  stop("Required denominator table was not found: ", write_schema, ".", config$ids_table)
}
if (!table_exists(con, komodo_schema, config$normalized_dx_table)) {
  stop("Required normalized diagnosis table was not found: ", komodo_schema, ".", config$normalized_dx_table)
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
  c("patient_id", "event_date", "source_table", "dx_code")
)

hiv_lookup <- read_lookup_csv(hiv_lookup_path)
require_columns(
  hiv_lookup,
  c("lookup_version", "metric", "feature_id", "feature_name", "code_system", "match_value", "match_type"),
  "HIV diagnosis lookup"
)

hiv_lookup$metric <- toupper(hiv_lookup$metric)
hiv_lookup$match_type <- tolower(hiv_lookup$match_type)
active_hiv_lookup <- hiv_lookup[
  hiv_lookup$metric == "HIV" &
    hiv_lookup$match_type == "exact" &
    !is.na(hiv_lookup$match_value) &
    hiv_lookup$match_value != "",
  c("lookup_version", "metric", "feature_id", "feature_name", "match_value", "match_type")
]

if (nrow(active_hiv_lookup) == 0L) {
  stop("No active HIV exact diagnosis lookup rows were found.")
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", hiv_stage_identifier, ";
     CREATE TEMP TABLE ", hiv_stage_identifier, " (
       lookup_version VARCHAR(128) NOT NULL,
       metric VARCHAR(32) NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       feature_name VARCHAR(256) NOT NULL,
       match_value VARCHAR(64) NOT NULL,
       match_type VARCHAR(32) NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(match_value);"
  )
)

execute_insert_batches(
  con,
  hiv_stage_identifier,
  c("lookup_version", "metric", "feature_id", "feature_name", "match_value", "match_type"),
  active_hiv_lookup
)

if (!table_exists(con, write_schema, config$hiv_evidence_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", hiv_evidence_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         diagnosis_date DATE NOT NULL,
         claim_setting VARCHAR(40) NOT NULL,
         dx_code VARCHAR(64) NOT NULL,
         metric VARCHAR(32) NOT NULL,
         feature_id VARCHAR(128) NOT NULL,
         feature_name VARCHAR(256) NOT NULL,
         match_type VARCHAR(32) NOT NULL,
         lookup_version VARCHAR(128) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid, diagnosis_date);"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$hiv_evidence_table,
  c(
    "patid",
    "analysis_year",
    "diagnosis_date",
    "claim_setting",
    "dx_code",
    "metric",
    "feature_id",
    "feature_name",
    "match_type",
    "lookup_version"
  )
)

dx_window_sql <- event_window_sql(config, "ids", "dx.event_date")

execute_sql_with_retry(
  con,
  paste0(
    "DELETE FROM ", hiv_evidence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");

     INSERT INTO ", hiv_evidence_identifier, " (
       patid,
       analysis_year,
       diagnosis_date,
       claim_setting,
       dx_code,
       metric,
       feature_id,
       feature_name,
       match_type,
       lookup_version
     )
     SELECT DISTINCT
       ids.patid,
       ids.analysis_year,
       CAST(dx.event_date AS DATE) AS diagnosis_date,
       CASE
         WHEN dx.source_table = 'INPATIENT_EVENTS' THEN 'inpatient'
         WHEN dx.source_table = 'NON_INPATIENT_EVENTS' THEN 'non_inpatient'
       END AS claim_setting,
       dx.dx_code,
       l.metric,
       l.feature_id,
       l.feature_name,
       l.match_type,
       l.lookup_version
     FROM ", ids_identifier, " ids
     INNER JOIN ", dx_identifier, " dx
       ON dx.patient_id = ids.patient_id
     INNER JOIN ", hiv_stage_identifier, " l
       ON dx.dx_code = l.match_value
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
       AND ", dx_window_sql, "
       AND dx.source_table IN ('INPATIENT_EVENTS', 'NON_INPATIENT_EVENTS');"
  ),
  label = "HIV diagnosis evidence scan"
)

print_query(
  con,
  "Checking normalized HIV evidence counts.",
  paste0(
    "SELECT
       analysis_year,
       claim_setting,
       COUNT(*)::BIGINT AS evidence_rows,
       COUNT(DISTINCT patid)::BIGINT AS patient_years,
       COUNT(DISTINCT diagnosis_date)::BIGINT AS distinct_dates
     FROM ", hiv_evidence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, claim_setting
     ORDER BY analysis_year, claim_setting"
  )
)

message(
  config$workflow_label,
  " HIV evidence preparation complete: ",
  write_schema,
  ".",
  config$hiv_evidence_table,
  "."
)

# Release the Redshift connection now that the script has completed.
disconnect_komodo(con)


