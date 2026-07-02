source("Code/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual clinical metrics
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-07-02
#
# ---- Purpose ----
# Build compact patient-year CFI-relevant procedure code presence from the
# cleaned normalized Komodo procedure event table:
#   - komodo_ext.normalized_procedure_events
#
# Diagnosis code presence is intentionally not materialized in the active
# pipeline because the all-code diagnosis table is too large for available
# Redshift disk. Diagnosis features are matched directly from
# `komodo_ext.normalized_dx_events` to lookup-filtered feature tables in
# `Code/3.6_match_annual_clinical_metric_features.R`.
#
# Procedure codes are filtered to the reviewed CFI CPT/HCPCS lookup ranges
# during extraction; this script does not persist all observed procedure codes.
# The external procedure scan uses configurable retry-wrapped chunks, defaulting
# to one chunk per year so flat, unsorted Parquet prefixes are not scanned many
# times unless finer chunks are empirically justified.
#
# This script writes selected years to:
#   - 2_annual_procedure_code_presence

config <- get_normalized_clinical_metrics_config()
con <- connect_komodo()
# Do NOT register on.exit(disconnect_komodo(con)) here. At the top level of a
# source()d script, on.exit() fires early and closes the connection before the
# script can query it. The connection is disconnected explicitly at the end.

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
procedure_identifier <- qualified_identifier(
  komodo_schema,
  config$normalized_procedure_table
)
procedure_presence_identifier <- qualified_identifier(
  write_schema,
  config$procedure_presence_table
)
procedure_lookup_path <- file.path(
  config$lookup_dir,
  "0.6_cfi_procedure_lookup.csv"
)
procedure_stage_table <- "clinical_metric_normalized_procedure_lookup_stage_6_4"
procedure_build_table <- "clinical_metric_normalized_procedure_presence_build_6_4"
procedure_stage_identifier <- quote_identifier(procedure_stage_table)
procedure_build_identifier <- quote_identifier(procedure_build_table)

if (!file.exists(procedure_lookup_path)) {
  stop("Missing CFI procedure lookup file: ", procedure_lookup_path)
}

if (!table_exists(con, write_schema, config$ids_table)) {
  stop("Required denominator table was not found: ", write_schema, ".", config$ids_table)
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
  config$normalized_procedure_table,
  c("patient_id", "event_date", "visit_id", "source_table", "source_field", "procedure_code")
)

procedure_lookup <- read_lookup_csv(procedure_lookup_path)
require_columns(
  procedure_lookup,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "code_system",
    "range_start",
    "range_end",
    "match_type"
  ),
  "CFI procedure lookup"
)

procedure_lookup$metric <- toupper(procedure_lookup$metric)
procedure_lookup$match_type <- tolower(procedure_lookup$match_type)

active_procedure_lookup <- procedure_lookup[
  procedure_lookup$code_system %in% c("CPT_HCPCS", "CPTHCPCS") &
    procedure_lookup$metric %in% "CFI" &
    procedure_lookup$match_type %in% "range" &
    !is.na(procedure_lookup$range_start) &
    procedure_lookup$range_start != "" &
    !is.na(procedure_lookup$range_end) &
    procedure_lookup$range_end != "",
  c("range_start", "range_end")
]
active_procedure_lookup <- unique(active_procedure_lookup)

if (nrow(active_procedure_lookup) == 0L) {
  stop("No active CFI CPT/HCPCS procedure lookup ranges were found.")
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", procedure_stage_identifier, ";
     CREATE TEMP TABLE ", procedure_stage_identifier, " (
       range_start VARCHAR(64) NOT NULL,
       range_end VARCHAR(64) NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(range_start, range_end);"
  )
)

execute_insert_batches(
  con,
  procedure_stage_identifier,
  c("range_start", "range_end"),
  active_procedure_lookup
)

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
  config$procedure_presence_table,
  c("patid", "analysis_year", "procedure_code")
)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", procedure_build_identifier, ";
     CREATE TEMP TABLE ", procedure_build_identifier, " (
       patid VARCHAR(256) NOT NULL,
       analysis_year INTEGER NOT NULL,
       procedure_code VARCHAR(64) NOT NULL
     )
     DISTKEY(patid)
     SORTKEY(analysis_year, procedure_code, patid);"
  )
)

procedure_scan_chunks <- event_scan_chunks(config)
for (chunk_row in seq_len(nrow(procedure_scan_chunks))) {
  chunk <- procedure_scan_chunks[chunk_row, ]
  procedure_window_sql <- event_chunk_window_sql(
    config,
    "ids",
    "px.event_date",
    chunk$chunk_start_date,
    chunk$chunk_end_date
  )
  message(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
    "Scanning CFI procedure-code presence chunk ",
    chunk$chunk_id,
    " of ",
    nrow(procedure_scan_chunks),
    " (",
    chunk$chunk_start_date,
    " to ",
    chunk$chunk_end_date,
    ")."
  )
  flush.console()
  execute_sql_with_retry(
    con,
    paste0(
      "INSERT INTO ", procedure_build_identifier, " (
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
       INNER JOIN ", procedure_stage_identifier, " l
         ON px.procedure_code >= l.range_start
        AND px.procedure_code <= l.range_end
       WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
         AND ", procedure_window_sql, "
         AND px.procedure_code IS NOT NULL
         AND px.procedure_code <> '';"
    ),
    label = paste0("CFI procedure-code presence scan chunk ", chunk$chunk_id)
  )
}

build_qa <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       COUNT(*)::BIGINT AS n_rows,
       SUM(CASE WHEN patid IS NULL OR procedure_code IS NULL THEN 1 ELSE 0 END)::BIGINT
         AS n_bad
     FROM ", procedure_build_identifier
  )
)
if (build_qa$n_bad[[1]] != 0) {
  stop("Procedure-code presence build stage contains NULL patid or procedure_code rows.")
}

execute_sql_with_retry(
  con,
  paste0(
    "DELETE FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");

     INSERT INTO ", procedure_presence_identifier, " (
       patid,
       analysis_year,
       procedure_code
     )
     SELECT DISTINCT
       patid,
       analysis_year,
       procedure_code
     FROM ", procedure_build_identifier, ";

     DROP TABLE IF EXISTS ", procedure_build_identifier, ";"
  ),
  label = "CFI procedure-code presence persistent replace"
)

print_query(
  con,
  "Checking normalized CFI-relevant procedure-code presence row counts.",
  paste0(
    "SELECT analysis_year,
       COUNT(*)::BIGINT AS rows,
       COUNT(DISTINCT patid)::BIGINT AS patient_years,
       COUNT(DISTINCT procedure_code)::BIGINT AS distinct_codes
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

duplicate_qa <- print_query(
  con,
  "Checking normalized procedure-code presence duplicate rows.",
  paste0(
    "SELECT
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR || '|' || procedure_code)
         AS duplicate_rows
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")"
  )
)

if (duplicate_qa$duplicate_rows[[1]] != 0) {
  stop("Normalized CFI-relevant procedure-code presence table contains duplicate selected-year rows.")
}

message(
  config$workflow_label,
  " CFI-relevant procedure-code presence preparation complete: ",
  write_schema,
  ".",
  config$procedure_presence_table,
  "."
)

# Release the Redshift connection now that the script has completed.
disconnect_komodo(con)


