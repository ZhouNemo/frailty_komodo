source("Code/6.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual clinical metrics
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Match compact patient-year diagnosis/procedure code-presence tables to the
# reviewed CFI, CCW, and Gagne lookup rules. This script does not read raw KRD
# event tables and does not flatten arrays. It writes selected years to:
#   - 2_annual_cfi_feature_matches
#   - 2_annual_ccw_condition_matches
#   - 2_annual_gagne_group_matches

config <- get_normalized_clinical_metrics_config()
con <- connect_komodo()

diagnosis_presence_identifier <- qualified_identifier(
  write_schema,
  config$diagnosis_presence_table
)
procedure_presence_identifier <- qualified_identifier(
  write_schema,
  config$procedure_presence_table
)
cfi_matches_identifier <- qualified_identifier(
  write_schema,
  config$cfi_feature_matches_table
)
ccw_matches_identifier <- qualified_identifier(
  write_schema,
  config$ccw_feature_matches_table
)
gagne_matches_identifier <- qualified_identifier(
  write_schema,
  config$gagne_feature_matches_table
)

diagnosis_lookup_path <- file.path(
  config$lookup_dir,
  "0.6_unified_diagnosis_rule_lookup.csv"
)
procedure_lookup_path <- file.path(
  config$lookup_dir,
  "0.6_cfi_procedure_lookup.csv"
)
diagnosis_stage_table <- "clinical_metric_normalized_diagnosis_lookup_stage"
procedure_stage_table <- "clinical_metric_normalized_procedure_lookup_stage"
diagnosis_stage_identifier <- quote_identifier(diagnosis_stage_table)
procedure_stage_identifier <- quote_identifier(procedure_stage_table)

missing_lookup_files <- c(
  diagnosis_lookup_path,
  procedure_lookup_path
)[!file.exists(c(diagnosis_lookup_path, procedure_lookup_path))]
if (length(missing_lookup_files) > 0L) {
  stop(
    "Missing normalized clinical metric lookup files:\n",
    paste(" -", missing_lookup_files, collapse = "\n")
  )
}

for (table in c(config$diagnosis_presence_table, config$procedure_presence_table)) {
  if (!table_exists(con, write_schema, table)) {
    stop("Required code-presence table was not found: ", write_schema, ".", table)
  }
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

diagnosis_lookup <- read_lookup_csv(diagnosis_lookup_path)
require_columns(
  diagnosis_lookup,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "code_system",
    "match_value",
    "range_start",
    "range_end",
    "range_end_inclusive",
    "match_type",
    "final_match_after_flattening"
  ),
  "Unified diagnosis lookup"
)

diagnosis_lookup$metric <- toupper(diagnosis_lookup$metric)
diagnosis_lookup$match_type <- tolower(diagnosis_lookup$match_type)

active_diagnosis_lookup <- diagnosis_lookup[
  diagnosis_lookup$code_system %in% "ICD10CM" &
    diagnosis_lookup$metric %in% c("CFI", "CCW", "GAGNE") &
    !is.na(diagnosis_lookup$final_match_after_flattening) &
    toupper(diagnosis_lookup$final_match_after_flattening) == "TRUE",
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "match_value",
    "range_start",
    "range_end",
    "range_end_inclusive",
    "match_type"
  )
]

if (nrow(active_diagnosis_lookup) == 0L) {
  stop("No active ICD-10-CM diagnosis lookup rows were found for CFI/CCW/Gagne.")
}

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
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "range_start",
    "range_end",
    "match_type"
  )
]

if (nrow(active_procedure_lookup) == 0L) {
  stop("No active CFI CPT/HCPCS procedure lookup rows were found.")
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", diagnosis_stage_identifier, ";
     CREATE TEMP TABLE ", diagnosis_stage_identifier, " (
       lookup_version VARCHAR(128) NOT NULL,
       metric VARCHAR(32) NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       feature_name VARCHAR(256) NOT NULL,
       match_value VARCHAR(64),
       range_start VARCHAR(64),
       range_end VARCHAR(64),
       range_end_inclusive VARCHAR(16),
       match_type VARCHAR(32) NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(metric, match_type, match_value, range_start, range_end);

     DROP TABLE IF EXISTS ", procedure_stage_identifier, ";
     CREATE TEMP TABLE ", procedure_stage_identifier, " (
       lookup_version VARCHAR(128) NOT NULL,
       metric VARCHAR(32) NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       feature_name VARCHAR(256) NOT NULL,
       range_start VARCHAR(64) NOT NULL,
       range_end VARCHAR(64) NOT NULL,
       match_type VARCHAR(32) NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(range_start, range_end);"
  )
)

execute_insert_batches(
  con,
  diagnosis_stage_identifier,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "match_value",
    "range_start",
    "range_end",
    "range_end_inclusive",
    "match_type"
  ),
  active_diagnosis_lookup
)
execute_insert_batches(
  con,
  procedure_stage_identifier,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "range_start",
    "range_end",
    "match_type"
  ),
  active_procedure_lookup
)

feature_table_sql <- function(identifier) {
  paste0(
    "CREATE TABLE ", identifier, " (
       patid VARCHAR(256) NOT NULL,
       analysis_year INTEGER NOT NULL,
       metric VARCHAR(32) NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       feature_name VARCHAR(256) NOT NULL,
       match_type VARCHAR(32) NOT NULL,
       lookup_version VARCHAR(128) NOT NULL
     )
     DISTKEY(patid)
     SORTKEY(analysis_year, patid, feature_id);"
  )
}

if (!table_exists(con, write_schema, config$cfi_feature_matches_table)) {
  DatabaseConnector::executeSql(con, feature_table_sql(cfi_matches_identifier))
}
if (!table_exists(con, write_schema, config$ccw_feature_matches_table)) {
  DatabaseConnector::executeSql(con, feature_table_sql(ccw_matches_identifier))
}
if (!table_exists(con, write_schema, config$gagne_feature_matches_table)) {
  DatabaseConnector::executeSql(con, feature_table_sql(gagne_matches_identifier))
}

for (table in c(
  config$cfi_feature_matches_table,
  config$ccw_feature_matches_table,
  config$gagne_feature_matches_table
)) {
  table_has_columns(
    con,
    write_schema,
    table,
    c(
      "patid",
      "analysis_year",
      "metric",
      "feature_id",
      "feature_name",
      "match_type",
      "lookup_version"
    )
  )
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", cfi_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");
     DELETE FROM ", ccw_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");
     DELETE FROM ", gagne_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");

     INSERT INTO ", cfi_matches_identifier, " (
       patid,
       analysis_year,
       metric,
       feature_id,
       feature_name,
       match_type,
       lookup_version
     )
     WITH diagnosis_matches AS (
       SELECT DISTINCT
         d.patid,
         d.analysis_year,
         l.metric,
         l.feature_id,
         l.feature_name,
         l.match_type,
         l.lookup_version
       FROM ", diagnosis_presence_identifier, " d
       INNER JOIN ", diagnosis_stage_identifier, " l
         ON (
           (
             l.match_type = 'exact'
             AND d.dx_code = l.match_value
           )
           OR (
             l.match_type = 'prefix'
             AND LEFT(d.dx_code, LENGTH(l.match_value)) = l.match_value
           )
           OR (
             l.match_type = 'range'
             AND d.dx_code >= l.range_start
             AND (
               (
                 COALESCE(UPPER(l.range_end_inclusive), 'TRUE') = 'FALSE'
                 AND d.dx_code < l.range_end
               )
               OR (
                 COALESCE(UPPER(l.range_end_inclusive), 'TRUE') <> 'FALSE'
                 AND d.dx_code <= l.range_end
               )
             )
           )
         )
       WHERE d.analysis_year IN (", sql_values(config$analysis_years), ")
         AND l.metric = 'CFI'
     ),
     procedure_matches AS (
       SELECT DISTINCT
         p.patid,
         p.analysis_year,
         l.metric,
         l.feature_id,
         l.feature_name,
         l.match_type,
         l.lookup_version
       FROM ", procedure_presence_identifier, " p
       INNER JOIN ", procedure_stage_identifier, " l
         ON p.procedure_code >= l.range_start
        AND p.procedure_code <= l.range_end
       WHERE p.analysis_year IN (", sql_values(config$analysis_years), ")
     )
     SELECT DISTINCT * FROM diagnosis_matches
     UNION
     SELECT DISTINCT * FROM procedure_matches;

     INSERT INTO ", ccw_matches_identifier, " (
       patid,
       analysis_year,
       metric,
       feature_id,
       feature_name,
       match_type,
       lookup_version
     )
     SELECT DISTINCT
       d.patid,
       d.analysis_year,
       l.metric,
       l.feature_id,
       l.feature_name,
       l.match_type,
       l.lookup_version
     FROM ", diagnosis_presence_identifier, " d
     INNER JOIN ", diagnosis_stage_identifier, " l
       ON (
         (
           l.match_type = 'exact'
           AND d.dx_code = l.match_value
         )
         OR (
           l.match_type = 'prefix'
           AND LEFT(d.dx_code, LENGTH(l.match_value)) = l.match_value
         )
         OR (
           l.match_type = 'range'
           AND d.dx_code >= l.range_start
           AND (
             (
               COALESCE(UPPER(l.range_end_inclusive), 'TRUE') = 'FALSE'
               AND d.dx_code < l.range_end
             )
             OR (
               COALESCE(UPPER(l.range_end_inclusive), 'TRUE') <> 'FALSE'
               AND d.dx_code <= l.range_end
             )
           )
         )
       )
     WHERE d.analysis_year IN (", sql_values(config$analysis_years), ")
       AND l.metric = 'CCW';

     INSERT INTO ", gagne_matches_identifier, " (
       patid,
       analysis_year,
       metric,
       feature_id,
       feature_name,
       match_type,
       lookup_version
     )
     SELECT DISTINCT
       d.patid,
       d.analysis_year,
       l.metric,
       l.feature_id,
       l.feature_name,
       l.match_type,
       l.lookup_version
     FROM ", diagnosis_presence_identifier, " d
     INNER JOIN ", diagnosis_stage_identifier, " l
       ON (
         (
           l.match_type = 'exact'
           AND d.dx_code = l.match_value
         )
         OR (
           l.match_type = 'prefix'
           AND LEFT(d.dx_code, LENGTH(l.match_value)) = l.match_value
         )
         OR (
           l.match_type = 'range'
           AND d.dx_code >= l.range_start
           AND (
             (
               COALESCE(UPPER(l.range_end_inclusive), 'TRUE') = 'FALSE'
               AND d.dx_code < l.range_end
             )
             OR (
               COALESCE(UPPER(l.range_end_inclusive), 'TRUE') <> 'FALSE'
               AND d.dx_code <= l.range_end
             )
           )
         )
       )
     WHERE d.analysis_year IN (", sql_values(config$analysis_years), ")
       AND l.metric = 'GAGNE';"
  )
)

print_query(
  con,
  "Checking normalized compact feature match counts.",
  paste0(
    "SELECT analysis_year, metric, match_type, COUNT(*)::BIGINT AS matched_rows
     FROM ", cfi_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, metric, match_type
     UNION ALL
     SELECT analysis_year, metric, match_type, COUNT(*)::BIGINT AS matched_rows
     FROM ", ccw_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, metric, match_type
     UNION ALL
     SELECT analysis_year, metric, match_type, COUNT(*)::BIGINT AS matched_rows
     FROM ", gagne_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, metric, match_type
     ORDER BY analysis_year, metric, match_type"
  )
)

message(
  config$workflow_label,
  " compact feature matching complete: ",
  paste(
    paste0(write_schema, ".", c(
      config$cfi_feature_matches_table,
      config$ccw_feature_matches_table,
      config$gagne_feature_matches_table
    )),
    collapse = ", "
  ),
  "."
)
