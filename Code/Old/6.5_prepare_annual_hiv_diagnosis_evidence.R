source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual HIV diagnosis evidence
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Build the compact HIV diagnosis evidence table. By default this script reads
# HIV-candidate staged events; set `use_candidate_event_stage = FALSE` for
# prefilter parity runs against the full staged event tables. HIV keeps the date
# and setting fields needed for the annual-only confirmation rule while avoiding
# event-level output for other metrics. The script writes selected years to:
#   - 2_annual_hiv_diagnosis_evidence

config <- get_optimized_clinical_metrics_config()
con <- connect_komodo()

if (config$use_candidate_event_stage) {
  inpatient_source_table <- config$inpatient_candidate_table
  non_inpatient_source_table <- config$non_inpatient_candidate_table
  hiv_filter <- "AND has_hiv_diagnosis_candidate"
  hiv_filter_i <- "AND i.has_hiv_diagnosis_candidate"
  hiv_filter_n <- "AND n.has_hiv_diagnosis_candidate"
  source_label <- "candidate event"
} else {
  inpatient_source_table <- config$inpatient_stage_table
  non_inpatient_source_table <- config$non_inpatient_stage_table
  hiv_filter <- ""
  hiv_filter_i <- ""
  hiv_filter_n <- ""
  source_label <- "full staged event"
}

inpatient_source_identifier <- qualified_identifier(write_schema, inpatient_source_table)
non_inpatient_source_identifier <- qualified_identifier(
  write_schema,
  non_inpatient_source_table
)
hiv_evidence_identifier <- qualified_identifier(write_schema, config$hiv_evidence_table)
hiv_lookup_path <- file.path(config$lookup_dir, "0.6_hiv_diagnosis_lookup.csv")
hiv_stage_table <- "clinical_metric_6_hiv_lookup_stage"
hiv_stage_identifier <- quote_identifier(hiv_stage_table)
diagnosis_position_table <- "clinical_metric_6_hiv_diagnosis_array_positions"

if (!file.exists(hiv_lookup_path)) {
  stop("Missing HIV diagnosis lookup: ", hiv_lookup_path)
}

for (table in c(inpatient_source_table, non_inpatient_source_table)) {
  if (!table_exists(con, write_schema, table)) {
    stop("Required ", source_label, " table was not found: ", write_schema, ".", table)
  }
}

hiv_lookup <- read_lookup_csv(hiv_lookup_path)
require_columns(
  hiv_lookup,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "code_system",
    "match_value",
    "match_type"
  ),
  "HIV diagnosis lookup"
)

hiv_lookup$match_type <- tolower(hiv_lookup$match_type)
hiv_lookup <- hiv_lookup[
  hiv_lookup$code_system %in% "ICD10CM" &
    hiv_lookup$match_type %in% "exact" &
    !is.na(hiv_lookup$match_value) &
    hiv_lookup$match_value != "",
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "match_value"
  )
]

if (nrow(hiv_lookup) == 0L) {
  stop("No active ICD-10-CM HIV diagnosis lookup rows were found.")
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
       match_value VARCHAR(64) NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(match_value);"
  )
)

execute_insert_batches(
  con,
  hiv_stage_identifier,
  c("lookup_version", "metric", "feature_id", "feature_name", "match_value"),
  hiv_lookup
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
         diagnosis_code VARCHAR(64) NOT NULL,
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

for (analysis_year in config$analysis_years) {
  message("Preparing optimized HIV diagnosis evidence for ", analysis_year, ".")

  array_validity <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         (SELECT COUNT(*) FROM ", inpatient_source_identifier, "
          WHERE analysis_year = ", analysis_year, "
            ", hiv_filter, "
            AND secondary_diagnosis_codes IS NOT NULL
            AND secondary_diagnosis_codes <> ''
            AND JSON_ARRAY_LENGTH(secondary_diagnosis_codes, TRUE) IS NULL
         )::BIGINT AS invalid_secondary_diagnosis_arrays,
         (SELECT COUNT(*) FROM ", non_inpatient_source_identifier, "
          WHERE analysis_year = ", analysis_year, "
            ", hiv_filter, "
            AND diagnosis_codes IS NOT NULL
            AND diagnosis_codes <> ''
            AND JSON_ARRAY_LENGTH(diagnosis_codes, TRUE) IS NULL
         )::BIGINT AS invalid_non_inpatient_diagnosis_arrays"
    )
  )

  if (sum(unlist(array_validity), na.rm = TRUE) > 0) {
    stop("Invalid JSON-style HIV candidate arrays were found for ", analysis_year, ".")
  }

  array_maxima <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(secondary_diagnosis_codes, TRUE))
            FROM ", inpatient_source_identifier, "
            WHERE analysis_year = ", analysis_year, "
              ", hiv_filter, "),
           0
         )::INTEGER AS max_inpatient_diagnosis_array,
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(diagnosis_codes, TRUE))
            FROM ", non_inpatient_source_identifier, "
            WHERE analysis_year = ", analysis_year, "
              ", hiv_filter, "),
           0
         )::INTEGER AS max_non_inpatient_diagnosis_array"
    )
  )

  observed_max_diagnosis_array_length <- max(
    array_maxima$max_inpatient_diagnosis_array[[1]],
    array_maxima$max_non_inpatient_diagnosis_array[[1]]
  )
  max_diagnosis_array_length <- min(
    observed_max_diagnosis_array_length,
    config$array_code_limit
  )

  message(
    analysis_year,
    " observed maximum HIV candidate diagnosis array length: ",
    observed_max_diagnosis_array_length,
    "; flattening first ",
    max_diagnosis_array_length,
    " position(s)."
  )

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", quote_identifier(diagnosis_position_table), ";
       CREATE TEMP TABLE ", quote_identifier(diagnosis_position_table), " AS
       WITH RECURSIVE positions(array_index) AS (
         SELECT 0
         WHERE ", max_diagnosis_array_length, " > 0
         UNION ALL
         SELECT array_index + 1
         FROM positions
         WHERE array_index + 1 < ", max_diagnosis_array_length, "
       )
       SELECT array_index
       FROM positions;"
    ),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DELETE FROM ", hiv_evidence_identifier, "
       WHERE analysis_year = ", analysis_year, ";

       INSERT INTO ", hiv_evidence_identifier, " (
         patid,
         analysis_year,
         diagnosis_date,
         claim_setting,
         diagnosis_code,
         metric,
         feature_id,
         feature_name,
         match_type,
         lookup_version
       )
       WITH raw_hiv_diagnoses AS (
         SELECT
           patid,
           analysis_year,
           event_date AS diagnosis_date,
           'inpatient'::VARCHAR(40) AS claim_setting,
           admission_diagnosis_code AS diagnosis_code
         FROM ", inpatient_source_identifier, "
         WHERE analysis_year = ", analysis_year, "
           ", hiv_filter, "
           AND admission_diagnosis_code IS NOT NULL

         UNION ALL

         SELECT
           patid,
           analysis_year,
           event_date AS diagnosis_date,
           'inpatient'::VARCHAR(40) AS claim_setting,
           primary_diagnosis_code AS diagnosis_code
         FROM ", inpatient_source_identifier, "
         WHERE analysis_year = ", analysis_year, "
           ", hiv_filter, "
           AND primary_diagnosis_code IS NOT NULL

         UNION ALL

         SELECT
           i.patid,
           i.analysis_year,
           i.event_date AS diagnosis_date,
           'inpatient'::VARCHAR(40) AS claim_setting,
           JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
             i.secondary_diagnosis_codes,
             p.array_index,
             TRUE
           ) AS diagnosis_code
         FROM ", inpatient_source_identifier, " i
         INNER JOIN ", quote_identifier(diagnosis_position_table), " p
           ON p.array_index < JSON_ARRAY_LENGTH(i.secondary_diagnosis_codes, TRUE)
         WHERE i.analysis_year = ", analysis_year, "
           ", hiv_filter_i, "

         UNION ALL

         SELECT
           n.patid,
           n.analysis_year,
           n.event_date AS diagnosis_date,
           'non_inpatient'::VARCHAR(40) AS claim_setting,
           JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
             n.diagnosis_codes,
             p.array_index,
             TRUE
           ) AS diagnosis_code
         FROM ", non_inpatient_source_identifier, " n
         INNER JOIN ", quote_identifier(diagnosis_position_table), " p
           ON p.array_index < JSON_ARRAY_LENGTH(n.diagnosis_codes, TRUE)
         WHERE n.analysis_year = ", analysis_year, "
           ", hiv_filter_n, "
       )
       SELECT DISTINCT
         d.patid,
         d.analysis_year,
         d.diagnosis_date,
         d.claim_setting,
         d.diagnosis_code,
         h.metric,
         h.feature_id,
         h.feature_name,
         'exact'::VARCHAR(32) AS match_type,
         h.lookup_version
       FROM raw_hiv_diagnoses d
       INNER JOIN ", hiv_stage_identifier, " h
         ON d.diagnosis_code = h.match_value
       WHERE d.diagnosis_code IS NOT NULL
         AND d.diagnosis_code <> '';"
    )
  )
}

print_query(
  con,
  "Checking optimized HIV evidence counts.",
  paste0(
    "SELECT
       analysis_year,
       claim_setting,
       COUNT(*)::BIGINT AS evidence_rows,
       COUNT(DISTINCT patid)::BIGINT AS patient_years
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
