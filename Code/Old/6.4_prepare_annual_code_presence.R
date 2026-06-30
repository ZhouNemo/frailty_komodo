source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual code presence
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Build lean patient-year diagnosis and procedure code-presence tables for CFI,
# CCW, and Gagne. By default this script reads the conservative candidate event
# stages; set `use_candidate_event_stage = FALSE` for prefilter parity runs
# against the full staged event tables. It deduplicates immediately to
# patient-year-code grain and writes selected years to:
#   - 2_annual_diagnosis_code_presence
#   - 2_annual_procedure_code_presence

config <- get_optimized_clinical_metrics_config()
con <- connect_komodo()

if (config$use_candidate_event_stage) {
  inpatient_source_table <- config$inpatient_candidate_table
  non_inpatient_source_table <- config$non_inpatient_candidate_table
  metric_diagnosis_filter <- "AND has_metric_diagnosis_candidate"
  metric_diagnosis_filter_i <- "AND i.has_metric_diagnosis_candidate"
  metric_diagnosis_filter_n <- "AND n.has_metric_diagnosis_candidate"
  procedure_filter <- "AND has_procedure_candidate"
  procedure_filter_i <- "AND i.has_procedure_candidate"
  source_label <- "candidate event"
} else {
  inpatient_source_table <- config$inpatient_stage_table
  non_inpatient_source_table <- config$non_inpatient_stage_table
  metric_diagnosis_filter <- ""
  metric_diagnosis_filter_i <- ""
  metric_diagnosis_filter_n <- ""
  procedure_filter <- ""
  procedure_filter_i <- ""
  source_label <- "full staged event"
}

inpatient_source_identifier <- qualified_identifier(write_schema, inpatient_source_table)
non_inpatient_source_identifier <- qualified_identifier(
  write_schema,
  non_inpatient_source_table
)
diagnosis_presence_identifier <- qualified_identifier(
  write_schema,
  config$diagnosis_presence_table
)
procedure_presence_identifier <- qualified_identifier(
  write_schema,
  config$procedure_presence_table
)
diagnosis_position_table <- "clinical_metric_6_diagnosis_array_positions"
procedure_position_table <- "clinical_metric_6_procedure_array_positions"

for (table in c(inpatient_source_table, non_inpatient_source_table)) {
  if (!table_exists(con, write_schema, table)) {
    stop("Required ", source_label, " table was not found: ", write_schema, ".", table)
  }
}

if (!table_exists(con, write_schema, config$diagnosis_presence_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", diagnosis_presence_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         diagnosis_code VARCHAR(64) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, diagnosis_code, patid);"
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

for (analysis_year in config$analysis_years) {
  message("Preparing optimized code-presence tables for ", analysis_year, ".")

  array_validity <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         (SELECT COUNT(*) FROM ", inpatient_source_identifier, "
          WHERE analysis_year = ", analysis_year, "
            ", metric_diagnosis_filter, "
            AND secondary_diagnosis_codes IS NOT NULL
            AND secondary_diagnosis_codes <> ''
            AND JSON_ARRAY_LENGTH(secondary_diagnosis_codes, TRUE) IS NULL
         )::BIGINT AS invalid_secondary_diagnosis_arrays,
         (SELECT COUNT(*) FROM ", non_inpatient_source_identifier, "
          WHERE analysis_year = ", analysis_year, "
            ", metric_diagnosis_filter, "
            AND diagnosis_codes IS NOT NULL
            AND diagnosis_codes <> ''
            AND JSON_ARRAY_LENGTH(diagnosis_codes, TRUE) IS NULL
         )::BIGINT AS invalid_non_inpatient_diagnosis_arrays,
         (SELECT COUNT(*) FROM ", inpatient_source_identifier, "
          WHERE analysis_year = ", analysis_year, "
            ", procedure_filter, "
            AND cpt_hcpcs_codes IS NOT NULL
            AND cpt_hcpcs_codes <> ''
            AND JSON_ARRAY_LENGTH(cpt_hcpcs_codes, TRUE) IS NULL
         )::BIGINT AS invalid_cpt_hcpcs_arrays"
    )
  )

  if (sum(unlist(array_validity), na.rm = TRUE) > 0) {
    stop("Invalid JSON-style candidate arrays were found for ", analysis_year, ".")
  }

  array_maxima <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(secondary_diagnosis_codes, TRUE))
            FROM ", inpatient_source_identifier, "
            WHERE analysis_year = ", analysis_year, "
              ", metric_diagnosis_filter, "),
           0
         )::INTEGER AS max_inpatient_diagnosis_array,
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(diagnosis_codes, TRUE))
            FROM ", non_inpatient_source_identifier, "
            WHERE analysis_year = ", analysis_year, "
              ", metric_diagnosis_filter, "),
           0
         )::INTEGER AS max_non_inpatient_diagnosis_array,
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(cpt_hcpcs_codes, TRUE))
            FROM ", inpatient_source_identifier, "
            WHERE analysis_year = ", analysis_year, "
              ", procedure_filter, "),
           0
         )::INTEGER AS max_inpatient_procedure_array"
    )
  )

  observed_max_diagnosis_array_length <- max(
    array_maxima$max_inpatient_diagnosis_array[[1]],
    array_maxima$max_non_inpatient_diagnosis_array[[1]]
  )
  observed_max_procedure_array_length <- array_maxima$max_inpatient_procedure_array[[1]]
  max_diagnosis_array_length <- min(
    observed_max_diagnosis_array_length,
    config$array_code_limit
  )
  max_procedure_array_length <- min(
    observed_max_procedure_array_length,
    config$array_code_limit
  )

  message(
    analysis_year,
    " observed maximum candidate diagnosis array length: ",
    observed_max_diagnosis_array_length,
    "; flattening first ",
    max_diagnosis_array_length,
    " position(s)."
  )
  message(
    analysis_year,
    " observed maximum candidate CPT/HCPCS array length: ",
    observed_max_procedure_array_length,
    "; flattening first ",
    max_procedure_array_length,
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
       FROM positions;

       DROP TABLE IF EXISTS ", quote_identifier(procedure_position_table), ";
       CREATE TEMP TABLE ", quote_identifier(procedure_position_table), " AS
       WITH RECURSIVE positions(array_index) AS (
         SELECT 0
         WHERE ", max_procedure_array_length, " > 0
         UNION ALL
         SELECT array_index + 1
         FROM positions
         WHERE array_index + 1 < ", max_procedure_array_length, "
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
      "DELETE FROM ", diagnosis_presence_identifier, "
       WHERE analysis_year = ", analysis_year, ";

       INSERT INTO ", diagnosis_presence_identifier, " (
         patid,
         analysis_year,
         diagnosis_code
       )
       WITH raw_diagnoses AS (
         SELECT
           patid,
           analysis_year,
           admission_diagnosis_code AS diagnosis_code
         FROM ", inpatient_source_identifier, "
         WHERE analysis_year = ", analysis_year, "
           ", metric_diagnosis_filter, "
           AND admission_diagnosis_code IS NOT NULL

         UNION ALL

         SELECT
           patid,
           analysis_year,
           primary_diagnosis_code AS diagnosis_code
         FROM ", inpatient_source_identifier, "
         WHERE analysis_year = ", analysis_year, "
           ", metric_diagnosis_filter, "
           AND primary_diagnosis_code IS NOT NULL

         UNION ALL

         SELECT
           i.patid,
           i.analysis_year,
           JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
             i.secondary_diagnosis_codes,
             p.array_index,
             TRUE
           ) AS diagnosis_code
         FROM ", inpatient_source_identifier, " i
         INNER JOIN ", quote_identifier(diagnosis_position_table), " p
           ON p.array_index < JSON_ARRAY_LENGTH(i.secondary_diagnosis_codes, TRUE)
         WHERE i.analysis_year = ", analysis_year, "
           ", metric_diagnosis_filter_i, "

         UNION ALL

         SELECT
           n.patid,
           n.analysis_year,
           JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
             n.diagnosis_codes,
             p.array_index,
             TRUE
           ) AS diagnosis_code
         FROM ", non_inpatient_source_identifier, " n
         INNER JOIN ", quote_identifier(diagnosis_position_table), " p
           ON p.array_index < JSON_ARRAY_LENGTH(n.diagnosis_codes, TRUE)
         WHERE n.analysis_year = ", analysis_year, "
           ", metric_diagnosis_filter_n, "
       )
       SELECT DISTINCT
         patid,
         analysis_year,
         diagnosis_code
       FROM raw_diagnoses
       WHERE diagnosis_code IS NOT NULL
         AND diagnosis_code <> '';"
    )
  )

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DELETE FROM ", procedure_presence_identifier, "
       WHERE analysis_year = ", analysis_year, ";

       INSERT INTO ", procedure_presence_identifier, " (
         patid,
         analysis_year,
         procedure_code
       )
       WITH raw_procedures AS (
         SELECT
           i.patid,
           i.analysis_year,
           JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
             i.cpt_hcpcs_codes,
             p.array_index,
             TRUE
           ) AS procedure_code
         FROM ", inpatient_source_identifier, " i
         INNER JOIN ", quote_identifier(procedure_position_table), " p
           ON p.array_index < JSON_ARRAY_LENGTH(i.cpt_hcpcs_codes, TRUE)
         WHERE i.analysis_year = ", analysis_year, "
           ", procedure_filter_i, "

         UNION ALL

         SELECT
           patid,
           analysis_year,
           procedure_code
         FROM ", non_inpatient_source_identifier, "
         WHERE analysis_year = ", analysis_year, "
           ", procedure_filter, "
           AND procedure_code IS NOT NULL
       )
       SELECT DISTINCT
         patid,
         analysis_year,
         procedure_code
       FROM raw_procedures
       WHERE procedure_code IS NOT NULL
         AND procedure_code <> ''
         AND procedure_code ~ '^[A-Z0-9]{5}$'
         AND procedure_code ~ '[0-9]$';"
    )
  )

  print_query(
    con,
    paste0("Checking ", analysis_year, " optimized code-presence counts."),
    paste0(
      "SELECT
         ", analysis_year, "::INTEGER AS analysis_year,
         (SELECT COUNT(*) FROM ", diagnosis_presence_identifier, "
          WHERE analysis_year = ", analysis_year, ")::BIGINT
          AS diagnosis_presence_rows,
         (SELECT COUNT(*) FROM ", procedure_presence_identifier, "
          WHERE analysis_year = ", analysis_year, ")::BIGINT
          AS procedure_presence_rows"
    )
  )
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
