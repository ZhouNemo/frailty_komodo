library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual CFI input preparation
# Author: Nemo Zhou
# Date started: 2026-06-15
# Date last updated: 2026-06-15
#
# ---- Purpose ----
# Prepare annual patient-year diagnosis and CPT/HCPCS inputs for Claims-Based
# Frailty Index (CFI) calculation. The production workflow processes one year
# at a time to limit peak Redshift temporary-disk use. For each eligible year,
# claims from January 1 through December 31 describe that calendar year, and
# the CFI index date is January 1 of the following year.
#
# The default production run processes 2016 through 2025 and writes:
#   - cfi_annual_ids
#   - cfi_annual_dx09
#   - cfi_annual_dx10
#   - cfi_annual_px
#
# Code/3.1_prepare_2016_cfi_inputs.R calls this script with a 2016-only
# configuration and separate validation table names. Patient-level data remain
# in Redshift. Only aggregate QA results are printed.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Analysis parameters ----
default_cfi_config <- list(
  analysis_years = 2016:2025,
  id_years = 2016:2025,
  ids_table = "cfi_annual_ids",
  dx09_table = "cfi_annual_dx09",
  dx10_table = "cfi_annual_dx10",
  px_table = "cfi_annual_px",
  workflow_label = "annual production",
  compare_2016_tables = TRUE
)

cfi_config <- utils::modifyList(
  default_cfi_config,
  getOption("frailty.cfi.config", list())
)

analysis_years <- sort(unique(as.integer(cfi_config$analysis_years)))
id_years <- sort(unique(as.integer(cfi_config$id_years)))

if (
  length(analysis_years) == 0L ||
    any(is.na(analysis_years)) ||
    any(analysis_years < 2016L | analysis_years > 2025L)
) {
  stop("analysis_years must contain years from 2016 through 2025.")
}

if (
  length(id_years) == 0L ||
    any(is.na(id_years)) ||
    any(id_years < 2016L | id_years > 2025L) ||
    length(setdiff(analysis_years, id_years)) > 0L
) {
  stop(
    "id_years must contain all processing years and remain within 2016-2025."
  )
}

eligibility_table <- "1_annual_eligible_cohort"
ids_table <- cfi_config$ids_table
dx09_table <- cfi_config$dx09_table
dx10_table <- cfi_config$dx10_table
px_table <- cfi_config$px_table

inpatient_stage_table <- "cfi_inpatient_stage"
non_inpatient_stage_table <- "cfi_non_inpatient_stage"
diagnosis_position_table <- "cfi_diagnosis_array_positions"
procedure_position_table <- "cfi_procedure_array_positions"

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Helpers ----
quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

qualified_identifier <- function(schema, table) {
  paste(
    quote_identifier(schema),
    quote_identifier(table),
    sep = "."
  )
}

sql_string <- function(value) {
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

print_query <- function(label, sql) {
  message(label)
  result <- DBI::dbGetQuery(con, sql)
  print(result)
  invisible(result)
}

table_exists <- function(schema, table) {
  result <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::INTEGER AS table_count
       FROM information_schema.tables
       WHERE table_schema = ", sql_string(schema), "
         AND table_name = ", sql_string(table)
    )
  )

  nrow(result) == 1L &&
    !is.na(result$table_count[[1]]) &&
    result$table_count[[1]] == 1L
}

drop_temporary_tables <- function() {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", inpatient_stage_table, ";
       DROP TABLE IF EXISTS ", non_inpatient_stage_table, ";
       DROP TABLE IF EXISTS ", diagnosis_position_table, ";
       DROP TABLE IF EXISTS ", procedure_position_table, ";"
    ),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
}

eligibility_table_identifier <- qualified_identifier(
  write_schema,
  eligibility_table
)
ids_table_identifier <- qualified_identifier(write_schema, ids_table)
dx09_table_identifier <- qualified_identifier(write_schema, dx09_table)
dx10_table_identifier <- qualified_identifier(write_schema, dx10_table)
px_table_identifier <- qualified_identifier(write_schema, px_table)

# ---- Validate source cohort table and columns ----
if (!table_exists(write_schema, eligibility_table)) {
  stop(
    "Required eligibility table was not found: ",
    write_schema,
    ".",
    eligibility_table,
    ". Run Code/1.1_build_annual_eligible_population.R first."
  )
}

required_eligibility_columns <- c(
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

eligibility_columns <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT LOWER(column_name) AS column_name
     FROM information_schema.columns
     WHERE table_schema = ", sql_string(write_schema), "
       AND table_name = ", sql_string(eligibility_table)
  )
)$column_name

missing_eligibility_columns <- setdiff(
  required_eligibility_columns,
  eligibility_columns
)

if (length(missing_eligibility_columns) > 0L) {
  stop(
    "Eligibility table is missing required columns: ",
    paste(missing_eligibility_columns, collapse = ", ")
  )
}

eligible_year_counts <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT analysis_year, COUNT(*)::BIGINT AS n_person_years
     FROM ", eligibility_table_identifier, "
     WHERE analysis_year IN (", paste(id_years, collapse = ", "), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

missing_years <- setdiff(id_years, eligible_year_counts$analysis_year)

if (length(missing_years) > 0L) {
  stop(
    "No eligible patient-years were found for: ",
    paste(missing_years, collapse = ", "),
    "."
  )
}

print(eligible_year_counts)

# ---- Materialize the configured CFI ID population ----
ids_sql <- paste0(
  "DROP TABLE IF EXISTS ", ids_table_identifier, ";
   CREATE TABLE ", ids_table_identifier, "
   DISTKEY(patient_id)
   SORTKEY(analysis_year, patient_id) AS
   SELECT DISTINCT
     e.patient_id || '_' || e.analysis_year::VARCHAR AS patid,
     e.patient_id,
     e.analysis_year,
     CAST(e.index_date AS DATE) AS eligibility_index_date,
     TO_DATE(
       (e.analysis_year + 1)::VARCHAR || '-01-01',
       'YYYY-MM-DD'
     ) AS cfi_index_date,
     TO_DATE(
       e.analysis_year::VARCHAR || '-01-01',
       'YYYY-MM-DD'
     ) AS lookback_start,
     TO_DATE(
       (e.analysis_year + 1)::VARCHAR || '-01-01',
       'YYYY-MM-DD'
     ) AS lookback_end,
     e.age,
     e.patient_gender,
     e.mx_insurance_group,
     e.mx_insurance_segment,
     e.mx_secondary_insurance_group,
     e.mx_secondary_insurance_segment,
     e.rx_insurance_group,
     e.rx_insurance_segment,
     e.rx_secondary_insurance_group,
     e.rx_secondary_insurance_segment
   FROM ", eligibility_table_identifier, " e
   WHERE e.analysis_year IN (", paste(id_years, collapse = ", "), ")
     AND e.patient_id IS NOT NULL;"
)

message(
  "Creating ",
  cfi_config$workflow_label,
  " CFI ID table: ",
  write_schema,
  ".",
  ids_table
)
DatabaseConnector::executeSql(con, ids_sql)

id_integrity <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_distinct_patid,
       SUM(CASE WHEN patid IS NULL OR patid = ''
         THEN 1 ELSE 0 END)::BIGINT AS missing_patid
     FROM ", ids_table_identifier
  )
)

if (
  id_integrity$n_rows[[1]] == 0 ||
    id_integrity$n_rows[[1]] != id_integrity$n_distinct_patid[[1]] ||
    id_integrity$missing_patid[[1]] != 0
) {
  stop("The configured CFI ID table failed its integrity check.")
}

# ---- Initialize output tables when needed ----
if (!table_exists(write_schema, dx10_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", dx10_table_identifier, "
       DISTKEY(patid)
       SORTKEY(patid, dx) AS
       SELECT
         CAST(NULL AS VARCHAR(256)) AS patid,
         CAST(NULL AS VARCHAR(20)) AS dx
       WHERE 1 = 0;"
    )
  )
}

if (!table_exists(write_schema, px_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", px_table_identifier, "
       DISTKEY(patid)
       SORTKEY(patid, px) AS
       SELECT
         CAST(NULL AS VARCHAR(256)) AS patid,
         CAST(NULL AS VARCHAR(20)) AS px
       WHERE 1 = 0;"
    )
  )
}

# All selected years are after the US ICD-10-CM transition.
DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", dx09_table_identifier, ";
     CREATE TABLE ", dx09_table_identifier, " AS
     SELECT
       CAST(NULL AS VARCHAR(256)) AS patid,
       CAST(NULL AS VARCHAR(20)) AS dx
     WHERE 1 = 0;"
  )
)

# ---- Process one year at a time ----
for (analysis_year in analysis_years) {
  year_start <- paste0(analysis_year, "-01-01")
  year_end <- paste0(analysis_year + 1L, "-01-01")

  message(
    "Starting ",
    analysis_year,
    " CFI event extraction (",
    match(analysis_year, analysis_years),
    " of ",
    length(analysis_years),
    ")."
  )

  drop_temporary_tables()

  stage_sql <- paste0(
    "CREATE TEMP TABLE ", inpatient_stage_table, "
     DISTKEY(patient_id)
     SORTKEY(event_date) AS
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
       FROM ", ids_table_identifier, "
       WHERE analysis_year = ", analysis_year, "
     ) ids
     INNER JOIN ", komodo_schema, ".inpatient_events i
       ON ids.patient_id = i.patient_id
     WHERE i.claim_from_date >= ", sql_string(year_start), "::DATE
       AND i.claim_from_date < ", sql_string(year_end), "::DATE;

     CREATE TEMP TABLE ", non_inpatient_stage_table, "
     DISTKEY(patient_id)
     SORTKEY(event_date) AS
     SELECT
       ids.patid,
       ids.analysis_year,
       n.patient_id,
       n.service_date AS event_date,
       n.diagnosis_codes,
       n.procedure_code
     FROM (
       SELECT patid, analysis_year, patient_id
       FROM ", ids_table_identifier, "
       WHERE analysis_year = ", analysis_year, "
     ) ids
     INNER JOIN ", komodo_schema, ".non_inpatient_events n
       ON ids.patient_id = n.patient_id
     WHERE n.service_date >= ", sql_string(year_start), "::DATE
       AND n.service_date < ", sql_string(year_end), "::DATE;"
  )

  message("Creating ", analysis_year, " restricted event staging tables.")
  DatabaseConnector::executeSql(con, stage_sql)

  print_query(
    paste0("Checking ", analysis_year, " staged event counts."),
    paste0(
      "SELECT
         'inpatient' AS source_table,
         COUNT(*)::BIGINT AS staged_rows,
         SUM(CASE WHEN event_date IS NULL
           THEN 1 ELSE 0 END)::BIGINT AS missing_event_date
       FROM ", inpatient_stage_table, "
       UNION ALL
       SELECT
         'non_inpatient' AS source_table,
         COUNT(*)::BIGINT AS staged_rows,
         SUM(CASE WHEN event_date IS NULL
           THEN 1 ELSE 0 END)::BIGINT AS missing_event_date
       FROM ", non_inpatient_stage_table, "
       ORDER BY source_table"
    )
  )

  array_validity <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         (SELECT COUNT(*) FROM ", inpatient_stage_table, "
          WHERE secondary_diagnosis_codes IS NOT NULL
            AND TRIM(secondary_diagnosis_codes) <> ''
            AND JSON_ARRAY_LENGTH(
              secondary_diagnosis_codes,
              TRUE
            ) IS NULL)::BIGINT AS invalid_secondary_diagnosis_arrays,
         (SELECT COUNT(*) FROM ", inpatient_stage_table, "
          WHERE cpt_hcpcs_codes IS NOT NULL
            AND TRIM(cpt_hcpcs_codes) <> ''
            AND JSON_ARRAY_LENGTH(
              cpt_hcpcs_codes,
              TRUE
            ) IS NULL)::BIGINT AS invalid_cpt_hcpcs_arrays,
         (SELECT COUNT(*) FROM ", non_inpatient_stage_table, "
          WHERE diagnosis_codes IS NOT NULL
            AND TRIM(diagnosis_codes) <> ''
            AND JSON_ARRAY_LENGTH(
              diagnosis_codes,
              TRUE
            ) IS NULL)::BIGINT AS invalid_diagnosis_arrays"
    )
  )

  if (sum(unlist(array_validity), na.rm = TRUE) > 0) {
    stop(
      "Invalid JSON-style arrays were found in ",
      analysis_year,
      " restricted event data."
    )
  }

  array_maxima <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(
              secondary_diagnosis_codes,
              TRUE
            ))
            FROM ", inpatient_stage_table, "),
           0
         )::INTEGER AS max_inpatient_diagnosis_array,
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(diagnosis_codes, TRUE))
            FROM ", non_inpatient_stage_table, "),
           0
         )::INTEGER AS max_non_inpatient_diagnosis_array,
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(cpt_hcpcs_codes, TRUE))
            FROM ", inpatient_stage_table, "),
           0
         )::INTEGER AS max_inpatient_procedure_array"
    )
  )

  max_diagnosis_array_length <- max(
    array_maxima$max_inpatient_diagnosis_array[[1]],
    array_maxima$max_non_inpatient_diagnosis_array[[1]]
  )
  max_procedure_array_length <-
    array_maxima$max_inpatient_procedure_array[[1]]

  message(
    analysis_year,
    " maximum diagnosis array length: ",
    max_diagnosis_array_length
  )
  message(
    analysis_year,
    " maximum inpatient CPT/HCPCS array length: ",
    max_procedure_array_length
  )

  position_sql <- paste0(
    "CREATE TEMP TABLE ", diagnosis_position_table, " AS
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

     CREATE TEMP TABLE ", procedure_position_table, " AS
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
  )

  DatabaseConnector::executeSql(
    con,
    position_sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  array_position_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         ", max_diagnosis_array_length,
      "::INTEGER AS observed_max_diagnosis_array_length,
         (SELECT COUNT(*) FROM ", diagnosis_position_table,
      ")::INTEGER AS generated_diagnosis_positions,
         ", max_procedure_array_length,
      "::INTEGER AS observed_max_procedure_array_length,
         (SELECT COUNT(*) FROM ", procedure_position_table,
      ")::INTEGER AS generated_procedure_positions"
    )
  )

  if (
    array_position_qa$observed_max_diagnosis_array_length[[1]] !=
      array_position_qa$generated_diagnosis_positions[[1]] ||
      array_position_qa$observed_max_procedure_array_length[[1]] !=
        array_position_qa$generated_procedure_positions[[1]]
  ) {
    stop(
      "Dynamic array-position generation did not cover the observed ",
      analysis_year,
      " maximum."
    )
  }

  # Delete only the current year so interrupted annual runs are restartable.
  DatabaseConnector::executeSql(
    con,
    paste0(
      "DELETE FROM ", dx10_table_identifier, "
       USING ", ids_table_identifier, " ids
       WHERE ", dx10_table_identifier, ".patid = ids.patid
         AND ids.analysis_year = ", analysis_year, ";
       DELETE FROM ", px_table_identifier, "
       USING ", ids_table_identifier, " ids
       WHERE ", px_table_identifier, ".patid = ids.patid
         AND ids.analysis_year = ", analysis_year, ";"
    ),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  dx10_sql <- paste0(
    "INSERT INTO ", dx10_table_identifier, " (patid, dx)
     WITH raw_diagnoses AS (
       SELECT
         patid,
         admission_diagnosis_code AS raw_diagnosis_code
       FROM ", inpatient_stage_table, "
       WHERE admission_diagnosis_code IS NOT NULL

       UNION ALL

       SELECT
         patid,
         primary_diagnosis_code AS raw_diagnosis_code
       FROM ", inpatient_stage_table, "
       WHERE primary_diagnosis_code IS NOT NULL

       UNION ALL

       SELECT
         i.patid,
         JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
           i.secondary_diagnosis_codes,
           p.array_index,
           TRUE
         ) AS raw_diagnosis_code
       FROM ", inpatient_stage_table, " i
       INNER JOIN ", diagnosis_position_table, " p
         ON p.array_index <
           JSON_ARRAY_LENGTH(i.secondary_diagnosis_codes, TRUE)

       UNION ALL

       SELECT
         n.patid,
         JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
           n.diagnosis_codes,
           p.array_index,
           TRUE
         ) AS raw_diagnosis_code
       FROM ", non_inpatient_stage_table, " n
       INNER JOIN ", diagnosis_position_table, " p
         ON p.array_index < JSON_ARRAY_LENGTH(n.diagnosis_codes, TRUE)
     ),
     normalized_diagnoses AS (
       SELECT
         patid,
         UPPER(
           REGEXP_REPLACE(
             TRIM(raw_diagnosis_code),
             '[^A-Za-z0-9]',
             ''
           )
         ) AS dx
       FROM raw_diagnoses
       WHERE raw_diagnosis_code IS NOT NULL
         AND TRIM(raw_diagnosis_code) <> ''
     )
     SELECT DISTINCT
       patid,
       dx
     FROM normalized_diagnoses
     WHERE dx IS NOT NULL
       AND dx <> ''
       AND dx ~ '^[A-Z0-9]+$';"
  )

  message("Appending ", analysis_year, " ICD-10 inputs.")
  DatabaseConnector::executeSql(con, dx10_sql)

  px_sql <- paste0(
    "INSERT INTO ", px_table_identifier, " (patid, px)
     WITH raw_procedures AS (
       SELECT
         i.patid,
         JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
           i.cpt_hcpcs_codes,
           p.array_index,
           TRUE
         ) AS raw_procedure_code
       FROM ", inpatient_stage_table, " i
       INNER JOIN ", procedure_position_table, " p
         ON p.array_index < JSON_ARRAY_LENGTH(i.cpt_hcpcs_codes, TRUE)

       UNION ALL

       SELECT
         patid,
         procedure_code AS raw_procedure_code
       FROM ", non_inpatient_stage_table, "
       WHERE procedure_code IS NOT NULL
     ),
     normalized_procedures AS (
       SELECT
         patid,
         CASE
           WHEN UPPER(TRIM(raw_procedure_code)) ~
             '^[A-Z0-9]{5}([-[:space:]].*)?$'
           THEN LEFT(UPPER(TRIM(raw_procedure_code)), 5)
           ELSE UPPER(TRIM(raw_procedure_code))
         END AS px
       FROM raw_procedures
       WHERE raw_procedure_code IS NOT NULL
         AND TRIM(raw_procedure_code) <> ''
     )
     SELECT DISTINCT
       patid,
       px
     FROM normalized_procedures
     WHERE px ~ '^[A-Z0-9]{5}$'
       AND px ~ '[0-9]$';"
  )

  message("Appending ", analysis_year, " CPT/HCPCS inputs.")
  DatabaseConnector::executeSql(con, px_sql)

  year_counts <- print_query(
    paste0("Completed ", analysis_year, " aggregate input counts."),
    paste0(
      "SELECT
         ", analysis_year, "::INTEGER AS analysis_year,
         (SELECT COUNT(*)
          FROM ", ids_table_identifier, "
          WHERE analysis_year = ", analysis_year, ")::BIGINT AS n_ids,
         (SELECT COUNT(*)
          FROM ", dx10_table_identifier, " dx
          INNER JOIN ", ids_table_identifier, " ids
            ON dx.patid = ids.patid
          WHERE ids.analysis_year = ", analysis_year, ")::BIGINT AS n_dx10_rows,
         (SELECT COUNT(*)
          FROM ", px_table_identifier, " px
          INNER JOIN ", ids_table_identifier, " ids
            ON px.patid = ids.patid
          WHERE ids.analysis_year = ", analysis_year, ")::BIGINT AS n_px_rows"
    )
  )

  if (year_counts$n_ids[[1]] == 0) {
    stop("No CFI IDs remained for ", analysis_year, ".")
  }

  drop_temporary_tables()
}

# ---- Validate final configured outputs ----
output_qa <- print_query(
  "Checking CFI output membership, duplicates, and code formats.",
  paste0(
    "SELECT
       (SELECT COUNT(*)
        FROM ", dx10_table_identifier, " dx
        LEFT JOIN ", ids_table_identifier, " ids
          ON dx.patid = ids.patid
        WHERE ids.patid IS NULL)::BIGINT AS dx_ids_outside_cohort,
       (SELECT COUNT(*)
        FROM ", px_table_identifier, " px
        LEFT JOIN ", ids_table_identifier, " ids
          ON px.patid = ids.patid
        WHERE ids.patid IS NULL)::BIGINT AS px_ids_outside_cohort,
       (SELECT COUNT(*) - COUNT(DISTINCT patid || '|' || dx)
        FROM ", dx10_table_identifier, ")::BIGINT AS duplicate_dx_rows,
       (SELECT COUNT(*) - COUNT(DISTINCT patid || '|' || px)
        FROM ", px_table_identifier, ")::BIGINT AS duplicate_px_rows,
       (SELECT COUNT(*)
        FROM ", dx10_table_identifier, "
        WHERE dx IS NULL
           OR dx = ''
           OR dx !~ '^[A-Z0-9]+$')::BIGINT AS invalid_dx_rows,
       (SELECT COUNT(*)
        FROM ", px_table_identifier, "
        WHERE px IS NULL
           OR px !~ '^[A-Z0-9]{5}$'
           OR px !~ '[0-9]$')::BIGINT AS invalid_px_rows,
       (SELECT COUNT(*)
        FROM ", dx09_table_identifier, ")::BIGINT AS dx09_rows"
  )
)

if (any(unlist(output_qa) != 0)) {
  stop("One or more configured CFI input QA checks failed.")
}

# Compare production 2016 aggregate counts with the separate validation tables.
validation_tables <- c(
  "cfi_2016_ids",
  "cfi_2016_dx10",
  "cfi_2016_px"
)

if (
  isTRUE(cfi_config$compare_2016_tables) &&
    2016L %in% analysis_years &&
    all(vapply(
      validation_tables,
      function(table) table_exists(write_schema, table),
      logical(1)
    ))
) {
  comparison <- print_query(
    "Comparing annual 2016 counts with the 2016 validation tables.",
    paste0(
      "SELECT
         (SELECT COUNT(*) FROM ",
      qualified_identifier(write_schema, "cfi_2016_ids"),
      ")::BIGINT AS validation_ids,
         (SELECT COUNT(*) FROM ", ids_table_identifier, "
          WHERE analysis_year = 2016)::BIGINT AS annual_ids,
         (SELECT COUNT(*) FROM ",
      qualified_identifier(write_schema, "cfi_2016_dx10"),
      ")::BIGINT AS validation_dx10,
         (SELECT COUNT(*) FROM ", dx10_table_identifier, " dx
          INNER JOIN ", ids_table_identifier, " ids
            ON dx.patid = ids.patid
          WHERE ids.analysis_year = 2016)::BIGINT AS annual_dx10,
         (SELECT COUNT(*) FROM ",
      qualified_identifier(write_schema, "cfi_2016_px"),
      ")::BIGINT AS validation_px,
         (SELECT COUNT(*) FROM ", px_table_identifier, " px
          INNER JOIN ", ids_table_identifier, " ids
            ON px.patid = ids.patid
          WHERE ids.analysis_year = 2016)::BIGINT AS annual_px"
    )
  )

  if (
    comparison$validation_ids[[1]] != comparison$annual_ids[[1]] ||
      comparison$validation_dx10[[1]] != comparison$annual_dx10[[1]] ||
      comparison$validation_px[[1]] != comparison$annual_px[[1]]
  ) {
    stop(
      "Annual 2016 aggregate counts do not match the 2016 validation tables."
    )
  }
}

message(
  cfi_config$workflow_label,
  " CFI input preparation complete. Tables created in ",
  write_schema,
  ": ",
  paste(c(ids_table, dx09_table, dx10_table, px_table), collapse = ", "),
  "."
)
