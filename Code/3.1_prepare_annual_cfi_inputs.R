library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual CFI input preparation
# Author: Nemo Zhou
# Date started: 2026-06-13
# Date last updated: 2026-06-13
#
# ---- Purpose ----
# Prepare annual patient-year diagnosis and CPT/HCPCS inputs for Claims-Based
# Frailty Index (CFI) calculation. For each eligible year from 2016 through
# 2025, claims from January 1 through December 31 describe that calendar year,
# and the CFI index date is January 1 of the following year.
#
# The script materializes four tables in the user's Redshift write schema:
#   - cfi_annual_ids
#   - cfi_annual_dx09
#   - cfi_annual_dx10
#   - cfi_annual_px
#
# It does not apply PATIENT_CLOSED eligibility and does not write patient-level
# data to local files. Run Code/0.5_check_event_table_structure.R before this
# production extraction.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Analysis parameters ----
analysis_year_start <- 2016L
analysis_year_end <- 2025L

eligibility_table <- "1_annual_eligible_cohort"
ids_table <- "cfi_annual_ids"
dx09_table <- "cfi_annual_dx09"
dx10_table <- "cfi_annual_dx10"
px_table <- "cfi_annual_px"

inpatient_stage_table <- "cfi_annual_inpatient_stage"
non_inpatient_stage_table <- "cfi_annual_non_inpatient_stage"
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

print_query <- function(label, sql) {
  message(label)
  result <- DBI::dbGetQuery(con, sql)
  print(result)
  invisible(result)
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
table_check <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*)::INTEGER AS table_count
     FROM information_schema.tables
     WHERE table_schema = '", write_schema, "'
       AND table_name = '", eligibility_table, "'"
  )
)

if (
  nrow(table_check) != 1L ||
    is.na(table_check$table_count[[1]]) ||
    table_check$table_count[[1]] != 1L
) {
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
     WHERE table_schema = '", write_schema, "'
       AND table_name = '", eligibility_table, "'"
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

eligible_year_check <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*)::BIGINT AS n_person_years
     FROM ", eligibility_table_identifier, "
     WHERE analysis_year BETWEEN ", analysis_year_start,
    " AND ", analysis_year_end
  )
)

if (
  nrow(eligible_year_check) != 1L ||
    is.na(eligible_year_check$n_person_years[[1]]) ||
    eligible_year_check$n_person_years[[1]] == 0
) {
  stop(
    "No eligible patient-years were found from ",
    analysis_year_start,
    " through ",
    analysis_year_end,
    "."
  )
}

# ---- Materialize the complete annual CFI ID population ----
# patid is unique for each patient-year and is used consistently by all four
# CFI input tables. The original patient_id remains available in the ID table.
ids_sql <- paste0(
  "DROP TABLE IF EXISTS ", ids_table_identifier, ";
CREATE TABLE ", ids_table_identifier, " AS
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
WHERE e.analysis_year BETWEEN ", analysis_year_start,
  " AND ", analysis_year_end, "
  AND e.patient_id IS NOT NULL;"
)

message("Creating annual CFI ID table: ", write_schema, ".", ids_table)
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
  stop("The annual CFI ID table failed its uniqueness or missingness check.")
}

# ---- Restrict event tables to eligible patient-year windows ----
# Staging occurs before array-length measurement so dynamic array expansion is
# based only on events that can contribute to the final CFI inputs.
stage_sql <- paste0(
  "DROP TABLE IF EXISTS ", inpatient_stage_table, ";
CREATE TEMP TABLE ", inpatient_stage_table, " AS
SELECT
  ids.patid,
  ids.analysis_year,
  i.patient_id,
  i.claim_from_date AS event_date,
  i.admission_diagnosis_code,
  i.primary_diagnosis_code,
  i.secondary_diagnosis_codes,
  i.cpt_hcpcs_codes
FROM ", ids_table_identifier, " ids
INNER JOIN ", komodo_schema, ".inpatient_events i
  ON ids.patient_id = i.patient_id
 AND i.claim_from_date >= ids.lookback_start
 AND i.claim_from_date < ids.lookback_end;

DROP TABLE IF EXISTS ", non_inpatient_stage_table, ";
CREATE TEMP TABLE ", non_inpatient_stage_table, " AS
SELECT
  ids.patid,
  ids.analysis_year,
  n.patient_id,
  n.service_date AS event_date,
  n.diagnosis_codes,
  n.procedure_code
FROM ", ids_table_identifier, " ids
INNER JOIN ", komodo_schema, ".non_inpatient_events n
  ON ids.patient_id = n.patient_id
 AND n.service_date >= ids.lookback_start
 AND n.service_date < ids.lookback_end;"
)

message("Creating cohort- and date-restricted event staging tables.")
DatabaseConnector::executeSql(con, stage_sql)

# ---- Check staged event counts and selected date completeness ----
# Null event dates cannot satisfy the lookback join, but this explicit check
# documents that the canonical dates are complete in the staged inputs.
print_query(
  "Checking staged event counts and selected event dates.",
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

# ---- Validate JSON arrays before production flattening ----
array_validity <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       SUM(CASE
         WHEN secondary_diagnosis_codes IS NOT NULL
          AND TRIM(secondary_diagnosis_codes) <> ''
          AND JSON_ARRAY_LENGTH(secondary_diagnosis_codes, TRUE) IS NULL
         THEN 1 ELSE 0
       END)::BIGINT AS invalid_secondary_diagnosis_arrays,
       SUM(CASE
         WHEN cpt_hcpcs_codes IS NOT NULL
          AND TRIM(cpt_hcpcs_codes) <> ''
          AND JSON_ARRAY_LENGTH(cpt_hcpcs_codes, TRUE) IS NULL
         THEN 1 ELSE 0
       END)::BIGINT AS invalid_cpt_hcpcs_arrays
     FROM ", inpatient_stage_table
  )
)

non_inpatient_array_validity <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       SUM(CASE
         WHEN diagnosis_codes IS NOT NULL
          AND TRIM(diagnosis_codes) <> ''
          AND JSON_ARRAY_LENGTH(diagnosis_codes, TRUE) IS NULL
         THEN 1 ELSE 0
       END)::BIGINT AS invalid_diagnosis_arrays
     FROM ", non_inpatient_stage_table
  )
)

invalid_array_count <- sum(
  unlist(array_validity),
  unlist(non_inpatient_array_validity),
  na.rm = TRUE
)

if (invalid_array_count > 0) {
  stop(
    "Invalid JSON-style arrays were found in the restricted event data. ",
    "Review the source structure before continuing."
  )
}

# ---- Measure actual maximum array lengths after restriction ----
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
  "Maximum diagnosis array length after restriction: ",
  max_diagnosis_array_length
)
message(
  "Maximum inpatient CPT/HCPCS array length after restriction: ",
  max_procedure_array_length
)

# ---- Generate complete dynamic array-position tables ----
# The recursion endpoints are the observed maxima above. There is no fixed
# production flattening cap.
position_sql <- paste0(
  "DROP TABLE IF EXISTS ", diagnosis_position_table, ";
CREATE TEMP TABLE ", diagnosis_position_table, " AS
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

DROP TABLE IF EXISTS ", procedure_position_table, ";
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

message("Creating dynamic array-position tables.")
DatabaseConnector::executeSql(con, position_sql)

# ---- Create empty ICD-9-compatible input table ----
# The selected study years begin after the US ICD-10-CM transition.
dx09_sql <- paste0(
  "DROP TABLE IF EXISTS ", dx09_table_identifier, ";
CREATE TABLE ", dx09_table_identifier, " AS
SELECT
  CAST(NULL AS VARCHAR(256)) AS patid,
  CAST(NULL AS VARCHAR(20)) AS dx
WHERE 1 = 0;"
)

message("Creating empty ICD-9 input table: ", write_schema, ".", dx09_table)
DatabaseConnector::executeSql(con, dx09_sql)

# ---- Extract and normalize annual ICD-10-CM diagnoses ----
# Inpatient diagnosis sources are admission, primary, and every secondary array
# element. NON_INPATIENT_EVENTS.diagnosis_codes already contains primary codes.
dx10_sql <- paste0(
  "DROP TABLE IF EXISTS ", dx10_table_identifier, ";
CREATE TABLE ", dx10_table_identifier, " AS
WITH raw_diagnoses AS (
  SELECT
    patid,
    event_date AS diagnosis_date,
    admission_diagnosis_code AS raw_diagnosis_code
  FROM ", inpatient_stage_table, "
  WHERE admission_diagnosis_code IS NOT NULL

  UNION ALL

  SELECT
    patid,
    event_date AS diagnosis_date,
    primary_diagnosis_code AS raw_diagnosis_code
  FROM ", inpatient_stage_table, "
  WHERE primary_diagnosis_code IS NOT NULL

  UNION ALL

  SELECT
    i.patid,
    i.event_date AS diagnosis_date,
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
    n.event_date AS diagnosis_date,
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
    diagnosis_date,
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
WHERE diagnosis_date IS NOT NULL
  AND dx IS NOT NULL
  AND dx <> ''
  AND dx ~ '^[A-Z0-9]+$';"
)

message("Creating annual ICD-10 input table: ", write_schema, ".", dx10_table)
DatabaseConnector::executeSql(con, dx10_sql)

# ---- Extract and normalize annual CPT/HCPCS procedures ----
# ICD-PCS is intentionally excluded. The final-numeric rule mirrors the
# supplied CFI model implementation.
px_sql <- paste0(
  "DROP TABLE IF EXISTS ", px_table_identifier, ";
CREATE TABLE ", px_table_identifier, " AS
WITH raw_procedures AS (
  SELECT
    i.patid,
    i.event_date AS procedure_date,
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
    event_date AS procedure_date,
    procedure_code AS raw_procedure_code
  FROM ", non_inpatient_stage_table, "
  WHERE procedure_code IS NOT NULL
),
normalized_procedures AS (
  SELECT
    patid,
    procedure_date,
    UPPER(TRIM(raw_procedure_code)) AS px
  FROM raw_procedures
  WHERE raw_procedure_code IS NOT NULL
    AND TRIM(raw_procedure_code) <> ''
)
SELECT DISTINCT
  patid,
  px
FROM normalized_procedures
WHERE procedure_date IS NOT NULL
  AND px ~ '^[A-Z0-9]{5}$'
  AND px ~ '[0-9]$';"
)

message("Creating annual CPT/HCPCS input table: ", write_schema, ".", px_table)
DatabaseConnector::executeSql(con, px_sql)

# ---- Aggregate annual input counts ----
annual_input_counts <- print_query(
  "Counting annual CFI input rows.",
  paste0(
    "WITH RECURSIVE years(analysis_year) AS (
       SELECT ", analysis_year_start, "
       UNION ALL
       SELECT analysis_year + 1
       FROM years
       WHERE analysis_year + 1 <= ", analysis_year_end, "
     ),
     id_counts AS (
       SELECT analysis_year, COUNT(*)::BIGINT AS n_ids
       FROM ", ids_table_identifier, "
       GROUP BY analysis_year
     ),
     dx_counts AS (
       SELECT ids.analysis_year, COUNT(*)::BIGINT AS n_dx10_rows
       FROM ", dx10_table_identifier, " dx
       INNER JOIN ", ids_table_identifier, " ids
         ON dx.patid = ids.patid
       GROUP BY ids.analysis_year
     ),
     px_counts AS (
       SELECT ids.analysis_year, COUNT(*)::BIGINT AS n_px_rows
       FROM ", px_table_identifier, " px
       INNER JOIN ", ids_table_identifier, " ids
         ON px.patid = ids.patid
       GROUP BY ids.analysis_year
     )
     SELECT
       y.analysis_year,
       COALESCE(i.n_ids, 0)::BIGINT AS n_ids,
       COALESCE(d.n_dx10_rows, 0)::BIGINT AS n_dx10_rows,
       COALESCE(p.n_px_rows, 0)::BIGINT AS n_px_rows
     FROM years y
     LEFT JOIN id_counts i
       ON y.analysis_year = i.analysis_year
     LEFT JOIN dx_counts d
       ON y.analysis_year = d.analysis_year
     LEFT JOIN px_counts p
       ON y.analysis_year = p.analysis_year
     ORDER BY y.analysis_year"
  )
)

# ---- Validate output membership, uniqueness, and formats ----
output_qa <- print_query(
  "Checking CFI input membership, duplicates, and code formats.",
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
  stop("One or more annual CFI input QA checks failed.")
}

# ---- Report excluded procedure formats ----
print_query(
  "Counting procedure records excluded by the CFI format rule.",
  paste0(
    "WITH raw_procedures AS (
       SELECT
         JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
           i.cpt_hcpcs_codes,
           p.array_index,
           TRUE
         ) AS raw_procedure_code
       FROM ", inpatient_stage_table, " i
       INNER JOIN ", procedure_position_table, " p
         ON p.array_index < JSON_ARRAY_LENGTH(i.cpt_hcpcs_codes, TRUE)

       UNION ALL

       SELECT procedure_code AS raw_procedure_code
       FROM ", non_inpatient_stage_table, "
       WHERE procedure_code IS NOT NULL
     ),
     normalized AS (
       SELECT UPPER(TRIM(raw_procedure_code)) AS px
       FROM raw_procedures
       WHERE raw_procedure_code IS NOT NULL
         AND TRIM(raw_procedure_code) <> ''
     )
     SELECT
       COUNT(*)::BIGINT AS nonblank_procedure_rows,
       SUM(CASE WHEN px !~ '^[A-Z0-9]{5}$'
         THEN 1 ELSE 0 END)::BIGINT AS excluded_non_five_character,
       SUM(CASE WHEN px ~ '^[A-Z0-9]{5}$'
                 AND px !~ '[0-9]$'
         THEN 1 ELSE 0 END)::BIGINT AS excluded_letter_ending
     FROM normalized"
  )
)

# ---- Confirm complete dynamic array expansion ----
array_position_qa <- print_query(
  "Confirming that dynamic array positions cover observed maxima.",
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
  stop("Dynamic array-position generation did not cover an observed maximum.")
}

if (any(annual_input_counts$n_ids == 0)) {
  warning(
    "At least one requested analysis year has no eligible IDs. ",
    "Review the annual counts above."
  )
}

message(
  "Annual CFI input preparation complete. Tables created in ",
  write_schema,
  ": ",
  paste(c(ids_table, dx09_table, dx10_table, px_table), collapse = ", "),
  "."
)
