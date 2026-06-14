library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto event-table structure diagnostics
# Author: Nemo Zhou
# Date started: 2026-06-12
# Date last updated: 2026-06-12
#
# ---- Purpose ----
# Validate the documented diagnosis and procedure structures in Komodo
# INPATIENT_EVENTS and NON_INPATIENT_EVENTS on small random samples. The script
# checks the selected event dates, scalar versus JSON-style array fields,
# exact array-element extraction, diagnosis normalization, and five-character
# CPT/HCPCS formatting before production CFI extraction is implemented.
#
# This diagnostic uses temporary Redshift tables and prints aggregate checks
# only. It does not print patient identifiers, dates, or individual codes.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Diagnostic parameters ----
# Sampling is intentionally small. Increase the probabilities only if a run
# returns too few rows for a useful structural check.
inpatient_sample_probability <- 0.001
non_inpatient_sample_probability <- 0.00001
max_sample_rows <- 5000L

# Arrays longer than this cap are counted but not completely flattened.
# The cap is deliberately generous for a small structural diagnostic.
max_array_elements <- 100L

inpatient_sample_table <- "event_structure_inpatient_sample"
non_inpatient_sample_table <- "event_structure_non_inpatient_sample"
diagnosis_long_table <- "event_structure_diagnosis_long"
procedure_long_table <- "event_structure_procedure_long"

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Helper: run an aggregate query and print its result ----
print_query <- function(label, sql) {
  message(label)
  result <- DBI::dbGetQuery(con, sql)
  print(result)
  invisible(result)
}

# ---- Create small random event samples ----
# Temporary tables remain only for the active database session.
sample_sql <- paste0(
  "DROP TABLE IF EXISTS ", inpatient_sample_table, ";
CREATE TEMP TABLE ", inpatient_sample_table, " AS
SELECT
  ROW_NUMBER() OVER () AS sample_row_id,
  patient_id,
  claim_from_date,
  admit_date,
  admission_diagnosis_code,
  primary_diagnosis_code,
  secondary_diagnosis_codes,
  cpt_hcpcs_codes,
  icd_pcs_codes
FROM (
  SELECT
    patient_id,
    claim_from_date,
    admit_date,
    admission_diagnosis_code,
    primary_diagnosis_code,
    secondary_diagnosis_codes,
    cpt_hcpcs_codes,
    icd_pcs_codes
  FROM ", komodo_schema, ".inpatient_events
  WHERE RANDOM() < ", inpatient_sample_probability, "
  LIMIT ", max_sample_rows, "
) sampled_inpatient;

DROP TABLE IF EXISTS ", non_inpatient_sample_table, ";
CREATE TEMP TABLE ", non_inpatient_sample_table, " AS
SELECT
  ROW_NUMBER() OVER () AS sample_row_id,
  patient_id,
  service_date,
  service_to_date,
  primary_diagnosis_code_array,
  diagnosis_codes,
  procedure_code,
  modifiers,
  icd_pcs_codes
FROM (
  SELECT
    patient_id,
    service_date,
    service_to_date,
    primary_diagnosis_code_array,
    diagnosis_codes,
    procedure_code,
    modifiers,
    icd_pcs_codes
  FROM ", komodo_schema, ".non_inpatient_events
  WHERE RANDOM() < ", non_inpatient_sample_probability, "
  LIMIT ", max_sample_rows, "
) sampled_non_inpatient;"
)

message("Creating temporary small-sample event tables.")
DatabaseConnector::executeSql(con, sample_sql)

# ---- Check sample size and event-date assumptions ----
print_query(
  "Checking sampled row counts and event-date completeness.",
  paste0(
    "SELECT
      'inpatient' AS source_table,
      COUNT(*)::BIGINT AS sampled_rows,
      SUM(CASE WHEN patient_id IS NULL THEN 1 ELSE 0 END)::BIGINT
        AS missing_patient_id,
      SUM(CASE WHEN claim_from_date IS NULL THEN 1 ELSE 0 END)::BIGINT
        AS missing_selected_event_date,
      SUM(CASE WHEN admit_date IS NULL THEN 1 ELSE 0 END)::BIGINT
        AS missing_alternative_date
    FROM ", inpatient_sample_table, "
    UNION ALL
    SELECT
      'non_inpatient' AS source_table,
      COUNT(*)::BIGINT AS sampled_rows,
      SUM(CASE WHEN patient_id IS NULL THEN 1 ELSE 0 END)::BIGINT
        AS missing_patient_id,
      SUM(CASE WHEN service_date IS NULL THEN 1 ELSE 0 END)::BIGINT
        AS missing_selected_event_date,
      SUM(CASE WHEN service_to_date IS NULL THEN 1 ELSE 0 END)::BIGINT
        AS missing_alternative_date
    FROM ", non_inpatient_sample_table, "
    ORDER BY source_table"
  )
)

# ---- Validate JSON-style array representation ----
# JSON_ARRAY_LENGTH(..., TRUE) returns NULL rather than failing for invalid JSON.
array_check_sql <- paste0(
  "SELECT
    source_table,
    field_name,
    COUNT(*)::BIGINT AS sampled_rows,
    SUM(CASE WHEN array_text IS NULL OR TRIM(array_text) = ''
      THEN 1 ELSE 0 END)::BIGINT AS missing_or_blank,
    SUM(CASE WHEN array_text IS NOT NULL
              AND TRIM(array_text) <> ''
              AND JSON_ARRAY_LENGTH(array_text, TRUE) IS NULL
      THEN 1 ELSE 0 END)::BIGINT AS invalid_json_arrays,
    SUM(CASE WHEN JSON_ARRAY_LENGTH(array_text, TRUE) = 0
      THEN 1 ELSE 0 END)::BIGINT AS empty_arrays,
    MAX(JSON_ARRAY_LENGTH(array_text, TRUE)) AS maximum_array_length,
    SUM(CASE WHEN JSON_ARRAY_LENGTH(array_text, TRUE) > ",
    max_array_elements, " THEN 1 ELSE 0 END)::BIGINT AS arrays_over_flatten_cap
  FROM (
    SELECT
      'inpatient' AS source_table,
      'secondary_diagnosis_codes' AS field_name,
      secondary_diagnosis_codes AS array_text
    FROM ", inpatient_sample_table, "
    UNION ALL
    SELECT
      'inpatient',
      'cpt_hcpcs_codes',
      cpt_hcpcs_codes
    FROM ", inpatient_sample_table, "
    UNION ALL
    SELECT
      'inpatient',
      'icd_pcs_codes',
      icd_pcs_codes
    FROM ", inpatient_sample_table, "
    UNION ALL
    SELECT
      'non_inpatient',
      'primary_diagnosis_code_array',
      primary_diagnosis_code_array
    FROM ", non_inpatient_sample_table, "
    UNION ALL
    SELECT
      'non_inpatient',
      'diagnosis_codes',
      diagnosis_codes
    FROM ", non_inpatient_sample_table, "
    UNION ALL
    SELECT
      'non_inpatient',
      'modifiers',
      modifiers
    FROM ", non_inpatient_sample_table, "
    UNION ALL
    SELECT
      'non_inpatient',
      'icd_pcs_codes',
      icd_pcs_codes
    FROM ", non_inpatient_sample_table, "
  ) arrays
  GROUP BY source_table, field_name
  ORDER BY source_table, field_name"
)

print_query("Checking JSON-style array fields.", array_check_sql)

# ---- Flatten diagnosis arrays and combine scalar diagnosis fields ----
# Array elements are extracted exactly by position. No substring matching is
# used. Normalization occurs only after each complete code has been extracted.
diagnosis_long_sql <- paste0(
  "DROP TABLE IF EXISTS ", diagnosis_long_table, ";
CREATE TEMP TABLE ", diagnosis_long_table, " AS
WITH RECURSIVE array_positions(array_index) AS (
  SELECT 0
  UNION ALL
  SELECT array_index + 1
  FROM array_positions
  WHERE array_index + 1 < ", max_array_elements, "
),
raw_diagnoses AS (
  SELECT
    patient_id,
    claim_from_date AS diagnosis_date,
    admission_diagnosis_code AS raw_diagnosis_code,
    'inpatient_admission'::VARCHAR(40) AS diagnosis_source
  FROM ", inpatient_sample_table, "
  WHERE admission_diagnosis_code IS NOT NULL

  UNION ALL

  SELECT
    patient_id,
    claim_from_date AS diagnosis_date,
    primary_diagnosis_code AS raw_diagnosis_code,
    'inpatient_primary'::VARCHAR(40) AS diagnosis_source
  FROM ", inpatient_sample_table, "
  WHERE primary_diagnosis_code IS NOT NULL

  UNION ALL

  SELECT
    i.patient_id,
    i.claim_from_date AS diagnosis_date,
    JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
      i.secondary_diagnosis_codes,
      p.array_index,
      TRUE
    ) AS raw_diagnosis_code,
    'inpatient_secondary'::VARCHAR(40) AS diagnosis_source
  FROM ", inpatient_sample_table, " i
  CROSS JOIN array_positions p
  WHERE p.array_index < JSON_ARRAY_LENGTH(i.secondary_diagnosis_codes, TRUE)

  UNION ALL

  SELECT
    n.patient_id,
    n.service_date AS diagnosis_date,
    JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
      n.diagnosis_codes,
      p.array_index,
      TRUE
    ) AS raw_diagnosis_code,
    'non_inpatient_all'::VARCHAR(40) AS diagnosis_source
  FROM ", non_inpatient_sample_table, " n
  CROSS JOIN array_positions p
  WHERE p.array_index < JSON_ARRAY_LENGTH(n.diagnosis_codes, TRUE)
)
SELECT
  patient_id,
  diagnosis_date,
  UPPER(REGEXP_REPLACE(TRIM(raw_diagnosis_code), '[^A-Za-z0-9]', ''))
    AS diagnosis_code,
  diagnosis_source
FROM raw_diagnoses
WHERE raw_diagnosis_code IS NOT NULL
  AND TRIM(raw_diagnosis_code) <> '';"
)

message("Creating temporary normalized diagnosis table.")
DatabaseConnector::executeSql(con, diagnosis_long_sql)

# ---- Check diagnosis normalization and source structure ----
print_query(
  "Checking the canonical diagnosis structure.",
  paste0(
    "SELECT
      diagnosis_source,
      COUNT(*)::BIGINT AS extracted_rows,
      COUNT(DISTINCT patient_id)::BIGINT AS patients,
      SUM(CASE WHEN diagnosis_date IS NULL THEN 1 ELSE 0 END)::BIGINT
        AS missing_diagnosis_date,
      SUM(CASE WHEN diagnosis_code IS NULL OR diagnosis_code = ''
        THEN 1 ELSE 0 END)::BIGINT AS blank_normalized_codes,
      SUM(CASE WHEN diagnosis_code !~ '^[A-Z0-9]+$'
        THEN 1 ELSE 0 END)::BIGINT AS non_alphanumeric_codes,
      SUM(CASE WHEN diagnosis_code ~ '[.]'
        THEN 1 ELSE 0 END)::BIGINT AS codes_with_decimal_points
    FROM ", diagnosis_long_table, "
    GROUP BY diagnosis_source
    ORDER BY diagnosis_source"
  )
)

# ---- Test whether primary non-inpatient codes occur in diagnosis_codes ----
# This checks exact normalized array elements, not text containment.
primary_containment_sql <- paste0(
  "WITH RECURSIVE array_positions(array_index) AS (
    SELECT 0
    UNION ALL
    SELECT array_index + 1
    FROM array_positions
    WHERE array_index + 1 < ", max_array_elements, "
  ),
  primary_codes AS (
    SELECT DISTINCT
      n.sample_row_id,
      UPPER(REGEXP_REPLACE(
        TRIM(JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
          n.primary_diagnosis_code_array,
          p.array_index,
          TRUE
        )),
        '[^A-Za-z0-9]',
        ''
      )) AS diagnosis_code
    FROM ", non_inpatient_sample_table, " n
    CROSS JOIN array_positions p
    WHERE p.array_index <
      JSON_ARRAY_LENGTH(n.primary_diagnosis_code_array, TRUE)
  ),
  all_codes AS (
    SELECT DISTINCT
      n.sample_row_id,
      UPPER(REGEXP_REPLACE(
        TRIM(JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
          n.diagnosis_codes,
          p.array_index,
          TRUE
        )),
        '[^A-Za-z0-9]',
        ''
      )) AS diagnosis_code
    FROM ", non_inpatient_sample_table, " n
    CROSS JOIN array_positions p
    WHERE p.array_index < JSON_ARRAY_LENGTH(n.diagnosis_codes, TRUE)
  )
  SELECT
    COUNT(*)::BIGINT AS sampled_primary_code_elements,
    SUM(CASE WHEN a.diagnosis_code IS NOT NULL
      THEN 1 ELSE 0 END)::BIGINT AS found_in_diagnosis_codes,
    SUM(CASE WHEN a.diagnosis_code IS NULL
      THEN 1 ELSE 0 END)::BIGINT AS not_found_in_diagnosis_codes
  FROM primary_codes p
  LEFT JOIN all_codes a
    ON p.sample_row_id = a.sample_row_id
   AND p.diagnosis_code = a.diagnosis_code
  WHERE p.diagnosis_code IS NOT NULL
    AND p.diagnosis_code <> ''"
)

print_query(
  "Checking whether primary non-inpatient diagnoses are contained in diagnosis_codes.",
  primary_containment_sql
)

# ---- Flatten inpatient CPT/HCPCS and combine non-inpatient procedure codes ----
procedure_long_sql <- paste0(
  "DROP TABLE IF EXISTS ", procedure_long_table, ";
CREATE TEMP TABLE ", procedure_long_table, " AS
WITH RECURSIVE array_positions(array_index) AS (
  SELECT 0
  UNION ALL
  SELECT array_index + 1
  FROM array_positions
  WHERE array_index + 1 < ", max_array_elements, "
),
raw_procedures AS (
  SELECT
    i.patient_id,
    i.claim_from_date AS procedure_date,
    JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
      i.cpt_hcpcs_codes,
      p.array_index,
      TRUE
    ) AS raw_procedure_code,
    'inpatient_cpt_hcpcs'::VARCHAR(40) AS procedure_source
  FROM ", inpatient_sample_table, " i
  CROSS JOIN array_positions p
  WHERE p.array_index < JSON_ARRAY_LENGTH(i.cpt_hcpcs_codes, TRUE)

  UNION ALL

  SELECT
    patient_id,
    service_date AS procedure_date,
    procedure_code AS raw_procedure_code,
    'non_inpatient_procedure'::VARCHAR(40) AS procedure_source
  FROM ", non_inpatient_sample_table, "
  WHERE procedure_code IS NOT NULL
)
SELECT
  patient_id,
  procedure_date,
  UPPER(TRIM(raw_procedure_code)) AS procedure_code,
  procedure_source
FROM raw_procedures
WHERE raw_procedure_code IS NOT NULL
  AND TRIM(raw_procedure_code) <> '';"
)

message("Creating temporary normalized procedure table.")
DatabaseConnector::executeSql(con, procedure_long_sql)

# ---- Check procedure formatting without printing codes ----
print_query(
  "Checking the canonical CPT/HCPCS procedure structure.",
  paste0(
    "SELECT
      procedure_source,
      COUNT(*)::BIGINT AS extracted_rows,
      COUNT(DISTINCT patient_id)::BIGINT AS patients,
      SUM(CASE WHEN procedure_date IS NULL THEN 1 ELSE 0 END)::BIGINT
        AS missing_procedure_date,
      SUM(CASE WHEN procedure_code ~ '^[A-Z0-9]{5}$'
        THEN 1 ELSE 0 END)::BIGINT AS five_character_alphanumeric,
      SUM(CASE WHEN procedure_code !~ '^[A-Z0-9]{5}$'
        THEN 1 ELSE 0 END)::BIGINT AS other_formats,
      SUM(CASE WHEN procedure_code ~ '^[A-Z0-9]{5}$'
                AND procedure_code ~ '[0-9]$'
        THEN 1 ELSE 0 END)::BIGINT AS cfi_format_ending_numeric
    FROM ", procedure_long_table, "
    GROUP BY procedure_source
    ORDER BY procedure_source"
  )
)

# ---- Check duplicate presence after canonical extraction ----
print_query(
  "Checking duplicate patient-date-code combinations.",
  paste0(
    "SELECT
      'diagnosis' AS event_type,
      COUNT(*)::BIGINT AS extracted_rows,
      COUNT(*) - COUNT(DISTINCT
        COALESCE(patient_id, '') || '|' ||
        COALESCE(diagnosis_date::VARCHAR, '') || '|' ||
        COALESCE(diagnosis_code, '')
      ) AS duplicate_patient_date_code_rows
    FROM ", diagnosis_long_table, "
    UNION ALL
    SELECT
      'procedure' AS event_type,
      COUNT(*)::BIGINT AS extracted_rows,
      COUNT(*) - COUNT(DISTINCT
        COALESCE(patient_id, '') || '|' ||
        COALESCE(procedure_date::VARCHAR, '') || '|' ||
        COALESCE(procedure_code, '')
      ) AS duplicate_patient_date_code_rows
    FROM ", procedure_long_table, "
    ORDER BY event_type"
  )
)

message(
  "Event-table structure diagnostic complete. ",
  "Review all aggregate checks before implementing production extraction."
)
