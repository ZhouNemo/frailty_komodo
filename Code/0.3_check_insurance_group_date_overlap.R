library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual eligibility diagnostics
# Author: Nemo Zhou
# Date started: 2026-06-03
# Date last updated: 2026-06-03
#
# Purpose:
# This diagnostic script checks whether a small random sample of patients in
# Komodo PATIENT_INSURANCE has overlapping payer-attribution date spans with
# different primary medical or prescription insurance groups at the same time.
# It is meant to answer whether same-time multiple primary
# mx_insurance_group or rx_insurance_group values appear in real data.
#
# Run after:
#   Code/0.2_model_komodo_workflow.R
#     Use this earlier model workflow as the reference for Redshift connection
#     setup, schema definitions, and project database conventions.
#
# Run before:
#   Code/1.1_build_annual_eligible_population.R
#     This diagnostic can inform whether the annual eligibility logic should
#     allow multi-group attribution instead of excluding patient-years with
#     multiple primary Mx or Rx groups.
#
# Privacy note:
# The script materializes a sampled patient list and overlap summaries in the
# user's write schema, but it only prints aggregate counts and suppressed
# group-pair summaries. It does not print patient IDs or raw patient-level rows.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Diagnostic parameters ----
# Increase sample_probability or max_sampled_patients if the first run returns
# too few sampled patients to observe rare overlap patterns.
sample_probability <- 0.0001
max_sampled_patients <- 5000L
min_count <- 11L

sample_patient_table <- "insurance_overlap_check_sample_patients"
overlap_summary_table <- "insurance_group_overlap_check_summary"
overlap_group_pair_table <- "insurance_group_overlap_check_group_pairs"

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Sample patients from PATIENT_INSURANCE ----
# RANDOM() avoids taking the first rows in storage order. The LIMIT prevents a
# larger-than-intended sample if the probability is too high for the table size.
sample_sql <- paste0(
  "DROP TABLE IF EXISTS ", write_schema, ".", sample_patient_table, ";
CREATE TABLE ", write_schema, ".", sample_patient_table, " AS
SELECT patient_id
FROM (
  SELECT DISTINCT patient_id
  FROM ", komodo_schema, ".patient_insurance
  WHERE patient_id IS NOT NULL
    AND RANDOM() < ", sample_probability, "
  LIMIT ", max_sampled_patients, "
) sampled_patients"
)

message("Creating sampled patient table: ", write_schema, ".", sample_patient_table)
DatabaseConnector::executeSql(con, sample_sql)

# ---- Summarize overlapping date spans with different primary groups ----
# Two PATIENT_INSURANCE rows overlap in time when each row starts on or before
# the other row ends. Within those overlapping pairs, this script separately
# flags different known primary Mx and Rx insurance groups.
overlap_summary_sql <- paste0(
  "DROP TABLE IF EXISTS ", write_schema, ".", overlap_summary_table, ";
CREATE TABLE ", write_schema, ".", overlap_summary_table, " AS
WITH sampled_insurance AS (
  SELECT
    pi.patient_id,
    pi.row_valid_start,
    pi.row_valid_end,
    pi.mx_insurance_group,
    pi.rx_insurance_group,
    ROW_NUMBER() OVER (
      PARTITION BY pi.patient_id
      ORDER BY
        pi.row_valid_start,
        pi.row_valid_end,
        COALESCE(pi.mx_insurance_group, ''),
        COALESCE(pi.rx_insurance_group, '')
    ) AS insurance_row_n
  FROM ", komodo_schema, ".patient_insurance pi
  INNER JOIN ", write_schema, ".", sample_patient_table, " sp
    ON pi.patient_id = sp.patient_id
  WHERE pi.row_valid_start IS NOT NULL
    AND pi.row_valid_end IS NOT NULL
),
overlapping_pairs AS (
  SELECT
    a.patient_id,
    CASE
      WHEN a.row_valid_start > b.row_valid_start THEN a.row_valid_start
      ELSE b.row_valid_start
    END AS overlap_start,
    CASE
      WHEN a.row_valid_end < b.row_valid_end THEN a.row_valid_end
      ELSE b.row_valid_end
    END AS overlap_end,
    a.mx_insurance_group AS mx_group_a,
    b.mx_insurance_group AS mx_group_b,
    a.rx_insurance_group AS rx_group_a,
    b.rx_insurance_group AS rx_group_b,
    CASE
      WHEN a.mx_insurance_group IS NOT NULL
       AND b.mx_insurance_group IS NOT NULL
       AND a.mx_insurance_group <> 'UNKNOWN'
       AND b.mx_insurance_group <> 'UNKNOWN'
       AND a.mx_insurance_group <> b.mx_insurance_group
      THEN 1 ELSE 0
    END AS has_distinct_known_mx_group,
    CASE
      WHEN a.rx_insurance_group IS NOT NULL
       AND b.rx_insurance_group IS NOT NULL
       AND a.rx_insurance_group <> 'UNKNOWN'
       AND b.rx_insurance_group <> 'UNKNOWN'
       AND a.rx_insurance_group <> b.rx_insurance_group
      THEN 1 ELSE 0
    END AS has_distinct_known_rx_group
  FROM sampled_insurance a
  INNER JOIN sampled_insurance b
    ON a.patient_id = b.patient_id
   AND a.insurance_row_n < b.insurance_row_n
   AND a.row_valid_start <= b.row_valid_end
   AND b.row_valid_start <= a.row_valid_end
)
SELECT
  'sampled_patients' AS metric,
  COUNT(*)::BIGINT AS value
FROM ", write_schema, ".", sample_patient_table, "
UNION ALL
SELECT
  'sampled_insurance_rows' AS metric,
  COUNT(*)::BIGINT AS value
FROM sampled_insurance
UNION ALL
SELECT
  'overlapping_insurance_row_pairs' AS metric,
  COUNT(*)::BIGINT AS value
FROM overlapping_pairs
UNION ALL
SELECT
  'overlapping_pairs_with_distinct_known_mx_groups' AS metric,
  COUNT(*)::BIGINT AS value
FROM overlapping_pairs
WHERE has_distinct_known_mx_group = 1
UNION ALL
SELECT
  'overlapping_pairs_with_distinct_known_rx_groups' AS metric,
  COUNT(*)::BIGINT AS value
FROM overlapping_pairs
WHERE has_distinct_known_rx_group = 1
UNION ALL
SELECT
  'patients_with_overlapping_distinct_known_mx_groups' AS metric,
  COUNT(DISTINCT patient_id)::BIGINT AS value
FROM overlapping_pairs
WHERE has_distinct_known_mx_group = 1
UNION ALL
SELECT
  'patients_with_overlapping_distinct_known_rx_groups' AS metric,
  COUNT(DISTINCT patient_id)::BIGINT AS value
FROM overlapping_pairs
WHERE has_distinct_known_rx_group = 1
UNION ALL
SELECT
  'patients_with_overlapping_distinct_known_mx_or_rx_groups' AS metric,
  COUNT(DISTINCT patient_id)::BIGINT AS value
FROM overlapping_pairs
WHERE has_distinct_known_mx_group = 1
   OR has_distinct_known_rx_group = 1"
)

message("Creating aggregate overlap summary: ", write_schema, ".", overlap_summary_table)
DatabaseConnector::executeSql(con, overlap_summary_sql)

# ---- Summarize which group pairs overlap, with small cells suppressed ----
overlap_group_pair_sql <- paste0(
  "DROP TABLE IF EXISTS ", write_schema, ".", overlap_group_pair_table, ";
CREATE TABLE ", write_schema, ".", overlap_group_pair_table, " AS
WITH sampled_insurance AS (
  SELECT
    pi.patient_id,
    pi.row_valid_start,
    pi.row_valid_end,
    pi.mx_insurance_group,
    pi.rx_insurance_group,
    ROW_NUMBER() OVER (
      PARTITION BY pi.patient_id
      ORDER BY
        pi.row_valid_start,
        pi.row_valid_end,
        COALESCE(pi.mx_insurance_group, ''),
        COALESCE(pi.rx_insurance_group, '')
    ) AS insurance_row_n
  FROM ", komodo_schema, ".patient_insurance pi
  INNER JOIN ", write_schema, ".", sample_patient_table, " sp
    ON pi.patient_id = sp.patient_id
  WHERE pi.row_valid_start IS NOT NULL
    AND pi.row_valid_end IS NOT NULL
),
overlapping_pairs AS (
  SELECT
    a.patient_id,
    a.mx_insurance_group AS mx_group_a,
    b.mx_insurance_group AS mx_group_b,
    a.rx_insurance_group AS rx_group_a,
    b.rx_insurance_group AS rx_group_b
  FROM sampled_insurance a
  INNER JOIN sampled_insurance b
    ON a.patient_id = b.patient_id
   AND a.insurance_row_n < b.insurance_row_n
   AND a.row_valid_start <= b.row_valid_end
   AND b.row_valid_start <= a.row_valid_end
),
distinct_group_pairs AS (
  SELECT
    'MX' AS coverage_type,
    CASE
      WHEN mx_group_a < mx_group_b THEN mx_group_a
      ELSE mx_group_b
    END AS group_1,
    CASE
      WHEN mx_group_a < mx_group_b THEN mx_group_b
      ELSE mx_group_a
    END AS group_2,
    patient_id
  FROM overlapping_pairs
  WHERE mx_group_a IS NOT NULL
    AND mx_group_b IS NOT NULL
    AND mx_group_a <> 'UNKNOWN'
    AND mx_group_b <> 'UNKNOWN'
    AND mx_group_a <> mx_group_b
  UNION ALL
  SELECT
    'RX' AS coverage_type,
    CASE
      WHEN rx_group_a < rx_group_b THEN rx_group_a
      ELSE rx_group_b
    END AS group_1,
    CASE
      WHEN rx_group_a < rx_group_b THEN rx_group_b
      ELSE rx_group_a
    END AS group_2,
    patient_id
  FROM overlapping_pairs
  WHERE rx_group_a IS NOT NULL
    AND rx_group_b IS NOT NULL
    AND rx_group_a <> 'UNKNOWN'
    AND rx_group_b <> 'UNKNOWN'
    AND rx_group_a <> rx_group_b
)
SELECT
  coverage_type,
  group_1,
  group_2,
  COUNT(*)::BIGINT AS n_overlap_pairs,
  COUNT(DISTINCT patient_id)::BIGINT AS n_patients
FROM distinct_group_pairs
GROUP BY coverage_type, group_1, group_2"
)

message("Creating suppressed group-pair summary: ", write_schema, ".", overlap_group_pair_table)
DatabaseConnector::executeSql(con, overlap_group_pair_sql)

# ---- Print aggregate QA output ----
overlap_summary <- tbl(con, inDatabaseSchema(write_schema, overlap_summary_table)) |>
  arrange(metric) |>
  collect()

message("Aggregate overlap check results:")
print(overlap_summary)

overlap_group_pairs <- tbl(con, inDatabaseSchema(write_schema, overlap_group_pair_table)) |>
  filter(n_patients >= min_count) |>
  arrange(coverage_type, desc(n_patients), group_1, group_2) |>
  collect()

if (nrow(overlap_group_pairs) == 0L) {
  message(
    "No Mx/Rx overlapping group-pair summaries met the minimum count of ",
    min_count,
    " sampled patients."
  )
} else {
  message("Overlapping group-pair summaries with n_patients >= ", min_count, ":")
  print(overlap_group_pairs)
}
