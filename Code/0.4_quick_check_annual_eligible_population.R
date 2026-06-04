library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual eligibility quick check
# Author: Nemo Zhou
# Date started: 2026-06-03
# Date last updated: 2026-06-04
#
# ---- Purpose ----
# Quick logic check for Code/1.1_build_annual_eligible_population.R.
# This script intentionally samples a small candidate patient set before the
# full insurance-attribution logic. Use it only to validate syntax, joins, date
# logic, and aggregate QA output before running the full 1.1 build. Do not use
# results as estimates.
#
# Run after:
#   Code/0.2_model_komodo_workflow.R
#   Documents/ANNUAL_ELIGIBILITY_LOGIC.md
#   Documents/PATIENT_CLOSED_AND_INSURANCE_STRUCTURE.md
#
# Run before:
#   Code/1.1_build_annual_eligible_population.R

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Quick-check parameters ----
analysis_year <- 2023L
min_age <- 40L
max_candidate_patients <- 5000L
candidate_table <- "annual_eligible_population_quick_candidates"
criteria_table <- "annual_eligible_population_quick_criteria"
eligibility_table <- "annual_eligible_population_quick_check"
min_count <- 11L

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Build SQL for a small candidate-patient table ----
# Candidate sampling happens before the full eligibility logic to keep this
# quick check small. The sample is deterministic for repeatable debugging.
candidate_sql <- paste0(
  "DROP TABLE IF EXISTS ", candidate_table, ";
CREATE TEMP TABLE ", candidate_table, " AS
WITH year_bounds AS (
  SELECT
    ", analysis_year, "::INTEGER AS analysis_year,
    TO_DATE('", analysis_year, "-01-01', 'YYYY-MM-DD') AS year_start,
    TO_DATE('", analysis_year, "-12-31', 'YYYY-MM-DD') AS year_end
)
SELECT DISTINCT d.patient_id
FROM ", komodo_schema, ".patient_demographics d
CROSS JOIN year_bounds y
INNER JOIN ", komodo_schema, ".patient_insurance pi
  ON d.patient_id = pi.patient_id
 AND pi.row_valid_start <= y.year_end
 AND pi.row_valid_end >= y.year_start
WHERE d.patient_dob IS NOT NULL
  AND DATEDIFF(year, d.patient_dob, y.year_start) >= ", min_age, "
ORDER BY d.patient_id
LIMIT ", max_candidate_patients
)

# ---- Build SQL for the sampled annual eligibility table ----
# This mirrors the production logic in 1.1, but every source table is restricted
# to the candidate patients selected above.
eligibility_sql <- paste0(
  "DROP TABLE IF EXISTS ", criteria_table, ";
CREATE TEMP TABLE ", criteria_table, " AS
WITH candidate_patients AS (
  SELECT patient_id
  FROM ", candidate_table, "
),
year_bounds AS (
  SELECT
    ", analysis_year, "::INTEGER AS analysis_year,
    TO_DATE('", analysis_year, "-01-01', 'YYYY-MM-DD') AS year_start,
    TO_DATE('", analysis_year, "-12-31', 'YYYY-MM-DD') AS year_end
),
demographics AS (
  SELECT
    y.analysis_year,
    y.year_start,
    y.year_end,
    d.patient_id,
    d.patient_dob,
    d.patient_gender,
    DATEDIFF(year, d.patient_dob, y.year_start) AS age
  FROM candidate_patients cp
  INNER JOIN ", komodo_schema, ".patient_demographics d
    ON cp.patient_id = d.patient_id
  CROSS JOIN year_bounds y
  WHERE d.patient_dob IS NOT NULL
    AND DATEDIFF(year, d.patient_dob, y.year_start) >= ", min_age, "
),
insurance_overlaps AS (
  SELECT
    y.analysis_year,
    y.year_start,
    y.year_end,
    pi.patient_id,
    CASE
      WHEN pi.row_valid_start < y.year_start THEN y.year_start
      ELSE pi.row_valid_start
    END AS span_start,
    CASE
      WHEN pi.row_valid_end > y.year_end THEN y.year_end
      ELSE pi.row_valid_end
    END AS span_end,
    pi.mx_insurance_group,
    pi.mx_insurance_segment,
    pi.mx_secondary_insurance_group,
    pi.mx_secondary_insurance_segment,
    pi.rx_insurance_group,
    pi.rx_insurance_segment,
    pi.rx_secondary_insurance_group,
    pi.rx_secondary_insurance_segment
  FROM candidate_patients cp
  INNER JOIN ", komodo_schema, ".patient_insurance pi
    ON cp.patient_id = pi.patient_id
  INNER JOIN year_bounds y
    ON pi.row_valid_start <= y.year_end
   AND pi.row_valid_end >= y.year_start
),
insurance_ordered AS (
  SELECT
    *,
    MAX(span_end) OVER (
      PARTITION BY analysis_year, patient_id
      ORDER BY span_start, span_end
      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
    ) AS previous_max_end
  FROM insurance_overlaps
),
insurance_summary AS (
  SELECT
    analysis_year,
    patient_id,
    MIN(year_start) AS year_start,
    MAX(year_end) AS year_end,
    MIN(span_start) AS first_span_start,
    MAX(span_end) AS last_span_end,
    COUNT(*) AS n_insurance_rows,
    COUNT(mx_insurance_group) AS n_nonmissing_mx_group,
    COUNT(mx_insurance_segment) AS n_nonmissing_mx_segment,
    COUNT(mx_secondary_insurance_group) AS n_nonmissing_mx_secondary_group,
    COUNT(mx_secondary_insurance_segment) AS n_nonmissing_mx_secondary_segment,
    COUNT(rx_insurance_group) AS n_nonmissing_rx_group,
    COUNT(rx_insurance_segment) AS n_nonmissing_rx_segment,
    COUNT(rx_secondary_insurance_group) AS n_nonmissing_rx_secondary_group,
    COUNT(rx_secondary_insurance_segment) AS n_nonmissing_rx_secondary_segment,
    COUNT(DISTINCT mx_insurance_group) AS n_mx_groups,
    COUNT(DISTINCT mx_insurance_segment) AS n_mx_segments,
    COUNT(DISTINCT mx_secondary_insurance_group) AS n_mx_secondary_groups,
    COUNT(DISTINCT mx_secondary_insurance_segment) AS n_mx_secondary_segments,
    COUNT(DISTINCT rx_insurance_group) AS n_rx_groups,
    COUNT(DISTINCT rx_insurance_segment) AS n_rx_segments,
    COUNT(DISTINCT rx_secondary_insurance_group) AS n_rx_secondary_groups,
    COUNT(DISTINCT rx_secondary_insurance_segment) AS n_rx_secondary_segments,
    MIN(mx_insurance_group) AS mx_insurance_group,
    MIN(mx_insurance_segment) AS mx_insurance_segment,
    MIN(mx_secondary_insurance_group) AS mx_secondary_insurance_group,
    MIN(mx_secondary_insurance_segment) AS mx_secondary_insurance_segment,
    MIN(rx_insurance_group) AS rx_insurance_group,
    MIN(rx_insurance_segment) AS rx_insurance_segment,
    MIN(rx_secondary_insurance_group) AS rx_secondary_insurance_group,
    MIN(rx_secondary_insurance_segment) AS rx_secondary_insurance_segment,
    MAX(
      CASE
        WHEN previous_max_end IS NOT NULL
         AND span_start > DATEADD(day, 1, previous_max_end)
        THEN 1 ELSE 0
      END
    ) AS has_insurance_gap
  FROM insurance_ordered
  GROUP BY analysis_year, patient_id
)
SELECT
  d.patient_id,
  d.analysis_year,
  CAST(d.year_start AS DATE) AS index_date,
  d.age,
  d.patient_gender,
  i.n_insurance_rows,
  i.first_span_start,
  i.last_span_end,
  i.has_insurance_gap,
  i.n_nonmissing_mx_group,
  i.n_nonmissing_mx_segment,
  i.n_nonmissing_mx_secondary_group,
  i.n_nonmissing_mx_secondary_segment,
  i.n_nonmissing_rx_group,
  i.n_nonmissing_rx_segment,
  i.n_nonmissing_rx_secondary_group,
  i.n_nonmissing_rx_secondary_segment,
  i.n_mx_groups,
  i.n_mx_segments,
  i.n_mx_secondary_groups,
  i.n_mx_secondary_segments,
  i.n_rx_groups,
  i.n_rx_segments,
  i.n_rx_secondary_groups,
  i.n_rx_secondary_segments,
  i.mx_insurance_group,
  i.mx_insurance_segment,
  i.mx_secondary_insurance_group,
  i.mx_secondary_insurance_segment,
  i.rx_insurance_group,
  i.rx_insurance_segment,
  i.rx_secondary_insurance_group,
  i.rx_secondary_insurance_segment,
  1 AS meets_age,
  CASE
    WHEN i.first_span_start <= d.year_start
     AND i.last_span_end >= d.year_end
    THEN 1 ELSE 0
  END AS has_full_year_insurance,
  CASE
    WHEN i.has_insurance_gap = 0
    THEN 1 ELSE 0
  END AS has_gap_free_insurance,
  CASE
    WHEN i.n_insurance_rows = i.n_nonmissing_mx_group
     AND i.n_insurance_rows = i.n_nonmissing_mx_segment
     AND i.n_insurance_rows = i.n_nonmissing_rx_group
     AND i.n_insurance_rows = i.n_nonmissing_rx_segment
    THEN 1 ELSE 0
  END AS has_nonmissing_primary_insurance,
  CASE
    WHEN i.n_mx_groups = 1
     AND i.n_mx_segments = 1
     AND i.n_rx_groups = 1
     AND i.n_rx_segments = 1
    THEN 1 ELSE 0
  END AS has_stable_primary_insurance,
  CASE
    WHEN i.n_mx_secondary_groups <= 1
     AND i.n_mx_secondary_segments <= 1
     AND i.n_rx_secondary_groups <= 1
     AND i.n_rx_secondary_segments <= 1
     AND i.n_nonmissing_mx_secondary_group IN (0, i.n_insurance_rows)
     AND i.n_nonmissing_mx_secondary_segment IN (0, i.n_insurance_rows)
     AND i.n_nonmissing_rx_secondary_group IN (0, i.n_insurance_rows)
     AND i.n_nonmissing_rx_secondary_segment IN (0, i.n_insurance_rows)
    THEN 1 ELSE 0
  END AS has_stable_optional_secondary_insurance,
  CASE
    WHEN i.mx_insurance_group <> 'UNKNOWN'
     AND i.mx_insurance_segment <> 'UNKNOWN'
     AND i.rx_insurance_group <> 'UNKNOWN'
     AND i.rx_insurance_segment <> 'UNKNOWN'
     AND COALESCE(i.mx_secondary_insurance_group, '') <> 'UNKNOWN'
     AND COALESCE(i.mx_secondary_insurance_segment, '') <> 'UNKNOWN'
     AND COALESCE(i.rx_secondary_insurance_group, '') <> 'UNKNOWN'
     AND COALESCE(i.rx_secondary_insurance_segment, '') <> 'UNKNOWN'
    THEN 1 ELSE 0
  END AS has_known_insurance_classification
FROM demographics d
INNER JOIN insurance_summary i
  ON d.patient_id = i.patient_id
 AND d.analysis_year = i.analysis_year;
DROP TABLE IF EXISTS ", eligibility_table, ";
CREATE TEMP TABLE ", eligibility_table, " AS
SELECT
  patient_id,
  analysis_year,
  index_date,
  age,
  patient_gender,
  mx_insurance_group,
  mx_insurance_segment,
  mx_secondary_insurance_group,
  mx_secondary_insurance_segment,
  rx_insurance_group,
  rx_insurance_segment,
  rx_secondary_insurance_group,
  rx_secondary_insurance_segment
FROM ", criteria_table, "
WHERE meets_age = 1
  AND has_full_year_insurance = 1
  AND has_gap_free_insurance = 1
  AND has_nonmissing_primary_insurance = 1
  AND has_stable_primary_insurance = 1
  AND has_stable_optional_secondary_insurance = 1
  AND has_known_insurance_classification = 1"
)

# ---- Create temporary quick-check tables ----
message(
  "Creating quick candidate table: ",
  candidate_table
)
message("Quick-check year: ", analysis_year)
message("Minimum age on January 1: ", min_age)
message("Maximum candidate patients: ", max_candidate_patients)

DatabaseConnector::executeSql(con, candidate_sql)

message(
  "Creating quick criteria and eligibility tables: ",
  criteria_table,
  " and ",
  eligibility_table
)

DatabaseConnector::executeSql(con, eligibility_sql)

message("Quick eligibility table created.")

# ---- Aggregate QA output ----
candidate_count <- tbl(
  con,
  dbplyr::sql(paste0("SELECT * FROM ", candidate_table))
) |>
  summarize(n_candidate_patients = n()) |>
  collect()

criteria_results <- tbl(
  con,
  dbplyr::sql(paste0("SELECT * FROM ", criteria_table))
)

eligible_population <- tbl(
  con,
  dbplyr::sql(paste0("SELECT * FROM ", eligibility_table))
)

eligible_count <- eligible_population |>
  summarize(n_eligible_person_years = n()) |>
  collect()

criteria_checks_sql <- paste0(
  "SELECT
    COUNT(*)::BIGINT AS n_sampled_patient_years,
    SUM(CASE WHEN meets_age = 1 THEN 1 ELSE 0 END)::BIGINT AS n_meets_age,
    SUM(CASE WHEN has_full_year_insurance = 1 THEN 1 ELSE 0 END)::BIGINT AS n_full_year_insurance,
    SUM(CASE WHEN has_gap_free_insurance = 1 THEN 1 ELSE 0 END)::BIGINT AS n_gap_free_insurance,
    SUM(CASE WHEN has_nonmissing_primary_insurance = 1 THEN 1 ELSE 0 END)::BIGINT AS n_nonmissing_primary_insurance,
    SUM(CASE WHEN has_stable_primary_insurance = 1 THEN 1 ELSE 0 END)::BIGINT AS n_stable_primary_insurance,
    SUM(CASE WHEN has_stable_optional_secondary_insurance = 1 THEN 1 ELSE 0 END)::BIGINT AS n_stable_optional_secondary_insurance,
    SUM(CASE WHEN has_known_insurance_classification = 1 THEN 1 ELSE 0 END)::BIGINT AS n_known_insurance_classification,
    SUM(
      CASE
        WHEN meets_age = 1
         AND has_full_year_insurance = 1
         AND has_gap_free_insurance = 1
         AND has_nonmissing_primary_insurance = 1
         AND has_stable_primary_insurance = 1
         AND has_stable_optional_secondary_insurance = 1
         AND has_known_insurance_classification = 1
        THEN 1 ELSE 0
      END
    )::BIGINT AS n_meets_all_criteria
  FROM ",
  criteria_table
)

criteria_checks <- tbl(con, dbplyr::sql(criteria_checks_sql)) |>
  collect()

criteria_checks <- criteria_checks |>
  mutate(
    n_fail_age = n_sampled_patient_years - n_meets_age,
    n_fail_full_year_insurance = n_sampled_patient_years - n_full_year_insurance,
    n_fail_gap_free_insurance = n_sampled_patient_years - n_gap_free_insurance,
    n_fail_nonmissing_primary_insurance =
      n_sampled_patient_years - n_nonmissing_primary_insurance,
    n_fail_stable_primary_insurance =
      n_sampled_patient_years - n_stable_primary_insurance,
    n_fail_stable_optional_secondary_insurance =
      n_sampled_patient_years - n_stable_optional_secondary_insurance,
    n_fail_known_insurance_classification =
      n_sampled_patient_years - n_known_insurance_classification,
    eligible_count_matches_all_criteria =
      n_meets_all_criteria == eligible_count$n_eligible_person_years
  )

eligibility_counts <- eligible_population |>
  count(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    mx_secondary_insurance_group,
    mx_secondary_insurance_segment,
    rx_insurance_group,
    rx_insurance_segment,
    rx_secondary_insurance_group,
    rx_secondary_insurance_segment,
    name = "n_person_years"
  ) |>
  filter(n_person_years >= min_count) |>
  arrange(analysis_year, mx_insurance_group, mx_insurance_segment) |>
  collect()

message("Candidate patients sampled:")
print(candidate_count)

message("Eligible person-years after full logic:")
print(eligible_count)

message("Eligibility criteria pass counts in sampled patient-years:")
print(criteria_checks)

message("Suppressed aggregate insurance distribution:")
print(eligibility_counts)
