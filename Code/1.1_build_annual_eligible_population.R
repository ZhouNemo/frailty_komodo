library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# ---- Purpose ----
# Annual eligibility builder for CFI, CCI, CCW, and polypharmacy analyses.
# Review Documents/ANNUAL_ELIGIBILITY_LOGIC.md before modifying this script.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Study parameters ----
# Update these values for the final study protocol.
analysis_years <- 2016:2024
min_age <- 50L
eligibility_table <- "annual_eligible_population"

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Helper: encode analysis years as SQL rows ----
format_year_values <- function(years) {
  paste(
    sprintf("SELECT %s::INTEGER AS analysis_year", as.integer(years)),
    collapse = "\nUNION ALL\n"
  )
}

year_values_sql <- format_year_values(analysis_years)

# ---- Build SQL for the annual eligibility table ----
# The SQL first defines year-specific denominators, then checks:
# age, full-year MX/RX closed observability, full-year insurance attribution,
# and stable non-missing medical and prescription insurance classification.
eligibility_sql <- paste0(
  "DROP TABLE IF EXISTS ", write_schema, ".", eligibility_table, ";
CREATE TABLE ", write_schema, ".", eligibility_table, " AS
/* Analysis years requested by the study protocol. */
WITH analysis_years AS (
", year_values_sql, "
),
/* Calendar-year start and end dates for each analysis year. */
year_bounds AS (
  SELECT
    analysis_year,
    TO_DATE(analysis_year::VARCHAR || '-01-01', 'YYYY-MM-DD') AS year_start,
    DATEADD(
      day,
      -1,
      DATEADD(year, 1, TO_DATE(analysis_year::VARCHAR || '-01-01', 'YYYY-MM-DD'))
    ) AS year_end
  FROM analysis_years
),
/* Age eligibility on January 1 of each analysis year. */
demographics AS (
  SELECT
    y.analysis_year,
    y.year_start,
    y.year_end,
    d.patient_id,
    d.patient_dob,
    d.patient_gender,
    DATEDIFF(year, d.patient_dob, y.year_start) AS age
  FROM ", komodo_schema, ".patient_demographics d
  CROSS JOIN year_bounds y
  WHERE d.patient_dob IS NOT NULL
    AND DATEDIFF(year, d.patient_dob, y.year_start) >= ", min_age, "
),
/* All MX/RX closed spans that overlap the calendar year, clipped to year bounds. */
closed_overlaps AS (
  SELECT
    y.analysis_year,
    y.year_start,
    y.year_end,
    pc.patient_id,
    pc.closed_type,
    CASE
      WHEN pc.closed_start_date < y.year_start THEN y.year_start
      ELSE pc.closed_start_date
    END AS span_start,
    CASE
      WHEN pc.closed_end_date > y.year_end THEN y.year_end
      ELSE pc.closed_end_date
    END AS span_end
  FROM ", komodo_schema, ".patient_closed pc
  INNER JOIN year_bounds y
    ON pc.closed_start_date <= y.year_end
   AND pc.closed_end_date >= y.year_start
  WHERE pc.closed_type IN ('MX CLOSED', 'RX CLOSED')
),
/* Order closed spans to identify gaps within each patient-year and closed type. */
closed_ordered AS (
  SELECT
    *,
    MAX(span_end) OVER (
      PARTITION BY analysis_year, patient_id, closed_type
      ORDER BY span_start, span_end
      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
    ) AS previous_max_end
  FROM closed_overlaps
),
/* Summarize span coverage and gap status separately for MX CLOSED and RX CLOSED. */
closed_by_type AS (
  SELECT
    analysis_year,
    patient_id,
    closed_type,
    MIN(year_start) AS year_start,
    MAX(year_end) AS year_end,
    MIN(span_start) AS first_span_start,
    MAX(span_end) AS last_span_end,
    MAX(
      CASE
        WHEN previous_max_end IS NOT NULL
         AND span_start > DATEADD(day, 1, previous_max_end)
        THEN 1 ELSE 0
      END
    ) AS has_gap
  FROM closed_ordered
  GROUP BY analysis_year, patient_id, closed_type
),
/* Require continuous full-year medical and prescription closed observability. */
closed_full_year AS (
  SELECT
    analysis_year,
    patient_id,
    MAX(
      CASE
        WHEN closed_type = 'MX CLOSED'
         AND first_span_start <= year_start
         AND last_span_end >= year_end
         AND has_gap = 0
        THEN 1 ELSE 0
      END
    ) AS has_full_mx_closed,
    MAX(
      CASE
        WHEN closed_type = 'RX CLOSED'
         AND first_span_start <= year_start
         AND last_span_end >= year_end
         AND has_gap = 0
        THEN 1 ELSE 0
      END
    ) AS has_full_rx_closed
  FROM closed_by_type
  GROUP BY analysis_year, patient_id
),
/* All insurance rows overlapping the year, including partial-year alternate plans. */
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
    pi.rx_insurance_group,
    pi.rx_insurance_segment
  FROM ", komodo_schema, ".patient_insurance pi
  INNER JOIN year_bounds y
    ON pi.row_valid_start <= y.year_end
   AND pi.row_valid_end >= y.year_start
),
/* Order insurance spans to identify attribution gaps within each patient-year. */
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
/* Summarize insurance coverage, missingness, and number of distinct categories. */
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
    COUNT(rx_insurance_group) AS n_nonmissing_rx_group,
    COUNT(rx_insurance_segment) AS n_nonmissing_rx_segment,
    COUNT(DISTINCT mx_insurance_group) AS n_mx_groups,
    COUNT(DISTINCT mx_insurance_segment) AS n_mx_segments,
    COUNT(DISTINCT rx_insurance_group) AS n_rx_groups,
    COUNT(DISTINCT rx_insurance_segment) AS n_rx_segments,
    MIN(mx_insurance_group) AS mx_insurance_group,
    MIN(mx_insurance_segment) AS mx_insurance_segment,
    MIN(rx_insurance_group) AS rx_insurance_group,
    MIN(rx_insurance_segment) AS rx_insurance_segment,
    MAX(
      CASE
        WHEN previous_max_end IS NOT NULL
         AND span_start > DATEADD(day, 1, previous_max_end)
        THEN 1 ELSE 0
      END
    ) AS has_insurance_gap
  FROM insurance_ordered
  GROUP BY analysis_year, patient_id
),
/* Keep patients with full-year, gap-free, stable MX and RX insurance classification. */
insurance_eligible AS (
  SELECT *
  FROM insurance_summary
  WHERE first_span_start <= year_start
    AND last_span_end >= year_end
    AND has_insurance_gap = 0
    AND n_insurance_rows = n_nonmissing_mx_group
    AND n_insurance_rows = n_nonmissing_mx_segment
    AND n_insurance_rows = n_nonmissing_rx_group
    AND n_insurance_rows = n_nonmissing_rx_segment
    AND n_mx_groups = 1
    AND n_mx_segments = 1
    AND n_rx_groups = 1
    AND n_rx_segments = 1
    AND mx_insurance_group <> 'UNKNOWN'
    AND mx_insurance_segment <> 'UNKNOWN'
    AND rx_insurance_group <> 'UNKNOWN'
    AND rx_insurance_segment <> 'UNKNOWN'
)
/* Final eligible patient-year denominator. */
SELECT
  d.patient_id,
  d.analysis_year,
  CAST(d.year_start AS DATE) AS index_date,
  d.age,
  d.patient_gender,
  i.mx_insurance_group,
  i.mx_insurance_segment,
  i.rx_insurance_group,
  i.rx_insurance_segment
FROM demographics d
INNER JOIN closed_full_year c
  ON d.patient_id = c.patient_id
 AND d.analysis_year = c.analysis_year
INNER JOIN insurance_eligible i
  ON d.patient_id = i.patient_id
 AND d.analysis_year = i.analysis_year
WHERE c.has_full_mx_closed = 1
  AND c.has_full_rx_closed = 1"
)

# ---- Materialize annual eligibility table ----
message("Creating annual eligibility table: ", write_schema, ".", eligibility_table)
message("Analysis years: ", paste(analysis_years, collapse = ", "))
message("Minimum age on January 1: ", min_age)

DatabaseConnector::executeSql(con, eligibility_sql)

message("Annual eligibility table created.")

# ---- Reference saved table for downstream analysis ----
eligible_population <- tbl(con, inDatabaseSchema(write_schema, eligibility_table))

# ---- Aggregate QA output ----
# Aggregate count check only.
eligibility_counts <- eligible_population |>
  count(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    rx_insurance_group,
    rx_insurance_segment,
    name = "n_person_years"
  ) |>
  filter(n_person_years >= 11L) |>
  arrange(analysis_year, mx_insurance_group, mx_insurance_segment)

print(eligibility_counts |> collect())
