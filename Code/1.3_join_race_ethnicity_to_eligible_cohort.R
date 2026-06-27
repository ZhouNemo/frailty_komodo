library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual eligibility
# Author: Nemo Zhou
# Date started: 2026-06-17
# Date last updated: 2026-06-24
#
# ---- Purpose ----
# Join the recommended KRD patient-level race/ethnicity variable onto the
# materialized annual eligible cohort created by:
# Code/1.1_build_annual_eligible_population.R
#
# The source PATIENT_RACE_ETHNICITY table is reduced to one row per patient
# before joining so the annual patient-year denominator is not duplicated.
# The original cohort table is archived as 1_annual_eligible_cohort_without_race,
# and the race/ethnicity-enhanced table is saved under the original
# 1_annual_eligible_cohort name for downstream scripts.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Table settings ----
eligibility_table <- "1_annual_eligible_cohort"
eligibility_without_race_table <- "1_annual_eligible_cohort_without_race"
race_table <- "patient_race_ethnicity"
eligibility_race_build_table <- "1_annual_eligible_cohort_race_build"
min_count <- 11L

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
  paste(quote_identifier(schema), quote_identifier(table), sep = ".")
}

table_is_accessible <- function(schema, table) {
  table_identifier <- qualified_identifier(schema, table)

  tryCatch(
    {
      DBI::dbGetQuery(
        con,
        paste0("SELECT * FROM ", table_identifier, " LIMIT 0")
      )
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

eligibility_table_identifier <- qualified_identifier(
  write_schema,
  eligibility_table
)

eligibility_without_race_table_identifier <- qualified_identifier(
  write_schema,
  eligibility_without_race_table
)

race_table_identifier <- qualified_identifier(
  komodo_schema,
  race_table
)

eligibility_race_build_table_identifier <- qualified_identifier(
  write_schema,
  eligibility_race_build_table
)

# ---- Validate source tables ----
source_eligibility_table <- if (
  table_is_accessible(write_schema, eligibility_without_race_table)
) {
  eligibility_without_race_table
} else {
  eligibility_table
}

source_eligibility_table_identifier <- qualified_identifier(
  write_schema,
  source_eligibility_table
)

if (!table_is_accessible(write_schema, source_eligibility_table)) {
  stop(
    "Missing eligibility table: ",
    write_schema,
    ".",
    eligibility_table,
    ". Run Code/1.1_build_annual_eligible_population.R first."
  )
}

if (!table_is_accessible(komodo_schema, race_table)) {
  stop(
    "Missing KRD race/ethnicity table: ",
    komodo_schema,
    ".",
    race_table,
    ". Confirm table availability with Code/0.7_check_krd_table_inventory.R."
  )
}

# ---- Materialize build table with race/ethnicity ----
# Conflicting non-missing race/ethnicity values are retained as an explicit QA
# category instead of creating duplicate patient-year rows.
eligibility_race_sql <- paste0(
  "DROP TABLE IF EXISTS ", eligibility_race_build_table_identifier, ";
CREATE TABLE ", eligibility_race_build_table_identifier, " AS
WITH race_by_patient AS (
  SELECT
    patient_id,
    CASE
      WHEN COUNT(DISTINCT NULLIF(TRIM(patient_race_ethnicity), '')) = 0
        THEN 'UNKNOWN'
      WHEN COUNT(DISTINCT NULLIF(TRIM(patient_race_ethnicity), '')) = 1
        THEN MAX(NULLIF(TRIM(patient_race_ethnicity), ''))
      ELSE 'MULTIPLE VALUES'
    END AS patient_race_ethnicity
  FROM ", race_table_identifier, "
  GROUP BY patient_id
)
SELECT
  eligible.*,
  COALESCE(race.patient_race_ethnicity, 'UNKNOWN') AS patient_race_ethnicity
FROM ", source_eligibility_table_identifier, " eligible
LEFT JOIN race_by_patient race
  ON eligible.patient_id = race.patient_id"
)

message(
  "Creating race/ethnicity build table from: ",
  write_schema,
  ".",
  source_eligibility_table
)

DatabaseConnector::executeSql(con, eligibility_race_sql)

# ---- Swap table names for downstream compatibility ----
if (!table_is_accessible(write_schema, eligibility_without_race_table)) {
  message(
    "Archiving original eligibility table as: ",
    write_schema,
    ".",
    eligibility_without_race_table
  )

  DatabaseConnector::executeSql(
    con,
    paste0(
      "ALTER TABLE ",
      eligibility_table_identifier,
      " RENAME TO ",
      quote_identifier(eligibility_without_race_table)
    )
  )
} else if (table_is_accessible(write_schema, eligibility_table)) {
  message(
    "Replacing existing race/ethnicity-enhanced eligibility table: ",
    write_schema,
    ".",
    eligibility_table
  )

  DatabaseConnector::executeSql(
    con,
    paste0("DROP TABLE IF EXISTS ", eligibility_table_identifier)
  )
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "ALTER TABLE ",
    eligibility_race_build_table_identifier,
    " RENAME TO ",
    quote_identifier(eligibility_table)
  )
)

message("Eligibility table with race/ethnicity created as: ", write_schema, ".", eligibility_table)

# ---- Reference saved table for aggregate QA ----
eligible_population <- tbl(
  con,
  dbplyr::sql(paste0("SELECT * FROM ", eligibility_without_race_table_identifier))
)

eligible_with_race <- tbl(
  con,
  dbplyr::sql(paste0("SELECT * FROM ", eligibility_table_identifier))
)

# ---- Aggregate QA output ----
message("Checking annual person-year counts before and after the join.")

source_annual_counts <- eligible_population |>
  count(analysis_year, name = "n_person_years") |>
  rename(n_source_person_years = n_person_years)

joined_annual_counts <- eligible_with_race |>
  count(analysis_year, name = "n_person_years") |>
  rename(n_joined_person_years = n_person_years)

annual_count_check <- source_annual_counts |>
  full_join(joined_annual_counts, by = "analysis_year") |>
  mutate(
    n_source_person_years = coalesce(n_source_person_years, 0L),
    n_joined_person_years = coalesce(n_joined_person_years, 0L),
    n_difference = n_joined_person_years - n_source_person_years
  ) |>
  arrange(analysis_year) |>
  collect()

print(annual_count_check)

message("Counting eligible person-years by race/ethnicity.")

race_ethnicity_counts <- eligible_with_race |>
  count(analysis_year, patient_race_ethnicity, name = "n_person_years") |>
  filter(n_person_years >= min_count) |>
  arrange(analysis_year, patient_race_ethnicity) |>
  collect()

print(race_ethnicity_counts)

message("Race/ethnicity join complete.")
