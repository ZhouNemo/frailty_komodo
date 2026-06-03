library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# ---- Purpose ----
# QA checks for the annual eligible population table created by:
# Code/1.1_build_annual_eligible_population.R
# This script prints only schema and aggregate outputs, not patient-level rows.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))
eligibility_table <- "annual_eligible_population"
min_count <- 11L

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Reference saved eligibility table ----
eligible_population <- tbl(
  con,
  inDatabaseSchema(write_schema, eligibility_table)
)

# ---- Check table structure without collecting patient rows ----
message("Checking columns in ", write_schema, ".", eligibility_table)

eligibility_columns <- eligible_population |>
  head(0) |>
  collect() |>
  names()

print(eligibility_columns)

required_columns <- c(
  "patient_id",
  "analysis_year",
  "index_date",
  "age",
  "patient_gender",
  "mx_insurance_group",
  "mx_insurance_segment",
  "rx_insurance_group",
  "rx_insurance_segment"
)

missing_columns <- setdiff(required_columns, eligibility_columns)

if (length(missing_columns) > 0) {
  stop(
    "Eligibility table is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

message("All required columns are present.")

# ---- Check annual person-year counts ----
message("Counting eligible person-years by analysis year.")

annual_counts <- eligible_population |>
  count(analysis_year, name = "n_person_years") |>
  arrange(analysis_year) |>
  collect()

print(annual_counts)

# ---- Check insurance distribution with small-cell suppression ----
message("Counting eligible person-years by insurance group and segment.")

insurance_counts <- eligible_population |>
  count(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    rx_insurance_group,
    rx_insurance_segment,
    name = "n_person_years"
  ) |>
  filter(n_person_years >= min_count) |>
  arrange(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    rx_insurance_group,
    rx_insurance_segment
  ) |>
  collect()

print(insurance_counts)

# ---- Check for unexpected missing or UNKNOWN insurance classifications ----
message("Checking for missing or UNKNOWN insurance classifications.")

unexpected_insurance_values <- eligible_population |>
  summarize(
    missing_mx_group = sum(is.na(mx_insurance_group)),
    missing_mx_segment = sum(is.na(mx_insurance_segment)),
    missing_rx_group = sum(is.na(rx_insurance_group)),
    missing_rx_segment = sum(is.na(rx_insurance_segment)),
    unknown_mx_group = sum(mx_insurance_group == "UNKNOWN", na.rm = TRUE),
    unknown_mx_segment = sum(mx_insurance_segment == "UNKNOWN", na.rm = TRUE),
    unknown_rx_group = sum(rx_insurance_group == "UNKNOWN", na.rm = TRUE),
    unknown_rx_segment = sum(rx_insurance_segment == "UNKNOWN", na.rm = TRUE)
  ) |>
  collect()

print(unexpected_insurance_values)

# ---- Check age range by year ----
message("Checking age range by analysis year.")

age_summary <- eligible_population |>
  group_by(analysis_year) |>
  summarize(
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(analysis_year) |>
  collect()

print(age_summary)

message("QA checks complete.")
