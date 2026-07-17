library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual Table 1
# Author: Nemo Zhou
# Date started: 2026-06-06
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Generate a separate aggregate Table 1 for each calendar year from 2016
# through 2025 using the annual eligible population created by
# Code/1_eligbility/1.1_build_annual_eligible_population.R. Each annual cohort contains
# one row per eligible patient with January 1 of that year as index_date.
# The script also summarizes annual participant counts by primary medical
# insurance segment, primary prescription insurance segment, and their
# combined medical/prescription segment classification. It also summarizes
# primary/secondary combinations separately for medical and prescription
# insurance and saves all result tables in the project Outputs folder.
#

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Analysis parameters ----
analysis_years <- 2016:2025
eligibility_table <- "1_annual_eligible_cohort"
annual_cohort_prefix <- "annual_table1_cohort_"
min_count <- 11L
output_dir <- file.path(getwd(), "Outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Helper: quote SQL identifiers ----
quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

eligibility_table_identifier <- paste(
  quote_identifier(write_schema),
  quote_identifier(eligibility_table),
  sep = "."
)

# ---- Confirm that every requested year has eligible patients ----
eligible_population <- tbl(
  con,
  dbplyr::sql(paste0("SELECT * FROM ", eligibility_table_identifier))
)

annual_counts <- eligible_population |>
  filter(analysis_year %in% analysis_years) |>
  count(analysis_year, name = "n_eligible") |>
  arrange(analysis_year) |>
  collect()

missing_years <- setdiff(analysis_years, annual_counts$analysis_year)

if (length(missing_years) > 0L) {
  stop(
    "No eligible cohort rows were found for: ",
    paste(missing_years, collapse = ", "),
    ". Run and check Code/1_eligbility/1.1_build_annual_eligible_population.R first."
  )
}

print(annual_counts)

# ---- Count annual participants by insurance segment ----
# Include insurance group with segment so segment labels remain interpretable.
mx_segment_counts <- eligible_population |>
  filter(analysis_year %in% analysis_years) |>
  group_by(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment
  ) |>
  summarize(
    n_participants = n(),
    .groups = "drop"
  ) |>
  filter(n_participants >= min_count) |>
  arrange(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment
  ) |>
  collect()

rx_segment_counts <- eligible_population |>
  filter(analysis_year %in% analysis_years) |>
  group_by(
    analysis_year,
    rx_insurance_group,
    rx_insurance_segment
  ) |>
  summarize(
    n_participants = n(),
    .groups = "drop"
  ) |>
  filter(n_participants >= min_count) |>
  arrange(
    analysis_year,
    rx_insurance_group,
    rx_insurance_segment
  ) |>
  collect()

mx_rx_segment_combination_counts <- eligible_population |>
  filter(analysis_year %in% analysis_years) |>
  group_by(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    rx_insurance_group,
    rx_insurance_segment
  ) |>
  summarize(
    n_participants = n(),
    .groups = "drop"
  ) |>
  filter(n_participants >= min_count) |>
  arrange(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    rx_insurance_group,
    rx_insurance_segment
  ) |>
  collect()

mx_primary_secondary_counts <- eligible_population |>
  filter(analysis_year %in% analysis_years) |>
  group_by(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    mx_secondary_insurance_group,
    mx_secondary_insurance_segment
  ) |>
  summarize(
    n_participants = n(),
    .groups = "drop"
  ) |>
  filter(n_participants >= min_count) |>
  arrange(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    mx_secondary_insurance_group,
    mx_secondary_insurance_segment
  ) |>
  collect()

rx_primary_secondary_counts <- eligible_population |>
  filter(analysis_year %in% analysis_years) |>
  group_by(
    analysis_year,
    rx_insurance_group,
    rx_insurance_segment,
    rx_secondary_insurance_group,
    rx_secondary_insurance_segment
  ) |>
  summarize(
    n_participants = n(),
    .groups = "drop"
  ) |>
  filter(n_participants >= min_count) |>
  arrange(
    analysis_year,
    rx_insurance_group,
    rx_insurance_segment,
    rx_secondary_insurance_group,
    rx_secondary_insurance_segment
  ) |>
  collect()

message("Annual participant counts by medical insurance segment:")
print(mx_segment_counts)

message("Annual participant counts by prescription insurance segment:")
print(rx_segment_counts)

message("Annual participant counts by combined medical and prescription segments:")
print(mx_rx_segment_combination_counts)

message("Annual participant counts by primary and secondary medical insurance:")
print(mx_primary_secondary_counts)

message("Annual participant counts by primary and secondary prescription insurance:")
print(rx_primary_secondary_counts)

# ---- Save annual count tables ----
write.csv(
  annual_counts,
  file.path(output_dir, "2.2_annual_counts_2016_2025.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  mx_segment_counts,
  file.path(output_dir, "2.2_mx_segment_counts_2016_2025.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  rx_segment_counts,
  file.path(output_dir, "2.2_rx_segment_counts_2016_2025.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  mx_rx_segment_combination_counts,
  file.path(output_dir, "2.2_mx_rx_segment_combination_counts_2016_2025.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  mx_primary_secondary_counts,
  file.path(output_dir, "2.2_mx_primary_secondary_counts_2016_2025.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  rx_primary_secondary_counts,
  file.path(output_dir, "2.2_rx_primary_secondary_counts_2016_2025.csv"),
  row.names = FALSE,
  na = ""
)

# ---- Generate one Table 1 per year ----
annual_table1 <- setNames(vector("list", length(analysis_years)), analysis_years)

for (analysis_year in analysis_years) {
  annual_cohort_table <- paste0(annual_cohort_prefix, analysis_year)
  annual_cohort_identifier <- paste(
    quote_identifier(write_schema),
    quote_identifier(annual_cohort_table),
    sep = "."
  )

  message("Preparing Table 1 cohort for ", analysis_year, ".")

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", annual_cohort_identifier, ";
       CREATE TABLE ", annual_cohort_identifier, " AS
       SELECT DISTINCT
         patient_id,
         CAST(index_date AS DATE) AS index_date
       FROM ", eligibility_table_identifier, "
       WHERE analysis_year = ", analysis_year
    )
  )

  annual_table1[[as.character(analysis_year)]] <- tryCatch(
    {
      message("Generating Table 1 for ", analysis_year, ".")

      ohdsilab::k_table1(
        con = con,
        cohort_table = annual_cohort_table,
        write_schema = write_schema,
        komodo_schema = komodo_schema,
        min_count = min_count
      )
    },
    finally = {
      DatabaseConnector::executeSql(
        con,
        paste0("DROP TABLE IF EXISTS ", annual_cohort_identifier)
      )
    }
  )

  message("Table 1 complete for ", analysis_year, ".")

  write.csv(
    as.data.frame(annual_table1[[as.character(analysis_year)]]),
    file.path(
      output_dir,
      paste0("2.2_table1_", analysis_year, ".csv")
    ),
    row.names = FALSE,
    na = ""
  )
}

# ---- Print annual aggregate results ----
for (analysis_year in names(annual_table1)) {
  message("Table 1: ", analysis_year)
  print(annual_table1[[analysis_year]])
}

message("Annual Table 1 generation complete for 2016 through 2025.")

View(annual_counts)
View(mx_segment_counts)
View(rx_segment_counts)
View(mx_rx_segment_combination_counts)
View(mx_primary_secondary_counts)
View(rx_primary_secondary_counts)
