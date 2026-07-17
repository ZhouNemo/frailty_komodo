library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto 2016 Table 1
# Author: Nemo Zhou
# Date started: 2026-06-07
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Report eligible participant counts for every year from 2016 through 2025,
# then generate detailed insurance summaries and Table 1 for 2016 only.
# Detailed summaries include primary Mx segments, primary Rx segments,
# combined primary Mx/Rx segments, primary/secondary Mx combinations, and
# primary/secondary Rx combinations. Result tables are saved as CSV files in
# the project Outputs folder.
#

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Analysis parameters ----
analysis_years <- 2016:2025
detail_year <- 2016L
eligibility_table <- "1_annual_eligible_cohort"
annual_cohort_table <- paste0("annual_table1_cohort_", detail_year)
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

annual_cohort_identifier <- paste(
  quote_identifier(write_schema),
  quote_identifier(annual_cohort_table),
  sep = "."
)

# ---- Reference the saved annual eligible population ----
eligible_population <- tbl(
  con,
  dbplyr::sql(paste0("SELECT * FROM ", eligibility_table_identifier))
)

# ---- Count eligible participants for all requested years ----
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

# ---- Count 2016 participants by insurance classification ----
detail_population <- eligible_population |>
  filter(analysis_year == detail_year)

mx_segment_counts <- detail_population |>
  group_by(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment
  ) |>
  summarize(n_participants = n(), .groups = "drop") |>
  filter(n_participants >= min_count) |>
  arrange(mx_insurance_group, mx_insurance_segment) |>
  collect()

rx_segment_counts <- detail_population |>
  group_by(
    analysis_year,
    rx_insurance_group,
    rx_insurance_segment
  ) |>
  summarize(n_participants = n(), .groups = "drop") |>
  filter(n_participants >= min_count) |>
  arrange(rx_insurance_group, rx_insurance_segment) |>
  collect()

mx_rx_segment_combination_counts <- detail_population |>
  group_by(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    rx_insurance_group,
    rx_insurance_segment
  ) |>
  summarize(n_participants = n(), .groups = "drop") |>
  filter(n_participants >= min_count) |>
  arrange(
    mx_insurance_group,
    mx_insurance_segment,
    rx_insurance_group,
    rx_insurance_segment
  ) |>
  collect()

mx_primary_secondary_counts <- detail_population |>
  group_by(
    analysis_year,
    mx_insurance_group,
    mx_insurance_segment,
    mx_secondary_insurance_group,
    mx_secondary_insurance_segment
  ) |>
  summarize(n_participants = n(), .groups = "drop") |>
  filter(n_participants >= min_count) |>
  arrange(
    mx_insurance_group,
    mx_insurance_segment,
    mx_secondary_insurance_group,
    mx_secondary_insurance_segment
  ) |>
  collect()

rx_primary_secondary_counts <- detail_population |>
  group_by(
    analysis_year,
    rx_insurance_group,
    rx_insurance_segment,
    rx_secondary_insurance_group,
    rx_secondary_insurance_segment
  ) |>
  summarize(n_participants = n(), .groups = "drop") |>
  filter(n_participants >= min_count) |>
  arrange(
    rx_insurance_group,
    rx_insurance_segment,
    rx_secondary_insurance_group,
    rx_secondary_insurance_segment
  ) |>
  collect()

message("2016 participant counts by medical insurance segment:")
print(mx_segment_counts)
message("2016 participant counts by prescription insurance segment:")
print(rx_segment_counts)
message("2016 participant counts by combined medical and prescription segments:")
print(mx_rx_segment_combination_counts)
message("2016 participant counts by primary and secondary medical insurance:")
print(mx_primary_secondary_counts)
message("2016 participant counts by primary and secondary prescription insurance:")
print(rx_primary_secondary_counts)

# ---- Save participant count tables ----
write.csv(
  annual_counts,
  file.path(output_dir, "2.1_annual_counts_2016_2025.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  mx_segment_counts,
  file.path(output_dir, "2.1_mx_segment_counts_2016.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  rx_segment_counts,
  file.path(output_dir, "2.1_rx_segment_counts_2016.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  mx_rx_segment_combination_counts,
  file.path(output_dir, "2.1_mx_rx_segment_combination_counts_2016.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  mx_primary_secondary_counts,
  file.path(output_dir, "2.1_mx_primary_secondary_counts_2016.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  rx_primary_secondary_counts,
  file.path(output_dir, "2.1_rx_primary_secondary_counts_2016.csv"),
  row.names = FALSE,
  na = ""
)

# ---- Generate Table 1 for 2016 only ----
message("Preparing Table 1 cohort for ", detail_year, ".")

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", annual_cohort_identifier, ";
     CREATE TABLE ", annual_cohort_identifier, " AS
     SELECT DISTINCT
       patient_id,
       CAST(index_date AS DATE) AS index_date
     FROM ", eligibility_table_identifier, "
     WHERE analysis_year = ", detail_year
  )
)

table1_2016 <- tryCatch(
  {
    message("Generating Table 1 for ", detail_year, ".")

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

print(table1_2016)

write.csv(
  as.data.frame(table1_2016),
  file.path(output_dir, "2.1_table1_2016.csv"),
  row.names = FALSE,
  na = ""
)

message("2016 Table 1 and output files are complete.")

View(annual_counts)
View(mx_segment_counts)
View(rx_segment_counts)
View(mx_rx_segment_combination_counts)
View(mx_primary_secondary_counts)
View(rx_primary_secondary_counts)
