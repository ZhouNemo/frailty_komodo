library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)

# Project: Frailty_Komoto 2016 Medicare FFS Table 1
# Author: Nemo Zhou
# Date started: 2026-06-08
# Date last updated: 2026-06-08
#
# ---- Purpose ----
# Generate an aggregate Table 1 for patients in the 2016 annual eligible
# population whose primary medical insurance classification is Medicare FFS.
# The source cohort is the materialized 1_annual_eligible_cohort created by
# Code/1.1_build_annual_eligible_population.R. The subgroup count and Table 1
# are saved as CSV files in the project Outputs folder.
#

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Analysis parameters ----
analysis_year <- 2016L
mx_insurance_group <- "MEDICARE"
mx_insurance_segment <- "FFS"
eligibility_table <- "1_annual_eligible_cohort"
table1_cohort <- "table1_2016_medicare_ffs_cohort"
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

table1_cohort_identifier <- paste(
  quote_identifier(write_schema),
  quote_identifier(table1_cohort),
  sep = "."
)

# ---- Identify the 2016 primary Medicare FFS subgroup ----
eligible_population <- tbl(
  con,
  dbplyr::sql(paste0("SELECT * FROM ", eligibility_table_identifier))
)

medicare_ffs_count <- eligible_population |>
  filter(
    analysis_year == !!analysis_year,
    mx_insurance_group == !!mx_insurance_group,
    mx_insurance_segment == !!mx_insurance_segment
  ) |>
  summarize(n_participants = n()) |>
  collect()

if (
  nrow(medicare_ffs_count) != 1L ||
    is.na(medicare_ffs_count$n_participants[[1]]) ||
    medicare_ffs_count$n_participants[[1]] == 0L
) {
  stop(
    "No eligible patients were found for primary Medicare FFS in ",
    analysis_year,
    ". Run and check Code/1.1_build_annual_eligible_population.R first."
  )
}

medicare_ffs_count <- medicare_ffs_count |>
  mutate(
    analysis_year = analysis_year,
    mx_insurance_group = mx_insurance_group,
    mx_insurance_segment = mx_insurance_segment,
    .before = n_participants
  )

message(
  "Eligible primary Medicare FFS participants in ",
  analysis_year,
  ": ",
  medicare_ffs_count$n_participants[[1]]
)

write.csv(
  medicare_ffs_count,
  file.path(output_dir, "2.3_medicare_ffs_count_2016.csv"),
  row.names = FALSE,
  na = ""
)

# ---- Materialize the subgroup required by k_table1() ----
message("Preparing the 2016 primary Medicare FFS Table 1 cohort.")

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", table1_cohort_identifier, ";
     CREATE TABLE ", table1_cohort_identifier, " AS
     SELECT DISTINCT
       patient_id,
       CAST(index_date AS DATE) AS index_date
     FROM ", eligibility_table_identifier, "
     WHERE analysis_year = ", analysis_year, "
       AND mx_insurance_group = 'MEDICARE'
       AND mx_insurance_segment = 'FFS'"
  )
)

# ---- Generate and save Table 1 ----
table1_2016_medicare_ffs <- tryCatch(
  {
    message("Generating Table 1 for the 2016 primary Medicare FFS subgroup.")

    ohdsilab::k_table1(
      con = con,
      cohort_table = table1_cohort,
      write_schema = write_schema,
      komodo_schema = komodo_schema,
      min_count = min_count
    )
  },
  finally = {
    DatabaseConnector::executeSql(
      con,
      paste0("DROP TABLE IF EXISTS ", table1_cohort_identifier)
    )
  }
)

print(table1_2016_medicare_ffs)

write.csv(
  as.data.frame(table1_2016_medicare_ffs),
  file.path(output_dir, "2.3_table1_2016_medicare_ffs.csv"),
  row.names = FALSE,
  na = ""
)

message("2016 primary Medicare FFS Table 1 generation is complete.")

