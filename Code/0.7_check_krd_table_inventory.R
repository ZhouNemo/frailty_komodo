library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(readr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto KRD table inventory diagnostic
# Author: Nemo Zhou
# Date started: 2026-06-17
# Date last updated: 2026-06-24
#
# ---- Purpose ----
# List all visible Komodo Research Dataset tables in the configured Redshift
# read schema and compare them with the project data dictionary. This is a
# schema-level diagnostic only; it does not query or print patient-level rows.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

data_dictionary_path <- file.path(
  "Documents",
  "Komodo Research Dataset Data Dictionary.xlsx - EXTERNAL FACING Data Dictionary.csv"
)

table_inventory_output <- file.path("Outputs", "0.7_krd_table_inventory.csv")
dictionary_comparison_output <- file.path(
  "Outputs",
  "0.7_krd_table_dictionary_comparison.csv"
)

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Read expected table names from the data dictionary ----
dictionary_tables <- readr::read_csv(
  data_dictionary_path,
  show_col_types = FALSE,
  progress = FALSE
) |>
  transmute(
    dictionary_table_name = toupper(table_name),
    expected_redshift_table_name = tolower(table_name)
  ) |>
  distinct() |>
  arrange(expected_redshift_table_name)

# ---- Pull visible KRD table names from Redshift metadata ----
krd_tables <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       table_schema,
       table_name,
       table_type
     FROM information_schema.tables
     WHERE table_schema = '", komodo_schema, "'
     ORDER BY table_name"
  )
) |>
  as_tibble() |>
  mutate(table_name = tolower(table_name))

dictionary_comparison <- dictionary_tables |>
  left_join(
    krd_tables |>
      transmute(
        expected_redshift_table_name = table_name,
        visible_in_redshift = TRUE,
        table_type
      ),
    by = "expected_redshift_table_name"
  ) |>
  mutate(visible_in_redshift = coalesce(visible_in_redshift, FALSE)) |>
  arrange(expected_redshift_table_name)

# ---- Print and save schema-level results ----
message("Visible tables in ", komodo_schema, ":")
print(krd_tables, n = Inf)

message("Dictionary tables compared with visible Redshift tables:")
print(dictionary_comparison, n = Inf)

readr::write_csv(krd_tables, table_inventory_output)
readr::write_csv(dictionary_comparison, dictionary_comparison_output)

message("Saved table inventory to: ", table_inventory_output)
message("Saved dictionary comparison to: ", dictionary_comparison_output)
