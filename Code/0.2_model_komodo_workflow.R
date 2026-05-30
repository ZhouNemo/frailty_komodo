library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Model script for future Komodo analyses.
# This template shows the standard workflow:
#   1. connect to Redshift,
#   2. define read/write schemas,
#   3. materialize condition and procedure event tables,
#   4. materialize a final cohort table,
#   5. create an aggregate Table 1.

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

model_t2d_table <- "model_t2d_events"
model_surgery_table <- "model_metabolic_surgery_events"
model_cohort_table <- "model_t2d_metformin_surgery_cohort"

con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# These ohdsilab helper functions materialize physical Redshift tables.
# They may take time on large concepts because they scan claims tables.
t2d_events <- ohdsilab::k_get_condition_events(
  con = con,
  codes = c("E11%"),
  write_schema = write_schema,
  table_name = model_t2d_table
)

metabolic_surgery_events <- ohdsilab::k_get_procedure_events(
  con = con,
  codes = c("43644", "43775"),
  write_schema = write_schema,
  table_name = model_surgery_table
)

metformin_events <- tbl(con, inDatabaseSchema(komodo_schema, "pharmacy_events")) |>
  filter(generic_name == "METFORMIN HCL") |>
  select(patient_id, date_prescription_written)

# Build a tutorial-style cohort:
#   - first type 2 diabetes diagnosis is the index date,
#   - metformin prescription written within 30 days after index,
#   - metabolic surgery within 180 days after index.
model_cohort <- t2d_events |>
  group_by(patient_id) |>
  summarize(index_date = min(diagnosis_date), .groups = "drop") |>
  inner_join(metformin_events, by = "patient_id") |>
  filter(
    date_prescription_written >= index_date,
    date_prescription_written <= index_date + 30L
  ) |>
  distinct(patient_id, index_date) |>
  inner_join(metabolic_surgery_events, by = "patient_id") |>
  filter(
    procedure_date >= index_date,
    procedure_date <= index_date + 180L
  ) |>
  distinct(patient_id, index_date) |>
  mutate(index_date = sql("CAST(index_date AS DATE)")) |>
  select(patient_id, index_date)

message("Creating cohort table: ", write_schema, ".", model_cohort_table)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", write_schema, ".", model_cohort_table, ";
     CREATE TABLE ", write_schema, ".", model_cohort_table, " AS ",
    dbplyr::sql_render(model_cohort)
  )
)

message("Cohort table created. Generating aggregate Table 1...")

table1_model <- ohdsilab::k_table1(
  con = con,
  cohort_table = model_cohort_table,
  write_schema = write_schema,
  komodo_schema = komodo_schema,
  min_count = 11
)

message("Table 1 complete.")
print(table1_model)

# For interactive review in RStudio, run after the script completes:
# View(table1_model)
