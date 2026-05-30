library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(keyring)
library(DBI)

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

# connect to Komodo
con <- ohdsilab_connect(
  username = key_get("db_username"),
  password = key_get("db_password")
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# generate a cohort
t2d_events <- ohdsilab::k_get_condition_events(
  con,
  codes = c("E11%"),
  start_date = as.Date("2020-01-01"),
  end_date = as.Date("2020-12-31"),
  write_schema = write_schema,
  table_name = "t2d_condition_events")

metabolic_surgery_events <- ohdsilab::k_get_procedure_events(
  con,
  codes = c("43644", "43775"),
  write_schema = write_schema,
  table_name = "metabolic_surgery_procedure_events")

metformin_events <- tbl(con, inDatabaseSchema(komodo_schema, "pharmacy_events")) |>
  filter(generic_name == "METFORMIN HCL") |>
  select(patient_id, date_prescription_written)

t2d_cohort <- t2d_events |>
  group_by(patient_id) |>
  summarize(index_date = min(diagnosis_date)) |>
  inner_join(metformin_events, by = "patient_id") |>
  filter(date_prescription_written >= index_date,
         date_prescription_written <= index_date + 30L) |>
  distinct(patient_id, index_date) |>
  inner_join(metabolic_surgery_events, by = "patient_id") |>
  filter(procedure_date >= index_date,
         procedure_date <= index_date + 180L) |>
  distinct(patient_id, index_date)

# view the data
# t2d_events |> head(5) |> collect()

# Save the cohort to a table in your write_schema for future access. 
# This step may take a while to run (e.g., this cohort took 58 minutes) 
# DatabaseConnector::executeSql(
#   con,
#   paste0(
#     "DROP TABLE IF EXISTS ", write_schema, ".t2d_cohort;
#      CREATE TABLE ", write_schema, ".t2d_cohort AS ",
#     dbplyr::sql_render(t2d_cohort)
#   )
# )
# 
# # Reference the saved table in your personal write schema
# saved_cohort <- tbl(con, inDatabaseSchema(write_schema, "t2d_cohort"))
