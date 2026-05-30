library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(keyring)
library(DBI)

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))
small_cohort_table <- "small_table1_cohort"
cohort_size <- 100L

con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# Create a small cohort of patients with closed enrollment on the index date.
# The cohort table must contain patient_id and index_date for k_table1().
index_date <- as.Date("2020-01-01")
index_date_sql <- dbplyr::sql("CAST('2020-01-01' AS DATE)")

small_cohort <- tbl(con, inDatabaseSchema(komodo_schema, "patient_closed")) |>
  filter(
    closed_start_date <= index_date,
    closed_end_date >= index_date
  ) |>
  distinct(patient_id) |>
  head(cohort_size) |>
  mutate(index_date = index_date_sql) |>
  select(patient_id, index_date)

message("Creating cohort table: ", write_schema, ".", small_cohort_table)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", write_schema, ".", small_cohort_table, ";
     CREATE TABLE ", write_schema, ".", small_cohort_table, " AS ",
    dbplyr::sql_render(small_cohort)
  )
)

message("Cohort table created. Generating Table 1...")

# Generate aggregate baseline demographics. min_count suppresses small cells.
table1_small_cohort <- ohdsilab::k_table1(
  con = con,
  cohort_table = small_cohort_table,
  write_schema = write_schema,
  komodo_schema = komodo_schema,
  min_count = 11
)

message("Table 1 complete.")
print(table1_small_cohort)
