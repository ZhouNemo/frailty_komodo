# If needed, install packages inside the OHDSI Lab project
# renv::upgrade()
# renv::install("roux-ohdsi/ohdsilab")
# renv::install(c("dplyr", "keyring", "DBI","DatabaseConnector"))

library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(dplyr)
library(DBI)

# Run these once to save credentials in the OHDSI Lab workspace
keyring::key_set("db_username", prompt = "Redshift Username")
keyring::key_set("db_password", prompt = "Redshift Password")

# Optional check
keyring::key_get("db_username")

keyring::key_set("atlas_username", prompt = "Atlas username")
keyring::key_set("atlas_password", prompt = "Atlas password")

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)
con =  DatabaseConnector::connect(
  dbms = "redshift",
  server = "ohdsi-lab-redshift-cluster-prod.clsyktjhufn7.us-east-1.redshift.amazonaws.com/ohdsi_lab",
  port = 5439,
  user = keyring::key_get("db_username"),
  password = keyring::key_get("db_password"))

# Test if the connection works
if (isTRUE(DatabaseConnector::dbIsValid(con))) print("Connected Successfully")

# make it easier for some r functions to find the database
options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# test if connecting successfully
tbl(
  con,
  inDatabaseSchema(komodo_schema, "patient_demographics")
) |>
  head() |>
  collect()

