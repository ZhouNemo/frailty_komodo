library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual shared clinical metrics table
# Author: Nemo Zhou
# Date started: 2026-06-27
# Date last updated: 2026-06-27
#
# ---- Purpose ----
# Build the final shared annual clinical metrics table by joining the shared
# denominator to completed CFI, CCW, Gagne, and HIV annual outputs. This script
# does not rescan raw KRD claims and does not recompute metric logic. It writes
# one row per eligible patient-year to:
#   - annual_clinical_metrics_shared
#
# The default configuration processes 2016 only for validation. Additional years
# can be supplied later with options("frailty.clinical_metrics.config").

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

default_config <- list(
  analysis_years = 2016L,
  ids_table = "2_annual_metric_ids",
  cfi_scores_table = "annual_cfi_scores",
  ccw_condition_indicators_table = "annual_ccw_condition_indicators",
  ccw_group_counts_table = "annual_ccw_group_counts",
  gagne_scores_table = "annual_gagne_scores",
  hiv_status_table = "annual_hiv_status",
  final_table = "annual_clinical_metrics_shared"
)

config <- utils::modifyList(
  default_config,
  getOption("frailty.clinical_metrics.config", list())
)

analysis_years <- sort(unique(as.integer(config$analysis_years)))
if (
  length(analysis_years) == 0L ||
    any(is.na(analysis_years)) ||
    any(analysis_years < 2016L | analysis_years > 2025L)
) {
  stop("analysis_years must contain years from 2016 through 2025.")
}

ids_table <- config$ids_table
cfi_scores_table <- config$cfi_scores_table
ccw_condition_indicators_table <- config$ccw_condition_indicators_table
ccw_group_counts_table <- config$ccw_group_counts_table
gagne_scores_table <- config$gagne_scores_table
hiv_status_table <- config$hiv_status_table
final_table <- config$final_table

con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

qualified_identifier <- function(schema, table) {
  paste(quote_identifier(schema), quote_identifier(table), sep = ".")
}

sql_string <- function(value) {
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

sql_values <- function(values) {
  paste(values, collapse = ", ")
}

print_query <- function(label, sql) {
  message(label)
  result <- DBI::dbGetQuery(con, sql)
  print(result)
  invisible(result)
}

table_exists <- function(schema, table) {
  table_identifier <- qualified_identifier(schema, table)
  isTRUE(
    tryCatch(
      {
        DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_identifier, " LIMIT 0"))
        TRUE
      },
      error = function(e) FALSE
    )
  )
}

get_columns <- function(schema, table) {
  table_identifier <- qualified_identifier(schema, table)
  result <- tryCatch(
    DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_identifier, " LIMIT 0")),
    error = function(e) {
      stop(
        "Required table was not accessible: ",
        schema,
        ".",
        table,
        ". ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  tolower(names(result))
}

table_has_columns <- function(schema, table, columns) {
  existing <- get_columns(schema, table)
  missing_columns <- setdiff(tolower(columns), existing)
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns in ",
      schema,
      ".",
      table,
      ": ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

required_tables <- c(
  ids_table,
  cfi_scores_table,
  ccw_condition_indicators_table,
  ccw_group_counts_table,
  gagne_scores_table,
  hiv_status_table
)

for (table in required_tables) {
  if (!table_exists(write_schema, table)) {
    stop("Required table was not found: ", write_schema, ".", table)
  }
}

key_columns <- c("patid", "patient_id", "analysis_year")
table_has_columns(write_schema, ids_table, key_columns)
table_has_columns(
  write_schema,
  cfi_scores_table,
  c(
    key_columns,
    "cfi_score",
    "cfi_feature_count",
    "cfi_intercept_only_flag",
    "cfi_weight_sum",
    "lookup_version"
  )
)
table_has_columns(
  write_schema,
  ccw_condition_indicators_table,
  c(key_columns, "ccw_condition_count", "lookup_version")
)
table_has_columns(
  write_schema,
  ccw_group_counts_table,
  c(key_columns, "ccw_total_condition_count", "lookup_version")
)
table_has_columns(
  write_schema,
  gagne_scores_table,
  c(key_columns, "gagne_group_count", "gagne_score", "lookup_version")
)
table_has_columns(
  write_schema,
  hiv_status_table,
  c(
    key_columns,
    "hiv_status",
    "hiv_inpatient_evidence",
    "hiv_non_inpatient_distinct_dates",
    "hiv_non_inpatient_second_date",
    "hiv_first_observed_date",
    "hiv_evidence_count",
    "lookup_version"
  )
)

ids_columns <- get_columns(write_schema, ids_table)
ccw_indicator_columns <- setdiff(
  grep("^ccw_", get_columns(write_schema, ccw_condition_indicators_table), value = TRUE),
  c("ccw_condition_count")
)
ccw_group_columns <- grep(
  "^index_",
  get_columns(write_schema, ccw_group_counts_table),
  value = TRUE
)
gagne_indicator_columns <- grep(
  "^gagne_group_[0-9][0-9]_",
  get_columns(write_schema, gagne_scores_table),
  value = TRUE
)

if (length(ccw_indicator_columns) == 0L) {
  stop("No CCW condition indicator columns were found.")
}
if (length(ccw_group_columns) == 0L) {
  stop("No CCW group-count columns were found.")
}
if (length(gagne_indicator_columns) == 0L) {
  stop("No Gagne group indicator columns were found.")
}

select_expressions <- c(
  paste0("ids.", quote_identifier(ids_columns), " AS ", quote_identifier(ids_columns)),
  "cfi.cfi_score",
  "cfi.cfi_feature_count",
  "cfi.cfi_intercept_only_flag",
  "cfi.cfi_weight_sum",
  "cfi.lookup_version AS cfi_lookup_version",
  paste0("ccw_ind.", quote_identifier(ccw_indicator_columns)),
  "ccw_ind.ccw_condition_count",
  "ccw_ind.lookup_version AS ccw_condition_lookup_version",
  paste0("ccw_group.", quote_identifier(ccw_group_columns)),
  "ccw_group.ccw_total_condition_count",
  "ccw_group.lookup_version AS ccw_group_lookup_version",
  paste0("gagne.", quote_identifier(gagne_indicator_columns)),
  "gagne.gagne_group_count",
  "gagne.gagne_score",
  "gagne.lookup_version AS gagne_lookup_version",
  "hiv.hiv_status",
  "hiv.hiv_inpatient_evidence",
  "hiv.hiv_non_inpatient_distinct_dates",
  "hiv.hiv_non_inpatient_second_date",
  "hiv.hiv_first_observed_date",
  "hiv.hiv_evidence_count",
  "hiv.lookup_version AS hiv_lookup_version"
)

insert_columns <- c(
  ids_columns,
  "cfi_score",
  "cfi_feature_count",
  "cfi_intercept_only_flag",
  "cfi_weight_sum",
  "cfi_lookup_version",
  ccw_indicator_columns,
  "ccw_condition_count",
  "ccw_condition_lookup_version",
  ccw_group_columns,
  "ccw_total_condition_count",
  "ccw_group_lookup_version",
  gagne_indicator_columns,
  "gagne_group_count",
  "gagne_score",
  "gagne_lookup_version",
  "hiv_status",
  "hiv_inpatient_evidence",
  "hiv_non_inpatient_distinct_dates",
  "hiv_non_inpatient_second_date",
  "hiv_first_observed_date",
  "hiv_evidence_count",
  "hiv_lookup_version"
)

ids_identifier <- qualified_identifier(write_schema, ids_table)
cfi_identifier <- qualified_identifier(write_schema, cfi_scores_table)
ccw_ind_identifier <- qualified_identifier(write_schema, ccw_condition_indicators_table)
ccw_group_identifier <- qualified_identifier(write_schema, ccw_group_counts_table)
gagne_identifier <- qualified_identifier(write_schema, gagne_scores_table)
hiv_identifier <- qualified_identifier(write_schema, hiv_status_table)
final_identifier <- qualified_identifier(write_schema, final_table)

select_sql <- paste0(
  "SELECT
       ",
  paste(select_expressions, collapse = ",\n       "),
  "
     FROM ", ids_identifier, " ids
     INNER JOIN ", cfi_identifier, " cfi
       ON ids.patid = cfi.patid
      AND ids.analysis_year = cfi.analysis_year
     INNER JOIN ", ccw_ind_identifier, " ccw_ind
       ON ids.patid = ccw_ind.patid
      AND ids.analysis_year = ccw_ind.analysis_year
     INNER JOIN ", ccw_group_identifier, " ccw_group
       ON ids.patid = ccw_group.patid
      AND ids.analysis_year = ccw_group.analysis_year
     INNER JOIN ", gagne_identifier, " gagne
       ON ids.patid = gagne.patid
      AND ids.analysis_year = gagne.analysis_year
     INNER JOIN ", hiv_identifier, " hiv
       ON ids.patid = hiv.patid
      AND ids.analysis_year = hiv.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")"
)

if (!table_exists(write_schema, final_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", final_identifier, "
       DISTKEY(patid)
       SORTKEY(analysis_year, patid) AS
       ",
      select_sql,
      ";"
    )
  )
} else {
  table_has_columns(write_schema, final_table, insert_columns)

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DELETE FROM ", final_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ");

       INSERT INTO ", final_identifier, " (
         ",
      paste(quote_identifier(insert_columns), collapse = ",\n         "),
      "
       )
       ",
      select_sql,
      ";"
    )
  )
}

final_qa <- print_query(
  "Checking final annual clinical metrics table integrity.",
  paste0(
    "SELECT
       ids.analysis_year,
       COUNT(ids.patid)::BIGINT AS denominator_rows,
       COUNT(final.patid)::BIGINT AS final_rows,
       SUM(CASE WHEN final.patid IS NULL THEN 1 ELSE 0 END)::BIGINT
         AS missing_final_rows
     FROM ", ids_identifier, " ids
     LEFT JOIN ", final_identifier, " final
       ON ids.patid = final.patid
      AND ids.analysis_year = final.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY ids.analysis_year
     ORDER BY ids.analysis_year"
  )
)

if (
  any(final_qa$denominator_rows != final_qa$final_rows) ||
    any(final_qa$missing_final_rows != 0)
) {
  stop("Final annual clinical metrics integrity checks failed.")
}

duplicate_qa <- print_query(
  "Checking final annual clinical metrics duplicate rows.",
  paste0(
    "SELECT
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
         AS duplicate_final_rows
     FROM ", final_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")"
  )
)

if (duplicate_qa$duplicate_final_rows[[1]] != 0) {
  stop("annual_clinical_metrics_shared contains duplicate selected-year rows.")
}

message(
  "Annual clinical metrics table complete. Table updated in ",
  write_schema,
  ": ",
  final_table,
  "."
)
