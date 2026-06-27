library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual shared clinical metric matching
# Author: Nemo Zhou
# Date started: 2026-06-27
# Date last updated: 2026-06-27
#
# ---- Purpose ----
# Build the shared matched-event layer for annual clinical metric processing.
# The workflow creates a reusable denominator and matched diagnosis/procedure
# feature tables for CFI, CCW, Gagne's combined comorbidity score, and HIV
# status. Metric scoring is intentionally left to downstream 5.x scripts.
#
# The script processes selected years one at a time to limit Redshift temporary
# disk pressure. Each run deletes and replaces only the selected years in:
#   - 2_annual_metric_ids
#   - 2_annual_diagnosis_matches
#   - 2_annual_procedure_matches
#
# Code/5.1_prepare_2016_clinical_metric_matches.R calls this engine with a
# 2016-only configuration. Patient-level matched events remain in Redshift. Only
# aggregate QA results are printed to the console.
#
# ---- Scope and conventions ----
# - Code system: this is the ICD-10-CM shared diagnosis matcher for the
#   2016-2025 analysis years. Only ICD-10-CM diagnosis lookup rows and CPT/HCPCS
#   procedure lookup rows participate in matching. KRD diagnoses are ICD-10-CM
#   from 2016 onward, so ICD-9-CM lookup rows are inert for this period; they are
#   excluded with a logged count rather than silently dropped.
# - Analysis window: events are restricted to the analysis calendar year
#   [year_start, year_end_exclusive). The shared match tables therefore hold
#   same-year evidence only. A future multi-year (pre-index) lookback cannot be
#   recovered from these tables and would require a separate extraction.
# - Date columns in 2_annual_metric_ids map to the metric-document language as:
#     analysis_start_date = year_start
#     analysis_end_date   = year_end_exclusive - 1 day (inclusive December 31)

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Analysis parameters ----
default_clinical_metric_config <- list(
  analysis_years = 2016:2025,
  id_years = 2016:2025,
  eligibility_table = "1_annual_eligible_cohort",
  ids_table = "2_annual_metric_ids",
  diagnosis_matches_table = "2_annual_diagnosis_matches",
  procedure_matches_table = "2_annual_procedure_matches",
  workflow_label = "annual production",
  # The array candidate prefilter is purely a performance row-reduction step.
  # It is disabled by default so it can never silently drop a true match. Only
  # re-enable it after measuring its row-reduction benefit and confirming a
  # prefilter-on vs prefilter-off aggregate match-count parity check passes.
  enable_candidate_prefilter = FALSE,
  lookup_dir = file.path(
    getwd(),
    "Documents",
    "Clinical Metric Look Up Tables"
  )
)

clinical_metric_config <- utils::modifyList(
  default_clinical_metric_config,
  getOption("frailty.clinical_metrics.config", list())
)

analysis_years <- sort(unique(as.integer(
  clinical_metric_config$analysis_years
)))
id_years <- sort(unique(as.integer(clinical_metric_config$id_years)))

if (
  length(analysis_years) == 0L ||
    any(is.na(analysis_years)) ||
    any(analysis_years < 2016L | analysis_years > 2025L)
) {
  stop("analysis_years must contain years from 2016 through 2025.")
}

if (
  length(id_years) == 0L ||
    any(is.na(id_years)) ||
    any(id_years < 2016L | id_years > 2025L) ||
    length(setdiff(analysis_years, id_years)) > 0L
) {
  stop(
    "id_years must contain all processing years and remain within 2016-2025."
  )
}

eligibility_table <- clinical_metric_config$eligibility_table
ids_table <- clinical_metric_config$ids_table
diagnosis_matches_table <- clinical_metric_config$diagnosis_matches_table
procedure_matches_table <- clinical_metric_config$procedure_matches_table
enable_candidate_prefilter <- isTRUE(
  clinical_metric_config$enable_candidate_prefilter
)
lookup_dir <- clinical_metric_config$lookup_dir

lookup_validation_path <- file.path(
  lookup_dir,
  "0.6_clinical_metric_lookup_validation.csv"
)
diagnosis_lookup_path <- file.path(
  lookup_dir,
  "0.6_unified_diagnosis_rule_lookup.csv"
)
procedure_lookup_path <- file.path(
  lookup_dir,
  "0.6_cfi_procedure_lookup.csv"
)

diagnosis_lookup_stage_table <- "clinical_metric_diagnosis_lookup_stage"
procedure_lookup_stage_table <- "clinical_metric_procedure_lookup_stage"
diagnosis_prefix_table <- "clinical_metric_diagnosis_prefixes"
procedure_prefix_table <- "clinical_metric_procedure_prefixes"
inpatient_stage_table <- "clinical_metric_inpatient_stage"
non_inpatient_stage_table <- "clinical_metric_non_inpatient_stage"
diagnosis_position_table <- "clinical_metric_diagnosis_array_positions"
procedure_position_table <- "clinical_metric_procedure_array_positions"

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Helpers ----
quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

qualified_identifier <- function(schema, table) {
  paste(
    quote_identifier(schema),
    quote_identifier(table),
    sep = "."
  )
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

normalize_code <- function(x) {
  gsub("[^A-Z0-9]", "", toupper(trimws(as.character(x))))
}

common_prefix <- function(start, end) {
  start <- as.character(start)
  end <- as.character(end)

  if (is.na(start) || is.na(end) || start == "" || end == "") {
    return("")
  }

  max_length <- min(nchar(start), nchar(end))
  if (max_length == 0L) {
    return("")
  }

  prefix_length <- 0L
  for (i in seq_len(max_length)) {
    if (substr(start, i, i) == substr(end, i, i)) {
      prefix_length <- i
    } else {
      break
    }
  }

  if (prefix_length == 0L) {
    ""
  } else {
    substr(start, 1L, prefix_length)
  }
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

table_has_columns <- function(schema, table, columns) {
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

  available_columns <- tolower(names(result))
  missing_columns <- setdiff(tolower(columns), available_columns)
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns in ",
      schema,
      ".",
      table,
      ": ",
      paste(missing_columns, collapse = ", "),
      ". Available columns reported by zero-row table query: ",
      if (length(available_columns) == 0L) {
        "<none>"
      } else {
        paste(available_columns, collapse = ", ")
      }
    )
  }

  invisible(TRUE)
}

read_lookup_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character",
    check.names = FALSE,
    na.strings = c("", "NA")
  )
}

require_columns <- function(data, columns, label) {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  invisible(TRUE)
}

execute_insert_batches <- function(
  table_identifier,
  columns,
  data,
  chunk_size = 1000L
) {
  if (nrow(data) == 0L) {
    return(invisible(NULL))
  }

  column_sql <- paste(quote_identifier(columns), collapse = ", ")
  starts <- seq.int(1L, nrow(data), by = chunk_size)

  for (start_row in starts) {
    end_row <- min(start_row + chunk_size - 1L, nrow(data))
    chunk <- data[start_row:end_row, columns, drop = FALSE]

    values <- apply(
      chunk,
      1L,
      function(row) {
        paste0(
          "(",
          paste(
            vapply(
              names(row),
              function(column) {
                value <- row[[column]]
                if (is.na(value)) {
                  "NULL"
                } else {
                  sql_string(value)
                }
              },
              character(1)
            ),
            collapse = ", "
          ),
          ")"
        )
      }
    )

    DatabaseConnector::executeSql(
      con,
      paste0(
        "INSERT INTO ",
        table_identifier,
        " (",
        column_sql,
        ") VALUES ",
        paste(values, collapse = ", "),
        ";"
      ),
      progressBar = FALSE,
      reportOverallTime = FALSE
    )
  }

  invisible(NULL)
}

build_diagnosis_prefixes <- function(lookup) {
  prefixes <- character(0)
  unsafe_rows <- 0L

  for (i in seq_len(nrow(lookup))) {
    match_type <- lookup$match_type[[i]]

    if (match_type %in% c("exact", "prefix")) {
      value <- lookup$match_value[[i]]
      if (is.na(value) || value == "") {
        unsafe_rows <- unsafe_rows + 1L
      } else {
        prefixes <- c(prefixes, substr(value, 1L, min(3L, nchar(value))))
      }
    } else if (match_type == "range") {
      prefix <- common_prefix(
        lookup$range_start[[i]],
        lookup$range_end[[i]]
      )
      if (prefix == "") {
        unsafe_rows <- unsafe_rows + 1L
      } else {
        prefixes <- c(prefixes, prefix)
      }
    } else {
      unsafe_rows <- unsafe_rows + 1L
    }
  }

  list(prefixes = sort(unique(prefixes)), unsafe_rows = unsafe_rows)
}

build_procedure_prefixes <- function(lookup) {
  prefixes <- character(0)
  unsafe_rows <- 0L

  for (i in seq_len(nrow(lookup))) {
    prefix <- common_prefix(lookup$range_start[[i]], lookup$range_end[[i]])
    if (prefix == "") {
      unsafe_rows <- unsafe_rows + 1L
    } else {
      prefixes <- c(prefixes, prefix)
    }
  }

  list(prefixes = sort(unique(prefixes)), unsafe_rows = unsafe_rows)
}

load_prefix_table <- function(table, prefixes) {
  table_identifier <- quote_identifier(table)

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", table_identifier, ";
       CREATE TEMP TABLE ", table_identifier, " (
         candidate_prefix VARCHAR(32) NOT NULL
       )
       DISTSTYLE ALL
       SORTKEY(candidate_prefix);"
    ),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  if (length(prefixes) > 0L) {
    execute_insert_batches(
      table_identifier,
      "candidate_prefix",
      data.frame(candidate_prefix = prefixes, stringsAsFactors = FALSE)
    )
  }
}

load_diagnosis_lookup <- function(lookup) {
  table_identifier <- quote_identifier(diagnosis_lookup_stage_table)

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", table_identifier, ";
       CREATE TEMP TABLE ", table_identifier, " (
         lookup_version VARCHAR(128) NOT NULL,
         metric VARCHAR(32) NOT NULL,
         feature_id VARCHAR(128) NOT NULL,
         feature_name VARCHAR(256) NOT NULL,
         code_system VARCHAR(32) NOT NULL,
         match_value VARCHAR(64),
         range_start VARCHAR(64),
         range_end VARCHAR(64),
         range_end_inclusive VARCHAR(16),
         match_type VARCHAR(32) NOT NULL
       )
       DISTSTYLE ALL
       SORTKEY(match_type, match_value, range_start, range_end);"
    )
  )

  execute_insert_batches(
    table_identifier,
    c(
      "lookup_version",
      "metric",
      "feature_id",
      "feature_name",
      "code_system",
      "match_value",
      "range_start",
      "range_end",
      "range_end_inclusive",
      "match_type"
    ),
    lookup
  )
}

load_procedure_lookup <- function(lookup) {
  table_identifier <- quote_identifier(procedure_lookup_stage_table)

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", table_identifier, ";
       CREATE TEMP TABLE ", table_identifier, " (
         lookup_version VARCHAR(128) NOT NULL,
         metric VARCHAR(32) NOT NULL,
         feature_id VARCHAR(128) NOT NULL,
         feature_name VARCHAR(256) NOT NULL,
         code_system VARCHAR(32) NOT NULL,
         range_start VARCHAR(64) NOT NULL,
         range_end VARCHAR(64) NOT NULL,
         match_type VARCHAR(32) NOT NULL
       )
       DISTSTYLE ALL
       SORTKEY(range_start, range_end);"
    )
  )

  execute_insert_batches(
    table_identifier,
    c(
      "lookup_version",
      "metric",
      "feature_id",
      "feature_name",
      "code_system",
      "range_start",
      "range_end",
      "match_type"
    ),
    lookup
  )
}

array_prefix_condition <- function(array_field, prefix_table, enabled) {
  if (!enabled) {
    return("1 = 1")
  }

  paste0(
    "EXISTS (
       SELECT 1
       FROM ",
    quote_identifier(prefix_table),
    " pref
       WHERE ",
    array_field,
    " LIKE ('%\"' || pref.candidate_prefix || '%')
     )"
  )
}

drop_temporary_tables <- function() {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", quote_identifier(inpatient_stage_table), ";
       DROP TABLE IF EXISTS ", quote_identifier(non_inpatient_stage_table), ";
       DROP TABLE IF EXISTS ", quote_identifier(diagnosis_position_table), ";
       DROP TABLE IF EXISTS ", quote_identifier(procedure_position_table), ";"
    ),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
}

ensure_ids_table <- function(identifier) {
  if (!table_exists(write_schema, ids_table)) {
    DatabaseConnector::executeSql(
      con,
      paste0(
        "CREATE TABLE ", identifier, " (
           patid VARCHAR(256) NOT NULL,
           patient_id VARCHAR(256) NOT NULL,
           analysis_year INTEGER NOT NULL,
           eligibility_index_date DATE,
           analysis_start_date DATE NOT NULL,
           analysis_end_date DATE NOT NULL,
           age INTEGER,
           patient_gender VARCHAR(64),
           patient_race_ethnicity VARCHAR(128),
           mx_insurance_group VARCHAR(128),
           mx_insurance_segment VARCHAR(128),
           mx_secondary_insurance_group VARCHAR(128),
           mx_secondary_insurance_segment VARCHAR(128),
           rx_insurance_group VARCHAR(128),
           rx_insurance_segment VARCHAR(128),
           rx_secondary_insurance_group VARCHAR(128),
           rx_secondary_insurance_segment VARCHAR(128)
         )
         DISTKEY(patid)
         SORTKEY(analysis_year, patid);"
      )
    )
  }

  table_has_columns(
    write_schema,
    ids_table,
    c(
      "patid",
      "patient_id",
      "analysis_year",
      "eligibility_index_date",
      "analysis_start_date",
      "analysis_end_date",
      "age",
      "patient_gender",
      "patient_race_ethnicity",
      "mx_insurance_group",
      "mx_insurance_segment",
      "mx_secondary_insurance_group",
      "mx_secondary_insurance_segment",
      "rx_insurance_group",
      "rx_insurance_segment",
      "rx_secondary_insurance_group",
      "rx_secondary_insurance_segment"
    )
  )
}

ensure_diagnosis_matches_table <- function(identifier) {
  if (!table_exists(write_schema, diagnosis_matches_table)) {
    DatabaseConnector::executeSql(
      con,
      paste0(
        "CREATE TABLE ", identifier, " (
           patid VARCHAR(256) NOT NULL,
           analysis_year INTEGER NOT NULL,
           diagnosis_date DATE NOT NULL,
           claim_setting VARCHAR(40) NOT NULL,
           diagnosis_source VARCHAR(40) NOT NULL,
           diagnosis_code VARCHAR(64) NOT NULL,
           code_system VARCHAR(32) NOT NULL,
           metric VARCHAR(32) NOT NULL,
           feature_id VARCHAR(128) NOT NULL,
           feature_name VARCHAR(256) NOT NULL,
           match_value VARCHAR(64),
           range_start VARCHAR(64),
           range_end VARCHAR(64),
           range_end_inclusive VARCHAR(16),
           match_type VARCHAR(32) NOT NULL,
           lookup_version VARCHAR(128) NOT NULL
         )
         DISTKEY(patid)
         SORTKEY(analysis_year, patid, metric, diagnosis_code);"
      )
    )
  }

  table_has_columns(
    write_schema,
    diagnosis_matches_table,
    c(
      "patid",
      "analysis_year",
      "diagnosis_date",
      "claim_setting",
      "diagnosis_source",
      "diagnosis_code",
      "code_system",
      "metric",
      "feature_id",
      "feature_name",
      "match_value",
      "range_start",
      "range_end",
      "range_end_inclusive",
      "match_type",
      "lookup_version"
    )
  )
}

ensure_procedure_matches_table <- function(identifier) {
  if (!table_exists(write_schema, procedure_matches_table)) {
    DatabaseConnector::executeSql(
      con,
      paste0(
        "CREATE TABLE ", identifier, " (
           patid VARCHAR(256) NOT NULL,
           analysis_year INTEGER NOT NULL,
           procedure_date DATE NOT NULL,
           procedure_source VARCHAR(40) NOT NULL,
           procedure_code VARCHAR(64) NOT NULL,
           metric VARCHAR(32) NOT NULL,
           feature_id VARCHAR(128) NOT NULL,
           feature_name VARCHAR(256) NOT NULL,
           range_start VARCHAR(64) NOT NULL,
           range_end VARCHAR(64) NOT NULL,
           match_type VARCHAR(32) NOT NULL,
           lookup_version VARCHAR(128) NOT NULL
         )
         DISTKEY(patid)
         SORTKEY(analysis_year, patid, procedure_code);"
      )
    )
  }

  table_has_columns(
    write_schema,
    procedure_matches_table,
    c(
      "patid",
      "analysis_year",
      "procedure_date",
      "procedure_source",
      "procedure_code",
      "metric",
      "feature_id",
      "feature_name",
      "range_start",
      "range_end",
      "match_type",
      "lookup_version"
    )
  )
}

# ---- Validate local lookup artifacts ----
required_lookup_files <- c(
  lookup_validation_path,
  diagnosis_lookup_path,
  procedure_lookup_path
)
missing_lookup_files <- required_lookup_files[!file.exists(required_lookup_files)]

if (length(missing_lookup_files) > 0L) {
  stop(
    "Missing required clinical metric lookup files:\n",
    paste(" -", missing_lookup_files, collapse = "\n")
  )
}

lookup_validation <- read_lookup_csv(lookup_validation_path)
require_columns(
  lookup_validation,
  c("metric", "check_name", "status", "detail"),
  "Clinical metric lookup validation summary"
)

failed_lookup_checks <- lookup_validation[
  is.na(lookup_validation$status) |
    tolower(lookup_validation$status) != "pass",
]

if (nrow(failed_lookup_checks) > 0L) {
  stop(
    "Clinical metric lookup validation has non-passing checks. ",
    "Run Code/0.6_validate_clinical_metric_lookups.R and review the summary."
  )
}

diagnosis_lookup <- read_lookup_csv(diagnosis_lookup_path)
require_columns(
  diagnosis_lookup,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "code_system",
    "match_value",
    "range_start",
    "range_end",
    "range_end_inclusive",
    "match_type",
    "final_match_after_flattening"
  ),
  "Unified diagnosis lookup"
)

diagnosis_lookup$code_system <- toupper(diagnosis_lookup$code_system)
diagnosis_lookup$match_type <- tolower(diagnosis_lookup$match_type)
diagnosis_lookup$match_value <- normalize_code(diagnosis_lookup$match_value)
diagnosis_lookup$range_start <- normalize_code(diagnosis_lookup$range_start)
diagnosis_lookup$range_end <- normalize_code(diagnosis_lookup$range_end)

active_diagnosis_filter <- !is.na(diagnosis_lookup$code_system) &
  diagnosis_lookup$code_system == "ICD10CM" &
  !is.na(diagnosis_lookup$final_match_after_flattening) &
  toupper(diagnosis_lookup$final_match_after_flattening) == "TRUE"

active_diagnosis_lookup <- diagnosis_lookup[
  active_diagnosis_filter,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "code_system",
    "match_value",
    "range_start",
    "range_end",
    "range_end_inclusive",
    "match_type"
  )
]

if (nrow(active_diagnosis_lookup) == 0L) {
  stop("No active ICD-10-CM diagnosis lookup rows were found.")
}

invalid_diagnosis_lookup <- active_diagnosis_lookup[
  !(active_diagnosis_lookup$match_type %in% c("exact", "prefix", "range")) |
    (
      active_diagnosis_lookup$match_type %in% c("exact", "prefix") &
        (
          is.na(active_diagnosis_lookup$match_value) |
            active_diagnosis_lookup$match_value == ""
        )
    ) |
    (
      active_diagnosis_lookup$match_type == "range" &
        (
          is.na(active_diagnosis_lookup$range_start) |
            active_diagnosis_lookup$range_start == "" |
            is.na(active_diagnosis_lookup$range_end) |
            active_diagnosis_lookup$range_end == ""
        )
    ),
]

if (nrow(invalid_diagnosis_lookup) > 0L) {
  stop("Active diagnosis lookup rows are missing required match fields.")
}

procedure_lookup <- read_lookup_csv(procedure_lookup_path)
require_columns(
  procedure_lookup,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "code_system",
    "range_start",
    "range_end",
    "match_type"
  ),
  "CFI procedure lookup"
)

procedure_lookup$code_system <- toupper(procedure_lookup$code_system)
procedure_lookup$match_type <- tolower(procedure_lookup$match_type)
procedure_lookup$range_start <- normalize_code(procedure_lookup$range_start)
procedure_lookup$range_end <- normalize_code(procedure_lookup$range_end)

active_procedure_filter <- !is.na(procedure_lookup$code_system) &
  procedure_lookup$code_system == "CPT_HCPCS" &
  !is.na(procedure_lookup$match_type) &
  procedure_lookup$match_type == "range"

active_procedure_lookup <- procedure_lookup[
  active_procedure_filter,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "code_system",
    "range_start",
    "range_end",
    "match_type"
  )
]

if (nrow(active_procedure_lookup) == 0L) {
  stop("No active CPT/HCPCS procedure lookup rows were found.")
}

invalid_procedure_lookup <- active_procedure_lookup[
  is.na(active_procedure_lookup$range_start) |
    active_procedure_lookup$range_start == "" |
    is.na(active_procedure_lookup$range_end) |
    active_procedure_lookup$range_end == "",
]

if (nrow(invalid_procedure_lookup) > 0L) {
  stop("Active procedure lookup rows are missing required range fields.")
}

# Make the code-system scope explicit. KRD diagnoses are ICD-10-CM for the
# 2016-2025 analysis years, so ICD-9-CM lookup rows are expected to be inert.
# Log how many final-match lookup rows are excluded by code system instead of
# dropping them silently.
final_match_diagnosis <- diagnosis_lookup[
  !is.na(diagnosis_lookup$final_match_after_flattening) &
    toupper(diagnosis_lookup$final_match_after_flattening) == "TRUE",
]
excluded_diagnosis_rows <- final_match_diagnosis[
  is.na(final_match_diagnosis$code_system) |
    final_match_diagnosis$code_system != "ICD10CM",
]
message(
  "Diagnosis lookup: ",
  nrow(active_diagnosis_lookup),
  " active ICD-10-CM final-match rows; ",
  nrow(excluded_diagnosis_rows),
  " final-match rows excluded by code system (not ICD10CM)."
)
if (nrow(excluded_diagnosis_rows) > 0L) {
  print(as.data.frame(
    table(code_system = excluded_diagnosis_rows$code_system),
    stringsAsFactors = FALSE
  ))
}

excluded_procedure_rows <- procedure_lookup[
  is.na(procedure_lookup$code_system) |
    procedure_lookup$code_system != "CPT_HCPCS",
]
message(
  "Procedure lookup: ",
  nrow(active_procedure_lookup),
  " active CPT/HCPCS range rows; ",
  nrow(excluded_procedure_rows),
  " rows excluded by code system (not CPT_HCPCS)."
)

diagnosis_prefix_result <- build_diagnosis_prefixes(active_diagnosis_lookup)
procedure_prefix_result <- build_procedure_prefixes(active_procedure_lookup)

diagnosis_prefilter_enabled <- enable_candidate_prefilter &&
  diagnosis_prefix_result$unsafe_rows == 0L &&
  length(diagnosis_prefix_result$prefixes) > 0L
procedure_prefilter_enabled <- enable_candidate_prefilter &&
  procedure_prefix_result$unsafe_rows == 0L &&
  length(procedure_prefix_result$prefixes) > 0L

if (enable_candidate_prefilter && !diagnosis_prefilter_enabled) {
  message(
    "Diagnosis array prefilter disabled because ",
    diagnosis_prefix_result$unsafe_rows,
    " active lookup row(s) did not have a safe candidate prefix."
  )
}

if (enable_candidate_prefilter && !procedure_prefilter_enabled) {
  message(
    "Procedure array prefilter disabled because ",
    procedure_prefix_result$unsafe_rows,
    " active lookup row(s) did not have a safe candidate prefix."
  )
}

# ---- Validate source Redshift tables and columns ----
eligibility_identifier <- qualified_identifier(write_schema, eligibility_table)
ids_identifier <- qualified_identifier(write_schema, ids_table)
diagnosis_matches_identifier <- qualified_identifier(
  write_schema,
  diagnosis_matches_table
)
procedure_matches_identifier <- qualified_identifier(
  write_schema,
  procedure_matches_table
)

if (!table_exists(write_schema, eligibility_table)) {
  stop(
    "Required eligibility table was not found: ",
    write_schema,
    ".",
    eligibility_table,
    ". Run Code/1.1_build_annual_eligible_population.R first."
  )
}

required_eligibility_columns <- c(
  "patient_id",
  "analysis_year",
  "index_date",
  "age",
  "patient_gender",
  "mx_insurance_group",
  "mx_insurance_segment",
  "mx_secondary_insurance_group",
  "mx_secondary_insurance_segment",
  "rx_insurance_group",
  "rx_insurance_segment",
  "rx_secondary_insurance_group",
  "rx_secondary_insurance_segment"
)

table_has_columns(write_schema, eligibility_table, required_eligibility_columns)

eligibility_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", eligibility_identifier, " LIMIT 0")
)))

race_select <- if ("patient_race_ethnicity" %in% eligibility_columns) {
  "e.patient_race_ethnicity"
} else {
  "CAST(NULL AS VARCHAR(128))"
}

table_has_columns(
  komodo_schema,
  "inpatient_events",
  c(
    "patient_id",
    "claim_from_date",
    "admission_diagnosis_code",
    "primary_diagnosis_code",
    "secondary_diagnosis_codes",
    "cpt_hcpcs_codes"
  )
)

table_has_columns(
  komodo_schema,
  "non_inpatient_events",
  c("patient_id", "service_date", "diagnosis_codes", "procedure_code")
)

eligible_year_counts <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT analysis_year, COUNT(*)::BIGINT AS n_person_years
     FROM ", eligibility_identifier, "
     WHERE analysis_year IN (", sql_values(id_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

missing_years <- setdiff(id_years, eligible_year_counts$analysis_year)
if (length(missing_years) > 0L) {
  stop(
    "No eligible patient-years were found for: ",
    paste(missing_years, collapse = ", "),
    "."
  )
}

print(eligible_year_counts)

# ---- Initialize outputs and temporary lookup tables ----
ensure_ids_table(ids_identifier)
ensure_diagnosis_matches_table(diagnosis_matches_identifier)
ensure_procedure_matches_table(procedure_matches_identifier)

load_diagnosis_lookup(active_diagnosis_lookup)
load_procedure_lookup(active_procedure_lookup)
load_prefix_table(diagnosis_prefix_table, diagnosis_prefix_result$prefixes)
load_prefix_table(procedure_prefix_table, procedure_prefix_result$prefixes)

prefilter_summary <- data.frame(
  prefilter = c("diagnosis_array", "procedure_array"),
  enabled = c(diagnosis_prefilter_enabled, procedure_prefilter_enabled),
  candidate_prefixes = c(
    length(diagnosis_prefix_result$prefixes),
    length(procedure_prefix_result$prefixes)
  ),
  unsafe_lookup_rows = c(
    diagnosis_prefix_result$unsafe_rows,
    procedure_prefix_result$unsafe_rows
  )
)
print(prefilter_summary)

# ---- Materialize or refresh the configured metric ID population ----
DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(id_years), ");

     INSERT INTO ", ids_identifier, " (
       patid,
       patient_id,
       analysis_year,
       eligibility_index_date,
       analysis_start_date,
       analysis_end_date,
       age,
       patient_gender,
       patient_race_ethnicity,
       mx_insurance_group,
       mx_insurance_segment,
       mx_secondary_insurance_group,
       mx_secondary_insurance_segment,
       rx_insurance_group,
       rx_insurance_segment,
       rx_secondary_insurance_group,
       rx_secondary_insurance_segment
     )
     SELECT DISTINCT
       e.patient_id || '_' || e.analysis_year::VARCHAR AS patid,
       e.patient_id,
       e.analysis_year,
       CAST(e.index_date AS DATE) AS eligibility_index_date,
       TO_DATE(e.analysis_year::VARCHAR || '-01-01', 'YYYY-MM-DD')
         AS analysis_start_date,
       TO_DATE((e.analysis_year + 1)::VARCHAR || '-01-01', 'YYYY-MM-DD') - 1
         AS analysis_end_date,
       e.age,
       e.patient_gender,
       ", race_select, " AS patient_race_ethnicity,
       e.mx_insurance_group,
       e.mx_insurance_segment,
       e.mx_secondary_insurance_group,
       e.mx_secondary_insurance_segment,
       e.rx_insurance_group,
       e.rx_insurance_segment,
       e.rx_secondary_insurance_group,
       e.rx_secondary_insurance_segment
     FROM ", eligibility_identifier, " e
     WHERE e.analysis_year IN (", sql_values(id_years), ")
       AND e.patient_id IS NOT NULL;"
  )
)

id_integrity <- print_query(
  "Checking 2_annual_metric_ids integrity for selected years.",
  paste0(
    "SELECT
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_distinct_patid,
       SUM(CASE WHEN patid IS NULL OR patid = ''
         THEN 1 ELSE 0 END)::BIGINT AS missing_patid
     FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(id_years), ")"
  )
)

if (
  id_integrity$n_rows[[1]] == 0 ||
    id_integrity$n_rows[[1]] != id_integrity$n_distinct_patid[[1]] ||
    id_integrity$missing_patid[[1]] != 0
) {
  stop("The configured 2_annual_metric_ids table failed its integrity check.")
}

# ---- Process one year at a time ----
for (analysis_year in analysis_years) {
  year_start <- paste0(analysis_year, "-01-01")
  year_end <- paste0(analysis_year + 1L, "-01-01")

  message(
    "Starting ",
    analysis_year,
    " shared clinical metric event matching (",
    match(analysis_year, analysis_years),
    " of ",
    length(analysis_years),
    ")."
  )

  drop_temporary_tables()

  stage_sql <- paste0(
    "CREATE TEMP TABLE ", quote_identifier(inpatient_stage_table), "
     DISTKEY(patient_id)
     SORTKEY(event_date) AS
     SELECT
       ids.patid,
       ids.analysis_year,
       i.patient_id,
       i.claim_from_date AS event_date,
       i.admission_diagnosis_code,
       i.primary_diagnosis_code,
       i.secondary_diagnosis_codes,
       i.cpt_hcpcs_codes
     FROM (
       SELECT patid, analysis_year, patient_id
       FROM ", ids_identifier, "
       WHERE analysis_year = ", analysis_year, "
     ) ids
     INNER JOIN ", quote_identifier(komodo_schema), ".",
    quote_identifier("inpatient_events"),
    " i
       ON ids.patient_id = i.patient_id
     WHERE i.claim_from_date >= ", sql_string(year_start), "::DATE
       AND i.claim_from_date < ", sql_string(year_end), "::DATE;

     CREATE TEMP TABLE ", quote_identifier(non_inpatient_stage_table), "
     DISTKEY(patient_id)
     SORTKEY(event_date) AS
     SELECT
       ids.patid,
       ids.analysis_year,
       n.patient_id,
       n.service_date AS event_date,
       n.diagnosis_codes,
       n.procedure_code
     FROM (
       SELECT patid, analysis_year, patient_id
       FROM ", ids_identifier, "
       WHERE analysis_year = ", analysis_year, "
     ) ids
     INNER JOIN ", quote_identifier(komodo_schema), ".",
    quote_identifier("non_inpatient_events"),
    " n
       ON ids.patient_id = n.patient_id
     WHERE n.service_date >= ", sql_string(year_start), "::DATE
       AND n.service_date < ", sql_string(year_end), "::DATE;"
  )

  message("Creating ", analysis_year, " restricted event staging tables.")
  DatabaseConnector::executeSql(con, stage_sql)

  print_query(
    paste0("Checking ", analysis_year, " staged event counts."),
    paste0(
      "SELECT
         'inpatient' AS source_table,
         COUNT(*)::BIGINT AS staged_rows,
         SUM(CASE WHEN event_date IS NULL
           THEN 1 ELSE 0 END)::BIGINT AS missing_event_date
       FROM ", quote_identifier(inpatient_stage_table), "
       UNION ALL
       SELECT
         'non_inpatient' AS source_table,
         COUNT(*)::BIGINT AS staged_rows,
         SUM(CASE WHEN event_date IS NULL
           THEN 1 ELSE 0 END)::BIGINT AS missing_event_date
       FROM ", quote_identifier(non_inpatient_stage_table), "
       ORDER BY source_table"
    )
  )

  array_validity <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         (SELECT COUNT(*) FROM ", quote_identifier(inpatient_stage_table), "
          WHERE secondary_diagnosis_codes IS NOT NULL
            AND TRIM(secondary_diagnosis_codes) <> ''
            AND JSON_ARRAY_LENGTH(
              secondary_diagnosis_codes,
              TRUE
            ) IS NULL)::BIGINT AS invalid_secondary_diagnosis_arrays,
         (SELECT COUNT(*) FROM ", quote_identifier(inpatient_stage_table), "
          WHERE cpt_hcpcs_codes IS NOT NULL
            AND TRIM(cpt_hcpcs_codes) <> ''
            AND JSON_ARRAY_LENGTH(
              cpt_hcpcs_codes,
              TRUE
            ) IS NULL)::BIGINT AS invalid_cpt_hcpcs_arrays,
         (SELECT COUNT(*) FROM ", quote_identifier(non_inpatient_stage_table), "
          WHERE diagnosis_codes IS NOT NULL
            AND TRIM(diagnosis_codes) <> ''
            AND JSON_ARRAY_LENGTH(
              diagnosis_codes,
              TRUE
            ) IS NULL)::BIGINT AS invalid_diagnosis_arrays"
    )
  )

  if (sum(unlist(array_validity), na.rm = TRUE) > 0) {
    stop(
      "Invalid JSON-style arrays were found in ",
      analysis_year,
      " restricted event data."
    )
  }

  array_maxima <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(
              secondary_diagnosis_codes,
              TRUE
            ))
            FROM ", quote_identifier(inpatient_stage_table), "),
           0
         )::INTEGER AS max_inpatient_diagnosis_array,
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(diagnosis_codes, TRUE))
            FROM ", quote_identifier(non_inpatient_stage_table), "),
           0
         )::INTEGER AS max_non_inpatient_diagnosis_array,
         COALESCE(
           (SELECT MAX(JSON_ARRAY_LENGTH(cpt_hcpcs_codes, TRUE))
            FROM ", quote_identifier(inpatient_stage_table), "),
           0
         )::INTEGER AS max_inpatient_procedure_array"
    )
  )

  max_diagnosis_array_length <- max(
    array_maxima$max_inpatient_diagnosis_array[[1]],
    array_maxima$max_non_inpatient_diagnosis_array[[1]]
  )
  max_procedure_array_length <-
    array_maxima$max_inpatient_procedure_array[[1]]

  message(
    analysis_year,
    " maximum diagnosis array length: ",
    max_diagnosis_array_length
  )
  message(
    analysis_year,
    " maximum inpatient CPT/HCPCS array length: ",
    max_procedure_array_length
  )

  position_sql <- paste0(
    "CREATE TEMP TABLE ", quote_identifier(diagnosis_position_table), " AS
     WITH RECURSIVE positions(array_index) AS (
       SELECT 0
       WHERE ", max_diagnosis_array_length, " > 0
       UNION ALL
       SELECT array_index + 1
       FROM positions
       WHERE array_index + 1 < ", max_diagnosis_array_length, "
     )
     SELECT array_index
     FROM positions;

     CREATE TEMP TABLE ", quote_identifier(procedure_position_table), " AS
     WITH RECURSIVE positions(array_index) AS (
       SELECT 0
       WHERE ", max_procedure_array_length, " > 0
       UNION ALL
       SELECT array_index + 1
       FROM positions
       WHERE array_index + 1 < ", max_procedure_array_length, "
     )
     SELECT array_index
     FROM positions;"
  )

  DatabaseConnector::executeSql(
    con,
    position_sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  array_position_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         ", max_diagnosis_array_length,
      "::INTEGER AS observed_max_diagnosis_array_length,
         (SELECT COUNT(*) FROM ", quote_identifier(diagnosis_position_table),
      ")::INTEGER AS generated_diagnosis_positions,
         ", max_procedure_array_length,
      "::INTEGER AS observed_max_procedure_array_length,
         (SELECT COUNT(*) FROM ", quote_identifier(procedure_position_table),
      ")::INTEGER AS generated_procedure_positions"
    )
  )

  if (
    array_position_qa$observed_max_diagnosis_array_length[[1]] !=
      array_position_qa$generated_diagnosis_positions[[1]] ||
      array_position_qa$observed_max_procedure_array_length[[1]] !=
        array_position_qa$generated_procedure_positions[[1]]
  ) {
    stop(
      "Dynamic array-position generation did not cover the observed ",
      analysis_year,
      " maximum."
    )
  }

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DELETE FROM ", diagnosis_matches_identifier, "
       WHERE analysis_year = ", analysis_year, ";
       DELETE FROM ", procedure_matches_identifier, "
       WHERE analysis_year = ", analysis_year, ";"
    ),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  diagnosis_sql <- paste0(
    "INSERT INTO ", diagnosis_matches_identifier, " (
       patid,
       analysis_year,
       diagnosis_date,
       claim_setting,
       diagnosis_source,
       diagnosis_code,
       code_system,
       metric,
       feature_id,
       feature_name,
       match_value,
       range_start,
       range_end,
       range_end_inclusive,
       match_type,
       lookup_version
     )
     WITH raw_diagnoses AS (
       SELECT
         patid,
         analysis_year,
         event_date AS diagnosis_date,
         'inpatient'::VARCHAR(40) AS claim_setting,
         'inpatient_admission'::VARCHAR(40) AS diagnosis_source,
         admission_diagnosis_code AS diagnosis_code_raw
       FROM ", quote_identifier(inpatient_stage_table), "
       WHERE admission_diagnosis_code IS NOT NULL

       UNION ALL

       SELECT
         patid,
         analysis_year,
         event_date AS diagnosis_date,
         'inpatient'::VARCHAR(40) AS claim_setting,
         'inpatient_primary'::VARCHAR(40) AS diagnosis_source,
         primary_diagnosis_code AS diagnosis_code_raw
       FROM ", quote_identifier(inpatient_stage_table), "
       WHERE primary_diagnosis_code IS NOT NULL

       UNION ALL

       SELECT
         i.patid,
         i.analysis_year,
         i.event_date AS diagnosis_date,
         'inpatient'::VARCHAR(40) AS claim_setting,
         'inpatient_secondary'::VARCHAR(40) AS diagnosis_source,
         JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
           i.secondary_diagnosis_codes,
           p.array_index,
           TRUE
         ) AS diagnosis_code_raw
       FROM ", quote_identifier(inpatient_stage_table), " i
       INNER JOIN ", quote_identifier(diagnosis_position_table), " p
         ON p.array_index < JSON_ARRAY_LENGTH(
           i.secondary_diagnosis_codes,
           TRUE
         )
       WHERE ", array_prefix_condition(
         "i.secondary_diagnosis_codes",
         diagnosis_prefix_table,
         diagnosis_prefilter_enabled
       ), "

       UNION ALL

       SELECT
         n.patid,
         n.analysis_year,
         n.event_date AS diagnosis_date,
         'non_inpatient'::VARCHAR(40) AS claim_setting,
         'non_inpatient_all'::VARCHAR(40) AS diagnosis_source,
         JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
           n.diagnosis_codes,
           p.array_index,
           TRUE
         ) AS diagnosis_code_raw
       FROM ", quote_identifier(non_inpatient_stage_table), " n
       INNER JOIN ", quote_identifier(diagnosis_position_table), " p
         ON p.array_index < JSON_ARRAY_LENGTH(n.diagnosis_codes, TRUE)
       WHERE ", array_prefix_condition(
         "n.diagnosis_codes",
         diagnosis_prefix_table,
         diagnosis_prefilter_enabled
       ), "
     ),
     normalized_diagnoses AS (
       SELECT
         patid,
         analysis_year,
         diagnosis_date,
         claim_setting,
         diagnosis_source,
         UPPER(
           REGEXP_REPLACE(
             TRIM(diagnosis_code_raw),
             '[^A-Za-z0-9]',
             ''
           )
         ) AS diagnosis_code
       FROM raw_diagnoses
       WHERE diagnosis_code_raw IS NOT NULL
         AND TRIM(diagnosis_code_raw) <> ''
     )
     SELECT DISTINCT
       n.patid,
       n.analysis_year,
       n.diagnosis_date,
       n.claim_setting,
       n.diagnosis_source,
       n.diagnosis_code,
       l.code_system,
       l.metric,
       l.feature_id,
       l.feature_name,
       l.match_value,
       l.range_start,
       l.range_end,
       l.range_end_inclusive,
       l.match_type,
       l.lookup_version
     FROM normalized_diagnoses n
     INNER JOIN ", quote_identifier(diagnosis_lookup_stage_table), " l
       ON (
         (
           l.match_type = 'exact'
           AND n.diagnosis_code = l.match_value
         )
         OR (
           l.match_type = 'prefix'
           AND LEFT(n.diagnosis_code, LENGTH(l.match_value)) = l.match_value
         )
         OR (
           l.match_type = 'range'
           AND n.diagnosis_code >= l.range_start
           AND (
             (
               COALESCE(UPPER(l.range_end_inclusive), 'TRUE') = 'FALSE'
               AND n.diagnosis_code < l.range_end
             )
             OR (
               COALESCE(UPPER(l.range_end_inclusive), 'TRUE') <> 'FALSE'
               AND n.diagnosis_code <= l.range_end
             )
           )
         )
       )
     WHERE n.diagnosis_code IS NOT NULL
       AND n.diagnosis_code <> ''
       AND n.diagnosis_code ~ '^[A-Z0-9]+$';"
  )

  message("Appending ", analysis_year, " diagnosis feature matches.")
  DatabaseConnector::executeSql(con, diagnosis_sql)

  procedure_sql <- paste0(
    "INSERT INTO ", procedure_matches_identifier, " (
       patid,
       analysis_year,
       procedure_date,
       procedure_source,
       procedure_code,
       metric,
       feature_id,
       feature_name,
       range_start,
       range_end,
       match_type,
       lookup_version
     )
     WITH raw_procedures AS (
       SELECT
         i.patid,
         i.analysis_year,
         i.event_date AS procedure_date,
         'inpatient_cpt_hcpcs'::VARCHAR(40) AS procedure_source,
         JSON_EXTRACT_ARRAY_ELEMENT_TEXT(
           i.cpt_hcpcs_codes,
           p.array_index,
           TRUE
         ) AS procedure_code_raw
       FROM ", quote_identifier(inpatient_stage_table), " i
       INNER JOIN ", quote_identifier(procedure_position_table), " p
         ON p.array_index < JSON_ARRAY_LENGTH(i.cpt_hcpcs_codes, TRUE)
       WHERE ", array_prefix_condition(
         "i.cpt_hcpcs_codes",
         procedure_prefix_table,
         procedure_prefilter_enabled
       ), "

       UNION ALL

       SELECT
         patid,
         analysis_year,
         event_date AS procedure_date,
         'non_inpatient_procedure'::VARCHAR(40) AS procedure_source,
         procedure_code AS procedure_code_raw
       FROM ", quote_identifier(non_inpatient_stage_table), "
       WHERE procedure_code IS NOT NULL
     ),
     normalized_procedures AS (
       SELECT
         patid,
         analysis_year,
         procedure_date,
         procedure_source,
         CASE
           WHEN UPPER(TRIM(procedure_code_raw)) ~
             '^[A-Z0-9]{5}([-[:space:]].*)?$'
           THEN LEFT(UPPER(TRIM(procedure_code_raw)), 5)
           ELSE UPPER(
             REGEXP_REPLACE(
               TRIM(procedure_code_raw),
               '[^A-Za-z0-9]',
               ''
             )
           )
         END AS procedure_code
       FROM raw_procedures
       WHERE procedure_code_raw IS NOT NULL
         AND TRIM(procedure_code_raw) <> ''
     )
     SELECT DISTINCT
       p.patid,
       p.analysis_year,
       p.procedure_date,
       p.procedure_source,
       p.procedure_code,
       l.metric,
       l.feature_id,
       l.feature_name,
       l.range_start,
       l.range_end,
       l.match_type,
       l.lookup_version
     FROM normalized_procedures p
     INNER JOIN ", quote_identifier(procedure_lookup_stage_table), " l
       ON p.procedure_code >= l.range_start
      AND p.procedure_code <= l.range_end
     WHERE p.procedure_code ~ '^[A-Z0-9]{5}$'
       AND p.procedure_code ~ '[0-9]$';"
  )

  message("Appending ", analysis_year, " procedure feature matches.")
  DatabaseConnector::executeSql(con, procedure_sql)

  print_query(
    paste0("Completed ", analysis_year, " aggregate matched-event counts."),
    paste0(
      "SELECT
         'diagnosis' AS match_layer,
         metric,
         match_type,
         COUNT(*)::BIGINT AS matched_rows
       FROM ", diagnosis_matches_identifier, "
       WHERE analysis_year = ", analysis_year, "
       GROUP BY metric, match_type
       UNION ALL
       SELECT
         'procedure' AS match_layer,
         metric,
         match_type,
         COUNT(*)::BIGINT AS matched_rows
       FROM ", procedure_matches_identifier, "
       WHERE analysis_year = ", analysis_year, "
       GROUP BY metric, match_type
       ORDER BY match_layer, metric, match_type"
    )
  )

  print_query(
    paste0("Completed ", analysis_year, " matched-event source counts."),
    paste0(
      "SELECT
         'diagnosis' AS match_layer,
         claim_setting AS source_group,
         diagnosis_source AS source_detail,
         COUNT(*)::BIGINT AS matched_rows
       FROM ", diagnosis_matches_identifier, "
       WHERE analysis_year = ", analysis_year, "
       GROUP BY claim_setting, diagnosis_source
       UNION ALL
       SELECT
         'procedure' AS match_layer,
         'procedure' AS source_group,
         procedure_source AS source_detail,
         COUNT(*)::BIGINT AS matched_rows
       FROM ", procedure_matches_identifier, "
       WHERE analysis_year = ", analysis_year, "
       GROUP BY procedure_source
       ORDER BY match_layer, source_group, source_detail"
    )
  )

  drop_temporary_tables()
}

# ---- Validate final configured outputs ----
output_qa <- print_query(
  "Checking matched-event membership, duplicates, and code formats.",
  paste0(
    "SELECT
       (SELECT COUNT(*)
        FROM ", diagnosis_matches_identifier, " d
        LEFT JOIN ", ids_identifier, " ids
          ON d.patid = ids.patid
         AND d.analysis_year = ids.analysis_year
        WHERE d.analysis_year IN (", sql_values(analysis_years), ")
          AND ids.patid IS NULL)::BIGINT AS diagnosis_ids_outside_cohort,
       (SELECT COUNT(*)
        FROM ", procedure_matches_identifier, " p
        LEFT JOIN ", ids_identifier, " ids
          ON p.patid = ids.patid
         AND p.analysis_year = ids.analysis_year
        WHERE p.analysis_year IN (", sql_values(analysis_years), ")
          AND ids.patid IS NULL)::BIGINT AS procedure_ids_outside_cohort,
       (SELECT COUNT(*) - COUNT(DISTINCT
          patid || '|' ||
          analysis_year::VARCHAR || '|' ||
          diagnosis_date::VARCHAR || '|' ||
          claim_setting || '|' ||
          diagnosis_source || '|' ||
          diagnosis_code || '|' ||
          metric || '|' ||
          feature_id || '|' ||
          COALESCE(match_value, '') || '|' ||
          COALESCE(range_start, '') || '|' ||
          COALESCE(range_end, '')
        )
        FROM ", diagnosis_matches_identifier, "
        WHERE analysis_year IN (", sql_values(analysis_years), ")
       )::BIGINT AS duplicate_diagnosis_match_rows,
       (SELECT COUNT(*) - COUNT(DISTINCT
          patid || '|' ||
          analysis_year::VARCHAR || '|' ||
          procedure_date::VARCHAR || '|' ||
          procedure_source || '|' ||
          procedure_code || '|' ||
          metric || '|' ||
          feature_id || '|' ||
          range_start || '|' ||
          range_end
        )
        FROM ", procedure_matches_identifier, "
        WHERE analysis_year IN (", sql_values(analysis_years), ")
       )::BIGINT AS duplicate_procedure_match_rows,
       (SELECT COUNT(*)
        FROM ", diagnosis_matches_identifier, "
        WHERE analysis_year IN (", sql_values(analysis_years), ")
          AND (
            diagnosis_code IS NULL
            OR diagnosis_code = ''
            OR diagnosis_code !~ '^[A-Z0-9]+$'
            OR code_system <> 'ICD10CM'
          )
       )::BIGINT AS invalid_diagnosis_match_rows,
       (SELECT COUNT(*)
        FROM ", procedure_matches_identifier, "
        WHERE analysis_year IN (", sql_values(analysis_years), ")
          AND (
            procedure_code IS NULL
            OR procedure_code !~ '^[A-Z0-9]{5}$'
            OR procedure_code !~ '[0-9]$'
          )
       )::BIGINT AS invalid_procedure_match_rows"
  )
)

if (any(unlist(output_qa) != 0)) {
  stop("One or more matched-event QA checks failed.")
}

message(
  clinical_metric_config$workflow_label,
  " shared clinical metric matching complete. Tables updated in ",
  write_schema,
  ": ",
  paste(
    c(ids_table, diagnosis_matches_table, procedure_matches_table),
    collapse = ", "
  ),
  "."
)
