library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto optimized annual clinical metrics helpers
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Provide shared configuration, connection, SQL quoting, CSV loading, and table
# validation helpers for the optimized 6.x clinical-metrics pipeline. This file
# is sourced by the production 6.x scripts and does not create Redshift tables
# on its own.

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

default_optimized_clinical_metrics_config <- list(
  analysis_years = 2016L,
  id_years = 2016L,
  eligibility_table = "1_annual_eligible_cohort",
  ids_table = "2_annual_metric_ids",
  inpatient_stage_table = "2_annual_inpatient_event_stage",
  non_inpatient_stage_table = "2_annual_non_inpatient_event_stage",
  inpatient_candidate_table = "2_annual_inpatient_candidate_event_stage",
  non_inpatient_candidate_table = "2_annual_non_inpatient_candidate_event_stage",
  diagnosis_presence_table = "2_annual_diagnosis_code_presence",
  procedure_presence_table = "2_annual_procedure_code_presence",
  hiv_evidence_table = "2_annual_hiv_diagnosis_evidence",
  cfi_feature_matches_table = "2_annual_cfi_feature_matches",
  ccw_feature_matches_table = "2_annual_ccw_condition_matches",
  gagne_feature_matches_table = "2_annual_gagne_group_matches",
  cfi_scores_table = "6_annual_cfi_scores",
  ccw_conditions_long_table = "6_annual_ccw_conditions_long",
  ccw_condition_indicators_table = "6_annual_ccw_condition_indicators",
  ccw_group_counts_table = "6_annual_ccw_group_counts",
  gagne_scores_table = "6_annual_gagne_scores",
  hiv_status_table = "6_annual_hiv_status",
  final_table = "6_annual_clinical_metrics_shared",
  lookup_dir = file.path(getwd(), "Documents", "Clinical Metric Look Up Tables"),
  output_dir = file.path(getwd(), "Outputs"),
  workflow_label = "optimized annual clinical metrics",
  refresh_metric_ids = TRUE,
  use_candidate_event_stage = TRUE,
  diagnosis_candidate_prefix_length = 3L,
  procedure_candidate_prefix_length = 1L,
  array_code_limit = 25L,
  event_start_date = NULL,
  event_end_date = NULL,
  run_cfi_2016_parity_check = TRUE,
  model_intercept = 0.10288
)

get_optimized_clinical_metrics_config <- function() {
  config <- utils::modifyList(
    default_optimized_clinical_metrics_config,
    getOption("frailty.optimized_clinical_metrics.config", list())
  )

  config$analysis_years <- sort(unique(as.integer(config$analysis_years)))
  config$id_years <- sort(unique(as.integer(config$id_years)))

  if (
    length(config$analysis_years) == 0L ||
      any(is.na(config$analysis_years)) ||
      any(config$analysis_years < 2016L | config$analysis_years > 2025L)
  ) {
    stop("analysis_years must contain years from 2016 through 2025.")
  }

  if (
    length(config$id_years) == 0L ||
      any(is.na(config$id_years)) ||
      any(config$id_years < 2016L | config$id_years > 2025L) ||
      length(setdiff(config$analysis_years, config$id_years)) > 0L
  ) {
    stop(
      "id_years must contain all processing years and remain within 2016-2025."
    )
  }

  if (xor(is.null(config$event_start_date), is.null(config$event_end_date))) {
    stop(
      "event_start_date and event_end_date must both be NULL or both be set."
    )
  }

  if (!is.null(config$event_start_date)) {
    config$event_start_date <- as.Date(config$event_start_date)
    config$event_end_date <- as.Date(config$event_end_date)
    if (
      is.na(config$event_start_date) ||
        is.na(config$event_end_date) ||
        config$event_start_date >= config$event_end_date
    ) {
      stop("event_start_date must be earlier than event_end_date.")
    }
  }

  config$use_candidate_event_stage <- isTRUE(config$use_candidate_event_stage)
  config$run_cfi_2016_parity_check <- isTRUE(config$run_cfi_2016_parity_check)
  config$diagnosis_candidate_prefix_length <- as.integer(
    config$diagnosis_candidate_prefix_length
  )
  config$procedure_candidate_prefix_length <- as.integer(
    config$procedure_candidate_prefix_length
  )
  config$array_code_limit <- as.integer(config$array_code_limit)

  if (
    length(config$diagnosis_candidate_prefix_length) != 1L ||
      is.na(config$diagnosis_candidate_prefix_length) ||
      config$diagnosis_candidate_prefix_length < 1L
  ) {
    stop("diagnosis_candidate_prefix_length must be a positive integer.")
  }

  if (
    length(config$procedure_candidate_prefix_length) != 1L ||
      is.na(config$procedure_candidate_prefix_length) ||
      config$procedure_candidate_prefix_length < 1L
  ) {
    stop("procedure_candidate_prefix_length must be a positive integer.")
  }

  if (
    length(config$array_code_limit) != 1L ||
      is.na(config$array_code_limit) ||
      config$array_code_limit < 1L
  ) {
    stop("array_code_limit must be a positive integer.")
  }

  config
}

connect_komodo <- function() {
  con <- ohdsilab_connect(
    username = keyring::key_get("db_username"),
    password = keyring::key_get("db_password")
  )

  options(con.default.value = con)
  options(schema.default.value = komodo_schema)
  options(write_schema.default.value = write_schema)
  con
}

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

normalize_code <- function(x) {
  gsub("[^A-Z0-9]", "", toupper(trimws(as.character(x))))
}

normalize_code_system <- function(x) {
  normalize_code(x)
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
}

print_query <- function(con, label, sql) {
  message(label)
  result <- DBI::dbGetQuery(con, sql)
  print(result)
  invisible(result)
}

table_exists <- function(con, schema, table) {
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

table_has_columns <- function(con, schema, table, columns) {
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
      paste(missing_columns, collapse = ", ")
    )
  }
}

execute_insert_batches <- function(
  con,
  table_identifier,
  columns,
  data,
  numeric_columns = character(),
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
                } else if (column %in% numeric_columns) {
                  value
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

event_window_for_year <- function(config, analysis_year) {
  year_start <- as.Date(paste0(analysis_year, "-01-01"))
  year_end <- as.Date(paste0(analysis_year + 1L, "-01-01"))

  if (is.null(config$event_start_date)) {
    return(list(start = as.character(year_start), end = as.character(year_end)))
  }

  if (config$event_start_date < year_start || config$event_end_date > year_end) {
    stop(
      "Configured event date window must stay within analysis year ",
      analysis_year,
      "."
    )
  }

  list(
    start = as.character(config$event_start_date),
    end = as.character(config$event_end_date)
  )
}
