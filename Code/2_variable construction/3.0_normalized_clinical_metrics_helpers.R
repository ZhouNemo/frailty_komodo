library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto normalized annual clinical metrics
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-08-06
#
# ---- Purpose ----
# Provide shared configuration, connection, SQL quoting, CSV loading, and table
# validation helpers for the normalized 3.x clinical-metrics pipeline. The
# pipeline reads the cleaned Komodo normalized diagnosis and procedure event
# tables instead of raw inpatient and non-inpatient event tables. This helper
# file does not create Redshift tables on its own.

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

redshift_server <- paste0(
  "ohdsi-lab-redshift-cluster-prod.clsyktjhufn7.us-east-1.redshift.amazonaws.com",
  "/ohdsi_lab"
)
redshift_port <- 5439L
komodo_schema <- "komodo_202606"
write_schema <- paste0("work_", keyring::key_get("db_username"))

default_normalized_clinical_metrics_config <- list(
  analysis_years = 2022L,
  id_years = 2022L,
  eligibility_table = "1_2022_komodo_medicare_comparison_cohort",
  ids_table = "2_2022_komodo_medicare_comparison_metric_ids",
  normalized_dx_table = "normalized_dx_events",
  normalized_procedure_table = "normalized_procedure_events",
  procedure_presence_table = "2_2022_komodo_medicare_comparison_procedure_code_presence",
  hiv_evidence_table = "2_2022_komodo_medicare_comparison_hiv_diagnosis_evidence",
  cfi_feature_matches_table = "2_2022_komodo_medicare_comparison_cfi_feature_matches",
  ccw_feature_matches_table = "2_2022_komodo_medicare_comparison_ccw_condition_matches",
  ccw_algorithm = "ccw56_presence",
  gagne_feature_matches_table = "2_2022_komodo_medicare_comparison_gagne_group_matches",
  candidate_stage_table = "2_2022_komodo_medicare_comparison_dx_candidate_stage",
  candidate_stage_manifest_table = "2_2022_komodo_medicare_comparison_dx_candidate_stage_manifest",
  reuse_candidate_stage = FALSE,
  cfi_scores_table = "6_2022_komodo_medicare_comparison_cfi_scores",
  ccw_conditions_long_table = "6_2022_komodo_medicare_comparison_ccw_conditions_long",
  ccw_condition_indicators_table = "6_2022_komodo_medicare_comparison_ccw_condition_indicators",
  ccw_group_counts_table = "6_2022_komodo_medicare_comparison_ccw_group_counts",
  gagne_scores_table = "6_2022_komodo_medicare_comparison_gagne_scores",
  hiv_status_table = "6_2022_komodo_medicare_comparison_hiv_status",
  hiv_non_inpatient_min_distinct_dates = 2L,
  final_table = "6_2022_komodo_medicare_comparison_clinical_metrics_shared",
  lookup_dir = file.path(getwd(), "Documents", "Clinical Metric Look Up Tables"),
  ccw30_lookup_dir = file.path(
    getwd(), "Documents", "Clinical Metric Look Up Tables", "CCW30"
  ),
  output_dir = file.path(getwd(), "Outputs"),
  workflow_label = "2022 Komodo Medicare comparison normalized clinical metrics",
  required_eligibility_columns = character(),
  refresh_metric_ids = TRUE,
  event_start_date = NULL,
  event_end_date = NULL,
  event_scan_chunk_by = "year",
  calculate_cfi = TRUE,
  reuse_cfi_scores = FALSE,
  run_cfi_2016_parity_check = FALSE,
  model_intercept = 0.10288
)

protected_normalized_clinical_metrics_tables <- c(
  "2_annual_metric_ids",
  "2_annual_procedure_code_presence",
  "2_annual_hiv_diagnosis_evidence",
  "2_annual_dx_candidate_stage",
  "2_annual_dx_candidate_stage_manifest",
  "2_annual_cfi_feature_matches",
  "2_annual_ccw_condition_matches",
  "2_annual_gagne_group_matches",
  "6_annual_cfi_scores",
  "6_annual_ccw_conditions_long",
  "6_annual_ccw_condition_indicators",
  "6_annual_ccw_group_counts",
  "6_annual_gagne_scores",
  "6_annual_hiv_status",
  "6_annual_clinical_metrics_shared",
  "6_annual_clinical_metrics_shared_2022"
)

validate_normalized_clinical_metrics_table_names <- function(config) {
  output_fields <- c(
    "ids_table",
    "procedure_presence_table",
    "hiv_evidence_table",
    "cfi_feature_matches_table",
    "ccw_feature_matches_table",
    "gagne_feature_matches_table",
    "candidate_stage_table",
    "candidate_stage_manifest_table",
    "cfi_scores_table",
    "ccw_conditions_long_table",
    "ccw_condition_indicators_table",
    "ccw_group_counts_table",
    "gagne_scores_table",
    "hiv_status_table",
    "final_table"
  )
  output_names <- unname(unlist(config[output_fields], use.names = FALSE))
  protected_outputs <- intersect(output_names, protected_normalized_clinical_metrics_tables)
  if (length(protected_outputs) > 0L) {
    stop(
      "Protected original normalized-metrics table name(s) were configured as outputs: ",
      paste(protected_outputs, collapse = ", "),
      ". Use the 2022 comparison-specific table names."
    )
  }
  if (anyDuplicated(output_names) > 0L) {
    duplicated_outputs <- unique(output_names[duplicated(output_names)])
    stop(
      "Normalized-metrics output table names must be unique: ",
      paste(duplicated_outputs, collapse = ", "), "."
    )
  }
  invisible(config)
}

get_normalized_clinical_metrics_config <- function() {
  config <- utils::modifyList(
    default_normalized_clinical_metrics_config,
    getOption("frailty.normalized_clinical_metrics.config", list())
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
    stop("id_years must contain all processing years and stay within 2016-2025.")
  }

  if (xor(is.null(config$event_start_date), is.null(config$event_end_date))) {
    stop("event_start_date and event_end_date must both be NULL or both be set.")
  }

  if (!is.null(config$event_start_date)) {
    config$event_start_date <- as.Date(config$event_start_date)
    config$event_end_date <- as.Date(config$event_end_date)
    if (
      is.na(config$event_start_date) ||
        is.na(config$event_end_date) ||
        config$event_start_date > config$event_end_date
    ) {
      stop("event_start_date must be on or before event_end_date.")
    }
  }

  config$hiv_non_inpatient_min_distinct_dates <- as.integer(
    config$hiv_non_inpatient_min_distinct_dates
  )
  if (
    length(config$hiv_non_inpatient_min_distinct_dates) != 1L ||
      is.na(config$hiv_non_inpatient_min_distinct_dates) ||
      config$hiv_non_inpatient_min_distinct_dates < 1L
  ) {
    stop(
      "hiv_non_inpatient_min_distinct_dates must be a single positive integer."
    )
  }

  config$event_scan_chunk_by <- tolower(as.character(config$event_scan_chunk_by))
  if (
    length(config$event_scan_chunk_by) != 1L ||
      !config$event_scan_chunk_by %in% c("all", "year", "quarter", "month")
  ) {
    stop("event_scan_chunk_by must be one of: all, year, quarter, month.")
  }

  # For a single-year run, default to static literal scan bounds so the external
  # diagnosis/procedure scans stay pushdown-prunable. Multi-year runs keep the
  # correlated per-patient-year window, and event_window_sql() adds literal
  # outer bounds from the selected analysis years for Spectrum pruning.
  if (is.null(config$event_start_date) && length(config$analysis_years) == 1L) {
    single_year <- config$analysis_years[[1]]
    config$event_start_date <- as.Date(sprintf("%04d-01-01", single_year))
    config$event_end_date <- as.Date(sprintf("%04d-12-31", single_year))
  }

  config$calculate_cfi <- isTRUE(config$calculate_cfi)
  config$reuse_cfi_scores <- isTRUE(config$reuse_cfi_scores)
  if (!config$calculate_cfi && !config$reuse_cfi_scores) {
    stop(
      "Set calculate_cfi = TRUE or reuse_cfi_scores = TRUE so the final ",
      "clinical-metrics table has an explicit CFI source."
    )
  }
  config$run_cfi_2016_parity_check <- isTRUE(config$run_cfi_2016_parity_check)
  config$ccw_algorithm <- tolower(trimws(as.character(config$ccw_algorithm)))
  if (
    length(config$ccw_algorithm) != 1L ||
      is.na(config$ccw_algorithm) ||
      !config$ccw_algorithm %in% c("ccw56_presence", "ccw30_normalized")
  ) {
    stop("ccw_algorithm must be 'ccw56_presence' or 'ccw30_normalized'.")
  }
  config$ccw30_lookup_dir <- as.character(config$ccw30_lookup_dir)
  if (
    length(config$ccw30_lookup_dir) != 1L ||
      is.na(config$ccw30_lookup_dir) ||
      !nzchar(config$ccw30_lookup_dir)
  ) {
    stop("ccw30_lookup_dir must be one nonempty directory path.")
  }
  config$required_eligibility_columns <- unique(tolower(as.character(
    config$required_eligibility_columns
  )))
  if (any(is.na(config$required_eligibility_columns) | !nzchar(config$required_eligibility_columns))) {
    stop("required_eligibility_columns must contain only nonempty column names.")
  }
  validate_normalized_clinical_metrics_table_names(config)
  config
}

connect_komodo <- function() {
  con <- DatabaseConnector::connect(
    dbms = "redshift",
    server = redshift_server,
    port = redshift_port,
    user = keyring::key_get("db_username"),
    password = keyring::key_get("db_password")
  )

  options(con.default.value = con)
  options(schema.default.value = komodo_schema)
  options(write_schema.default.value = write_schema)
  con
}

disconnect_komodo <- function(con) {
  if (!is.null(con)) {
    try(DatabaseConnector::disconnect(con), silent = TRUE)
  }
  invisible(NULL)
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

# Deterministic content fingerprint used to decide whether a banked candidate
# stage still matches the current run (candidate prefix set + lookup versions).
# Uses tools::md5sum (base R) so no extra package dependency is introduced.
string_fingerprint <- function(values) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(paste(values, collapse = "\n"), tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
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
  catalog_result <- tryCatch(
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT 1 AS table_found
         FROM information_schema.tables
         WHERE table_schema = ", sql_string(schema), "
           AND table_name = ", sql_string(table), "
         LIMIT 1"
      )
    ),
    error = function(e) NULL
  )
  if (!is.null(catalog_result) && nrow(catalog_result) > 0L) {
    return(TRUE)
  }

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

get_columns <- function(con, schema, table) {
  table_identifier <- qualified_identifier(schema, table)
  catalog_columns <- tryCatch(
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT column_name
         FROM information_schema.columns
         WHERE table_schema = ", sql_string(schema), "
           AND table_name = ", sql_string(table)
      )
    ),
    error = function(e) NULL
  )

  if (!is.null(catalog_columns) && nrow(catalog_columns) > 0L) {
    return(tolower(catalog_columns$column_name))
  }

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

table_has_columns <- function(con, schema, table, columns) {
  available_columns <- get_columns(con, schema, table)

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
                  as.character(value)
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

# Emit the event-date window predicate for a diagnosis/procedure scan. The event
# date column is referenced bare (no CAST wrapper) so Redshift Spectrum can use
# Parquet row-group min/max stats to skip data; wrapping the column in a
# function defeats that pruning. The upper bound is half-open (< end + 1 day) so
# intraday timestamps on the final day are still captured, which is equivalent
# to the previous CAST-to-DATE inclusive BETWEEN but keeps the column prunable.
# `event_column` is the fully qualified event-date column (e.g. "dx.event_date");
# `alias` is the patient-year denominator alias used for the correlated window.
event_window_sql <- function(config, alias = "ids", event_column = "event_date") {
  if (is.null(config$event_start_date)) {
    return(paste0(
      event_literal_window_sql(
        sprintf("%04d-01-01", min(config$analysis_years)),
        sprintf("%04d-12-31", max(config$analysis_years)),
        event_column
      ),
      " AND ", event_column, " >= ", alias, ".analysis_start_date",
      " AND ", event_column, " < ", alias, ".analysis_end_date + 1"
    ))
  }

  event_literal_window_sql(config$event_start_date, config$event_end_date, event_column)
}

event_literal_window_sql <- function(start_date, end_date, event_column = "event_date") {
  paste0(
    event_column, " >= ", sql_string(as.character(start_date)), "::DATE",
    " AND ", event_column, " < (", sql_string(as.character(end_date)), "::DATE + 1)"
  )
}

event_chunk_window_sql <- function(
  config,
  alias = "ids",
  event_column = "event_date",
  chunk_start_date,
  chunk_end_date
) {
  literal_window <- event_literal_window_sql(
    chunk_start_date,
    chunk_end_date,
    event_column
  )
  if (!is.null(config$event_start_date)) {
    return(literal_window)
  }

  paste0(
    literal_window,
    " AND ", event_column, " >= ", alias, ".analysis_start_date",
    " AND ", event_column, " < ", alias, ".analysis_end_date + 1"
  )
}

event_scan_chunks <- function(config, chunk_by = config$event_scan_chunk_by) {
  chunk_by <- tolower(as.character(chunk_by))
  if (!chunk_by %in% c("all", "year", "quarter", "month")) {
    stop("chunk_by must be one of: all, year, quarter, month.")
  }

  start_date <- if (is.null(config$event_start_date)) {
    as.Date(sprintf("%04d-01-01", min(config$analysis_years)))
  } else {
    config$event_start_date
  }
  end_date <- if (is.null(config$event_end_date)) {
    as.Date(sprintf("%04d-12-31", max(config$analysis_years)))
  } else {
    config$event_end_date
  }

  if (chunk_by == "all") {
    chunk_starts <- start_date
    chunk_ends <- end_date
    return(data.frame(
      chunk_id = 1L,
      chunk_by = chunk_by,
      chunk_start_date = chunk_starts,
      chunk_end_date = chunk_ends
    ))
  } else if (chunk_by == "year") {
    first_period <- as.Date(format(start_date, "%Y-01-01"))
    period_starts <- seq(first_period, end_date, by = "year")
    next_period_starts <- seq(
      first_period,
      by = "year",
      length.out = length(period_starts) + 1L
    )[-1L]
  } else if (chunk_by == "quarter") {
    start_month <- as.integer(format(start_date, "%m"))
    quarter_start_month <- ((start_month - 1L) %/% 3L) * 3L + 1L
    first_period <- as.Date(sprintf(
      "%04d-%02d-01",
      as.integer(format(start_date, "%Y")),
      quarter_start_month
    ))
    period_starts <- seq(first_period, end_date, by = "3 months")
    next_period_starts <- seq(
      first_period,
      by = "3 months",
      length.out = length(period_starts) + 1L
    )[-1L]
  } else {
    first_period <- as.Date(format(start_date, "%Y-%m-01"))
    period_starts <- seq(first_period, end_date, by = "month")
    next_period_starts <- seq(
      first_period,
      by = "month",
      length.out = length(period_starts) + 1L
    )[-1L]
  }

  chunk_starts <- pmax(period_starts, start_date)
  chunk_ends <- pmin(next_period_starts - 1L, end_date)

  data.frame(
    chunk_id = seq_along(chunk_starts),
    chunk_by = chunk_by,
    chunk_start_date = chunk_starts,
    chunk_end_date = chunk_ends
  )
}

# Run a (possibly multi-statement) SQL batch with bounded retries and linear
# backoff. Large Spectrum scans of the external normalized event tables can fail
# with transient "Spectrum Scan Error: Retries exceeded" / "No more data to
# read" errors; retrying the same idempotent DELETE+INSERT batch usually
# succeeds. The batch must be self-idempotent or land in a later-deduplicated
# temp build table so a retry does not create duplicate persistent rows. When
# reconnect_fn is supplied, it is called between attempts; only use that for SQL
# units that do not depend on session TEMP tables.
execute_sql_with_retry <- function(
  con,
  sql,
  attempts = 3L,
  backoff_seconds = 30L,
  label = "SQL batch",
  reconnect_fn = NULL
) {
  attempts <- max(1L, as.integer(attempts))
  for (attempt in seq_len(attempts)) {
    outcome <- tryCatch(
      {
        DatabaseConnector::executeSql(
          con,
          sql,
          progressBar = FALSE,
          reportOverallTime = FALSE
        )
        TRUE
      },
      error = function(e) conditionMessage(e)
    )

    if (isTRUE(outcome)) {
      return(invisible(con))
    }

    message(
      format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
      label, " attempt ", attempt, " of ", attempts, " failed: ", outcome
    )
    if (attempt < attempts) {
      wait_seconds <- backoff_seconds * attempt
      message(
        format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
        "Retrying ", label, " in ", wait_seconds, " seconds."
      )
      Sys.sleep(wait_seconds)
      if (!is.null(reconnect_fn)) {
        message(
          format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
          "Reconnecting before retrying ", label, "."
        )
        disconnect_komodo(con)
        con <- reconnect_fn()
      }
    }
  }

  stop(label, " failed after ", attempts, " attempts.", call. = FALSE)
}


