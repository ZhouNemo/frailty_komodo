source("Code/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto annual polypharmacy
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-03
#
# ---- Purpose ----
# Provide shared configuration, date-window helpers, transaction-filter guards,
# and local file paths for the 5.x annual polypharmacy pipeline. The pipeline
# reuses `2_annual_metric_ids`, reads `komodo_ext.pharmacy_events`, stages a
# versioned NDC11-to-ATC crosswalk, and builds annual patient-year
# polypharmacy metrics. This helper file does not create Redshift tables on its
# own.

default_annual_polypharmacy_config <- list(
  analysis_years = 2016L,
  ids_table = "2_annual_metric_ids",
  pharmacy_table = "pharmacy_events",
  fills_table = "2_polypharmacy_pharmacy_fills",
  fill_extraction_qa_table = "2_polypharmacy_fill_extraction_qa",
  unique_ndc_table = "2_polypharmacy_unique_ndc11",
  crosswalk_table = "2_polypharmacy_ndc11_atc_crosswalk",
  episodes_table = "2_annual_polypharmacy_exposure_episodes",
  final_table = "6_annual_polypharmacy_metrics",
  output_dir = file.path(getwd(), "Outputs"),
  workflow_label = "annual polypharmacy",
  fill_scan_chunk_by = "year",
  allow_prior_fill_carry_in = FALSE,
  carry_in_lookback_days = 365L,
  days_supply_max = NULL,
  transaction_result_keep = NULL,
  transaction_status_keep = NULL,
  transaction_source_type_keep = NULL,
  allow_unfiltered_transactions = FALSE,
  mapping_source = "n2c_rxnav",
  mapping_level = "atc4_to_atc3",
  mapping_version_date = "unversioned",
  n2c_commit_or_download_date = NA_character_,
  cache_file_name = NA_character_,
  unique_ndc_export_path = NULL,
  crosswalk_input_path = file.path(
    getwd(),
    "Outputs",
    "5.3_polypharmacy_ndc11_atc_crosswalk.csv"
  ),
  art_excluded = FALSE
)

get_annual_polypharmacy_config <- function() {
  config <- utils::modifyList(
    default_annual_polypharmacy_config,
    getOption("frailty.annual_polypharmacy.config", list())
  )

  config$analysis_years <- sort(unique(as.integer(config$analysis_years)))
  if (
    length(config$analysis_years) == 0L ||
      any(is.na(config$analysis_years)) ||
      any(config$analysis_years < 2016L | config$analysis_years > 2025L)
  ) {
    stop("analysis_years must contain years from 2016 through 2025.")
  }

  config$fill_scan_chunk_by <- tolower(as.character(config$fill_scan_chunk_by))
  if (
    length(config$fill_scan_chunk_by) != 1L ||
      !config$fill_scan_chunk_by %in% c("all", "year", "quarter", "month")
  ) {
    stop("fill_scan_chunk_by must be one of: all, year, quarter, month.")
  }

  config$allow_prior_fill_carry_in <- isTRUE(config$allow_prior_fill_carry_in)
  config$carry_in_lookback_days <- as.integer(config$carry_in_lookback_days)
  if (
    length(config$carry_in_lookback_days) != 1L ||
      is.na(config$carry_in_lookback_days) ||
      config$carry_in_lookback_days < 0L
  ) {
    stop("carry_in_lookback_days must be a nonnegative integer.")
  }

  if (!is.null(config$days_supply_max)) {
    config$days_supply_max <- as.integer(config$days_supply_max)
    if (
      length(config$days_supply_max) != 1L ||
        is.na(config$days_supply_max) ||
        config$days_supply_max <= 0L
    ) {
      stop("days_supply_max must be NULL or a positive integer.")
    }
  }

  if (isTRUE(config$art_excluded)) {
    stop(
      "ART exclusion is not implemented in the active general-population ",
      "polypharmacy workflow. Leave art_excluded = FALSE unless the protocol ",
      "is revised and an ART NDC list is added."
    )
  }

  config$transaction_result_keep <- normalize_optional_keep_values(
    config$transaction_result_keep
  )
  config$transaction_status_keep <- normalize_optional_keep_values(
    config$transaction_status_keep
  )
  config$transaction_source_type_keep <- normalize_optional_keep_values(
    config$transaction_source_type_keep
  )
  config$allow_unfiltered_transactions <- isTRUE(
    config$allow_unfiltered_transactions
  )

  if (!is.null(config$unique_ndc_export_path)) {
    config$unique_ndc_export_path <- as.character(config$unique_ndc_export_path)
  }
  config$crosswalk_input_path <- as.character(config$crosswalk_input_path)
  config$mapping_source <- as.character(config$mapping_source)
  config$mapping_level <- as.character(config$mapping_level)
  config$mapping_version_date <- as.character(config$mapping_version_date)
  config$n2c_commit_or_download_date <- as.character(
    config$n2c_commit_or_download_date
  )
  config$cache_file_name <- as.character(config$cache_file_name)

  config
}

polypharmacy_require_transaction_filter_decision <- function(config) {
  has_transaction_filter <- any(c(
    length(config$transaction_result_keep),
    length(config$transaction_status_keep),
    length(config$transaction_source_type_keep)
  ) > 0L)

  if (!has_transaction_filter && !isTRUE(config$allow_unfiltered_transactions)) {
    stop(
      "No reviewed pharmacy transaction filter is configured. Set one or more ",
      "of transaction_result_keep, transaction_status_keep, or ",
      "transaction_source_type_keep after reviewing aggregate transaction ",
      "values. For an exploratory unfiltered run only, set ",
      "allow_unfiltered_transactions = TRUE."
    )
  }

  invisible(TRUE)
}

normalize_optional_keep_values <- function(values) {
  if (is.null(values) || length(values) == 0L) {
    return(character())
  }
  values <- toupper(trimws(as.character(values)))
  values[!is.na(values) & values != ""]
}

polypharmacy_year_label <- function(config) {
  years <- config$analysis_years
  if (length(years) == 1L) {
    return(as.character(years[[1]]))
  }
  paste0(min(years), "_", max(years))
}

polypharmacy_default_unique_ndc_export_path <- function(config) {
  file.path(
    config$output_dir,
    paste0("5.2_polypharmacy_unique_ndc11_", polypharmacy_year_label(config), ".txt")
  )
}

polypharmacy_scan_bounds <- function(config) {
  start_date <- as.Date(sprintf("%04d-01-01", min(config$analysis_years)))
  end_date <- as.Date(sprintf("%04d-12-31", max(config$analysis_years)))
  if (isTRUE(config$allow_prior_fill_carry_in)) {
    start_date <- start_date - config$carry_in_lookback_days
  }
  list(start_date = start_date, end_date = end_date)
}

polypharmacy_fill_scan_chunks <- function(config) {
  bounds <- polypharmacy_scan_bounds(config)
  event_scan_chunks(list(
    analysis_years = config$analysis_years,
    event_start_date = bounds$start_date,
    event_end_date = bounds$end_date,
    event_scan_chunk_by = config$fill_scan_chunk_by
  ))
}

polypharmacy_fill_window_sql <- function(
  config,
  ids_alias = "ids",
  fill_column = "rx.fill_date",
  days_supply_column = "rx.days_supply",
  chunk_start_date,
  chunk_end_date
) {
  literal_window <- event_literal_window_sql(
    chunk_start_date,
    chunk_end_date,
    fill_column
  )

  patient_year_window <- if (isTRUE(config$allow_prior_fill_carry_in)) {
    paste0(
      fill_column, " <= ", ids_alias, ".analysis_end_date",
      " AND DATEADD(day, ", days_supply_column, " - 1, ", fill_column, ") >= ",
      ids_alias, ".analysis_start_date"
    )
  } else {
    paste0(
      fill_column, " >= ", ids_alias, ".analysis_start_date",
      " AND ", fill_column, " < ", ids_alias, ".analysis_end_date + 1"
    )
  }

  paste0(literal_window, " AND ", patient_year_window)
}

polypharmacy_transaction_filter_sql <- function(config, alias = "rx") {
  filters <- character()
  add_filter <- function(column, values) {
    if (length(values) == 0L) {
      return(NULL)
    }
    paste0(
      "UPPER(COALESCE(",
      alias,
      ".",
      column,
      ", '')) IN (",
      paste(vapply(values, sql_string, character(1)), collapse = ", "),
      ")"
    )
  }

  filters <- c(
    filters,
    add_filter("transaction_result", config$transaction_result_keep),
    add_filter("transaction_status", config$transaction_status_keep),
    add_filter("transaction_source_type", config$transaction_source_type_keep)
  )
  filters <- filters[!is.na(filters) & filters != ""]

  if (length(filters) == 0L) {
    return("1 = 1")
  }
  paste(filters, collapse = " AND ")
}

polypharmacy_days_supply_filter_sql <- function(config, alias = "rx") {
  base_filter <- paste0(alias, ".days_supply IS NOT NULL AND ", alias, ".days_supply > 0")
  if (is.null(config$days_supply_max)) {
    return(base_filter)
  }
  paste0(base_filter, " AND ", alias, ".days_supply <= ", config$days_supply_max)
}

clean_ndc11_sql <- function(alias = "rx", column = "ndc11") {
  paste0("REGEXP_REPLACE(TRIM(", alias, ".", column, "), '[^0-9]', '')")
}

find_first_column <- function(data, candidates, label) {
  normalized_names <- tolower(gsub("[^A-Za-z0-9]", "", names(data)))
  normalized_candidates <- tolower(gsub("[^A-Za-z0-9]", "", candidates))
  match_index <- match(normalized_candidates, normalized_names, nomatch = 0L)
  match_index <- match_index[match_index > 0L]
  if (length(match_index) == 0L) {
    stop(
      "Could not find ",
      label,
      " column. Expected one of: ",
      paste(candidates, collapse = ", ")
    )
  }
  names(data)[match_index[[1]]]
}

make_day_offsets <- function(max_days = 366L) {
  data.frame(day_offset = seq.int(0L, as.integer(max_days) - 1L))
}

days_supply_bucket_sql <- function(expression) {
  paste0(
    "CASE
       WHEN ", expression, " IS NULL THEN 'missing'
       WHEN ", expression, " <= 0 THEN 'nonpositive'
       WHEN ", expression, " <= 30 THEN '001_030'
       WHEN ", expression, " <= 60 THEN '031_060'
       WHEN ", expression, " <= 90 THEN '061_090'
       WHEN ", expression, " <= 180 THEN '091_180'
       WHEN ", expression, " <= 365 THEN '181_365'
       ELSE '366_plus'
     END"
  )
}
