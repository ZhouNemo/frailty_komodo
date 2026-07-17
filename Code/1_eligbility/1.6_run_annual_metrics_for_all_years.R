# Project: Frailty_Komoto annual metrics all-year runner
# Author: Nemo Zhou
# Date started: 2026-07-11
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Run the selected-year annual clinical metrics and polypharmacy workflow for
# one or more analysis years. This wrapper applies the non-inpatient-claim
# eligibility restriction, joins annual geography, refreshes normalized clinical
# metrics, renders clinical-metrics descriptive reports, and runs the annual
# polypharmacy selected-year runner for each selected year. The polypharmacy
# runner writes CSVs and HTML reports under a year-specific output folder.
#
# The script does not rebuild the base annual eligible cohort by default.
# Set `run_base_setup <- TRUE` only when the lookup files and unrestricted
# annual eligible cohort are not already current.

# ---- Annual Run Settings ----
analysis_years <- 2022
restricted_eligibility_table <- "1_annual_eligible_cohort_non_inpatient_claim_eligible"
run_base_setup <- FALSE

parse_analysis_years <- function(value) {
  value <- trimws(as.character(value))
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("analysis_years must be supplied as one nonempty value.", call. = FALSE)
  }

  if (grepl(":", value, fixed = TRUE)) {
    bounds <- as.integer(strsplit(value, ":", fixed = TRUE)[[1]])
    if (length(bounds) != 2L || any(is.na(bounds))) {
      stop("Year ranges must look like 2016:2025.", call. = FALSE)
    }
    return(seq.int(bounds[[1]], bounds[[2]]))
  }

  years <- as.integer(trimws(strsplit(value, ",", fixed = TRUE)[[1]]))
  if (length(years) == 0L || any(is.na(years))) {
    stop("Year lists must look like 2016,2017,2022.", call. = FALSE)
  }
  years
}

command_args <- commandArgs(trailingOnly = TRUE)
if (length(command_args) >= 1L && nzchar(command_args[[1]])) {
  analysis_years <- parse_analysis_years(command_args[[1]])
}

env_analysis_years <- Sys.getenv("FRAILTY_ANALYSIS_YEARS", unset = "")
if (nzchar(env_analysis_years)) {
  analysis_years <- parse_analysis_years(env_analysis_years)
}

analysis_years <- sort(unique(as.integer(analysis_years)))
if (
  length(analysis_years) == 0L ||
    any(is.na(analysis_years)) ||
    any(analysis_years < 2016L | analysis_years > 2025L)
) {
  stop("analysis_years must contain years from 2016 through 2025.")
}

run_workflow_stage <- function(label, expr) {
  started_at <- Sys.time()
  message(format(started_at, "[%Y-%m-%d %H:%M:%S] "), "START ", label)
  result <- force(expr)
  finished_at <- Sys.time()
  message(
    format(finished_at, "[%Y-%m-%d %H:%M:%S] "),
    "DONE  ",
    label,
    ". Elapsed minutes: ",
    round(as.numeric(difftime(finished_at, started_at, units = "mins")), 2),
    "."
  )
  result
}

run_source_stage <- function(script) {
  run_workflow_stage(script, source(script))
}

run_selected_year <- function(analysis_year) {
  analysis_year <- as.integer(analysis_year)
  Sys.setenv(FRAILTY_ANALYSIS_YEAR = as.character(analysis_year))

  message("")
  message("============================================================")
  message("Running annual metrics workflow for ", analysis_year, ".")
  message("============================================================")

  options("frailty.normalized_clinical_metrics.config" = list(
    analysis_years = analysis_year,
    id_years = analysis_year,
    eligibility_table = "1_annual_eligible_cohort"
  ))

  options("frailty.non_inpatient_claim_filter.config" = list(
    analysis_years = analysis_year,
    source_eligibility_table = "1_annual_eligible_cohort",
    restricted_eligibility_table = restricted_eligibility_table
  ))
  run_source_stage("Code/1_eligbility/1.4_filter_clinical_metrics_to_non_inpatient_claim_eligible.R")

  options("frailty.patient_geography_join.config" = list(
    analysis_years = analysis_year,
    source_eligibility_table = restricted_eligibility_table
  ))
  run_source_stage("Code/1_eligbility/1.5_join_patient_geography_to_clinical_metrics.R")

  options("frailty.normalized_clinical_metrics.config" = list(
    analysis_years = analysis_year,
    id_years = analysis_year,
    eligibility_table = restricted_eligibility_table,
    refresh_metric_ids = TRUE,
    reuse_candidate_stage = FALSE,
    run_cfi_2016_parity_check = FALSE
  ))
  run_source_stage("Code/2_variable construction/3.13_run_normalized_annual_clinical_metrics.R")

  run_source_stage("Code/2_EDA/4.4_run_annual_clinical_metrics_descriptive_analysis.R")

  run_source_stage("Code/2_variable construction/5.7_run_annual_polypharmacy.R")

  message("Annual metrics workflow complete for ", analysis_year, ".")
}

previous_frailty_analysis_year <- Sys.getenv("FRAILTY_ANALYSIS_YEAR", unset = NA)
previous_options <- list(
  normalized = getOption("frailty.normalized_clinical_metrics.config"),
  non_inpatient = getOption("frailty.non_inpatient_claim_filter.config"),
  geography = getOption("frailty.patient_geography_join.config"),
  polypharmacy = getOption("frailty.annual_polypharmacy.config")
)

tryCatch(
  {
    if (isTRUE(run_base_setup)) {
      run_source_stage("Code/0_test/0.6_validate_clinical_metric_lookups.R")
      run_source_stage("Code/1_eligbility/1.1_build_annual_eligible_population.R")
      run_source_stage("Code/1_eligbility/1.2_check_annual_eligible_population.R")
      run_source_stage("Code/1_eligbility/1.3_join_race_ethnicity_to_eligible_cohort.R")
    }

    for (analysis_year in analysis_years) {
      run_selected_year(analysis_year)
    }

    message(
      "All requested annual metrics workflows complete: ",
      paste(analysis_years, collapse = ", "),
      "."
    )
  },
  error = function(e) {
    message("Annual metrics workflow stopped: ", conditionMessage(e))
    message(
      "If the stop came from Code/2_variable construction/5.7_run_annual_polypharmacy.R because a ",
      "selected-year NDC11-to-ATC crosswalk is missing, run the printed ",
      "PowerShell n2c command and then rerun this 1.6 script."
    )
    stop(e)
  },
  finally = {
    if (is.na(previous_frailty_analysis_year)) {
      Sys.unsetenv("FRAILTY_ANALYSIS_YEAR")
    } else {
      Sys.setenv(FRAILTY_ANALYSIS_YEAR = previous_frailty_analysis_year)
    }

    options(
      "frailty.normalized_clinical_metrics.config" = previous_options$normalized,
      "frailty.non_inpatient_claim_filter.config" = previous_options$non_inpatient,
      "frailty.patient_geography_join.config" = previous_options$geography,
      "frailty.annual_polypharmacy.config" = previous_options$polypharmacy
    )
  }
)
