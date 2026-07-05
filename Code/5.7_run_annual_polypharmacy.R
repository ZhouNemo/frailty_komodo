# Project: Frailty_Komoto annual polypharmacy runner
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-05
#
# ---- Purpose ----
# Standard annual runner for the 5.x polypharmacy pipeline. Edit the annual run
# settings below, then source this file from R. The runner:
#   1. builds selected-year pharmacy fills and exports unique NDC11 values;
#   2. stops with the exact PowerShell n2c command if the selected-year
#      crosswalk CSV is missing; and
#   3. after the crosswalk exists, stages the mapping, builds exposure episodes,
#      calculates annual polypharmacy metrics, runs QA, and writes top-20 ATC3
#      prevalence CSVs.
#
# Durable Redshift tables are refreshed only for `analysis_year`, while local
# report-ready CSVs in Outputs reflect the latest selected run.
#
# ---- Reviewed transaction filter ----
# A 2016 exploratory unfiltered run of `5.1` surfaced the distinct pharmacy
# transaction values in `komodo_ext.pharmacy_events` (no NULL/blank buckets):
#   transaction_result:      PAID / REJECTED / REVERSED
#   transaction_status:      STANDALONE / FINAL
#   transaction_source_type: PAID ONLY / LIFECYCLE
# PAID rows accounted for 500,045,674 of 567,282,857 candidate 2016 fills
# (88.1%); the remaining ~11.9% were REJECTED (6.7%) or REVERSED (5.2%) claims
# that never resulted in an active dispensing. PAID co-occurs only with the two
# terminal statuses (STANDALONE, FINAL), so keeping `transaction_result = 'PAID'`
# yields one terminal paid row per claim and makes a status/source filter
# redundant. See `Documents/12_POLYPHARMACY_DATA_PROCESSING_FLOW.md`.

# ---- Annual Run Settings ----
analysis_year <- 2025L
mapping_version_date <- paste0(analysis_year, "_n2c_rxnav")

unique_ndc_export_path <- file.path(
  getwd(),
  "Outputs",
  paste0("5.2_polypharmacy_unique_ndc11_", analysis_year, ".txt")
)
crosswalk_input_path <- file.path(
  getwd(),
  "Outputs",
  paste0("5.3_polypharmacy_ndc11_atc_crosswalk_", analysis_year, ".csv")
)

previous_annual_polypharmacy_config <- getOption(
  "frailty.annual_polypharmacy.config"
)
runner_polypharmacy_config <- list(
  analysis_years = analysis_year,
  transaction_result_keep = c("PAID"),
  mapping_version_date = mapping_version_date,
  unique_ndc_export_path = unique_ndc_export_path,
  crosswalk_input_path = crosswalk_input_path
)

format_powershell_mapping_command <- function(input_path, crosswalk_path) {
  relative_input <- normalizePath(input_path, winslash = "\\", mustWork = FALSE)
  relative_crosswalk <- normalizePath(crosswalk_path, winslash = "\\", mustWork = FALSE)
  repo_root <- normalizePath(getwd(), winslash = "\\", mustWork = TRUE)

  if (startsWith(relative_input, paste0(repo_root, "\\"))) {
    relative_input <- substring(relative_input, nchar(repo_root) + 2L)
  }
  if (startsWith(relative_crosswalk, paste0(repo_root, "\\"))) {
    relative_crosswalk <- substring(relative_crosswalk, nchar(repo_root) + 2L)
  }

  paste0(
    ".\\Code\\5.25_download_and_run_polypharmacy_n2c_mapping.ps1 `\n",
    "  -InputPath \"", relative_input, "\" `\n",
    "  -CrosswalkOutputPath \"", relative_crosswalk, "\""
  )
}

run_stage <- function(script) {
  message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "START ", script)
  tryCatch(
    {
      source(script)
      message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "DONE  ", script)
    },
    error = function(e) {
      message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "FAILED ", script)
      stop(e)
    }
  )
}

options(
  "frailty.annual_polypharmacy.config" = utils::modifyList(
    if (is.null(previous_annual_polypharmacy_config)) {
      list()
    } else {
      previous_annual_polypharmacy_config
    },
    runner_polypharmacy_config
  )
)

tryCatch(
  {
    if (!file.exists(crosswalk_input_path)) {
      for (script in c(
        "Code/5.1_prepare_polypharmacy_pharmacy_fills.R",
        "Code/5.2_export_polypharmacy_unique_ndc11.R"
      )) {
        run_stage(script)
      }

      stop(
        paste(
          "The selected-year NDC11 export is ready, but the n2c/RxNav crosswalk is missing.",
          "Run this PowerShell command from the project root, then source Code/5.7_run_annual_polypharmacy.R again:",
          "",
          format_powershell_mapping_command(unique_ndc_export_path, crosswalk_input_path),
          sep = "\n"
        ),
        call. = FALSE
      )
    }

    message("Using selected-year crosswalk: ", crosswalk_input_path)
    for (script in c(
      "Code/5.3_stage_polypharmacy_ndc11_atc_crosswalk.R",
      "Code/5.4_build_annual_polypharmacy_exposures.R",
      "Code/5.5_calculate_annual_polypharmacy_metrics.R",
      "Code/5.6_check_annual_polypharmacy_metrics.R",
      "Code/5.8_describe_annual_polypharmacy_atc3_prevalence.R"
    )) {
      run_stage(script)
    }

    message(
      "Annual polypharmacy run complete for ",
      analysis_year,
      ". To render the report, run: ",
      'rmarkdown::render("Code/5.9_visualize_annual_polypharmacy_outputs.Rmd")'
    )
  },
  finally = options(
    "frailty.annual_polypharmacy.config" = previous_annual_polypharmacy_config
  )
)
