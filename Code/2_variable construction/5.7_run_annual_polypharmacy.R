# Project: Frailty_Komoto annual polypharmacy runner
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Standard annual runner for the 5.x polypharmacy pipeline. Set the analysis
# year below, pass it as the first command-line argument, or set
# `FRAILTY_ANALYSIS_YEAR` before sourcing this file from R. The runner:
#   1. builds selected-year pharmacy fills and exports unique NDC11 values;
#   2. stops with the exact PowerShell n2c command if the selected-year
#      crosswalk CSV is missing; and
#   3. after the crosswalk exists, stages the mapping, builds exposure episodes,
#      calculates annual polypharmacy metrics, runs QA, writes top-20 ATC3
#      prevalence CSVs, prepares insurance-subgroup inputs, and renders both
#      polypharmacy reports.
#
# Durable Redshift tables are refreshed only for `analysis_year`, while local
# report-ready files are written under
# Outputs/5.x_annual_polypharmacy_<analysis_year>.
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
analysis_year <- 2022L

command_args <- commandArgs(trailingOnly = TRUE)
if (length(command_args) >= 1L && nzchar(command_args[[1]])) {
  analysis_year <- as.integer(command_args[[1]])
}

env_analysis_year <- Sys.getenv("FRAILTY_ANALYSIS_YEAR", unset = "")
if (nzchar(env_analysis_year)) {
  analysis_year <- as.integer(env_analysis_year)
}

if (
  length(analysis_year) != 1L ||
    is.na(analysis_year) ||
    analysis_year < 2016L ||
    analysis_year > 2025L
) {
  stop("analysis_year must be one integer year from 2016 through 2025.")
}

mapping_version_date <- paste0(analysis_year, "_n2c_rxnav")

polypharmacy_output_root <- file.path(getwd(), "Outputs")
polypharmacy_output_dir <- file.path(
  getwd(),
  "Outputs",
  paste0("5.x_annual_polypharmacy_", analysis_year)
)
unique_ndc_export_path <- file.path(
  polypharmacy_output_dir,
  paste0("5.2_polypharmacy_unique_ndc11_", analysis_year, ".txt")
)
crosswalk_input_path <- file.path(
  polypharmacy_output_dir,
  paste0("5.3_polypharmacy_ndc11_atc_crosswalk_", analysis_year, ".csv")
)

if (!dir.exists(polypharmacy_output_dir)) {
  dir.create(polypharmacy_output_dir, recursive = TRUE)
}

previous_annual_polypharmacy_config <- getOption(
  "frailty.annual_polypharmacy.config"
)
runner_polypharmacy_config <- list(
  analysis_years = analysis_year,
  output_root = polypharmacy_output_root,
  output_dir = polypharmacy_output_dir,
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
    "& '.\\Code\\2_variable construction\\5.2.5_download_and_run_polypharmacy_n2c_mapping.ps1' `\n",
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

ensure_rmarkdown_pandoc <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render the polypharmacy reports.")
  }

  if (!rmarkdown::pandoc_available("1.12.3")) {
    rstudio_pandoc_paths <- c(
      "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
      "C:/Program Files/RStudio/bin/pandoc",
      "C:/Program Files/Posit Software/RStudio/resources/app/bin/quarto/bin/tools",
      "C:/Program Files/Posit Software/RStudio/bin/pandoc"
    )
    rstudio_pandoc_path <- rstudio_pandoc_paths[
      file.exists(file.path(rstudio_pandoc_paths, "pandoc.exe"))
    ][1]

    if (!is.na(rstudio_pandoc_path)) {
      Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc_path)
    }
  }

  if (!rmarkdown::pandoc_available("1.12.3")) {
    stop("Pandoc 1.12.3 or higher is required to render the polypharmacy reports.")
  }
}

render_polypharmacy_report <- function(input, output_file) {
  ensure_rmarkdown_pandoc()
  message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "START render ", output_file)
  rmarkdown::render(
    input,
    output_file = output_file,
    output_dir = polypharmacy_output_dir,
    params = list(
      analysis_year = analysis_year,
      polypharmacy_output_dir = polypharmacy_output_dir
    ),
    envir = new.env(parent = globalenv())
  )
  message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "DONE  render ", output_file)
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
        "Code/2_variable construction/5.1_prepare_polypharmacy_pharmacy_fills.R",
        "Code/2_variable construction/5.2_export_polypharmacy_unique_ndc11.R"
      )) {
        run_stage(script)
      }

      stop(
        paste(
          "The selected-year NDC11 export is ready, but the n2c/RxNav crosswalk is missing.",
          "Run this PowerShell command from the project root, then source Code/2_variable construction/5.7_run_annual_polypharmacy.R again:",
          "",
          format_powershell_mapping_command(unique_ndc_export_path, crosswalk_input_path),
          sep = "\n"
        ),
        call. = FALSE
      )
    }

    message("Using selected-year crosswalk: ", crosswalk_input_path)
    for (script in c(
      "Code/2_variable construction/5.3_stage_polypharmacy_ndc11_atc_crosswalk.R",
      "Code/2_variable construction/5.4_build_annual_polypharmacy_exposures.R",
      "Code/2_variable construction/5.5_calculate_annual_polypharmacy_metrics.R",
      "Code/2_variable construction/5.6_check_annual_polypharmacy_metrics.R",
      "Code/2_EDA/5.8_describe_annual_polypharmacy_atc3_prevalence.R",
      "Code/2_EDA/5.10_prepare_polypharmacy_insurance_subgroup_inputs.R"
    )) {
      run_stage(script)
    }

    render_polypharmacy_report(
      "Code/2_EDA/5.9_visualize_annual_polypharmacy_outputs.Rmd",
      paste0("5.9_visualize_annual_polypharmacy_outputs_", analysis_year, ".html")
    )
    render_polypharmacy_report(
      "Code/2_EDA/5.10_visualize_polypharmacy_by_insurance_groups.Rmd",
      paste0("5.10_visualize_polypharmacy_by_insurance_groups_", analysis_year, ".html")
    )

    message(
      "Annual polypharmacy run complete for ",
      analysis_year,
      ". Outputs are in ",
      polypharmacy_output_dir,
      "."
    )
  },
  finally = options(
    "frailty.annual_polypharmacy.config" = previous_annual_polypharmacy_config
  )
)
