source("Code/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto annual clinical metrics descriptive analysis
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-03
#
# ---- Purpose ----
# Run the aggregate descriptive-analysis workflow for a selected annual
# clinical-metrics analysis year. The script writes year-specific aggregate
# outputs and, by default, renders the matching HTML report so a 2025 run does
# not overwrite 2016 descriptive files.

analysis_year <- 2025L
render_report <- TRUE

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

year_label <- as.character(analysis_year)
descriptive_output_dir <- file.path(
  getwd(),
  "Outputs",
  paste0("4.1_annual_clinical_metrics_descriptive_", year_label)
)

options(
  "frailty.normalized_clinical_metrics.config" = list(
    analysis_years = analysis_year,
    id_years = analysis_year,
    descriptive_output_dir = descriptive_output_dir
  ),
  "frailty.clinical_metrics_descriptive.output_dir" = descriptive_output_dir
)

message("Running annual clinical metrics descriptive analysis for ", year_label, ".")
message("Writing outputs to ", descriptive_output_dir)

source("Code/4.1_describe_annual_clinical_metrics.R")
source("Code/4.3_prepare_annual_clinical_metrics_visualization_inputs.R")

if (isTRUE(render_report)) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required to render the descriptive report.")
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
    stop(
      "Pandoc version 1.12.3 or higher is required to render the HTML report. ",
      "Install Pandoc or set RSTUDIO_PANDOC to the folder containing pandoc.exe."
    )
  }

  report_file <- paste0(
    "4.2_annual_clinical_metrics_descriptive_",
    year_label,
    ".html"
  )

  rmarkdown::render(
    input = "Code/4.2_visualize_annual_clinical_metrics_descriptive_outputs.Rmd",
    output_file = report_file,
    output_dir = descriptive_output_dir,
    params = list(
      descriptive_output_dir = descriptive_output_dir,
      analysis_year_label = year_label
    ),
    envir = new.env(parent = globalenv())
  )

  message("Rendered ", file.path(descriptive_output_dir, report_file))
}
