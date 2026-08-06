# Project: Frailty_Komoto healthcare utilization
# Author: Nemo Zhou
# Date started: 2026-07-17
# Date last updated: 2026-08-02
#
# ---- Purpose ----
# Run the complete 2022 healthcare-utilization workflow for all eligible
# medical-insurance groups and segments. It assumes that the configured
# clinical-metrics table already exists and does not rerun
# eligibility or clinical-metric construction. After aggregate QA succeeds,
# render the offline HTML report from the generated CSV inputs.

analysis_year <- 2022L
render_report <- TRUE
command_args <- commandArgs(trailingOnly = TRUE)
if (length(command_args) >= 1L && nzchar(command_args[[1]])) {
  analysis_year <- as.integer(command_args[[1]])
}

env_analysis_year <- Sys.getenv("FRAILTY_ANALYSIS_YEAR", unset = "")
if (nzchar(env_analysis_year)) {
  analysis_year <- as.integer(env_analysis_year)
}

if (length(analysis_year) != 1L || is.na(analysis_year) || analysis_year != 2022L) {
  stop("This fixed healthcare-utilization runner supports analysis_year = 2022 only.")
}

previous_config <- getOption("frailty.annual_healthcare_utilization.config")
configured_output_dir <- if (
  !is.null(previous_config) && !is.null(previous_config$output_dir)
) {
  as.character(previous_config$output_dir)
} else {
  NULL
}
output_dir <- if (!is.null(configured_output_dir) && length(configured_output_dir) == 1L) {
  configured_output_dir
} else {
  file.path(
    getwd(), "Outputs", paste0("6.x_annual_healthcare_utilization_", analysis_year)
  )
}
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

options(
  "frailty.annual_healthcare_utilization.config" = utils::modifyList(
    if (is.null(previous_config)) list() else previous_config,
    list(analysis_year = analysis_year, output_dir = output_dir)
  )
)

run_stage <- function(script) {
  started_at <- Sys.time()
  message(format(started_at, "[%Y-%m-%d %H:%M:%S] "), "START ", script)
  tryCatch(
    {
      source(script)
      message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "DONE  ", script)
    },
    error = function(error) {
      message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "FAILED ", script)
      stop(error)
    }
  )
}

tryCatch(
  {
    for (script in c(
      "Code/2_variable construction/6.1_prepare_annual_healthcare_utilization_events.R",
      "Code/2_variable construction/6.2_calculate_annual_healthcare_utilization_variables.R",
      "Code/2_variable construction/6.3_build_2022_clinical_metrics_with_utilization.R",
      "Code/2_variable construction/6.4_describe_annual_healthcare_utilization.R",
      "Code/2_variable construction/6.5_check_annual_healthcare_utilization.R"
    )) {
      run_stage(script)
    }

    if (isTRUE(render_report)) {
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        stop("Package 'rmarkdown' is required to render the utilization report.")
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
          "Pandoc version 1.12.3 or higher is required to render the utilization report. ",
          "Install Pandoc or set RSTUDIO_PANDOC to the folder containing pandoc.exe."
        )
      }

      report_file <- paste0(
        "6.7_annual_healthcare_utilization_",
        analysis_year,
        ".html"
      )
      rmarkdown::render(
        input = "Code/2_EDA/6.7_visualize_annual_healthcare_utilization.Rmd",
        output_file = report_file,
        output_dir = output_dir,
        params = list(
          analysis_year = analysis_year,
          utilization_output_dir = output_dir
        ),
        envir = new.env(parent = globalenv())
      )
      message("Rendered ", file.path(output_dir, report_file))
    }

    message("2022 all-insurance healthcare-utilization workflow complete. Outputs are in ", output_dir)
  },
  finally = options("frailty.annual_healthcare_utilization.config" = previous_config)
)
