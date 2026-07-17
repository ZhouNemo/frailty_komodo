source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual clinical metrics table
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Build the final normalized annual clinical metrics table from the 3.x metric
# outputs. This script points the validated final-table builder archived under
# `Code/Old` at:
#   - 6_annual_clinical_metrics_shared

config <- get_normalized_clinical_metrics_config()
previous_clinical_metric_config <- getOption("frailty.clinical_metrics.config")

options(
  "frailty.clinical_metrics.config" = list(
    analysis_years = config$analysis_years,
    ids_table = config$ids_table,
    cfi_scores_table = config$cfi_scores_table,
    ccw_condition_indicators_table = config$ccw_condition_indicators_table,
    ccw_group_counts_table = config$ccw_group_counts_table,
    gagne_scores_table = config$gagne_scores_table,
    hiv_status_table = config$hiv_status_table,
    final_table = config$final_table
  )
)

tryCatch(
  source("Code/Old/5.7_build_annual_clinical_metrics.R"),
  finally = {
    options("frailty.clinical_metrics.config" = previous_clinical_metric_config)
    if (exists("con", inherits = FALSE)) {
      disconnect_komodo(con)
    }
  }
)


