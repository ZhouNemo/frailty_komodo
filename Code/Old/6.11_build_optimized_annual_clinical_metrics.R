source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual clinical metrics table
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-29
#
# ---- Purpose ----
# Build the final optimized annual clinical metrics table from the 6.x metric
# outputs. This wrapper reuses the validated `Code/5.7` final-table builder
# while pointing it at:
#   - 6_annual_clinical_metrics_shared

config <- get_optimized_clinical_metrics_config()
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
  source("Code/5.7_build_annual_clinical_metrics.R"),
  finally = options(
    "frailty.clinical_metrics.config" = previous_clinical_metric_config
  )
)

