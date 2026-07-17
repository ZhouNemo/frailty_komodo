source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual CCW variables
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Calculate CCW variables for the normalized 3.x pipeline from compact CCW
# feature matches. This script points the validated shared scoring engine
# archived under `Code/Old` at:
#   - 2_annual_ccw_condition_matches
#   - 6_annual_ccw_conditions_long
#   - 6_annual_ccw_condition_indicators
#   - 6_annual_ccw_group_counts

config <- get_normalized_clinical_metrics_config()
previous_clinical_metric_config <- getOption("frailty.clinical_metrics.config")

options(
  "frailty.clinical_metrics.config" = list(
    analysis_years = config$analysis_years,
    ids_table = config$ids_table,
    diagnosis_matches_table = config$ccw_feature_matches_table,
    ccw_conditions_long_table = config$ccw_conditions_long_table,
    ccw_condition_indicators_table = config$ccw_condition_indicators_table,
    ccw_group_counts_table = config$ccw_group_counts_table,
    lookup_dir = config$lookup_dir
  )
)

tryCatch(
  source("Code/Old/5.4_calculate_annual_ccw_variables.R"),
  finally = {
    options("frailty.clinical_metrics.config" = previous_clinical_metric_config)
    if (exists("con", inherits = FALSE)) {
      disconnect_komodo(con)
    }
  }
)


