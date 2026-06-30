source("Code/6.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual CFI scoring
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Calculate CFI scores for the normalized 6.x pipeline from compact CFI feature
# matches. This script points the validated shared scoring engine archived under
# `Code/Old` at:
#   - 2_annual_cfi_feature_matches
#   - 6_annual_cfi_scores

config <- get_normalized_clinical_metrics_config()
previous_clinical_metric_config <- getOption("frailty.clinical_metrics.config")

options(
  "frailty.clinical_metrics.config" = list(
    analysis_years = config$analysis_years,
    ids_table = config$ids_table,
    diagnosis_matches_table = config$cfi_feature_matches_table,
    procedure_matches_table = config$cfi_feature_matches_table,
    cfi_scores_table = config$cfi_scores_table,
    lookup_dir = config$lookup_dir,
    model_intercept = config$model_intercept
  )
)

tryCatch(
  source("Code/Old/5.3_calculate_annual_cfi_scores.R"),
  finally = options(
    "frailty.clinical_metrics.config" = previous_clinical_metric_config
  )
)
