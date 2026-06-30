source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual CFI scoring
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-29
#
# ---- Purpose ----
# Calculate CFI scores for the optimized 6.x pipeline from compact CFI feature
# matches. This wrapper reuses the validated `Code/5.3` scoring logic while
# pointing it at:
#   - 2_annual_cfi_feature_matches
#   - 6_annual_cfi_scores

config <- get_optimized_clinical_metrics_config()
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
  source("Code/5.3_calculate_annual_cfi_scores.R"),
  finally = options(
    "frailty.clinical_metrics.config" = previous_clinical_metric_config
  )
)

