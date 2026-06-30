source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual Gagne score
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-29
#
# ---- Purpose ----
# Calculate Gagne combined comorbidity scores for the optimized 6.x pipeline
# from compact Gagne feature matches. This wrapper reuses the validated
# `Code/5.5` logic while pointing it at:
#   - 2_annual_gagne_group_matches
#   - 6_annual_gagne_scores

config <- get_optimized_clinical_metrics_config()
previous_clinical_metric_config <- getOption("frailty.clinical_metrics.config")

options(
  "frailty.clinical_metrics.config" = list(
    analysis_years = config$analysis_years,
    ids_table = config$ids_table,
    diagnosis_matches_table = config$gagne_feature_matches_table,
    gagne_scores_table = config$gagne_scores_table,
    lookup_dir = config$lookup_dir
  )
)

tryCatch(
  source("Code/5.5_calculate_annual_gagne_comorbidity_score.R"),
  finally = options(
    "frailty.clinical_metrics.config" = previous_clinical_metric_config
  )
)

