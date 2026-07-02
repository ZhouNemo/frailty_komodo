source("Code/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual Gagne score
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-07-02
#
# ---- Purpose ----
# Calculate Gagne combined comorbidity scores for the normalized 3.x pipeline
# from compact Gagne feature matches. This script points the validated shared
# scoring engine archived under `Code/Old` at:
#   - 2_annual_gagne_group_matches
#   - 6_annual_gagne_scores

config <- get_normalized_clinical_metrics_config()
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
  source("Code/Old/5.5_calculate_annual_gagne_comorbidity_score.R"),
  finally = {
    options("frailty.clinical_metrics.config" = previous_clinical_metric_config)
    if (exists("con", inherits = FALSE)) {
      disconnect_komodo(con)
    }
  }
)


