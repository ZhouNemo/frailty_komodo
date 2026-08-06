# Project: Frailty_Komoto normalized annual clinical metrics runner
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-08-05
#
# ---- Purpose ----
# Run the normalized 3.x annual clinical-metrics pipeline in order. The pipeline
# starts from `2_2022_komodo_medicare_comparison_metric_ids`,
# `komodo_202606.normalized_dx_events`, and
# `komodo_202606.normalized_procedure_events`; the SAS-derived CCW-30 branch
# uses normalized diagnoses only and does not stage or flatten raw claims. When `calculate_cfi = FALSE` and
# `reuse_cfi_scores = TRUE`, the runner skips CFI preparation and scoring and
# reuses the configured existing CFI score table.

source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")
config <- get_normalized_clinical_metrics_config()

scripts <- c(
  "Code/2_variable construction/3.1_prepare_annual_metric_ids.R",
  if (config$calculate_cfi) {
    "Code/2_variable construction/3.4_prepare_annual_code_presence.R"
  },
  if (!identical(config$ccw_algorithm, "ccw30_normalized")) {
    "Code/2_variable construction/3.5_prepare_annual_hiv_diagnosis_evidence.R"
  },
  "Code/2_variable construction/3.6_match_annual_clinical_metric_features.R",
  if (config$calculate_cfi) {
    "Code/2_variable construction/3.7_calculate_normalized_annual_cfi_scores.R"
  },
  if (identical(config$ccw_algorithm, "ccw30_normalized")) {
    "Code/2_variable construction/3.8a_calculate_normalized_ccw30_variables.R"
  } else {
    "Code/2_variable construction/3.8_calculate_normalized_annual_ccw_variables.R"
  },
  "Code/2_variable construction/3.9_calculate_normalized_annual_gagne_score.R",
  "Code/2_variable construction/3.10_calculate_normalized_annual_hiv_status.R",
  "Code/2_variable construction/3.11_build_normalized_annual_clinical_metrics.R",
  "Code/2_variable construction/3.12_check_normalized_annual_clinical_metrics.R"
)

for (script in scripts) {
  message("Running ", script)
  source(script)
}
