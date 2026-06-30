# Project: Frailty_Komoto normalized annual clinical metrics runner
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Run the normalized 6.x annual clinical-metrics pipeline in order. The pipeline
# starts from `2_annual_metric_ids`, `komodo_ext.normalized_dx_events`, and
# `komodo_ext.normalized_procedure_events`; it does not stage or flatten raw
# inpatient/non-inpatient event tables.

scripts <- c(
  "Code/6.1_prepare_annual_metric_ids.R",
  "Code/6.4_prepare_annual_code_presence.R",
  "Code/6.5_prepare_annual_hiv_diagnosis_evidence.R",
  "Code/6.6_match_annual_clinical_metric_features.R",
  "Code/6.7_calculate_normalized_annual_cfi_scores.R",
  "Code/6.8_calculate_normalized_annual_ccw_variables.R",
  "Code/6.9_calculate_normalized_annual_gagne_score.R",
  "Code/6.10_calculate_normalized_annual_hiv_status.R",
  "Code/6.11_build_normalized_annual_clinical_metrics.R",
  "Code/6.12_check_normalized_annual_clinical_metrics.R"
)

for (script in scripts) {
  message("Running ", script)
  source(script)
}
