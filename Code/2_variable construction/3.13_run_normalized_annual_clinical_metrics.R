# Project: Frailty_Komoto normalized annual clinical metrics runner
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Run the normalized 3.x annual clinical-metrics pipeline in order. The pipeline
# starts from `2_annual_metric_ids`, `komodo_ext.normalized_dx_events`, and
# `komodo_ext.normalized_procedure_events`; it does not stage or flatten raw
# inpatient/non-inpatient event tables.

scripts <- c(
  "Code/2_variable construction/3.1_prepare_annual_metric_ids.R",
  "Code/2_variable construction/3.4_prepare_annual_code_presence.R",
  "Code/2_variable construction/3.5_prepare_annual_hiv_diagnosis_evidence.R",
  "Code/2_variable construction/3.6_match_annual_clinical_metric_features.R",
  "Code/2_variable construction/3.7_calculate_normalized_annual_cfi_scores.R",
  "Code/2_variable construction/3.8_calculate_normalized_annual_ccw_variables.R",
  "Code/2_variable construction/3.9_calculate_normalized_annual_gagne_score.R",
  "Code/2_variable construction/3.10_calculate_normalized_annual_hiv_status.R",
  "Code/2_variable construction/3.11_build_normalized_annual_clinical_metrics.R",
  "Code/2_variable construction/3.12_check_normalized_annual_clinical_metrics.R"
)

for (script in scripts) {
  message("Running ", script)
  source(script)
}
