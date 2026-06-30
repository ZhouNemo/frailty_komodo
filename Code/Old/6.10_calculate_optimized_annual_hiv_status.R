source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual HIV status
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-29
#
# ---- Purpose ----
# Calculate annual HIV status for the optimized 6.x pipeline from compact HIV
# diagnosis evidence. This wrapper reuses the validated `Code/5.6` confirmation
# logic while pointing it at:
#   - 2_annual_hiv_diagnosis_evidence
#   - 6_annual_hiv_status

config <- get_optimized_clinical_metrics_config()
previous_clinical_metric_config <- getOption("frailty.clinical_metrics.config")

options(
  "frailty.clinical_metrics.config" = list(
    analysis_years = config$analysis_years,
    ids_table = config$ids_table,
    diagnosis_matches_table = config$hiv_evidence_table,
    hiv_status_table = config$hiv_status_table,
    lookup_dir = config$lookup_dir
  )
)

tryCatch(
  source("Code/5.6_calculate_annual_hiv_status.R"),
  finally = options(
    "frailty.clinical_metrics.config" = previous_clinical_metric_config
  )
)

