# Project: Frailty_Komoto optimized annual clinical metrics run-all wrapper
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-29
#
# ---- Purpose ----
# Run the full optimized 6.x clinical-metrics flow in order. Configure selected
# years with options("frailty.optimized_clinical_metrics.config") before
# sourcing this script. The default configuration is 2016 only.

source("Code/6.0_optimized_clinical_metrics_helpers.R")
pipeline_config <- get_optimized_clinical_metrics_config()

run_stage <- function(label, path) {
  start_time <- Sys.time()
  message(
    "\n[",
    format(start_time, "%Y-%m-%d %H:%M:%S"),
    "] START ",
    label
  )
  flush.console()

  source(path)

  end_time <- Sys.time()
  elapsed_minutes <- round(
    as.numeric(difftime(end_time, start_time, units = "mins")),
    1
  )
  message(
    "[",
    format(end_time, "%Y-%m-%d %H:%M:%S"),
    "] DONE  ",
    label,
    " (",
    elapsed_minutes,
    " min)"
  )
  flush.console()
}

pipeline_start_time <- Sys.time()
message(
  "[",
  format(pipeline_start_time, "%Y-%m-%d %H:%M:%S"),
  "] START optimized annual clinical metrics pipeline"
)
flush.console()

run_stage("6.1 metric IDs", "Code/6.1_prepare_annual_metric_ids.R")
run_stage("6.2 event staging", "Code/6.2_stage_annual_clinical_metric_events.R")
if (pipeline_config$use_candidate_event_stage) {
  run_stage("6.3 candidate events", "Code/6.3_prepare_annual_candidate_events.R")
} else {
  message("\nSkipping 6.3 candidate events because use_candidate_event_stage = FALSE")
  flush.console()
}
run_stage("6.4 code presence", "Code/6.4_prepare_annual_code_presence.R")
run_stage("6.5 HIV evidence", "Code/6.5_prepare_annual_hiv_diagnosis_evidence.R")
run_stage("6.6 feature matching", "Code/6.6_match_annual_clinical_metric_features.R")
run_stage("6.7 CFI scoring", "Code/6.7_calculate_optimized_annual_cfi_scores.R")
run_stage("6.8 CCW variables", "Code/6.8_calculate_optimized_annual_ccw_variables.R")
run_stage("6.9 Gagne score", "Code/6.9_calculate_optimized_annual_gagne_score.R")
run_stage("6.10 HIV status", "Code/6.10_calculate_optimized_annual_hiv_status.R")
run_stage("6.11 final table", "Code/6.11_build_optimized_annual_clinical_metrics.R")
run_stage("6.12 QA", "Code/6.12_check_optimized_annual_clinical_metrics.R")

pipeline_end_time <- Sys.time()
pipeline_minutes <- round(
  as.numeric(difftime(pipeline_end_time, pipeline_start_time, units = "mins")),
  1
)
message(
  "[",
  format(pipeline_end_time, "%Y-%m-%d %H:%M:%S"),
  "] DONE optimized annual clinical metrics pipeline (",
  pipeline_minutes,
  " min)"
)
flush.console()
