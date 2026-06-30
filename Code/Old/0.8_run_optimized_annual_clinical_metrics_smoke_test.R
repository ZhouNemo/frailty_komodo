source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual clinical metrics smoke test
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Run a small January 2016 smoke test of the optimized 6.x clinical-metrics
# pipeline without overwriting production 6.x tables. This diagnostic wrapper
# reuses the existing `2_annual_metric_ids` denominator and writes all event,
# candidate, presence, evidence, match, score, status, final, and QA outputs to
# `0_8_*_smoke_2016_jan` tables or `Outputs/0.8_smoke_test`.
#
# The smoke test is intended to validate SQL shape, Redshift compatibility,
# first-25-array-element logic, and downstream table contracts. Its outputs are
# not scientific estimates.

previous_optimized_config <- getOption("frailty.optimized_clinical_metrics.config")

options(
  "frailty.optimized_clinical_metrics.config" = list(
    analysis_years = 2016L,
    id_years = 2016L,
    ids_table = "2_annual_metric_ids",
    inpatient_stage_table = "0_8_inpatient_event_stage_smoke_2016_jan",
    non_inpatient_stage_table = "0_8_non_inpatient_event_stage_smoke_2016_jan",
    inpatient_candidate_table = "0_8_inpatient_candidate_event_stage_smoke_2016_jan",
    non_inpatient_candidate_table = "0_8_non_inpatient_candidate_event_stage_smoke_2016_jan",
    diagnosis_presence_table = "0_8_diagnosis_code_presence_smoke_2016_jan",
    procedure_presence_table = "0_8_procedure_code_presence_smoke_2016_jan",
    hiv_evidence_table = "0_8_hiv_diagnosis_evidence_smoke_2016_jan",
    cfi_feature_matches_table = "0_8_cfi_feature_matches_smoke_2016_jan",
    ccw_feature_matches_table = "0_8_ccw_condition_matches_smoke_2016_jan",
    gagne_feature_matches_table = "0_8_gagne_group_matches_smoke_2016_jan",
    cfi_scores_table = "0_8_cfi_scores_smoke_2016_jan",
    ccw_conditions_long_table = "0_8_ccw_conditions_long_smoke_2016_jan",
    ccw_condition_indicators_table = "0_8_ccw_condition_indicators_smoke_2016_jan",
    ccw_group_counts_table = "0_8_ccw_group_counts_smoke_2016_jan",
    gagne_scores_table = "0_8_gagne_scores_smoke_2016_jan",
    hiv_status_table = "0_8_hiv_status_smoke_2016_jan",
    final_table = "0_8_clinical_metrics_shared_smoke_2016_jan",
    output_dir = file.path(getwd(), "Outputs", "0.8_smoke_test"),
    refresh_metric_ids = FALSE,
    use_candidate_event_stage = TRUE,
    event_start_date = "2016-01-01",
    event_end_date = "2016-02-01",
    array_code_limit = 25L,
    run_cfi_2016_parity_check = FALSE
  )
)

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

tryCatch(
  {
    smoke_start_time <- Sys.time()
    message(
      "[",
      format(smoke_start_time, "%Y-%m-%d %H:%M:%S"),
      "] START optimized annual clinical metrics January 2016 smoke test"
    )
    flush.console()

    run_stage("6.2 event staging smoke test", "Code/6.2_stage_annual_clinical_metric_events.R")
    run_stage("6.3 candidate events smoke test", "Code/6.3_prepare_annual_candidate_events.R")
    run_stage("6.4 code presence smoke test", "Code/6.4_prepare_annual_code_presence.R")
    run_stage("6.5 HIV evidence smoke test", "Code/6.5_prepare_annual_hiv_diagnosis_evidence.R")
    run_stage("6.6 feature matching smoke test", "Code/6.6_match_annual_clinical_metric_features.R")
    run_stage("6.7 CFI scoring smoke test", "Code/6.7_calculate_optimized_annual_cfi_scores.R")
    run_stage("6.8 CCW variables smoke test", "Code/6.8_calculate_optimized_annual_ccw_variables.R")
    run_stage("6.9 Gagne score smoke test", "Code/6.9_calculate_optimized_annual_gagne_score.R")
    run_stage("6.10 HIV status smoke test", "Code/6.10_calculate_optimized_annual_hiv_status.R")
    run_stage("6.11 final table smoke test", "Code/6.11_build_optimized_annual_clinical_metrics.R")
    run_stage("6.12 QA smoke test", "Code/6.12_check_optimized_annual_clinical_metrics.R")

    smoke_end_time <- Sys.time()
    smoke_minutes <- round(
      as.numeric(difftime(smoke_end_time, smoke_start_time, units = "mins")),
      1
    )
    message(
      "[",
      format(smoke_end_time, "%Y-%m-%d %H:%M:%S"),
      "] DONE optimized annual clinical metrics January 2016 smoke test (",
      smoke_minutes,
      " min)"
    )
    flush.console()
  },
  finally = options(
    "frailty.optimized_clinical_metrics.config" = previous_optimized_config
  )
)
