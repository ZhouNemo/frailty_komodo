# Project: Frailty_Komoto 2022 Komodo Medicare comparison
# Author: Nemo Zhou
# Date started: 2026-07-31
# Date last updated: 2026-08-05
#
# ---- Purpose ----
# Run the fixed-2022 Komodo comparison pipeline using the broader cohort that
# does not require a same-year non-inpatient claim. Existing CFI scores are
# reused from the current non-inpatient-restricted CFI table; this runner does
# not recalculate or overwrite CFI outputs.
# CCW is instead calculated with the SAS-derived normalized-table CCW-30
# adaptation for calendar year 2022; it does not alter the default CCW-56 path.
#
# For this comparison, HIV status is positive with one qualifying inpatient
# diagnosis match or two distinct qualifying non-inpatient diagnosis dates.
#
# The existing all-eligible cohort is the default downstream handoff. Rebuilding
# that cohort is an explicit opt-in because it is an expensive upstream stage
# and the cohort builder still requires separate eligibility review before a
# production rebuild.
#
# The pipeline uses or refreshes, in order:
#   1. 1_2022_komodo_medicare_comparison_cohort_all_eligible
#   2. 2_2022_komodo_medicare_comparison_all_eligible_metric_ids for 2022
#   3. 6_2022_komodo_medicare_comparison_all_eligible_clinical_metrics_shared
#   4. 6_2022_komodo_medicare_comparison_all_eligible_clinical_metrics_with_utilization
#      with Census-region and state-plus-ZIP3 RUCC rurality fields refreshed by
#      7.1 from the versioned local reference lookup.
#
# This runner intentionally does not render the final Table 1 report. The
# resulting Redshift table and utilization/geography QA outputs are produced
# first; it verifies the comparison-report column contract before handing the
# R Markdown render command to the user.
#
# Run this file from the project root:
#   source("Code/1_run_2022_komodo_medicare_comparison_pipeline.R")
#
# To explicitly rebuild the broader cohort first:
#   options(frailty.2022_komodo_medicare_comparison.rebuild_cohort = TRUE)
#   source("Code/1_run_2022_komodo_medicare_comparison_pipeline.R")

analysis_year <- 2022L
rebuild_cohort <- isTRUE(
  getOption("frailty.2022_komodo_medicare_comparison.rebuild_cohort", FALSE)
)
rebuild_cohort_env <- Sys.getenv(
  "FRAILTY_REBUILD_2022_COMPARISON_COHORT",
  unset = ""
)
if (nzchar(rebuild_cohort_env)) {
  rebuild_cohort <- tolower(trimws(rebuild_cohort_env)) %in% c(
    "1", "true", "t", "yes", "y"
  )
}

comparison_cohort_table <- "1_2022_komodo_medicare_comparison_cohort_all_eligible"
comparison_ids_table <- "2_2022_komodo_medicare_comparison_all_eligible_metric_ids"
comparison_final_table <- "6_2022_komodo_medicare_comparison_all_eligible_clinical_metrics_shared"
comparison_combined_table <- paste0(
  "6_2022_komodo_medicare_comparison_all_eligible_clinical_metrics_with_utilization"
)
existing_cfi_scores_table <- "6_2022_komodo_medicare_comparison_cfi_scores"
existing_cfi_matches_table <- "2_2022_komodo_medicare_comparison_cfi_feature_matches"
existing_procedure_presence_table <- paste0(
  "2_2022_komodo_medicare_comparison_procedure_code_presence"
)
project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
rurality_lookup_path <- file.path(
  project_root,
  "Documents",
  "Rurality Reference Tables",
  "rucc_2023_zip3_rurality_lookup.csv"
)

required_root_directories <- c("Code", "Documents", "Outputs")
missing_root_directories <- required_root_directories[
  !dir.exists(file.path(project_root, required_root_directories))
]
if (length(missing_root_directories) > 0L) {
  stop(
    "Run this pipeline from the project root. Missing directories: ",
    paste(missing_root_directories, collapse = ", "),
    "."
  )
}
if (!file.exists(rurality_lookup_path)) {
  stop(
    "Required rurality lookup was not found: ", rurality_lookup_path,
    ". Run Code/0_test/0.9_build_rucc_2023_zip3_rurality_lookup.R first."
  )
}

stage_paths <- c(
  cohort = file.path(
    project_root,
    "Code",
    "1_eligbility",
    "1.1_build_2022_komodo_medicare_comparison_cohort.R"
  ),
  normalized_metrics = file.path(
    project_root,
    "Code",
    "2_variable construction",
    "3.13_run_normalized_annual_clinical_metrics.R"
  ),
  healthcare_utilization = file.path(
    project_root,
    "Code",
    "2_variable construction",
    "6.6_run_annual_healthcare_utilization.R"
  ),
  geography = file.path(
    project_root,
    "Code",
    "2_variable construction",
    "7.1_add_census_region_and_rurality.R"
  )
)

required_stage_paths <- stage_paths[names(stage_paths) != "cohort" | rebuild_cohort]
missing_stage_paths <- required_stage_paths[!file.exists(required_stage_paths)]
if (length(missing_stage_paths) > 0L) {
  stop(
    "Required pipeline script(s) not found: ",
    paste(missing_stage_paths, collapse = ", "),
    "."
  )
}

run_pipeline_stage <- function(stage_name, stage_path) {
  started_at <- Sys.time()
  message(
    format(started_at, "[%Y-%m-%d %H:%M:%S] "),
    "START: ", stage_name
  )
  source(stage_path, local = .GlobalEnv)
  finished_at <- Sys.time()
  message(
    format(finished_at, "[%Y-%m-%d %H:%M:%S] "),
    "DONE: ", stage_name,
    ". Elapsed minutes: ",
    round(as.numeric(difftime(finished_at, started_at, units = "mins")), 2),
    "."
  )
}

validate_comparison_report_contract <- function() {
  required_columns <- c(
    "patient_id", "analysis_year", "overall_comparison_eligible",
    "plan_comparison_eligible", "plan_comparison_group",
    "pooled_medicare_segment", "n_valid_medicare_segments",
    "has_same_year_non_inpatient_event", "has_medicare_medicaid_dual_coverage",
    "cfi_score", "long_term_care_any_visit", "census_region", "rurality_group"
  )
  con <- connect_komodo()
  tryCatch(
    table_has_columns(
      con,
      write_schema,
      comparison_combined_table,
      required_columns
    ),
    finally = disconnect_komodo(con)
  )
}

previous_metrics_config <- getOption(
  "frailty.normalized_clinical_metrics.config"
)
previous_utilization_config <- getOption(
  "frailty.annual_healthcare_utilization.config"
)
previous_cohort_config <- getOption(
  "frailty.2022_komodo_medicare_comparison_cohort.config"
)
options(
  "frailty.2022_komodo_medicare_comparison_cohort.config" = list(
    cohort_table = comparison_cohort_table,
    temp_flags_table = "tmp_2022_komodo_medicare_comparison_all_eligible_flags",
    output_dir = file.path(project_root, "Outputs", "1_eligibility_2022_all_eligible"),
    require_same_year_non_inpatient = FALSE
  ),
  "frailty.normalized_clinical_metrics.config" = utils::modifyList(
    if (is.null(previous_metrics_config)) list() else previous_metrics_config,
    list(
      analysis_years = analysis_year,
      id_years = analysis_year,
      eligibility_table = comparison_cohort_table,
      required_eligibility_columns = c(
        "overall_comparison_eligible",
        "plan_comparison_eligible",
        "plan_comparison_group",
        "pooled_medicare_segment",
        "n_valid_medicare_segments",
        "has_same_year_non_inpatient_event",
        "has_medicare_medicaid_dual_coverage"
      ),
      ids_table = comparison_ids_table,
      output_dir = file.path(
        project_root,
        "Outputs",
        "3.x_normalized_clinical_metrics_2022_all_eligible"
      ),
      workflow_label = paste(
        "2022 Komodo Medicare comparison normalized clinical metrics",
        "(all eligible denominator)"
      ),
      procedure_presence_table = existing_procedure_presence_table,
      hiv_evidence_table = "2_2022_komodo_medicare_comparison_all_eligible_hiv_diagnosis_evidence",
      cfi_feature_matches_table = existing_cfi_matches_table,
      ccw_algorithm = "ccw30_normalized",
      ccw_feature_matches_table = "2_2022_komodo_medicare_comparison_all_eligible_ccw30_matched_evidence",
      gagne_feature_matches_table = "2_2022_komodo_medicare_comparison_all_eligible_gagne_group_matches",
      candidate_stage_table = "2_2022_komodo_medicare_comparison_all_eligible_dx_candidate_stage",
      candidate_stage_manifest_table = "2_2022_komodo_medicare_comparison_all_eligible_dx_candidate_stage_manifest",
      cfi_scores_table = existing_cfi_scores_table,
      ccw_conditions_long_table = "6_2022_komodo_medicare_comparison_all_eligible_ccw30_conditions_long",
      ccw_condition_indicators_table = "6_2022_komodo_medicare_comparison_all_eligible_ccw30_condition_indicators",
      ccw_group_counts_table = "6_2022_komodo_medicare_comparison_all_eligible_ccw30_total_counts",
      gagne_scores_table = "6_2022_komodo_medicare_comparison_all_eligible_gagne_scores",
      hiv_status_table = "6_2022_komodo_medicare_comparison_all_eligible_hiv_status",
      hiv_non_inpatient_min_distinct_dates = 2L,
      final_table = comparison_final_table,
      refresh_metric_ids = TRUE,
      reuse_candidate_stage = FALSE,
      calculate_cfi = FALSE,
      reuse_cfi_scores = TRUE,
      run_cfi_2016_parity_check = FALSE
    )
  ),
  "frailty.annual_healthcare_utilization.config" = utils::modifyList(
    if (is.null(previous_utilization_config)) list() else previous_utilization_config,
    list(
      analysis_year = analysis_year,
      cohort_table = comparison_final_table,
      events_table = "6_2022_komodo_medicare_comparison_all_eligible_healthcare_utilization_events",
      metrics_table = "6_2022_komodo_medicare_comparison_all_eligible_healthcare_utilization_metrics",
      combined_table = comparison_combined_table,
      rurality_lookup_path = rurality_lookup_path,
      output_dir = file.path(
        project_root,
        "Outputs",
        "6.x_annual_healthcare_utilization_2022_all_eligible"
      )
    )
  )
)

tryCatch(
  {
    if (rebuild_cohort) {
      run_pipeline_stage(
        "Build 2022 Komodo Medicare comparison cohort",
        stage_paths[["cohort"]]
      )
    } else {
      message(
        "SKIP: Build 2022 Komodo Medicare comparison cohort. Reusing existing ",
        "configured write-schema table ", comparison_cohort_table,
        ". Set FRAILTY_REBUILD_2022_COMPARISON_COHORT=true or the corresponding ",
        "R option to rebuild it explicitly."
      )
    }
    run_pipeline_stage(
      "Build normalized 2022 clinical metrics",
      stage_paths[["normalized_metrics"]]
    )
    run_pipeline_stage(
      "Build 2022 healthcare utilization dataset",
      stage_paths[["healthcare_utilization"]]
    )
    run_pipeline_stage(
      "Add 2022 Census region and ZIP3 rurality",
      stage_paths[["geography"]]
    )
  },
  finally = {
    options(
      "frailty.normalized_clinical_metrics.config" = previous_metrics_config,
      "frailty.annual_healthcare_utilization.config" = previous_utilization_config,
      "frailty.2022_komodo_medicare_comparison_cohort.config" = previous_cohort_config
    )
  }
)

validate_comparison_report_contract()

write_schema <- paste0("work_", keyring::key_get("db_username"))
final_dataset_identifier <- paste0(write_schema, ".", comparison_combined_table)

message("2022 pipeline complete.")
message(
  if (rebuild_cohort) {
    "The broader all-eligible cohort was rebuilt before downstream processing."
  } else {
    "The existing broader all-eligible cohort was reused; no cohort rebuild was run."
  }
)
message("Final table for aggregate table creation: ", final_dataset_identifier)
message(
  "Review Outputs/1_eligibility_2022_all_eligible, ",
  "Outputs/6.x_annual_healthcare_utilization_2022_all_eligible, and the 7.1 ",
  "geography QA CSV. The rurality source was ", rurality_lookup_path,
  ". Required report render: rmarkdown::render('Code/3_result/",
  "3.1_create_2022_komodo_medicare_table1.Rmd', output_file = ",
  "'3.1_2022_komodo_medicare_table1.html', output_dir = 'Outputs/3_result_2022', ",
  "params = list(result_output_dir = 'Outputs/3_result_2022', analysis_table = '",
  comparison_combined_table, "', utilization_output_dir = 'Outputs/",
  "6.x_annual_healthcare_utilization_2022_all_eligible'))."
)
