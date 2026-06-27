# Code Folder README

This folder contains the R scripts used to connect to Komodo, prototype claims workflows, diagnose annual eligibility assumptions, build the annual eligible population, generate aggregate summaries, and prepare claims-based index inputs. Scripts should be run in numeric order unless a task-specific note says otherwise.

Numeric order logic: 0.xx: code for testing/checking/diagnosis; 1.xx: code to generate the study sample; 2.xx: code to do summary analyses; 3.xx: existing CFI preparation and scoring workflows; 4.xx: existing CFI descriptive reporting from prepared analysis tables; 5.xx: planned shared clinical-metric processing for CFI, CCW, Gagne, and HIV, as specified in `Documents/01_CLINICAL_METRICS_DATA_PROCESSING_FLOW.md`.

## `0.1_connect to Komodo.R`

Initial connection script for the OHDSI Lab Komodo Redshift environment. It loads the core database packages, sets the JDBC driver folder, and establishes the standard Redshift connection pattern used by later scripts.

Run this first when validating credentials, JDBC configuration, or basic access to the Komodo workspace.

## `0.2_model_komodo_workflow.R`

Reusable model workflow for future Komodo analyses. It demonstrates the preferred project pattern for connecting to Redshift, defining `komodo_schema` and `write_schema`, using `ohdsilab::k_get_condition_events()` and `ohdsilab::k_get_procedure_events()`, materializing a cohort table, and generating an aggregate Table 1.

Use this as the implementation template before writing extraction, cohort-building, or summary scripts.

## `0.3_check_insurance_group_date_overlap.R`

Diagnostic script for `PATIENT_INSURANCE` date-overlap behavior. It creates a small random sample of patients from `komodo_ext.patient_insurance`, pulls all insurance rows for those sampled patients, and checks whether overlapping `row_valid_start` / `row_valid_end` spans contain different known primary `mx_insurance_group` or `rx_insurance_group` values at the same time.

Run this before deciding whether annual eligibility should exclude multiple primary insurance groups or allow multi-group patient-year attribution.

## `0.4_quick_check_annual_eligible_population.R`

Small-sample syntax and logic check for `1.1_build_annual_eligible_population.R`. It intentionally restricts work to a sampled candidate set before running the full insurance-attribution logic. It mirrors the current primary 1.1 criteria: age 40 or older, no `PATIENT_CLOSED` requirement, full-year gap-free insurance attribution, stable non-missing primary Mx/Rx group and segment, and stable optional secondary Mx/Rx group and segment. It uses temporary Redshift tables for the active session, prints aggregate pass/fail counts for each eligibility criterion, and does not save a permanent SQL output. Its output is for code validation only and must not be interpreted as a scientific estimate.

Run this before the full production build when changing annual eligibility SQL.

## `0.5_check_event_table_structure.R`

Small-sample structural diagnostic for `INPATIENT_EVENTS` and `NON_INPATIENT_EVENTS`. It validates the use of inpatient `claim_from_date` and non-inpatient `service_date`, checks that documented array fields contain valid JSON-style arrays, flattens diagnosis and inpatient CPT/HCPCS arrays by exact array position, verifies diagnosis normalization and five-character procedure formats, and checks whether non-inpatient primary diagnoses are already contained in `diagnosis_codes`. It uses temporary Redshift tables and prints aggregate checks without displaying patient identifiers, dates, or individual codes.

Run this before implementing the production CFI diagnosis and CPT/HCPCS extraction described in `Documents/04_INPATIENT_AND_NON_INPATIENT_EVENT_STRUCTURE.md`.

## `0.6_validate_clinical_metric_lookups.R`

Local diagnostic and conversion script for the lookup inputs needed before annual clinical metric processing. It validates the CFI diagnosis/procedure/weight files, parses the supplied CCW SAS diagnosis rules into prefix-match rows, writes the diagnosis-only HIV lookup with the calendar-year claim-window rule, and converts the 20 Gagne SAS format datasets plus `comorb_weight.sas7bdat` into plain CSV lookups. The Gagne conversion preserves SAS format endpoint semantics, including prefix-like sentinel endpoints such as `I42:` or `425[` and exclusive range endpoints such as `C80[`. It writes lookup artifacts and a validation summary to `Documents/Clinical Metric Look Up Tables` and does not connect to Redshift or create persistent write-schema tables.

Run this before shared annual diagnosis extraction. If the Gagne conversion reports that the `haven` package is missing, install it with `renv::install("haven")` and rerun this diagnostic before writing or running the Gagne scoring workflow.

## `0.7_check_krd_table_inventory.R`

Schema-level diagnostic for listing all visible tables in the configured Komodo Redshift read schema, currently `komodo_ext`. It reads the project data dictionary, queries `information_schema.tables`, compares expected dictionary table names with visible Redshift table names, prints the results, and saves `0.7_krd_table_inventory.csv` and `0.7_krd_table_dictionary_comparison.csv` in `Outputs`.

Run this when checking available KRD table names or validating whether a documented table is accessible in the current OHDSI Lab workspace.

## `1.1_build_annual_eligible_population.R`

Production script for building the `1_annual_eligible_cohort` patient-year denominator in the user's Redshift write schema. It derives candidate calendar years dynamically from `PATIENT_INSURANCE`, applies the age criterion of 40 years or older on January 1, uses all overlapping `PATIENT_INSURANCE` rows for each year, requires full-year gap-free insurance attribution, keeps only patient-years with one stable non-missing primary Mx group/segment and one stable non-missing primary Rx group/segment, and also requires optional secondary Mx/Rx group and segment to be stable when present. The current primary definition does not require `PATIENT_CLOSED` coverage.

Run this after the annual eligibility logic has been finalized and after any diagnostics, such as `0.3_check_insurance_group_date_overlap.R`, have been reviewed.

## `1.2_check_annual_eligible_population.R`

Aggregate QA script for the materialized `1_annual_eligible_cohort` table. It should be used after `1.1_build_annual_eligible_population.R` to verify row counts, insurance-category distributions, and other non-patient-level diagnostics needed before downstream annual analyses.

## `1.3_join_race_ethnicity_to_eligible_cohort.R`

Production script for adding the KRD recommended patient-level race/ethnicity variable to the annual eligible cohort. It archives the original race-free table as `work_<username>.1_annual_eligible_cohort_without_race`, collapses `komodo_ext.patient_race_ethnicity` to one row per patient, left joins the result, and saves the race/ethnicity-enhanced cohort back to `work_<username>.1_annual_eligible_cohort` so downstream scripts can use the original table name. It prints aggregate annual counts and race/ethnicity distributions for QA.

Run this after `1.1_build_annual_eligible_population.R` and `1.2_check_annual_eligible_population.R` when race/ethnicity-stratified summaries are needed.

## `2.1_generate_2016_table1.R`

Faster initial summary script. It reports eligible participant counts for all years from 2016 through 2025, but limits detailed insurance summaries and `ohdsilab::k_table1()` to 2016. The detailed summaries cover primary Mx segments, primary Rx segments, combined primary Mx/Rx segments, primary/secondary Mx combinations, and primary/secondary Rx combinations. Result tables are saved as CSV files in `Outputs`.

Run this after `1.1_build_annual_eligible_population.R` and `1.2_check_annual_eligible_population.R`.

## `2.2_generate_annual_table1.R`

Full annual version of the Table 1 workflow. It generates detailed insurance summaries and a separate aggregate Table 1 for every year from 2016 through 2025. It includes the same primary segment and primary/secondary insurance combination summaries as `2.1`, saves all result tables in `Outputs`, and may take substantially longer to complete.

Run this after reviewing the 2016 results from `2.1_generate_2016_table1.R`.

## `2.3_generate_2016_medicare_ffs_table1.R`

Focused subgroup analysis for the 2016 annual eligible population. It selects patients whose primary medical insurance group is `MEDICARE` and whose primary medical insurance segment is `FFS`, verifies the subgroup count, materializes the required `patient_id` and `index_date` cohort, and generates an aggregate Table 1 with `ohdsilab::k_table1()`. The subgroup count and Table 1 are saved as `2.3_medicare_ffs_count_2016.csv` and `2.3_table1_2016_medicare_ffs.csv` in `Outputs`.

Run this after `1.1_build_annual_eligible_population.R` and `1.2_check_annual_eligible_population.R`.

## `2.4_plot_2016_primary_insurance_counts.R`

Reads the existing `2.1_mx_segment_counts_2016.csv` and `2.1_rx_segment_counts_2016.csv` aggregate files without reconnecting to Redshift or recalculating participant counts. It creates two horizontal bar charts of primary Mx and Rx insurance group/segment counts, ordered from the highest participant count to the lowest. The plots are saved as `2.4_primary_mx_segment_participants_2016.png` and `2.4_primary_rx_segment_participants_2016.png` in `Outputs`.

Run this after `2.1_generate_2016_table1.R`.

## `2.5_generate_2021_medicare_ffs_table1.R`

Focused subgroup analysis for the 2021 annual eligible population. It selects patients whose primary medical insurance group is `MEDICARE` and whose primary medical insurance segment is `FFS`, verifies the subgroup count, materializes the required `patient_id` and `index_date` cohort, and generates an aggregate Table 1 with `ohdsilab::k_table1()`. The subgroup count and Table 1 are saved as `2.5_medicare_ffs_count_2021.csv` and `2.5_table1_2021_medicare_ffs.csv` in `Outputs`.

Run this after `1.1_build_annual_eligible_population.R` and `1.2_check_annual_eligible_population.R`.

## `3.1_prepare_2016_cfi_inputs.R`

Validation preparation script for 2016 Claims-Based Frailty Index inputs. It invokes the same year-batched extraction engine as `3.2_prepare_annual_cfi_inputs.R`, but restricts processing to 2016 and writes separate `cfi_2016_ids`, `cfi_2016_dx09`, `cfi_2016_dx10`, and `cfi_2016_px` tables. Use these outputs to confirm that one year fits within available Redshift resources and passes membership, uniqueness, and code-format checks without modifying production annual tables.

Run this after `0.5_check_event_table_structure.R`, `1.1_build_annual_eligible_population.R`, and `1.2_check_annual_eligible_population.R`.

## `3.2_prepare_annual_cfi_inputs.R`

Production preparation script for annual Claims-Based Frailty Index inputs from 2016 through 2025. It creates the full `cfi_annual_ids` denominator, then processes inpatient and non-inpatient events one calendar year at a time to limit peak Redshift temporary-disk use. Each year replaces only its own existing diagnosis and procedure rows before appending deduplicated results, allowing interrupted or selected-year runs to be restarted without duplicating data. The script writes `cfi_annual_ids`, `cfi_annual_dx09`, `cfi_annual_dx10`, and `cfi_annual_px`.

Run `3.1_prepare_2016_cfi_inputs.R` successfully before this production script. See `Documents/05_ANNUAL_CFI_INPUT_PREPARATION.md` for the detailed time-window, extraction, normalization, batching, output-table, and QA definitions.

## `3.3_compute_2016_cfi_scores.R`

Scoring script for the 2016 Claims-Based Frailty Index validation workflow. It consumes the `cfi_2016_ids`, `cfi_2016_dx09`, `cfi_2016_dx10`, and `cfi_2016_px` tables created by `3.1_prepare_2016_cfi_inputs.R`, stages the official CFI diagnosis, procedure, and model-weight lookup files from the local CFI reference package, and materializes one Redshift score row per 2016 eligible patient-year in `cfi_2016_scores`. Patient-level scores remain in Redshift. The script writes aggregate-only QA, descriptive, and category summaries to `Outputs`.

Run this after `3.1_prepare_2016_cfi_inputs.R` succeeds and before extending CFI scoring to all annual production tables.

## `3.4_summarize_2016_cfi_by_subgroups.R`

Aggregate reporting script for the 2016 CFI validation scores. It summarizes `cfi_2016_scores` overall and by age group, sex, primary medical insurance group, and primary prescription insurance group. When `komodo_ext.patient_race_ethnicity` is available, it also adds race/ethnicity summaries. The script reports CFI mean, minimum, Q1, median, Q3, maximum, the number of patient-years with the model-intercept value, and frailty categories using the cut points `<0.15`, `0.15 to <0.25`, `0.25 to <0.35`, `0.35 to <0.45`, and `>=0.45`. It writes aggregate-only CSVs to `Outputs`.

Run this after `3.3_compute_2016_cfi_scores.R` has created `cfi_2016_scores`.

## `4.1_extract_2016_cfi_descriptive_outputs.R`

Redshift extraction script for the 2016 CFI descriptive analysis. It queries aggregate summaries from `cfi_2016_scores` and `cfi_2016_ids`, adds race/ethnicity from the race-enhanced annual eligible cohort when available, and writes reusable aggregate CSVs to `Outputs`. The outputs cover CFI overall and by age group, sex, race/ethnicity, primary Mx group, primary Rx group, primary Mx segment, and primary Rx segment; the Table 1 shows percentages with frailty-group counts in the column headers, and histogram bins are overall only. Run this only when upstream data or analysis definitions change.

Run this after `1.3_join_race_ethnicity_to_eligible_cohort.R`, `3.3_compute_2016_cfi_scores.R`, and `3.4_summarize_2016_cfi_by_subgroups.R`.

## `4.2_visualize_2016_cfi_descriptive_outputs.Rmd`

Fast R Markdown report for visualizing the aggregate CSV files created by `4.1_extract_2016_cfi_descriptive_outputs.R`. It does not connect to Redshift. It reads the saved `4.1_*.csv` files from `Outputs`, renders the frailty-group Table 1, CFI summary table, overall histogram, and box plots.

Run this after `4.1_extract_2016_cfi_descriptive_outputs.R` has successfully generated the CSV files.

## `5.1_prepare_2016_clinical_metric_matches.R`

Validation wrapper for the shared clinical-metric matched-event workflow. It runs `5.2_prepare_annual_clinical_metric_matches.R` with a 2016-only configuration and writes the 2016 rows in `2_annual_metric_ids`, `2_annual_diagnosis_matches`, and `2_annual_procedure_matches`. It is the first Redshift extraction step for the new parallel CFI, CCW, Gagne combined comorbidity score, and HIV status rebuild, and it prints only aggregate QA.

Run this after `0.6_validate_clinical_metric_lookups.R`, `1.1_build_annual_eligible_population.R`, `1.2_check_annual_eligible_population.R`, and `1.3_join_race_ethnicity_to_eligible_cohort.R`.

## `5.2_prepare_annual_clinical_metric_matches.R`

Reusable year-batched engine for preparing the shared clinical-metric matched-event layer. It stages the validated local lookup CSVs in temporary Redshift tables, refreshes selected years in `2_annual_metric_ids`, scans eligible inpatient and non-inpatient events one year at a time, applies conservative lookup-derived candidate prefilters before array flattening when safe, normalizes extracted codes, and persists only final exact, prefix, or range feature matches in `2_annual_diagnosis_matches` and `2_annual_procedure_matches`.

Run `5.1_prepare_2016_clinical_metric_matches.R` successfully before using this script for additional years. Downstream scripts should score CFI, CCW, Gagne, and HIV from these matched-event tables rather than rescanning or reflattening raw diagnosis/procedure events.

## `5.3_calculate_annual_cfi_scores.R`

Annual shared-pipeline CFI scoring script. It consumes `2_annual_metric_ids`, `2_annual_diagnosis_matches`, `2_annual_procedure_matches`, and `0.6_cfi_weight_lookup.csv`, keeps each CFI feature once per patient-year, adds the `0.10288` CFI intercept, and writes one row per eligible patient-year to `annual_cfi_scores`. The default run is 2016 only.

Run this after `5.1_prepare_2016_clinical_metric_matches.R` has created the matched-event tables.

## `5.4_calculate_annual_ccw_variables.R`

Annual shared-pipeline CCW variable script. It consumes `2_annual_diagnosis_matches` and `0.6_ccw_diagnosis_lookup.csv`, creates the long matched condition table, one wide 56-condition indicator table, and the reviewed CCW group-count table. It writes `annual_ccw_conditions_long`, `annual_ccw_condition_indicators`, and `annual_ccw_group_counts`. The default run is 2016 only.

Run this after `5.1_prepare_2016_clinical_metric_matches.R`.

## `5.5_calculate_annual_gagne_comorbidity_score.R`

Annual shared-pipeline Gagne combined comorbidity score script. It consumes `2_annual_diagnosis_matches`, `0.6_gagne_diagnosis_lookup.csv`, and `0.6_gagne_weight_lookup.csv`, keeps each Gagne group once per patient-year, creates all 20 group indicators, and writes `annual_gagne_scores`. Patients with no matched Gagne group receive score zero. The default run is 2016 only.

Run this after `5.1_prepare_2016_clinical_metric_matches.R`.

## `5.6_calculate_annual_hiv_status.R`

Annual shared-pipeline HIV status script. It consumes `2_annual_diagnosis_matches` and `0.6_hiv_diagnosis_lookup.csv`, applies the annual-only confirmation rule directly in SQL, and writes `annual_hiv_status`. HIV status is one for at least one inpatient HIV diagnosis match or at least two distinct non-inpatient HIV diagnosis dates in the same patient-year; status is not carried forward. The default run is 2016 only.

Run this after `5.1_prepare_2016_clinical_metric_matches.R`.

## `5.7_build_annual_clinical_metrics.R`

Final shared annual clinical-metrics table builder. It joins `2_annual_metric_ids` to completed CFI, CCW, Gagne, and HIV outputs and writes one row per eligible patient-year to `annual_clinical_metrics_shared`. It discovers the wide CCW and Gagne indicator columns from the upstream tables rather than hard-coding them. The default run is 2016 only.

Run this after `5.3`, `5.4`, `5.5`, and `5.6` have completed successfully.

## `5.8_check_annual_clinical_metrics.R`

Aggregate QA script for the shared annual clinical-metric pipeline. It checks selected-year row counts, duplicate keys, matched-event counts, CFI intercept flags, CCW and Gagne indicator counts, HIV confirmation-rule consistency, and final-table completeness. It writes aggregate results to `Outputs/5.8_annual_clinical_metrics_qa.csv`.

Run this after `5.7_build_annual_clinical_metrics.R`.
