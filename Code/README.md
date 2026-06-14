# Code Folder README

This folder contains the R scripts used to connect to Komodo, prototype claims workflows, diagnose annual eligibility assumptions, build the annual eligible population, generate aggregate summaries, and prepare claims-based index inputs. Scripts should be run in numeric order unless a task-specific note says otherwise.

Numeric order logic: 0.xx: code for testing/checking/diagnosis; 1.xx: code to generate the study sample; 2.xx: code to do summary analyses; 3.xx: code to prepare and calculate claims-based indices

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

Run this before implementing the production CFI diagnosis and CPT/HCPCS extraction described in `Documents/INPATIENT_AND_NON_INPATIENT_EVENT_STRUCTURE.md`.

## `1.1_build_annual_eligible_population.R`

Production script for building the `1_annual_eligible_cohort` patient-year denominator in the user's Redshift write schema. It derives candidate calendar years dynamically from `PATIENT_INSURANCE`, applies the age criterion of 40 years or older on January 1, uses all overlapping `PATIENT_INSURANCE` rows for each year, requires full-year gap-free insurance attribution, keeps only patient-years with one stable non-missing primary Mx group/segment and one stable non-missing primary Rx group/segment, and also requires optional secondary Mx/Rx group and segment to be stable when present. The current primary definition does not require `PATIENT_CLOSED` coverage.

Run this after the annual eligibility logic has been finalized and after any diagnostics, such as `0.3_check_insurance_group_date_overlap.R`, have been reviewed.

## `1.2_check_annual_eligible_population.R`

Aggregate QA script for the materialized `1_annual_eligible_cohort` table. It should be used after `1.1_build_annual_eligible_population.R` to verify row counts, insurance-category distributions, and other non-patient-level diagnostics needed before downstream annual analyses.

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

## `3.1_prepare_annual_cfi_inputs.R`

Production preparation script for annual Claims-Based Frailty Index inputs from 2016 through 2025. It uses the materialized `1_annual_eligible_cohort` without adding a `PATIENT_CLOSED` requirement, assigns claims from each calendar year to a patient-year CFI index date of January 1 in the following year, and materializes `cfi_annual_ids`, `cfi_annual_dx09`, `cfi_annual_dx10`, and `cfi_annual_px` in the user's Redshift write schema. Inpatient events use `claim_from_date`; non-inpatient events use `service_date`. Diagnosis and inpatient CPT/HCPCS arrays are expanded through their complete observed lengths without a fixed cap, codes are normalized, and patient-year/code combinations are deduplicated.

Run this after `0.5_check_event_table_structure.R`, `1.1_build_annual_eligible_population.R`, and `1.2_check_annual_eligible_population.R`. See `Documents/ANNUAL_CFI_INPUT_PREPARATION.md` for the detailed time-window, extraction, normalization, output-table, and QA definitions. The resulting tables are inputs for the subsequent CFI lookup and scoring script.
