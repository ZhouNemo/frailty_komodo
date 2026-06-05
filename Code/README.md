# Code Folder README

This folder contains the R scripts used to connect to Komodo, prototype claims workflows, diagnose annual eligibility assumptions, build the annual eligible population, and check aggregate outputs. Scripts should be run in numeric order unless a task-specific note says otherwise.

## `0.1_connect to Komodo.R`

Initial connection script for the OHDSI Lab Komodo Redshift environment. It loads the core database packages, sets the JDBC driver folder, and establishes the standard Redshift connection pattern used by later scripts.

Run this first when validating credentials, JDBC configuration, or basic access to the Komodo workspace.

## `0.2_model_komodo_workflow.R`

Reusable model workflow for future Komodo analyses. It demonstrates the preferred project pattern for connecting to Redshift, defining `komodo_schema` and `write_schema`, using `ohdsilab::k_get_condition_events()` and `ohdsilab::k_get_procedure_events()`, materializing a cohort table, and generating an aggregate Table 1.

Use this as the implementation template before writing extraction, cohort-building, or summary scripts.

## `0.3_check_insurance_group_date_overlap.R`

Diagnostic script for `PATIENT_INSURANCE` date-overlap behavior. It creates a small random sample of patients from `komodo_ext.patient_insurance`, pulls all insurance rows for those sampled patients, and checks whether overlapping `row_valid_start` / `row_valid_end` spans contain different known primary `mx_insurance_group` or `rx_insurance_group` values at the same time.

Run this before deciding whether annual eligibility should exclude multiple primary insurance groups or allow multi-group patient-year attribution.

## `1.1_build_annual_eligible_population.R`

Production script for building the `1_annual_eligible_cohort` patient-year denominator in the user's Redshift write schema. It derives candidate calendar years dynamically from `PATIENT_INSURANCE`, applies the age criterion of 40 years or older on January 1, uses all overlapping `PATIENT_INSURANCE` rows for each year, requires full-year gap-free insurance attribution, keeps only patient-years with one stable non-missing primary Mx group/segment and one stable non-missing primary Rx group/segment, and also requires optional secondary Mx/Rx group and segment to be stable when present. The current primary definition does not require `PATIENT_CLOSED` coverage.

Run this after the annual eligibility logic has been finalized and after any diagnostics, such as `0.3_check_insurance_group_date_overlap.R`, have been reviewed.

## `0.4_quick_check_annual_eligible_population.R`

Small-sample syntax and logic check for `1.1_build_annual_eligible_population.R`. It intentionally restricts work to a sampled candidate set before running the full insurance-attribution logic. It mirrors the current primary 1.1 criteria: age 40 or older, no `PATIENT_CLOSED` requirement, full-year gap-free insurance attribution, stable non-missing primary Mx/Rx group and segment, and stable optional secondary Mx/Rx group and segment. It uses temporary Redshift tables for the active session, prints aggregate pass/fail counts for each eligibility criterion, and does not save a permanent SQL output. Its output is for code validation only and must not be interpreted as a scientific estimate.

Run this before the full production build when changing annual eligibility SQL.

## `1.2_check_annual_eligible_population.R`

Aggregate QA script for the materialized `1_annual_eligible_cohort` table. It should be used after `1.1_build_annual_eligible_population.R` to verify row counts, insurance-category distributions, and other non-patient-level diagnostics needed before downstream annual analyses.
