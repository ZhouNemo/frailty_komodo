# Code Folder README

Project: Frailty_Komoto
Author: Nemo Zhou
Date started: 2026-06-16
Date last updated: 2026-07-04

This folder contains the R and helper scripts used to connect to Komodo,
diagnose source tables, build annual eligibility cohorts, generate aggregate
summaries, run the normalized annual clinical-metrics pipeline, and run the
annual polypharmacy pipeline. Scripts should be run in numeric order unless a
task-specific note says otherwise.

The previous CFI and raw-event clinical-metrics processing scripts are archived
in `Code/Old/`. The active clinical-metrics pathway now follows
`Documents/01_CLINICAL_METRICS_DATA_PROCESSING_FLOW.md` and uses:

```text
komodo_ext.normalized_dx_events
komodo_ext.normalized_procedure_events
```

It does not stage raw inpatient/non-inpatient events, flatten arrays, apply
candidate event prefilters, or apply the older first-25-array-elements cap.

## Numeric Order

- `0.xx`: connection, diagnostics, source validation, and lookup conversion.
- `1.xx`: annual eligible cohort construction and cohort QA.
- `2.xx`: aggregate Table 1 and subgroup summary scripts.
- `3.xx`: active normalized annual clinical-metrics build pipeline.
- `4.xx`: descriptive analyses using completed annual analysis tables.
- `5.xx`: active annual polypharmacy build pipeline.
- `Code/Old`: historical scripts that are no longer part of the active workflow,
  retained for provenance or scoring-engine reuse. `Documents/Old` and
  `Outputs/Old` follow the same convention for historical documents and outputs.

## `0.1_connect to Komodo.R`

Initial connection script for the OHDSI Lab Komodo Redshift environment. It
loads the core database packages, sets the JDBC driver folder, and establishes
the standard Redshift connection pattern used by later scripts.

Run this first when validating credentials, JDBC configuration, or basic access
to the Komodo workspace.

## `0.2_model_komodo_workflow.R`

Reusable model workflow for future Komodo analyses. It demonstrates the
preferred project pattern for connecting to Redshift, defining `komodo_schema`
and `write_schema`, using `ohdsilab::k_get_condition_events()` and
`ohdsilab::k_get_procedure_events()`, materializing a cohort table, and
generating an aggregate Table 1.

Use this as the implementation template before writing unrelated new extraction,
cohort-building, or summary scripts.

## `0.3_check_insurance_group_date_overlap.R`

Diagnostic script for `PATIENT_INSURANCE` date-overlap behavior. It creates a
small random sample of patients from `komodo_ext.patient_insurance`, pulls
insurance rows for those sampled patients, and checks overlapping
`row_valid_start` / `row_valid_end` spans for simultaneous primary insurance
group differences.

Run this before changing annual insurance-attribution rules.

## `0.4_quick_check_annual_eligible_population.R`

Small-sample syntax and logic check for
`1.1_build_annual_eligible_population.R`. It uses temporary Redshift tables and
prints aggregate pass/fail counts for each eligibility criterion. Its output is
for code validation only and must not be interpreted as a scientific estimate.

Run this before the full production build when changing annual eligibility SQL.

## `0.5_check_event_table_structure.R`

Historical raw-event structural diagnostic for `INPATIENT_EVENTS` and
`NON_INPATIENT_EVENTS`. It remains useful for source provenance, but the active
clinical-metrics pipeline no longer depends on raw event flattening.

Run this only when investigating raw KRD event structure or updating the raw
event provenance reference.

## `0.6_validate_clinical_metric_lookups.R`

Local diagnostic and conversion script for lookup inputs needed before annual
clinical metric processing. It validates the CFI diagnosis/procedure/weight
files, parses CCW diagnosis rules, writes the diagnosis-only HIV lookup, and
converts the supplied Gagne SAS assets into plain CSV lookups. It writes lookup
artifacts and a validation summary to:

```text
Documents/Clinical Metric Look Up Tables
```

Run this before the normalized annual clinical-metrics pipeline.

## `0.7_check_krd_table_inventory.R`

Schema-level diagnostic for listing visible tables in the configured Komodo
Redshift read schema, currently `komodo_ext`. It compares expected dictionary
table names with visible Redshift table names and saves aggregate inventory
outputs under `Outputs`.

Run this when checking available KRD table names, including normalized source
tables.

## `1.1_build_annual_eligible_population.R`

Production script for building the `1_annual_eligible_cohort` patient-year
denominator in the user's Redshift write schema. It derives candidate calendar
years dynamically from `PATIENT_INSURANCE`, applies the age criterion, requires
full-year gap-free insurance attribution, and saves one eligible patient-year
row per included patient and analysis year.

Run this after annual eligibility logic has been finalized.

## `1.2_check_annual_eligible_population.R`

Aggregate QA script for the materialized `1_annual_eligible_cohort` table. It
verifies row counts, insurance-category distributions, and other non-patient
level diagnostics needed before downstream annual analyses.

Run this after `1.1_build_annual_eligible_population.R`.

## `1.3_join_race_ethnicity_to_eligible_cohort.R`

Production script for adding the KRD recommended patient-level race/ethnicity
variable to the annual eligible cohort. It archives the original race-free
cohort as `1_annual_eligible_cohort_without_race`, collapses
`komodo_ext.patient_race_ethnicity` to one row per patient, and saves the
race-enhanced cohort back to `1_annual_eligible_cohort`.

Run this after `1.1` and `1.2` when race/ethnicity-stratified summaries or
clinical metrics are needed.

## `2.1_generate_2016_table1.R`

Faster initial summary script. It reports eligible participant counts for all
years from 2016 through 2025, but limits detailed insurance summaries and
`ohdsilab::k_table1()` to 2016. Result tables are saved under `Outputs`.

Run this after `1.1` and `1.2`.

## `2.2_generate_annual_table1.R`

Full annual Table 1 workflow. It generates detailed insurance summaries and a
separate aggregate Table 1 for every year from 2016 through 2025. Result tables
are saved under `Outputs`.

Run this after reviewing the 2016 results from `2.1`.

## `2.3_generate_2016_medicare_ffs_table1.R`

Focused subgroup analysis for 2016 Medicare FFS patient-years. It materializes
the required `patient_id` and `index_date` cohort and generates an aggregate
Table 1 with `ohdsilab::k_table1()`.

Run this after `1.1` and `1.2`.

## `2.4_plot_2016_primary_insurance_counts.R`

Reads existing `2.1` aggregate CSV files and creates horizontal bar charts of
primary Mx and Rx insurance group/segment counts. It does not reconnect to
Redshift.

Run this after `2.1_generate_2016_table1.R`.

## `2.5_generate_2021_medicare_ffs_table1.R`

Focused subgroup analysis for 2021 Medicare FFS patient-years. It materializes
the required `patient_id` and `index_date` cohort and generates an aggregate
Table 1 with `ohdsilab::k_table1()`.

Run this after `1.1` and `1.2`.

## `3.0_normalized_clinical_metrics_helpers.R`

Shared helper file for the active normalized `3.x` clinical-metrics pipeline.
It defines the standard configuration, Redshift connection, SQL quoting,
lookup loading, table validation, batched CSV lookup staging, event-window
predicate builders, configurable external-scan chunk helpers, connection cleanup,
and a bounded SQL retry wrapper. It does not create Redshift tables when
sourced by itself.

`event_window_sql()` emits the event-date filter using the bare event-date
column (no `CAST` wrapper) with a half-open upper bound, so external Parquet
scans stay prunable. Single-year or explicit `event_start_date` /
`event_end_date` runs use static date literals. Multi-year runs use static
outer bounds derived from the selected years plus the correlated patient-year
predicate, so Spectrum still gets a pushdown-prunable range while patient-year
attribution remains correct. `event_scan_chunks()` and
`event_chunk_window_sql()` split long external scans into configurable chunks:
`year` by default, `quarter` and `month` after measuring whether Parquet
row-group pruning makes finer chunks cheap, or `all` when a single scan over
the full selected-year window is preferred.
`execute_sql_with_retry()` re-runs idempotent or temp-staged SQL batches with
linear backoff to survive transient Spectrum scan errors; optional reconnection
is supported only for retry units that do not depend on session temp tables.

The main override option is:

```r
options("frailty.normalized_clinical_metrics.config" = list(...))
```

The default configuration processes 2016 only. Single-year runs automatically
use static full-year event bounds to keep external scans pushdown-prunable.

## `3.1_prepare_annual_metric_ids.R`

Normalized denominator preparation script. It creates or refreshes selected
years in `2_annual_metric_ids` from `1_annual_eligible_cohort`, preserving
patient-year dates, demographics, race/ethnicity when available, and annual
insurance fields. `patient_id` is retained for normalized event joins and
`patid` is retained as the downstream patient-year key.

Run this after `0.6`, `1.1`, `1.2`, and `1.3`.

## `3.4_prepare_annual_code_presence.R`

Builds compact patient-year CFI-relevant procedure code presence from
`komodo_ext.normalized_procedure_events`. It joins `2_annual_metric_ids` by
`patient_id`, restricts `event_date` to the patient-year window (bare-column,
half-open bounds via the helper predicates), filters `procedure_code` to the
reviewed CFI CPT/HCPCS lookup ranges, and writes the following table. The
external procedure scan is run in configurable retry-wrapped chunks into a temp
build table, then the persistent selected-year slice is replaced from a final
`SELECT DISTINCT`. The default chunk granularity is one year, because monthly
chunks can multiply scans on a flat, unsorted Parquet prefix. Use
`event_scan_chunk_by = "month"` or `"quarter"` only after checking Spectrum
bytes scanned, and use `"all"` when a single selected-window scan is preferred:

```text
2_annual_procedure_code_presence
```

The active diagnosis path does not materialize full patient-year diagnosis code
presence. `3.6_match_annual_clinical_metric_features.R` matches
`komodo_ext.normalized_dx_events` directly to the reviewed diagnosis lookup
rules and writes compact feature matches instead.

Run this after `3.1_prepare_annual_metric_ids.R`.

## `3.5_prepare_annual_hiv_diagnosis_evidence.R`

Builds compact HIV-only diagnosis evidence from
`komodo_ext.normalized_dx_events`. It keeps `event_date` as `diagnosis_date`,
derives `claim_setting` from `source_table`, applies the exact HIV diagnosis
lookup, and writes:

```text
2_annual_hiv_diagnosis_evidence
```

Run this after `3.1_prepare_annual_metric_ids.R`.

## `3.6_match_annual_clinical_metric_features.R`

Stages the reviewed diagnosis lookup rules, joins
`2_annual_metric_ids` directly to `komodo_ext.normalized_dx_events`, and writes
compact lookup-filtered CFI, CCW, and Gagne diagnosis feature matches without
building all-code diagnosis presence. It first builds a selected-year temporary
ID stage distributed by `patient_id`, then builds a **persistent, restartable**
diagnosis candidate stage (`2_annual_dx_candidate_stage`) containing only
diagnosis codes whose prefixes can match CFI, CCW, or Gagne lookup rules.

The candidate stage banks the expensive Spectrum scan of the external diagnosis
table using bare-column half-open scan windows. The scan runs in configurable
retry-wrapped chunks, defaulting to one year per chunk; each chunk is split by
literal candidate-prefix length so Redshift can hash-join each prefix branch.
Finer chunks (`quarter` or `month`) should be used only after measuring S3
bytes scanned or when accepting extra scan exposure as a reliability experiment;
`all` is available when a single full selected-window scan is the better
tradeoff. Chunks land in a raw TEMP build table, are collapsed to distinct
patient-year diagnosis codes, and validate before persistent rows are replaced.
The manifest is invalidated before the
persistent data slice is touched, so a failed replace cannot leave a completed
manifest pointing at missing or partial data. A companion manifest
(`2_annual_dx_candidate_stage_manifest`) records the scan window, prefix length,
prefix set, and lookup version(s) per built year. Set
`reuse_candidate_stage = TRUE` to skip the scan and resume at feature matching,
which reuses the banked stage only when every requested year's manifest matches
this run (so a stale, smoke, or differently-parameterized stage is rebuilt).
The manifest does not fingerprint every row in `2_annual_metric_ids`, so
candidate-stage reuse assumes the selected-year denominator has not changed
since the stage was banked; keep the default `reuse_candidate_stage = FALSE`
after rebuilding `3.1_prepare_annual_metric_ids.R`.
Compact features are matched from that candidate stage by first building a
small distinct-code-to-feature map and then hash-joining the map back to the
patient-year candidate stage. CFI-relevant procedure presence uses the same
distinct-code map pattern before being carried into CFI feature matches. It
writes:

```text
2_annual_dx_candidate_stage
2_annual_dx_candidate_stage_manifest
2_annual_cfi_feature_matches
2_annual_ccw_condition_matches
2_annual_gagne_group_matches
```

Run this after `3.4_prepare_annual_code_presence.R`.

## `3.7_calculate_normalized_annual_cfi_scores.R`

Calculates CFI scores from `2_annual_cfi_feature_matches`, joins CFI weights,
adds the `0.10288` intercept, and writes:

```text
6_annual_cfi_scores
```

Run this after `3.6_match_annual_clinical_metric_features.R`.

## `3.8_calculate_normalized_annual_ccw_variables.R`

Calculates CCW long condition rows, wide condition indicators, and reviewed
group counts from `2_annual_ccw_condition_matches`. It writes:

```text
6_annual_ccw_conditions_long
6_annual_ccw_condition_indicators
6_annual_ccw_group_counts
```

Run this after `3.6_match_annual_clinical_metric_features.R`.

## `3.9_calculate_normalized_annual_gagne_score.R`

Calculates Gagne combined comorbidity scores and group indicators from
`2_annual_gagne_group_matches`. It writes:

```text
6_annual_gagne_scores
```

Run this after `3.6_match_annual_clinical_metric_features.R`.

## `3.10_calculate_normalized_annual_hiv_status.R`

Calculates annual-only, diagnosis-based HIV status from
`2_annual_hiv_diagnosis_evidence`. The confirmation rule remains one inpatient
HIV diagnosis evidence date or at least two distinct non-inpatient HIV
diagnosis dates in the same patient-year. It writes:

```text
6_annual_hiv_status
```

Run this after `3.5_prepare_annual_hiv_diagnosis_evidence.R`.

## `3.11_build_normalized_annual_clinical_metrics.R`

Builds the final normalized annual clinical-metrics table by joining
`2_annual_metric_ids` to the completed CFI, CCW, Gagne, and HIV outputs. It
writes:

```text
6_annual_clinical_metrics_shared
```

Run this after `3.7`, `3.8`, `3.9`, and `3.10` have completed successfully.

## `3.12_check_normalized_annual_clinical_metrics.R`

Aggregate QA script for the normalized pipeline. It checks normalized source
schemas, selected-year row counts, duplicate keys, compact extraction counts,
HIV confirmation-rule consistency, and final-table completeness. It writes:

```text
Outputs/3.12_normalized_annual_clinical_metrics_qa.csv
```

Run this after `3.11_build_normalized_annual_clinical_metrics.R`.

## `3.13_run_normalized_annual_clinical_metrics.R`

Run-all wrapper for the normalized `3.x` flow. It sources `3.1`, `3.4`,
`3.5`, `3.6`, `3.7`, `3.8`, `3.9`, `3.10`, `3.11`, and `3.12` in order using
the current `frailty.normalized_clinical_metrics.config` option. The default
configuration is 2016 only.

Use this only after reviewing the configured years and confirming that selected
years should be refreshed.

## `4.1_describe_annual_clinical_metrics.R`

Aggregate descriptive analysis script for the completed normalized annual
clinical metrics table. It reads `6_annual_clinical_metrics_shared`, keeps
patient-level rows in Redshift, and writes aggregate CSVs plus PNG plots under:

```text
Outputs/4.1_annual_clinical_metrics_descriptive
```

The script produces CFI and Gagne overall summaries, histograms, and
aggregate-statistic box plots by age group, sex, primary medical insurance,
medical insurance segment, and race/ethnicity, with one combined subgroup
box-plot figure per metric. It also produces CCW condition and group prevalence
tables, CCW burden by age/sex/payer/race, HIV-stratified CCW burden by
age/sex/payer, and categorical/continuous Table 1 summaries by CFI frailty
level. It also writes long categorical Table 1 source files by CFI frailty
level, Gagne score level, and HIV status for the R Markdown report to format
with group denominators in headers and percentages in cells. Gagne Table 1
columns use the bands `Gagne <0`, `Gagne 0`, `Gagne 1-2`, `Gagne 3-5`, and
`Gagne 6+`. Small cells below 11 are suppressed in exported aggregate tables.

Run this after `3.11_build_normalized_annual_clinical_metrics.R` and
`3.12_check_normalized_annual_clinical_metrics.R` have completed successfully.

## `4.2_visualize_annual_clinical_metrics_descriptive_outputs.Rmd`

CSV-only R Markdown report for the normalized annual clinical metrics
descriptive outputs. It reads files from
`Outputs/4.1_annual_clinical_metrics_descriptive` and does not reconnect to
Redshift. The report is intentionally lean: for CFI and Gagne it presents the
histogram, subgroup box plots, stacked metric-level subgroup bar plots, and
Table 1 in that order; it also presents an HIV-status Table 1, HIV-status
stacked subgroup bar plots, and overall CCW group/disease prevalence tables.
Table 1 headers include the total N for each metric level, and Table 1 cells
show only percentages. CCW prevalence cells show count and percentage.

Run this after `4.3_prepare_annual_clinical_metrics_visualization_inputs.R`
has generated the visualization-input CSV files.

## `4.3_prepare_annual_clinical_metrics_visualization_inputs.R`

CSV-only preparation script for plots used by
`4.2_visualize_annual_clinical_metrics_descriptive_outputs.Rmd`. It reads the
existing aggregate categorical Table 1 CSV files from
`Outputs/4.1_annual_clinical_metrics_descriptive` and writes within-subgroup
metric-level distribution CSVs for stacked percentage bar plots:

```text
4.3_cfi_level_distribution_by_subgroup.csv
4.3_gagne_level_distribution_by_subgroup.csv
4.3_hiv_status_distribution_by_subgroup.csv
```

It does not connect to Redshift and does not rerun `4.1`.

Run this after `4.1_describe_annual_clinical_metrics.R` has generated the
categorical Table 1 CSV files, and before knitting `4.2`.

## `4.4_run_annual_clinical_metrics_descriptive_analysis.R`

Year-specific wrapper for the completed normalized annual clinical metrics
descriptive workflow. It sets `analysis_years` and `id_years` for one selected
year, writes aggregate `4.1` and `4.3` outputs to a year-specific folder such
as:

```text
Outputs/4.1_annual_clinical_metrics_descriptive_2025
```

By default it also renders
`Code/4.2_visualize_annual_clinical_metrics_descriptive_outputs.Rmd` to a
year-specific HTML file in that same folder, so a 2025 run does not overwrite
2016 descriptive outputs. Edit `analysis_year <- 2025L` at the top of the file
or pass a year as the first command-line argument.

## `5.0_annual_polypharmacy_helpers.R`

Shared helper file for the active `5.x` annual polypharmacy pipeline. It sources
the existing normalized clinical-metrics helper utilities, then defines
polypharmacy-specific configuration, pharmacy fill-window helpers, transaction
filter helpers, NDC11 export paths, and n2c/RxNav mapping metadata. It does not
create Redshift tables when sourced by itself.

The main override option is:

```r
options("frailty.annual_polypharmacy.config" = list(...))
```

The default configuration processes 2016 only, reuses `2_annual_metric_ids` as
the patient-year denominator, does not apply `PATIENT_CLOSED` or `RX CLOSED`
coverage filters, and expects a reviewed transaction-status decision before
`5.1` extracts pharmacy fills. Configure one or more of
`transaction_result_keep`, `transaction_status_keep`, or
`transaction_source_type_keep` after reviewing aggregate transaction values.
For an exploratory unfiltered run only, set
`allow_unfiltered_transactions = TRUE`. The reviewed NDC11-to-ATC crosswalk CSV
is expected at:

```text
Outputs/5.3_polypharmacy_ndc11_atc_crosswalk.csv
```

## `5.1_prepare_polypharmacy_pharmacy_fills.R`

Builds cleaned selected-year pharmacy fills from `komodo_ext.pharmacy_events`
after joining to `2_annual_metric_ids` by `patient_id`. It validates the
confirmed pharmacy schema fields, keeps character 11-digit NDC11 values,
requires positive days supply, and applies reviewed transaction filters from
the `frailty.annual_polypharmacy.config` option. It stops unless a reviewed
filter is configured or `allow_unfiltered_transactions = TRUE` is set for an
exploratory run. It writes durable pre/post extraction QA for excluded fills,
days-supply buckets, and transaction values. It writes:

```text
2_polypharmacy_pharmacy_fills
2_polypharmacy_fill_extraction_qa
```

Run this after `3.1_prepare_annual_metric_ids.R`.

## `5.2_export_polypharmacy_unique_ndc11.R`

Builds the selected-year unique NDC11 table from cleaned pharmacy fills and
exports one NDC11 per line for n2c/RxNav mapping. The local export contains drug
codes only, not patient-level rows. It writes:

```text
2_polypharmacy_unique_ndc11
Outputs/5.2_polypharmacy_unique_ndc11_<years>.txt
```

Run this after `5.1_prepare_polypharmacy_pharmacy_fills.R`, then map the text
file with n2c before staging the crosswalk.

## `5.25_download_and_run_polypharmacy_n2c_mapping.ps1`

PowerShell helper for the one-time external n2c/RxNav mapping step between
`5.2` and `5.3`. It downloads n2c into the project root if needed, creates a
project-local Python virtual environment under `n2c/.venv`, installs n2c's
Python dependencies, runs ATC4 mapping on the exported unique NDC11 text file,
and copies the completed n2c CSV to:

```text
Outputs/5.3_polypharmacy_ndc11_atc_crosswalk.csv
```

This script only needs to be run once per exported unique-NDC list and mapping
version. If the crosswalk CSV already exists, the script exits without rerunning
the multi-hour RxNav mapping unless `-Force` is supplied. Reuse the staged
crosswalk for later `5.3` through `5.6` reruns unless the NDC input or mapping
version intentionally changes.

Run this after `5.2_export_polypharmacy_unique_ndc11.R` and before
`5.3_stage_polypharmacy_ndc11_atc_crosswalk.R`.

## `5.3_stage_polypharmacy_ndc11_atc_crosswalk.R`

Stages a versioned NDC11-to-ATC4/ATC3 crosswalk from an n2c/RxNav output CSV or
reviewed crosswalk CSV with an NDC column and an ATC4, ATC3, or generic ATC
code column. The script expands multiple ATC codes packed into one input cell,
keeps unmapped selected-year NDC11 values as `mapping_status = 'unmapped'` for
coverage QA, converts ATC4 inputs to ATC3, and retains ATC3 inputs as ATC3-level
mappings instead of mislabeled ATC4. It writes:

```text
2_polypharmacy_ndc11_atc_crosswalk
```

Run this after saving the crosswalk CSV to the configured
`crosswalk_input_path`.

## `5.4_build_annual_polypharmacy_exposures.R`

Joins cleaned pharmacy fills to the staged NDC11-to-ATC crosswalk and builds
calendar-year-clipped ATC3 exposure episodes using
`fill_date + days_supply - 1`. One fill can produce multiple exposure rows when
an NDC maps to multiple ATC classes. It writes:

```text
2_annual_polypharmacy_exposure_episodes
```

Run this after `5.3_stage_polypharmacy_ndc11_atc_crosswalk.R`.

## `5.5_calculate_annual_polypharmacy_metrics.R`

Expands clipped ATC3 exposure episodes to patient-day active class counts using
a day-offset table sized from the selected `2_annual_metric_ids` analysis
windows, deduplicates multiple drugs in the same ATC3 class on the same day,
and flags polypharmacy when at least 5 ATC3 classes are active on at least
90 days in the patient-year. It left joins results back to every selected row
in `2_annual_metric_ids` and writes:

```text
6_annual_polypharmacy_metrics
```

Run this after `5.4_build_annual_polypharmacy_exposures.R`.

## `5.6_check_annual_polypharmacy_metrics.R`

Aggregate QA script for the annual polypharmacy pipeline. It checks source
schema fields, selected-year denominator and fill counts, durable extraction QA
from `2_polypharmacy_fill_extraction_qa`, transaction-value counts before and
after filtering, days-supply buckets before and after exclusions, NDC mapping
coverage by unique NDC11 and fill row, multiple ATC4/ATC3 mappings,
exposure-episode validity, final-table completeness, duplicate keys, and
prescription-insurance prevalence summaries. Counts below 11 in the insurance
prevalence section are suppressed before CSV export. It writes:

```text
Outputs/5.6_annual_polypharmacy_metrics_qa.csv
```

Run this after `5.5_calculate_annual_polypharmacy_metrics.R`.

## `5.7_run_annual_polypharmacy.R`

Run-all wrapper for the `5.x` annual polypharmacy flow. It sources `5.1`,
`5.2`, `5.3`, `5.4`, `5.5`, and `5.6` in order. The default configuration is
2016 only. The runner sets the reviewed transaction filter
`transaction_result_keep = c("PAID")` and a date-stamped
`mapping_version_date` in `frailty.annual_polypharmacy.config` before sourcing
the stages (restoring any prior option value on exit), so the pipeline runs end
to end without stopping at `5.1`. The `PAID`-only decision and its supporting
2016 transaction-value counts are documented in
`Documents/12_POLYPHARMACY_DATA_PROCESSING_FLOW.md`.

Use this after reviewing the configured years. The runner will stop at `5.3`
with a clear message if the n2c/RxNav crosswalk CSV has not yet been saved.

## `Code/Old`

Historical scripts from the previous CFI and raw-event clinical-metrics
workflows, including the old raw-event prefilter and optimized smoke-test
diagnostics. These files are no longer part of the active workflow. They are
kept for provenance and, where explicitly sourced by the active normalized
wrappers, for reuse of already-validated scoring SQL. `Documents/Old` and
`Outputs/Old` follow the same convention for historical documents and outputs.
