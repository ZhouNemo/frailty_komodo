library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto 2016 shared clinical metric matching
# Author: Nemo Zhou
# Date started: 2026-06-27
# Date last updated: 2026-06-27
#
# ---- Purpose ----
# Run the shared clinical metric matched-event preparation workflow for 2016
# only. This is the validation path before expanding the workflow year by year
# from 2016 through 2025 in Code/5.2_prepare_annual_clinical_metric_matches.R.
#
# The script writes or updates the 2016 rows in:
#   - 2_annual_metric_ids
#   - 2_annual_diagnosis_matches
#   - 2_annual_procedure_matches
#
# Patient-level matched events remain in Redshift. Only aggregate QA results are
# printed to the console.

previous_clinical_metric_config <- getOption("frailty.clinical_metrics.config")

options(
  "frailty.clinical_metrics.config" = list(
    analysis_years = 2016L,
    id_years = 2016L,
    ids_table = "2_annual_metric_ids",
    diagnosis_matches_table = "2_annual_diagnosis_matches",
    procedure_matches_table = "2_annual_procedure_matches",
    workflow_label = "2016 validation",
    # Keep the performance-only array prefilter off (matches the 5.2 default) so
    # the 2016 validation run cannot silently drop a true match. Re-enable only
    # after a prefilter-on vs prefilter-off match-count parity check passes.
    enable_candidate_prefilter = FALSE
  )
)

tryCatch(
  source("Code/5.2_prepare_annual_clinical_metric_matches.R"),
  finally = options(
    "frailty.clinical_metrics.config" = previous_clinical_metric_config
  )
)
