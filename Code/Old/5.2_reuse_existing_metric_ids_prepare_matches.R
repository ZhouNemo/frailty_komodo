library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual shared clinical metric matching
# Author: Nemo Zhou
# Date started: 2026-06-28
# Date last updated: 2026-06-28
#
# ---- Purpose ----
# Run the 2016 shared clinical metric matched-event preparation workflow while
# reusing an already materialized 2_annual_metric_ids denominator. This avoids
# redoing the denominator refresh when the 2016 ID rows have already been
# validated and only diagnosis/procedure feature matches need to be filled.
#
# The script updates selected years in:
#   - 2_annual_diagnosis_matches
#   - 2_annual_procedure_matches
#
# It validates that 2_annual_metric_ids has nonmissing, unique patid rows for
# 2016 before matching. Patient-level matched events remain in
# Redshift. Only aggregate QA results are printed to the console.

previous_clinical_metric_config <- getOption("frailty.clinical_metrics.config")

options(
  "frailty.clinical_metrics.config" = list(
    analysis_years = 2016L,
    id_years = 2016L,
    ids_table = "2_annual_metric_ids",
    diagnosis_matches_table = "2_annual_diagnosis_matches",
    procedure_matches_table = "2_annual_procedure_matches",
    workflow_label = "2016 reuse existing metric IDs",
    refresh_metric_ids = FALSE,
    enable_candidate_prefilter = FALSE
  )
)

tryCatch(
  source("Code/5.2_prepare_annual_clinical_metric_matches.R"),
  finally = options(
    "frailty.clinical_metrics.config" = previous_clinical_metric_config
  )
)
