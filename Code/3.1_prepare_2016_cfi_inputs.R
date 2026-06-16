library(ohdsilab)
library(DatabaseConnector)
library(dplyr)
library(dbplyr)
library(keyring)
library(DBI)

# Project: Frailty_Komoto 2016 CFI input preparation
# Author: Nemo Zhou
# Date started: 2026-06-15
# Date last updated: 2026-06-15
#
# ---- Purpose ----
# Run the annual Claims-Based Frailty Index (CFI) input preparation workflow
# for 2016 only. This is the validation path to run before the full year-batched
# production workflow in Code/3.2_prepare_annual_cfi_inputs.R.
#
# The script writes separate 2016 validation tables:
#   - cfi_2016_ids
#   - cfi_2016_dx09
#   - cfi_2016_dx10
#   - cfi_2016_px
#
# Patient-level data remain in Redshift. Only aggregate QA results are printed.

previous_cfi_config <- getOption("frailty.cfi.config")

options(
  "frailty.cfi.config" = list(
    analysis_years = 2016L,
    id_years = 2016L,
    ids_table = "cfi_2016_ids",
    dx09_table = "cfi_2016_dx09",
    dx10_table = "cfi_2016_dx10",
    px_table = "cfi_2016_px",
    workflow_label = "2016 validation",
    compare_2016_tables = FALSE
  )
)

tryCatch(
  source("Code/3.2_prepare_annual_cfi_inputs.R"),
  finally = options("frailty.cfi.config" = previous_cfi_config)
)
