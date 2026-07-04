# Project: Frailty_Komoto annual polypharmacy runner
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-04
#
# ---- Purpose ----
# Run the 5.x annual polypharmacy pipeline in order. The default configuration
# processes 2016 only and reuses `2_annual_metric_ids` as the patient-year
# denominator. This runner sets the reviewed pharmacy transaction filter so the
# pipeline runs end to end without stopping at `5.1`. It also stamps the
# current run date into `mapping_version_date` unless the caller already set a
# specific mapping version. The runner still pauses naturally at `5.3` if the
# external n2c/RxNav crosswalk CSV has not yet been saved to the configured path.
#
# ---- Reviewed transaction filter ----
# A 2016 exploratory unfiltered run of `5.1` surfaced the distinct pharmacy
# transaction values in `komodo_ext.pharmacy_events` (no NULL/blank buckets):
#   transaction_result:      PAID / REJECTED / REVERSED
#   transaction_status:      STANDALONE / FINAL
#   transaction_source_type: PAID ONLY / LIFECYCLE
# PAID rows accounted for 500,045,674 of 567,282,857 candidate 2016 fills
# (88.1%); the remaining ~11.9% were REJECTED (6.7%) or REVERSED (5.2%) claims
# that never resulted in an active dispensing. PAID co-occurs only with the two
# terminal statuses (STANDALONE, FINAL), so keeping `transaction_result = 'PAID'`
# yields one terminal paid row per claim and makes a status/source filter
# redundant. See `Documents/12_POLYPHARMACY_DATA_PROCESSING_FLOW.md`.
previous_annual_polypharmacy_config <- getOption(
  "frailty.annual_polypharmacy.config"
)
runner_polypharmacy_config <- list(transaction_result_keep = c("PAID"))
if (
  is.null(previous_annual_polypharmacy_config$mapping_version_date) ||
    length(previous_annual_polypharmacy_config$mapping_version_date) == 0L
) {
  runner_polypharmacy_config$mapping_version_date <- as.character(Sys.Date())
}
options(
  "frailty.annual_polypharmacy.config" = utils::modifyList(
    if (is.null(previous_annual_polypharmacy_config)) {
      list()
    } else {
      previous_annual_polypharmacy_config
    },
    runner_polypharmacy_config
  )
)

scripts <- c(
  "Code/5.1_prepare_polypharmacy_pharmacy_fills.R",
  "Code/5.2_export_polypharmacy_unique_ndc11.R",
  "Code/5.3_stage_polypharmacy_ndc11_atc_crosswalk.R",
  "Code/5.4_build_annual_polypharmacy_exposures.R",
  "Code/5.5_calculate_annual_polypharmacy_metrics.R",
  "Code/5.6_check_annual_polypharmacy_metrics.R"
)

run_stage <- function(script) {
  message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "START ", script)
  tryCatch(
    {
      source(script)
      message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "DONE  ", script)
    },
    error = function(e) {
      message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "FAILED ", script)
      stop(e)
    }
  )
}

tryCatch(
  for (script in scripts) {
    run_stage(script)
  },
  finally = options(
    "frailty.annual_polypharmacy.config" = previous_annual_polypharmacy_config
  )
)
