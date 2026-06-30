library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto 2016 clinical metric prefilter test
# Author: Nemo Zhou
# Date started: 2026-06-28
# Date last updated: 2026-06-28
#
# ---- Purpose ----
# Run a short January 2016 diagnostic comparison of the shared clinical metric
# matcher with and without the candidate array prefilter. This script reuses
# existing 2016 rows in 2_annual_metric_ids and writes only diagnostic matched
# event tables so production 2016 match tables are not touched.
#
# The script runs the same event-date window twice:
#   1. prefilter enabled
#   2. prefilter disabled
#
# It then prints aggregate row-count parity checks by match layer, metric, and
# match type. Patient-level matched events remain in Redshift diagnostic tables.

run_prefilter_test <- function(
  enable_candidate_prefilter,
  diagnosis_matches_table,
  procedure_matches_table,
  workflow_label
) {
  options(
    "frailty.clinical_metrics.config" = list(
      analysis_years = 2016L,
      id_years = 2016L,
      ids_table = "2_annual_metric_ids",
      diagnosis_matches_table = diagnosis_matches_table,
      procedure_matches_table = procedure_matches_table,
      workflow_label = workflow_label,
      refresh_metric_ids = FALSE,
      event_start_date = "2016-01-01",
      event_end_date = "2016-02-01",
      enable_candidate_prefilter = enable_candidate_prefilter
    )
  )

  source("Code/5.2_prepare_annual_clinical_metric_matches.R")
}

previous_clinical_metric_config <- getOption("frailty.clinical_metrics.config")

prefilter_diagnosis_table <- "0_7_diagnosis_matches_prefilter_test_2016_jan"
prefilter_procedure_table <- "0_7_procedure_matches_prefilter_test_2016_jan"
no_prefilter_diagnosis_table <- "0_7_diagnosis_matches_no_prefilter_test_2016_jan"
no_prefilter_procedure_table <- "0_7_procedure_matches_no_prefilter_test_2016_jan"

tryCatch(
  {
    run_prefilter_test(
      enable_candidate_prefilter = TRUE,
      diagnosis_matches_table = prefilter_diagnosis_table,
      procedure_matches_table = prefilter_procedure_table,
      workflow_label = "2016 January prefilter diagnostic"
    )

    run_prefilter_test(
      enable_candidate_prefilter = FALSE,
      diagnosis_matches_table = no_prefilter_diagnosis_table,
      procedure_matches_table = no_prefilter_procedure_table,
      workflow_label = "2016 January no-prefilter diagnostic"
    )

    write_schema <- paste0("work_", keyring::key_get("db_username"))
    con <- ohdsilab_connect(
      username = keyring::key_get("db_username"),
      password = keyring::key_get("db_password")
    )

    quote_identifier <- function(identifier) {
      paste0("\"", gsub("\"", "\"\"", identifier, fixed = TRUE), "\"")
    }

    qualified_identifier <- function(schema, table) {
      paste(quote_identifier(schema), quote_identifier(table), sep = ".")
    }

    prefilter_diagnosis_identifier <- qualified_identifier(
      write_schema,
      prefilter_diagnosis_table
    )
    prefilter_procedure_identifier <- qualified_identifier(
      write_schema,
      prefilter_procedure_table
    )
    no_prefilter_diagnosis_identifier <- qualified_identifier(
      write_schema,
      no_prefilter_diagnosis_table
    )
    no_prefilter_procedure_identifier <- qualified_identifier(
      write_schema,
      no_prefilter_procedure_table
    )

    parity_sql <- paste0(
      "WITH prefilter_counts AS (
         SELECT
           'diagnosis' AS match_layer,
           metric,
           match_type,
           COUNT(*)::BIGINT AS prefilter_rows
         FROM ", prefilter_diagnosis_identifier, "
         WHERE analysis_year = 2016
         GROUP BY metric, match_type
         UNION ALL
         SELECT
           'procedure' AS match_layer,
           metric,
           match_type,
           COUNT(*)::BIGINT AS prefilter_rows
         FROM ", prefilter_procedure_identifier, "
         WHERE analysis_year = 2016
         GROUP BY metric, match_type
       ),
       no_prefilter_counts AS (
         SELECT
           'diagnosis' AS match_layer,
           metric,
           match_type,
           COUNT(*)::BIGINT AS no_prefilter_rows
         FROM ", no_prefilter_diagnosis_identifier, "
         WHERE analysis_year = 2016
         GROUP BY metric, match_type
         UNION ALL
         SELECT
           'procedure' AS match_layer,
           metric,
           match_type,
           COUNT(*)::BIGINT AS no_prefilter_rows
         FROM ", no_prefilter_procedure_identifier, "
         WHERE analysis_year = 2016
         GROUP BY metric, match_type
       )
       SELECT
         COALESCE(p.match_layer, n.match_layer) AS match_layer,
         COALESCE(p.metric, n.metric) AS metric,
         COALESCE(p.match_type, n.match_type) AS match_type,
         COALESCE(p.prefilter_rows, 0)::BIGINT AS prefilter_rows,
         COALESCE(n.no_prefilter_rows, 0)::BIGINT AS no_prefilter_rows,
         (
           COALESCE(p.prefilter_rows, 0) -
           COALESCE(n.no_prefilter_rows, 0)
         )::BIGINT AS row_difference
       FROM prefilter_counts p
       FULL OUTER JOIN no_prefilter_counts n
         ON p.match_layer = n.match_layer
        AND p.metric = n.metric
        AND p.match_type = n.match_type
       ORDER BY match_layer, metric, match_type"
    )

    message("Checking January 2016 prefilter vs no-prefilter aggregate parity.")
    parity_result <- DBI::dbGetQuery(con, parity_sql)
    print(parity_result)

    if (any(parity_result$row_difference != 0)) {
      stop(
        "Prefilter diagnostic did not match the no-prefilter diagnostic. ",
        "Do not enable the candidate prefilter for production yet."
      )
    }

    message(
      "January 2016 prefilter diagnostic matched the no-prefilter aggregate ",
      "counts exactly."
    )
  },
  finally = options(
    "frailty.clinical_metrics.config" = previous_clinical_metric_config
  )
)
