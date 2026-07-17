source("Code/2_variable construction/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy repair
# Author: Nemo Zhou
# Date started: 2026-07-06
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Repair selected-year duplicate event keys in `2_polypharmacy_pharmacy_fills`
# without rescanning `komodo_ext.pharmacy_events`. This is intended for the rare
# case where the cleaned fills table contains more than one row for the same
# patient-year and pharmacy event key, but the duplicate rows agree on the
# analytic exposure fields (`fill_date`, `ndc11`, and `days_supply`). It keeps
# patient-level rows inside Redshift and prints only aggregate duplicate counts.

config <- get_annual_polypharmacy_config()
con <- connect_komodo()

fills_identifier <- qualified_identifier(write_schema, config$fills_table)
repair_table <- "polypharmacy_fills_dedup_repair_0_8"
repair_identifier <- quote_identifier(repair_table)

if (!table_exists(con, write_schema, config$fills_table)) {
  stop("Required fills table was not found: ", write_schema, ".", config$fills_table)
}

table_has_columns(
  con,
  write_schema,
  config$fills_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "analysis_start_date",
    "analysis_end_date",
    "pharmacy_event_id",
    "fill_date",
    "ndc11",
    "days_supply",
    "quantity",
    "transaction_result",
    "transaction_status",
    "transaction_source_type"
  )
)

message(
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
  "START staging selected-year deduplicated fills."
)
execute_sql_with_retry(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", repair_identifier, ";
     CREATE TEMP TABLE ", repair_identifier, " AS
     SELECT
       patid,
       patient_id,
       analysis_year,
       analysis_start_date,
       analysis_end_date,
       pharmacy_event_id,
       fill_date,
       ndc11,
       days_supply,
       quantity,
       transaction_result,
       transaction_status,
       transaction_source_type,
       duplicate_count,
       duplicate_rank,
       analytic_discordant
     FROM (
       SELECT
         fills.*,
         COUNT(*) OVER (
           PARTITION BY
             patid,
             analysis_year,
             COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11)
         ) AS duplicate_count,
         ROW_NUMBER() OVER (
           PARTITION BY
             patid,
             analysis_year,
             COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11)
           ORDER BY
             CASE WHEN transaction_status = 'FINAL' THEN 0
                  WHEN transaction_status = 'STANDALONE' THEN 1
                  ELSE 2 END,
             CASE WHEN transaction_source_type = 'LIFECYCLE' THEN 0
                  WHEN transaction_source_type = 'PAID ONLY' THEN 1
                  ELSE 2 END,
             CASE WHEN quantity IS NULL THEN 1 ELSE 0 END,
             quantity DESC
         ) AS duplicate_rank,
         CASE WHEN
           MIN(fill_date) OVER (
             PARTITION BY
               patid,
               analysis_year,
               COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11)
           ) <> MAX(fill_date) OVER (
             PARTITION BY
               patid,
               analysis_year,
               COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11)
           )
           OR MIN(ndc11) OVER (
             PARTITION BY
               patid,
               analysis_year,
               COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11)
           ) <> MAX(ndc11) OVER (
             PARTITION BY
               patid,
               analysis_year,
               COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11)
           )
           OR MIN(days_supply) OVER (
             PARTITION BY
               patid,
               analysis_year,
               COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11)
           ) <> MAX(days_supply) OVER (
             PARTITION BY
               patid,
               analysis_year,
               COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11)
           )
         THEN 1 ELSE 0 END AS analytic_discordant
       FROM ", fills_identifier, " fills
       WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     ) staged;"
  ),
  label = "polypharmacy fill duplicate repair staging"
)
message(
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
  "DONE  staging selected-year deduplicated fills."
)

duplicate_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       analysis_year,
       COUNT(DISTINCT CASE WHEN duplicate_count > 1 THEN
         patid || '|' || analysis_year::VARCHAR || '|' ||
         COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11)
       END)::BIGINT AS n_duplicate_keys,
       SUM(CASE WHEN duplicate_count > 1 THEN 1 ELSE 0 END)::BIGINT
         AS n_rows_in_duplicate_keys,
       SUM(CASE WHEN duplicate_count > 1 AND duplicate_rank > 1 THEN 1 ELSE 0 END)::BIGINT
         AS n_excess_rows,
       SUM(CASE WHEN duplicate_count > 1 AND analytic_discordant = 1
         AND duplicate_rank = 1 THEN 1 ELSE 0 END)::BIGINT
         AS n_analytic_discordant_keys
     FROM ", repair_identifier, "
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)
message("Selected-year polypharmacy fill duplicate event-key summary:")
print(duplicate_summary)

if (
  nrow(duplicate_summary) == 0L ||
    all(is.na(duplicate_summary$n_duplicate_keys) | duplicate_summary$n_duplicate_keys == 0)
) {
  message("No duplicate fill event keys found. No repair was needed.")
  DatabaseConnector::executeSql(
    con,
    paste0("DROP TABLE IF EXISTS ", repair_identifier, ";"),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  disconnect_komodo(con)
} else if (any(duplicate_summary$n_analytic_discordant_keys > 0)) {
  DatabaseConnector::executeSql(
    con,
    paste0("DROP TABLE IF EXISTS ", repair_identifier, ";"),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  disconnect_komodo(con)
  stop(
    "Duplicate fill event keys disagree on fill_date, ndc11, or days_supply. ",
    "No rows were changed; inspect the aggregate duplicate summary before repair."
  )
} else {
  message(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
    "START replacing selected-year fills with deduplicated rows."
  )
  execute_sql_with_retry(
    con,
    paste0(
      "DELETE FROM ", fills_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ");

       INSERT INTO ", fills_identifier, " (
         patid,
         patient_id,
         analysis_year,
         analysis_start_date,
         analysis_end_date,
         pharmacy_event_id,
         fill_date,
         ndc11,
         days_supply,
         quantity,
         transaction_result,
         transaction_status,
         transaction_source_type
       )
       SELECT
         patid,
         patient_id,
         analysis_year,
         analysis_start_date,
         analysis_end_date,
         pharmacy_event_id,
         fill_date,
         ndc11,
         days_supply,
         quantity,
         transaction_result,
         transaction_status,
         transaction_source_type
       FROM ", repair_identifier, "
       WHERE duplicate_rank = 1;

       DROP TABLE IF EXISTS ", repair_identifier, ";"
    ),
    label = "polypharmacy fill duplicate repair replace"
  )
  message(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
    "DONE  replacing selected-year fills with deduplicated rows."
  )

  post_repair_summary <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         'fills_table' AS qa_check,
         COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR || '|' ||
           COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11))
           AS duplicate_rows
       FROM ", fills_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ")"
    )
  )
  message("Post-repair duplicate event-key summary:")
  print(post_repair_summary)

  if (any(post_repair_summary$duplicate_rows != 0)) {
    disconnect_komodo(con)
    stop("Polypharmacy fill duplicate repair did not remove all duplicate keys.")
  }

  message(
    "Polypharmacy fill duplicate repair complete for selected years: ",
    paste(config$analysis_years, collapse = ", "),
    ". Re-run Code/2_variable construction/5.6_check_annual_polypharmacy_metrics.R next."
  )

  disconnect_komodo(con)
}
