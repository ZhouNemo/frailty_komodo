source("Code/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-03
#
# ---- Purpose ----
# Build cleaned eligible pharmacy fills for selected patient-years in
# `2_annual_metric_ids` from `komodo_ext.pharmacy_events`. The default
# configuration processes 2016 only. The script requires either reviewed
# transaction-status keep values or an explicit exploratory unfiltered override.
# It keeps patient-level rows inside Redshift and writes selected years plus
# durable aggregate extraction QA to:
#   - 2_polypharmacy_pharmacy_fills
#   - 2_polypharmacy_fill_extraction_qa

config <- get_annual_polypharmacy_config()
polypharmacy_require_transaction_filter_decision(config)
con <- connect_komodo()
# Do NOT register on.exit(disconnect_komodo(con)) here. At the top level of a
# source()d script, on.exit() fires early and closes the connection before the
# script can query it. The connection is disconnected explicitly at the end.

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
pharmacy_identifier <- qualified_identifier(komodo_schema, config$pharmacy_table)
fills_identifier <- qualified_identifier(write_schema, config$fills_table)
fill_extraction_qa_identifier <- qualified_identifier(
  write_schema,
  config$fill_extraction_qa_table
)
fills_build_table <- "polypharmacy_fills_build_5_1"
fills_build_identifier <- quote_identifier(fills_build_table)
ndc_expr <- clean_ndc11_sql("rx", "ndc11")

if (!table_exists(con, write_schema, config$ids_table)) {
  stop("Required denominator table was not found: ", write_schema, ".", config$ids_table)
}
if (!table_exists(con, komodo_schema, config$pharmacy_table)) {
  stop("Required pharmacy table was not found: ", komodo_schema, ".", config$pharmacy_table)
}

table_has_columns(
  con,
  write_schema,
  config$ids_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "analysis_start_date",
    "analysis_end_date"
  )
)
table_has_columns(
  con,
  komodo_schema,
  config$pharmacy_table,
  c(
    "pharmacy_event_id",
    "patient_id",
    "fill_date",
    "ndc11",
    "days_supply",
    "quantity",
    "transaction_result",
    "transaction_status",
    "transaction_source_type"
  )
)

if (!table_exists(con, write_schema, config$fills_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", fills_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         analysis_start_date DATE NOT NULL,
         analysis_end_date DATE NOT NULL,
         pharmacy_event_id VARCHAR(256),
         fill_date DATE NOT NULL,
         ndc11 VARCHAR(32) NOT NULL,
         days_supply INTEGER NOT NULL,
         quantity DOUBLE PRECISION,
         transaction_result VARCHAR(128),
         transaction_status VARCHAR(128),
         transaction_source_type VARCHAR(128)
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid, fill_date, ndc11);"
    )
  )
}

if (!table_exists(con, write_schema, config$fill_extraction_qa_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", fill_extraction_qa_identifier, " (
         analysis_year INTEGER,
         qa_section VARCHAR(128) NOT NULL,
         transaction_result VARCHAR(128),
         transaction_status VARCHAR(128),
         transaction_source_type VARCHAR(128),
         days_supply_bucket VARCHAR(32),
         n_rows BIGINT,
         n_patient_years BIGINT,
         n_distinct_ndc11 BIGINT,
         n_missing_fill_date BIGINT,
         n_missing_ndc11 BIGINT,
         n_invalid_ndc11 BIGINT,
         n_missing_days_supply BIGINT,
         n_nonpositive_days_supply BIGINT,
         n_days_supply_over_max BIGINT,
         min_days_supply INTEGER,
         max_days_supply INTEGER,
         mean_days_supply DOUBLE PRECISION,
         built_at TIMESTAMP
       )
       DISTKEY(analysis_year)
       SORTKEY(analysis_year, qa_section);"
    )
  )
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
    "days_supply"
  )
)
table_has_columns(
  con,
  write_schema,
  config$fill_extraction_qa_table,
  c("analysis_year", "qa_section", "n_rows", "built_at")
)

id_integrity <- print_query(
  con,
  "Checking annual metric denominator coverage for polypharmacy.",
  paste0(
    "SELECT analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_distinct_patid
     FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)
missing_years <- setdiff(config$analysis_years, id_integrity$analysis_year)
if (length(missing_years) > 0L || any(id_integrity$n_rows != id_integrity$n_distinct_patid)) {
  stop("The selected-year denominator in ", config$ids_table, " is missing or duplicated.")
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", fills_build_identifier, ";
     CREATE TEMP TABLE ", fills_build_identifier, " (
       patid VARCHAR(256) NOT NULL,
       patient_id VARCHAR(256) NOT NULL,
       analysis_year INTEGER NOT NULL,
       analysis_start_date DATE NOT NULL,
       analysis_end_date DATE NOT NULL,
       pharmacy_event_id VARCHAR(256),
       fill_date DATE NOT NULL,
       ndc11 VARCHAR(32) NOT NULL,
       days_supply INTEGER NOT NULL,
       quantity DOUBLE PRECISION,
       transaction_result VARCHAR(128),
       transaction_status VARCHAR(128),
       transaction_source_type VARCHAR(128)
     )
     DISTKEY(patid)
     SORTKEY(analysis_year, patid, fill_date, ndc11);"
  )
)

fill_scan_chunks <- polypharmacy_fill_scan_chunks(config)
transaction_filter <- polypharmacy_transaction_filter_sql(config, "rx")
days_supply_filter <- polypharmacy_days_supply_filter_sql(config, "rx")
days_supply_cap_sql <- if (is.null(config$days_supply_max)) {
  "0"
} else {
  paste0("CASE WHEN rx.days_supply > ", config$days_supply_max, " THEN 1 ELSE 0 END")
}
scan_bounds <- polypharmacy_scan_bounds(config)
qa_literal_window_sql <- event_literal_window_sql(
  scan_bounds$start_date,
  scan_bounds$end_date,
  "rx.fill_date"
)
qa_fill_window_sql <- polypharmacy_fill_window_sql(
  config,
  ids_alias = "ids",
  fill_column = "rx.fill_date",
  days_supply_column = "rx.days_supply",
  chunk_start_date = scan_bounds$start_date,
  chunk_end_date = scan_bounds$end_date
)
qa_candidate_patient_window_sql <- paste0(
  "rx.fill_date <= ids.analysis_end_date AND ",
  if (isTRUE(config$allow_prior_fill_carry_in)) {
    "DATEADD(day, COALESCE(rx.days_supply, 0) - 1, rx.fill_date) >= ids.analysis_start_date"
  } else {
    "rx.fill_date >= ids.analysis_start_date"
  }
)

for (chunk_row in seq_len(nrow(fill_scan_chunks))) {
  chunk <- fill_scan_chunks[chunk_row, ]
  fill_window_sql <- polypharmacy_fill_window_sql(
    config,
    ids_alias = "ids",
    fill_column = "rx.fill_date",
    days_supply_column = "rx.days_supply",
    chunk_start_date = chunk$chunk_start_date,
    chunk_end_date = chunk$chunk_end_date
  )
  message(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
    "Scanning pharmacy fill chunk ",
    chunk$chunk_id,
    " of ",
    nrow(fill_scan_chunks),
    " (",
    chunk$chunk_start_date,
    " to ",
    chunk$chunk_end_date,
    ")."
  )
  flush.console()
  execute_sql_with_retry(
    con,
    paste0(
      "INSERT INTO ", fills_build_identifier, " (
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
       SELECT DISTINCT
         ids.patid,
         ids.patient_id,
         ids.analysis_year,
         ids.analysis_start_date,
         ids.analysis_end_date,
         rx.pharmacy_event_id,
         CAST(rx.fill_date AS DATE) AS fill_date,
         ", ndc_expr, " AS ndc11,
         CAST(rx.days_supply AS INTEGER) AS days_supply,
         rx.quantity,
         rx.transaction_result,
         rx.transaction_status,
         rx.transaction_source_type
       FROM ", ids_identifier, " ids
       INNER JOIN ", pharmacy_identifier, " rx
         ON rx.patient_id = ids.patient_id
       WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
         AND ", fill_window_sql, "
         AND rx.patient_id IS NOT NULL
         AND rx.fill_date IS NOT NULL
         AND rx.ndc11 IS NOT NULL
         AND ", days_supply_filter, "
         AND ", transaction_filter, "
         AND LEN(", ndc_expr, ") = 11;"
    ),
    label = paste0("polypharmacy pharmacy fill scan chunk ", chunk$chunk_id)
  )
}

build_qa <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       COUNT(*)::BIGINT AS n_rows,
       SUM(CASE WHEN patid IS NULL OR ndc11 IS NULL OR fill_date IS NULL
         OR days_supply IS NULL THEN 1 ELSE 0 END)::BIGINT AS n_bad
     FROM ", fills_build_identifier
  )
)
if (build_qa$n_bad[[1]] != 0) {
  stop("Polypharmacy fill build stage contains missing required values.")
}

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
     SELECT DISTINCT
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
     FROM ", fills_build_identifier, ";

     DROP TABLE IF EXISTS ", fills_build_identifier, ";"
  ),
  label = "polypharmacy pharmacy fill persistent replace"
)

message("Writing selected-year polypharmacy fill extraction QA.")
DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", fill_extraction_qa_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
        OR analysis_year IS NULL;

     INSERT INTO ", fill_extraction_qa_identifier, " (
       analysis_year,
       qa_section,
       transaction_result,
       transaction_status,
       transaction_source_type,
       days_supply_bucket,
       n_rows,
       n_patient_years,
       n_distinct_ndc11,
       n_missing_fill_date,
       n_missing_ndc11,
       n_invalid_ndc11,
       n_missing_days_supply,
       n_nonpositive_days_supply,
       n_days_supply_over_max,
       min_days_supply,
       max_days_supply,
       mean_days_supply,
       built_at
     )
     SELECT
       ids.analysis_year,
       'candidate_before_filters' AS qa_section,
       NULL AS transaction_result,
       NULL AS transaction_status,
       NULL AS transaction_source_type,
       NULL AS days_supply_bucket,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT ids.patid)::BIGINT AS n_patient_years,
       COUNT(DISTINCT CASE WHEN LEN(", ndc_expr, ") = 11 THEN ", ndc_expr, " END)::BIGINT
         AS n_distinct_ndc11,
       SUM(CASE WHEN rx.fill_date IS NULL THEN 1 ELSE 0 END)::BIGINT
         AS n_missing_fill_date,
       SUM(CASE WHEN rx.ndc11 IS NULL OR TRIM(rx.ndc11) = '' THEN 1 ELSE 0 END)::BIGINT
         AS n_missing_ndc11,
       SUM(CASE WHEN rx.ndc11 IS NOT NULL AND LEN(", ndc_expr, ") <> 11
         THEN 1 ELSE 0 END)::BIGINT AS n_invalid_ndc11,
       SUM(CASE WHEN rx.days_supply IS NULL THEN 1 ELSE 0 END)::BIGINT
         AS n_missing_days_supply,
       SUM(CASE WHEN rx.days_supply IS NOT NULL AND rx.days_supply <= 0
         THEN 1 ELSE 0 END)::BIGINT AS n_nonpositive_days_supply,
       SUM(", days_supply_cap_sql, ")::BIGINT AS n_days_supply_over_max,
       MIN(CASE WHEN rx.days_supply IS NOT NULL THEN rx.days_supply END)::INTEGER
         AS min_days_supply,
       MAX(CASE WHEN rx.days_supply IS NOT NULL THEN rx.days_supply END)::INTEGER
         AS max_days_supply,
       AVG(CASE WHEN rx.days_supply IS NOT NULL THEN rx.days_supply::DOUBLE PRECISION END)
         AS mean_days_supply,
       GETDATE() AS built_at
     FROM ", ids_identifier, " ids
     INNER JOIN ", pharmacy_identifier, " rx
       ON rx.patient_id = ids.patient_id
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
       AND ", qa_literal_window_sql, "
       AND ", qa_candidate_patient_window_sql, "
     GROUP BY ids.analysis_year;

     INSERT INTO ", fill_extraction_qa_identifier, " (
       analysis_year,
       qa_section,
       n_rows,
       built_at
     )
     SELECT
       ids.analysis_year,
       'source_missing_fill_date_unattributed' AS qa_section,
       COUNT(*)::BIGINT AS n_rows,
       GETDATE() AS built_at
     FROM ", ids_identifier, " ids
     INNER JOIN ", pharmacy_identifier, " rx
       ON rx.patient_id = ids.patient_id
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
       AND rx.fill_date IS NULL
     GROUP BY ids.analysis_year;

     INSERT INTO ", fill_extraction_qa_identifier, " (
       analysis_year,
       qa_section,
       transaction_result,
       transaction_status,
       transaction_source_type,
       days_supply_bucket,
       n_rows,
       n_patient_years,
       n_distinct_ndc11,
       built_at
     )
     SELECT
       ids.analysis_year,
       'transaction_values_before_filter' AS qa_section,
       COALESCE(rx.transaction_result, '<NULL>') AS transaction_result,
       COALESCE(rx.transaction_status, '<NULL>') AS transaction_status,
       COALESCE(rx.transaction_source_type, '<NULL>') AS transaction_source_type,
       NULL AS days_supply_bucket,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT ids.patid)::BIGINT AS n_patient_years,
       COUNT(DISTINCT ", ndc_expr, ")::BIGINT AS n_distinct_ndc11,
       GETDATE() AS built_at
     FROM ", ids_identifier, " ids
     INNER JOIN ", pharmacy_identifier, " rx
       ON rx.patient_id = ids.patient_id
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
       AND ", qa_fill_window_sql, "
       AND rx.patient_id IS NOT NULL
       AND rx.fill_date IS NOT NULL
       AND rx.ndc11 IS NOT NULL
       AND ", days_supply_filter, "
       AND LEN(", ndc_expr, ") = 11
     GROUP BY
       ids.analysis_year,
       COALESCE(rx.transaction_result, '<NULL>'),
       COALESCE(rx.transaction_status, '<NULL>'),
       COALESCE(rx.transaction_source_type, '<NULL>');

     INSERT INTO ", fill_extraction_qa_identifier, " (
       analysis_year,
       qa_section,
       transaction_result,
       transaction_status,
       transaction_source_type,
       days_supply_bucket,
       n_rows,
       n_patient_years,
       n_distinct_ndc11,
       built_at
     )
     SELECT
       analysis_year,
       'transaction_values_after_filter' AS qa_section,
       COALESCE(transaction_result, '<NULL>') AS transaction_result,
       COALESCE(transaction_status, '<NULL>') AS transaction_status,
       COALESCE(transaction_source_type, '<NULL>') AS transaction_source_type,
       NULL AS days_supply_bucket,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_patient_years,
       COUNT(DISTINCT ndc11)::BIGINT AS n_distinct_ndc11,
       GETDATE() AS built_at
     FROM ", fills_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, transaction_result, transaction_status, transaction_source_type;

     INSERT INTO ", fill_extraction_qa_identifier, " (
       analysis_year,
       qa_section,
       days_supply_bucket,
       n_rows,
       min_days_supply,
       max_days_supply,
       mean_days_supply,
       built_at
     )
     SELECT
       ids.analysis_year,
       'days_supply_before_exclusions' AS qa_section,
       ", days_supply_bucket_sql("rx.days_supply"), " AS days_supply_bucket,
       COUNT(*)::BIGINT AS n_rows,
       MIN(rx.days_supply)::INTEGER AS min_days_supply,
       MAX(rx.days_supply)::INTEGER AS max_days_supply,
       AVG(rx.days_supply::DOUBLE PRECISION) AS mean_days_supply,
       GETDATE() AS built_at
     FROM ", ids_identifier, " ids
     INNER JOIN ", pharmacy_identifier, " rx
       ON rx.patient_id = ids.patient_id
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
       AND ", qa_literal_window_sql, "
       AND ", qa_candidate_patient_window_sql, "
     GROUP BY ids.analysis_year, ", days_supply_bucket_sql("rx.days_supply"), ";

     INSERT INTO ", fill_extraction_qa_identifier, " (
       analysis_year,
       qa_section,
       days_supply_bucket,
       n_rows,
       min_days_supply,
       max_days_supply,
       mean_days_supply,
       built_at
     )
     SELECT
       analysis_year,
       'days_supply_after_exclusions' AS qa_section,
       ", days_supply_bucket_sql("days_supply"), " AS days_supply_bucket,
       COUNT(*)::BIGINT AS n_rows,
       MIN(days_supply)::INTEGER AS min_days_supply,
       MAX(days_supply)::INTEGER AS max_days_supply,
       AVG(days_supply::DOUBLE PRECISION) AS mean_days_supply,
       GETDATE() AS built_at
     FROM ", fills_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, ", days_supply_bucket_sql("days_supply"), ";"
  ),
  progressBar = FALSE,
  reportOverallTime = FALSE
)

print_query(
  con,
  "Checking selected-year polypharmacy fill counts.",
  paste0(
    "SELECT analysis_year,
       COUNT(*)::BIGINT AS fill_rows,
       COUNT(DISTINCT patid)::BIGINT AS patient_years_with_fills,
       COUNT(DISTINCT ndc11)::BIGINT AS distinct_ndc11,
       MIN(fill_date) AS first_fill_date,
       MAX(fill_date) AS last_fill_date
     FROM ", fills_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

print_query(
  con,
  "Checking selected-year pharmacy transaction values after filtering.",
  paste0(
    "SELECT analysis_year,
       COALESCE(transaction_result, '<NULL>') AS transaction_result,
       COALESCE(transaction_status, '<NULL>') AS transaction_status,
       COALESCE(transaction_source_type, '<NULL>') AS transaction_source_type,
       COUNT(*)::BIGINT AS fill_rows
     FROM ", fills_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, transaction_result, transaction_status, transaction_source_type
     ORDER BY analysis_year, fill_rows DESC
     LIMIT 100"
  )
)

message(
  config$workflow_label,
  " pharmacy fill preparation complete: ",
  write_schema,
  ".",
  config$fills_table,
  "."
)

disconnect_komodo(con)
