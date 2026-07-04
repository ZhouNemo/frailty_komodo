source("Code/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy QA
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-04
#
# ---- Purpose ----
# Run aggregate QA for the 5.x annual polypharmacy pipeline. The checks validate
# source schemas, selected-year table presence, denominator completeness,
# durable extraction QA, NDC mapping coverage, exposure interval validity,
# final-table completeness, and duplicate keys. Patient-level rows are not
# printed. Small insurance-prevalence cells are suppressed before CSV export.

config <- get_annual_polypharmacy_config()
con <- connect_komodo()
min_count <- 11L

if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
fills_identifier <- qualified_identifier(write_schema, config$fills_table)
fill_extraction_qa_identifier <- qualified_identifier(
  write_schema,
  config$fill_extraction_qa_table
)
unique_ndc_identifier <- qualified_identifier(write_schema, config$unique_ndc_table)
crosswalk_identifier <- qualified_identifier(write_schema, config$crosswalk_table)
episodes_identifier <- qualified_identifier(write_schema, config$episodes_table)
final_identifier <- qualified_identifier(write_schema, config$final_table)

table_has_columns(
  con,
  komodo_schema,
  config$pharmacy_table,
  c(
    "patient_id",
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

required_write_tables <- c(
  config$ids_table,
  config$fills_table,
  config$fill_extraction_qa_table,
  config$unique_ndc_table,
  config$crosswalk_table,
  config$episodes_table,
  config$final_table
)
missing_write_tables <- required_write_tables[
  !vapply(
    required_write_tables,
    function(table) table_exists(con, write_schema, table),
    logical(1)
  )
]
if (length(missing_write_tables) > 0L) {
  stop(
    "Missing required polypharmacy tables in ",
    write_schema,
    ": ",
    paste(missing_write_tables, collapse = ", ")
  )
}

qa_results <- list()

qa_results$denominator <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'denominator' AS qa_check,
       analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_distinct_patient_years
     FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

qa_results$fills <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'pharmacy_fills' AS qa_check,
       analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_patient_years,
       COUNT(DISTINCT ndc11)::BIGINT AS n_distinct_ndc11,
       MIN(fill_date) AS first_fill_date,
       MAX(fill_date) AS last_fill_date
     FROM ", fills_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

qa_results$fill_extraction <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT *
     FROM ", fill_extraction_qa_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
        OR analysis_year IS NULL
     ORDER BY analysis_year, qa_section, n_rows DESC"
  )
)

qa_results$transaction_values <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'transaction_values' AS qa_check,
       analysis_year,
       COALESCE(transaction_result, '<NULL>') AS transaction_result,
       COALESCE(transaction_status, '<NULL>') AS transaction_status,
       COALESCE(transaction_source_type, '<NULL>') AS transaction_source_type,
       COUNT(*)::BIGINT AS n_rows
     FROM ", fills_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, transaction_result, transaction_status, transaction_source_type
     ORDER BY analysis_year, n_rows DESC"
  )
)

qa_results$unique_ndc <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'unique_ndc11' AS qa_check,
       NULL::INTEGER AS analysis_year,
       COUNT(*)::BIGINT AS n_unique_ndc11,
       SUM(n_fill_rows)::BIGINT AS represented_fill_rows
     FROM ", unique_ndc_identifier
  )
)

qa_results$mapping_coverage <- DBI::dbGetQuery(
  con,
  paste0(
    "WITH selected_mapping AS (
       SELECT *
       FROM ", crosswalk_identifier, "
       WHERE mapping_source = ", sql_string(config$mapping_source), "
         AND mapping_version_date = ", sql_string(config$mapping_version_date), "
     ),
     total_submitted AS (
       SELECT COUNT(DISTINCT ndc11)::DOUBLE PRECISION AS n_unique_submitted
       FROM selected_mapping
     )
     SELECT
       'mapping_coverage' AS qa_check,
       m.mapping_status,
       COUNT(DISTINCT m.ndc11)::BIGINT AS n_unique_ndc11,
       COUNT(*)::BIGINT AS n_mapping_rows,
       CAST(
         100.0 * COUNT(DISTINCT m.ndc11)::DOUBLE PRECISION /
           NULLIF(MAX(t.n_unique_submitted), 0)
         AS DECIMAL(18, 2)
       ) AS pct_unique_ndc11
     FROM selected_mapping m
     CROSS JOIN total_submitted t
     GROUP BY m.mapping_status
     ORDER BY m.mapping_status"
  )
)

qa_results$multi_mapping <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'multi_atc_mapping' AS qa_check,
       COUNT(DISTINCT CASE WHEN n_distinct_atc4 > 1 THEN ndc11 END)::BIGINT
         AS n_ndc11_with_multiple_atc4,
       COUNT(DISTINCT CASE WHEN n_distinct_atc3 > 1 THEN ndc11 END)::BIGINT
         AS n_ndc11_with_multiple_atc3
     FROM (
       SELECT
         ndc11,
         COUNT(DISTINCT atc4) AS n_distinct_atc4,
         COUNT(DISTINCT atc3) AS n_distinct_atc3
       FROM ", crosswalk_identifier, "
       WHERE mapping_source = ", sql_string(config$mapping_source), "
         AND mapping_version_date = ", sql_string(config$mapping_version_date), "
         AND mapping_status = 'mapped'
       GROUP BY ndc11
     ) x"
  )
)

qa_results$mapping_fill_coverage <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'mapping_fill_coverage' AS qa_check,
       CASE
         WHEN cw.ndc11 IS NULL THEN 'unmapped'
         ELSE 'mapped'
       END AS mapping_status,
       COUNT(DISTINCT f.ndc11)::BIGINT AS n_unique_ndc11,
       COUNT(*)::BIGINT AS n_fill_rows,
       CAST(
         100.0 * COUNT(*)::DOUBLE PRECISION /
           NULLIF(SUM(COUNT(*)) OVER (), 0)
         AS DECIMAL(18, 2)
       ) AS pct_fill_rows
     FROM ", fills_identifier, " f
     LEFT JOIN (
       SELECT DISTINCT ndc11
       FROM ", crosswalk_identifier, "
       WHERE mapping_source = ", sql_string(config$mapping_source), "
         AND mapping_version_date = ", sql_string(config$mapping_version_date), "
         AND mapping_status = 'mapped'
         AND atc3 IS NOT NULL
         AND atc3 <> ''
     ) cw
       ON f.ndc11 = cw.ndc11
     WHERE f.analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY
       CASE
         WHEN cw.ndc11 IS NULL THEN 'unmapped'
         ELSE 'mapped'
       END
     ORDER BY mapping_status"
  )
)

qa_results$episodes <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'exposure_episodes' AS qa_check,
       analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_patient_years,
       COUNT(DISTINCT atc3)::BIGINT AS n_distinct_atc3,
       SUM(CASE WHEN episode_end_clipped < episode_start_clipped
         THEN 1 ELSE 0 END)::BIGINT AS invalid_clipped_intervals
     FROM ", episodes_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

qa_results$final_completeness <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       ids.analysis_year,
       COUNT(ids.patid)::BIGINT AS denominator_rows,
       COUNT(final.patid)::BIGINT AS final_rows,
       SUM(CASE WHEN final.patid IS NULL THEN 1 ELSE 0 END)::BIGINT
         AS missing_final_rows
     FROM ", ids_identifier, " ids
     LEFT JOIN ", final_identifier, " final
       ON ids.patid = final.patid
      AND ids.analysis_year = final.analysis_year
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY ids.analysis_year
     ORDER BY ids.analysis_year"
  )
)

qa_results$duplicates <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT 'final_table' AS qa_check,
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
         AS duplicate_rows
     FROM ", final_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT 'fills_table' AS qa_check,
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR || '|' ||
         COALESCE(pharmacy_event_id, fill_date::VARCHAR || '|' || ndc11))
         AS duplicate_rows
     FROM ", fills_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")"
  )
)

qa_results$polypharmacy_distribution <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'polypharmacy_distribution' AS qa_check,
       analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       SUM(polypharmacy)::BIGINT AS n_polypharmacy,
       MIN(total_days_5plus) AS min_total_days_5plus,
       MAX(total_days_5plus) AS max_total_days_5plus,
       AVG(total_days_5plus::DOUBLE PRECISION) AS mean_total_days_5plus
     FROM ", final_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

qa_results$insurance_prevalence <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'rx_insurance_prevalence' AS qa_check,
       ids.analysis_year,
       ids.rx_insurance_group,
       ids.rx_insurance_segment,
       COUNT(*)::BIGINT AS n_patient_years,
       SUM(final.polypharmacy)::BIGINT AS n_polypharmacy
     FROM ", ids_identifier, " ids
     INNER JOIN ", final_identifier, " final
       ON ids.patid = final.patid
      AND ids.analysis_year = final.analysis_year
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY ids.analysis_year, ids.rx_insurance_group, ids.rx_insurance_segment
     ORDER BY ids.analysis_year, n_patient_years DESC"
  )
)
qa_results$insurance_prevalence$suppression_applied <- ifelse(
  is.na(qa_results$insurance_prevalence$n_patient_years) |
    qa_results$insurance_prevalence$n_patient_years < min_count |
    is.na(qa_results$insurance_prevalence$n_polypharmacy) |
    qa_results$insurance_prevalence$n_polypharmacy < min_count,
  "yes",
  "no"
)
qa_results$insurance_prevalence$n_patient_years <- ifelse(
  qa_results$insurance_prevalence$n_patient_years < min_count,
  NA,
  qa_results$insurance_prevalence$n_patient_years
)
qa_results$insurance_prevalence$n_polypharmacy <- ifelse(
  qa_results$insurance_prevalence$n_polypharmacy < min_count,
  NA,
  qa_results$insurance_prevalence$n_polypharmacy
)

for (name in names(qa_results)) {
  message("QA: ", name)
  print(qa_results[[name]])
}

if (
  any(qa_results$final_completeness$denominator_rows != qa_results$final_completeness$final_rows) ||
    any(qa_results$final_completeness$missing_final_rows != 0)
) {
  stop("Annual polypharmacy final-table completeness checks failed.")
}

if (any(qa_results$duplicates$duplicate_rows != 0)) {
  stop("Annual polypharmacy duplicate checks failed.")
}

if (
  nrow(qa_results$episodes) > 0L &&
    any(qa_results$episodes$invalid_clipped_intervals != 0)
) {
  stop("Annual polypharmacy exposure interval checks failed.")
}

qa_frames <- lapply(
  names(qa_results),
  function(name) {
    data <- qa_results[[name]]
    data$qa_section <- name
    data[] <- lapply(data, as.character)
    data
  }
)
all_columns <- unique(unlist(lapply(qa_frames, names)))
qa_frames <- lapply(
  qa_frames,
  function(data) {
    missing_columns <- setdiff(all_columns, names(data))
    for (column in missing_columns) {
      data[[column]] <- NA_character_
    }
    data[, all_columns, drop = FALSE]
  }
)
qa_output <- do.call(rbind, qa_frames)

qa_path <- file.path(config$output_dir, "5.6_annual_polypharmacy_metrics_qa.csv")
utils::write.csv(qa_output, qa_path, row.names = FALSE)

message(
  config$workflow_label,
  " QA complete. Aggregate QA written to: ",
  qa_path
)

disconnect_komodo(con)
