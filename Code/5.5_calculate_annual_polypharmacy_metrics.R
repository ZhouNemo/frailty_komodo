source("Code/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-03
#
# ---- Purpose ----
# Calculate annual polypharmacy metrics from clipped ATC3 exposure episodes.
# The patient-day expansion uses a day-offset table sized from the selected
# analysis windows. Polypharmacy is defined as at least 5 active ATC3 classes on
# at least 90 days in the patient-year. The final table keeps one row per
# selected `2_annual_metric_ids` patient-year and writes:
#   - 6_annual_polypharmacy_metrics

config <- get_annual_polypharmacy_config()
con <- connect_komodo()

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
fills_identifier <- qualified_identifier(write_schema, config$fills_table)
crosswalk_identifier <- qualified_identifier(write_schema, config$crosswalk_table)
episodes_identifier <- qualified_identifier(write_schema, config$episodes_table)
final_identifier <- qualified_identifier(write_schema, config$final_table)
offsets_table <- "polypharmacy_day_offsets_5_5"
active_atc3_table <- "polypharmacy_active_atc3_days_5_5"
daily_counts_table <- "polypharmacy_daily_atc3_counts_5_5"
fill_mapping_table <- "polypharmacy_fill_mapping_status_5_5"
offsets_identifier <- quote_identifier(offsets_table)
active_atc3_identifier <- quote_identifier(active_atc3_table)
daily_counts_identifier <- quote_identifier(daily_counts_table)
fill_mapping_identifier <- quote_identifier(fill_mapping_table)

for (table in c(config$ids_table, config$fills_table, config$crosswalk_table, config$episodes_table)) {
  if (!table_exists(con, write_schema, table)) {
    stop("Required polypharmacy table was not found: ", write_schema, ".", table)
  }
}

if (!table_exists(con, write_schema, config$final_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", final_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         total_days_5plus INTEGER NOT NULL,
         polypharmacy INTEGER NOT NULL,
         n_active_class_days INTEGER NOT NULL,
         n_mapped_ndc11 INTEGER NOT NULL,
         n_unmapped_ndc11 INTEGER NOT NULL,
         n_mapped_fill_rows INTEGER NOT NULL,
         n_unmapped_fill_rows INTEGER NOT NULL,
         n_distinct_atc3 INTEGER NOT NULL,
         mapping_level VARCHAR(64) NOT NULL,
         mapping_source VARCHAR(128) NOT NULL,
         mapping_version_date VARCHAR(64) NOT NULL,
         art_excluded INTEGER NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid);"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$final_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "total_days_5plus",
    "polypharmacy",
    "n_active_class_days",
    "n_mapped_ndc11",
    "n_unmapped_ndc11",
    "n_mapped_fill_rows",
    "n_unmapped_fill_rows",
    "n_distinct_atc3"
  )
)

mapping_levels <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT DISTINCT mapping_level
     FROM ", crosswalk_identifier, "
     WHERE mapping_source = ", sql_string(config$mapping_source), "
       AND mapping_version_date = ", sql_string(config$mapping_version_date), "
       AND mapping_status = 'mapped'
       AND mapping_level IS NOT NULL
     ORDER BY mapping_level"
  )
)$mapping_level
mapping_level_label <- if (length(mapping_levels) == 0L) {
  config$mapping_level
} else {
  paste(unique(mapping_levels), collapse = "+")
}

max_window_days <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT MAX(DATEDIFF(day, analysis_start_date, analysis_end_date) + 1)::INTEGER
       AS max_window_days
     FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")"
  )
)$max_window_days[[1]]

if (is.na(max_window_days) || max_window_days <= 0L) {
  stop("Could not determine a positive selected-year analysis window length.")
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", offsets_identifier, ";
     CREATE TEMP TABLE ", offsets_identifier, " (
       day_offset INTEGER NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(day_offset);"
  ),
  progressBar = FALSE,
  reportOverallTime = FALSE
)
execute_insert_batches(
  con,
  offsets_identifier,
  c("day_offset"),
  make_day_offsets(max_window_days),
  numeric_columns = "day_offset"
)

execute_sql_with_retry(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", active_atc3_identifier, ";
     CREATE TEMP TABLE ", active_atc3_identifier, " (
       patid VARCHAR(256) NOT NULL,
       patient_id VARCHAR(256) NOT NULL,
       analysis_year INTEGER NOT NULL,
       date_day DATE NOT NULL,
       atc3 VARCHAR(16) NOT NULL
     )
     DISTKEY(patid)
     SORTKEY(analysis_year, patid, date_day, atc3);

     INSERT INTO ", active_atc3_identifier, " (
       patid,
       patient_id,
       analysis_year,
       date_day,
       atc3
     )
     SELECT DISTINCT
       e.patid,
       e.patient_id,
       e.analysis_year,
       DATEADD(day, o.day_offset, e.episode_start_clipped) AS date_day,
       e.atc3
     FROM ", episodes_identifier, " e
     INNER JOIN ", offsets_identifier, " o
       ON DATEADD(day, o.day_offset, e.episode_start_clipped) <= e.episode_end_clipped
     WHERE e.analysis_year IN (", sql_values(config$analysis_years), ");

     DROP TABLE IF EXISTS ", daily_counts_identifier, ";
     CREATE TEMP TABLE ", daily_counts_identifier, " AS
     SELECT
       patid,
       patient_id,
       analysis_year,
       date_day,
       COUNT(DISTINCT atc3)::INTEGER AS daily_atc3_count
     FROM ", active_atc3_identifier, "
     GROUP BY patid, patient_id, analysis_year, date_day;

     DROP TABLE IF EXISTS ", fill_mapping_identifier, ";
     CREATE TEMP TABLE ", fill_mapping_identifier, " AS
     SELECT
       f.patid,
       f.patient_id,
       f.analysis_year,
       COALESCE(f.pharmacy_event_id, f.patid || '|' || f.fill_date::VARCHAR || '|' || f.ndc11)
         AS fill_row_id,
       f.ndc11,
       MAX(CASE
         WHEN cw.mapping_status = 'mapped'
          AND cw.atc3 IS NOT NULL
          AND cw.atc3 <> ''
         THEN 1 ELSE 0 END)::INTEGER AS has_mapping
     FROM ", fills_identifier, " f
     LEFT JOIN ", crosswalk_identifier, " cw
       ON f.ndc11 = cw.ndc11
      AND cw.mapping_source = ", sql_string(config$mapping_source), "
      AND cw.mapping_version_date = ", sql_string(config$mapping_version_date), "
     WHERE f.analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY
       f.patid,
       f.patient_id,
       f.analysis_year,
       COALESCE(f.pharmacy_event_id, f.patid || '|' || f.fill_date::VARCHAR || '|' || f.ndc11),
       f.ndc11;"
  ),
  label = "polypharmacy day-level staging"
)

execute_sql_with_retry(
  con,
  paste0(
    "DELETE FROM ", final_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");

     INSERT INTO ", final_identifier, " (
       patid,
       patient_id,
       analysis_year,
       total_days_5plus,
       polypharmacy,
       n_active_class_days,
       n_mapped_ndc11,
       n_unmapped_ndc11,
       n_mapped_fill_rows,
       n_unmapped_fill_rows,
       n_distinct_atc3,
       mapping_level,
       mapping_source,
       mapping_version_date,
       art_excluded
     )
     WITH daily_summary AS (
       SELECT
         patid,
         patient_id,
         analysis_year,
         SUM(CASE WHEN daily_atc3_count >= 5 THEN 1 ELSE 0 END)::INTEGER
           AS total_days_5plus,
         COUNT(*)::INTEGER AS n_active_class_days
       FROM ", daily_counts_identifier, "
       GROUP BY patid, patient_id, analysis_year
     ),
     atc3_summary AS (
       SELECT
         patid,
         patient_id,
         analysis_year,
         COUNT(DISTINCT atc3)::INTEGER AS n_distinct_atc3
       FROM ", active_atc3_identifier, "
       GROUP BY patid, patient_id, analysis_year
     ),
     fill_summary AS (
       SELECT
         patid,
         patient_id,
         analysis_year,
         COUNT(DISTINCT CASE WHEN has_mapping = 1 THEN ndc11 END)::INTEGER
           AS n_mapped_ndc11,
         COUNT(DISTINCT CASE WHEN has_mapping = 0 THEN ndc11 END)::INTEGER
           AS n_unmapped_ndc11,
         SUM(CASE WHEN has_mapping = 1 THEN 1 ELSE 0 END)::INTEGER
           AS n_mapped_fill_rows,
         SUM(CASE WHEN has_mapping = 0 THEN 1 ELSE 0 END)::INTEGER
           AS n_unmapped_fill_rows
       FROM ", fill_mapping_identifier, "
       GROUP BY patid, patient_id, analysis_year
     )
     SELECT
       ids.patid,
       ids.patient_id,
       ids.analysis_year,
       COALESCE(d.total_days_5plus, 0) AS total_days_5plus,
       CASE WHEN COALESCE(d.total_days_5plus, 0) >= 90 THEN 1 ELSE 0 END
         AS polypharmacy,
       COALESCE(d.n_active_class_days, 0) AS n_active_class_days,
       COALESCE(f.n_mapped_ndc11, 0) AS n_mapped_ndc11,
       COALESCE(f.n_unmapped_ndc11, 0) AS n_unmapped_ndc11,
       COALESCE(f.n_mapped_fill_rows, 0) AS n_mapped_fill_rows,
       COALESCE(f.n_unmapped_fill_rows, 0) AS n_unmapped_fill_rows,
       COALESCE(a.n_distinct_atc3, 0) AS n_distinct_atc3,
       ", sql_string(mapping_level_label), " AS mapping_level,
       ", sql_string(config$mapping_source), " AS mapping_source,
       ", sql_string(config$mapping_version_date), " AS mapping_version_date,
       ", ifelse(isTRUE(config$art_excluded), 1L, 0L), " AS art_excluded
     FROM ", ids_identifier, " ids
     LEFT JOIN daily_summary d
       ON ids.patid = d.patid
      AND ids.analysis_year = d.analysis_year
     LEFT JOIN atc3_summary a
       ON ids.patid = a.patid
      AND ids.analysis_year = a.analysis_year
     LEFT JOIN fill_summary f
       ON ids.patid = f.patid
      AND ids.analysis_year = f.analysis_year
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ");"
  ),
  label = "annual polypharmacy metric final table build"
)

final_qa <- print_query(
  con,
  "Checking annual polypharmacy final table row counts.",
  paste0(
    "SELECT analysis_year,
       COUNT(*)::BIGINT AS final_rows,
       SUM(polypharmacy)::BIGINT AS polypharmacy_patient_years,
       MIN(total_days_5plus) AS min_total_days_5plus,
       MAX(total_days_5plus) AS max_total_days_5plus
     FROM ", final_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

if (any(final_qa$final_rows == 0)) {
  stop("Annual polypharmacy final table has zero selected-year rows.")
}

message(
  config$workflow_label,
  " metric calculation complete: ",
  write_schema,
  ".",
  config$final_table,
  "."
)

disconnect_komodo(con)
