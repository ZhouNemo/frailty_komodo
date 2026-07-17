source("Code/2_variable construction/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Calculate annual polypharmacy metrics from clipped ATC3 exposure episodes.
# The calculation collapses overlapping exposure intervals within each
# patient-year and ATC3 class, converts those intervals to active-class count
# change events, and sums the resulting spans where at least 5 ATC3 classes are
# active. This avoids materializing one row per patient-day-ATC3 class while
# preserving the same annual definition: at least 5 active ATC3 classes on at
# least 90 days in the patient-year. The final table keeps one row per selected
# `2_annual_metric_ids` patient-year and writes:
#   - 6_annual_polypharmacy_metrics

config <- get_annual_polypharmacy_config()
con <- connect_komodo()

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
fills_identifier <- qualified_identifier(write_schema, config$fills_table)
crosswalk_identifier <- qualified_identifier(write_schema, config$crosswalk_table)
episodes_identifier <- qualified_identifier(write_schema, config$episodes_table)
final_identifier <- qualified_identifier(write_schema, config$final_table)
merged_intervals_table <- "polypharmacy_atc3_merged_intervals_5_5"
change_events_table <- "polypharmacy_atc3_change_events_5_5"
active_spans_table <- "polypharmacy_active_atc3_spans_5_5"
active_summary_table <- "polypharmacy_active_atc3_summary_5_5"
atc3_summary_table <- "polypharmacy_atc3_summary_5_5"
fill_mapping_table <- "polypharmacy_fill_mapping_status_5_5"
merged_intervals_identifier <- quote_identifier(merged_intervals_table)
change_events_identifier <- quote_identifier(change_events_table)
active_spans_identifier <- quote_identifier(active_spans_table)
active_summary_identifier <- quote_identifier(active_summary_table)
atc3_summary_identifier <- quote_identifier(atc3_summary_table)
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

run_sql_stage <- function(label, sql) {
  started_at <- Sys.time()
  message(format(started_at, "[%Y-%m-%d %H:%M:%S] "), "START ", label)
  execute_sql_with_retry(con, sql, label = label)
  finished_at <- Sys.time()
  elapsed_minutes <- round(
    as.numeric(difftime(finished_at, started_at, units = "mins")),
    1
  )
  message(
    format(finished_at, "[%Y-%m-%d %H:%M:%S] "),
    "DONE  ",
    label,
    " (",
    elapsed_minutes,
    " min)"
  )
  invisible(NULL)
}

run_sql_stage(
  "Build merged ATC3 exposure intervals",
  paste0(
    "DROP TABLE IF EXISTS ", merged_intervals_identifier, ";
     CREATE TEMP TABLE ", merged_intervals_identifier, " (
       patid VARCHAR(256) NOT NULL,
       patient_id VARCHAR(256) NOT NULL,
       analysis_year INTEGER NOT NULL,
       atc3 VARCHAR(16) NOT NULL,
       interval_start DATE NOT NULL,
       interval_end DATE NOT NULL
     )
     DISTKEY(patid)
     SORTKEY(analysis_year, patid, atc3, interval_start);

     INSERT INTO ", merged_intervals_identifier, " (
       patid,
       patient_id,
       analysis_year,
       atc3,
       interval_start,
       interval_end
     )
     WITH ordered_intervals AS (
       SELECT
         patid,
         patient_id,
         analysis_year,
         atc3,
         episode_start_clipped,
         episode_end_clipped,
         MAX(episode_end_clipped) OVER (
           PARTITION BY patid, patient_id, analysis_year, atc3
           ORDER BY episode_start_clipped, episode_end_clipped
           ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
         ) AS previous_max_end
       FROM ", episodes_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ")
         AND atc3 IS NOT NULL
         AND atc3 <> ''
         AND episode_start_clipped IS NOT NULL
         AND episode_end_clipped IS NOT NULL
         AND episode_end_clipped >= episode_start_clipped
     ),
     interval_flags AS (
       SELECT
         *,
         CASE
           WHEN previous_max_end IS NULL THEN 1
           WHEN episode_start_clipped > DATEADD(day, 1, previous_max_end) THEN 1
           ELSE 0
         END AS starts_new_interval
       FROM ordered_intervals
     ),
     interval_groups AS (
       SELECT
         *,
         SUM(starts_new_interval) OVER (
           PARTITION BY patid, patient_id, analysis_year, atc3
           ORDER BY episode_start_clipped, episode_end_clipped
           ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
         ) AS interval_group
       FROM interval_flags
     )
     SELECT
       patid,
       patient_id,
       analysis_year,
       atc3,
       MIN(episode_start_clipped) AS interval_start,
       MAX(episode_end_clipped) AS interval_end
     FROM interval_groups
     GROUP BY patid, patient_id, analysis_year, atc3, interval_group;
     "
  )
)

run_sql_stage(
  "Build ATC3 active-count change events",
  paste0(
    "DROP TABLE IF EXISTS ", change_events_identifier, ";
     CREATE TEMP TABLE ", change_events_identifier, " (
       patid VARCHAR(256) NOT NULL,
       patient_id VARCHAR(256) NOT NULL,
       analysis_year INTEGER NOT NULL,
       event_date DATE NOT NULL,
       atc3_delta INTEGER NOT NULL
     )
     DISTKEY(patid)
     SORTKEY(analysis_year, patid, event_date);

     INSERT INTO ", change_events_identifier, " (
       patid,
       patient_id,
       analysis_year,
       event_date,
       atc3_delta
     )
     SELECT
       patid,
       patient_id,
       analysis_year,
       event_date,
       SUM(atc3_delta)::INTEGER AS atc3_delta
     FROM (
       SELECT
         patid,
         patient_id,
         analysis_year,
         interval_start AS event_date,
         1 AS atc3_delta
       FROM ", merged_intervals_identifier, "
       UNION ALL
       SELECT
         patid,
         patient_id,
         analysis_year,
         DATEADD(day, 1, interval_end) AS event_date,
         -1 AS atc3_delta
       FROM ", merged_intervals_identifier, "
     ) events
     GROUP BY patid, patient_id, analysis_year, event_date;
     "
  )
)

run_sql_stage(
  "Build ATC3 active-count spans",
  paste0(
    "DROP TABLE IF EXISTS ", active_spans_identifier, ";
     CREATE TEMP TABLE ", active_spans_identifier, " AS
     WITH running_counts AS (
       SELECT
         patid,
         patient_id,
         analysis_year,
         event_date,
         CAST(
           SUM(atc3_delta) OVER (
             PARTITION BY patid, patient_id, analysis_year
             ORDER BY event_date
             ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
           ) AS INTEGER
         ) AS active_atc3_count,
         LEAD(event_date) OVER (
           PARTITION BY patid, patient_id, analysis_year
           ORDER BY event_date
         ) AS next_event_date
       FROM ", change_events_identifier, "
     )
     SELECT
       patid,
       patient_id,
       analysis_year,
       event_date AS span_start,
       next_event_date AS span_end_exclusive,
       active_atc3_count,
       DATEDIFF(day, event_date, next_event_date)::INTEGER AS span_days
     FROM running_counts
     WHERE next_event_date IS NOT NULL
       AND next_event_date > event_date;
     "
  )
)

run_sql_stage(
  "Summarize annual active ATC3 days",
  paste0(
    "DROP TABLE IF EXISTS ", active_summary_identifier, ";
     CREATE TEMP TABLE ", active_summary_identifier, " AS
     SELECT
       patid,
       patient_id,
       analysis_year,
       SUM(CASE WHEN active_atc3_count >= 5 THEN span_days ELSE 0 END)::INTEGER
         AS total_days_5plus,
       SUM(CASE WHEN active_atc3_count >= 1 THEN span_days ELSE 0 END)::INTEGER
         AS n_active_class_days
     FROM ", active_spans_identifier, "
     GROUP BY patid, patient_id, analysis_year;
     "
  )
)

run_sql_stage(
  "Summarize annual distinct ATC3 classes",
  paste0(
    "DROP TABLE IF EXISTS ", atc3_summary_identifier, ";
     CREATE TEMP TABLE ", atc3_summary_identifier, " AS
     SELECT
       patid,
       patient_id,
       analysis_year,
       COUNT(DISTINCT atc3)::INTEGER AS n_distinct_atc3
     FROM ", merged_intervals_identifier, "
     GROUP BY patid, patient_id, analysis_year;
     "
  )
)

run_sql_stage(
  "Summarize NDC11 mapping coverage for fills",
  paste0(
    "DROP TABLE IF EXISTS ", fill_mapping_identifier, ";
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
  )
)

run_sql_stage(
  "Build final annual polypharmacy metrics table",
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
         total_days_5plus,
         n_active_class_days
       FROM ", active_summary_identifier, "
     ),
     atc3_summary AS (
       SELECT
         patid,
         patient_id,
         analysis_year,
         n_distinct_atc3
       FROM ", atc3_summary_identifier, "
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
  )
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
