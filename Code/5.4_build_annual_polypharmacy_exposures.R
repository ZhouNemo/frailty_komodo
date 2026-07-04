source("Code/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-03
#
# ---- Purpose ----
# Join cleaned selected-year pharmacy fills to the staged NDC11-to-ATC
# crosswalk and build calendar-year-clipped ATC3 exposure episodes. One fill can
# produce multiple rows if its NDC maps to multiple ATC classes. The script
# writes selected years to:
#   - 2_annual_polypharmacy_exposure_episodes

config <- get_annual_polypharmacy_config()
con <- connect_komodo()

fills_identifier <- qualified_identifier(write_schema, config$fills_table)
crosswalk_identifier <- qualified_identifier(write_schema, config$crosswalk_table)
episodes_identifier <- qualified_identifier(write_schema, config$episodes_table)

for (table in c(config$fills_table, config$crosswalk_table)) {
  if (!table_exists(con, write_schema, table)) {
    stop("Required polypharmacy table was not found: ", write_schema, ".", table)
  }
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
  config$crosswalk_table,
  c("ndc11", "atc4", "atc3", "mapping_source", "mapping_version_date", "mapping_status")
)

if (!table_exists(con, write_schema, config$episodes_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", episodes_identifier, " (
         patid VARCHAR(256) NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         pharmacy_event_id VARCHAR(256),
         ndc11 VARCHAR(32) NOT NULL,
         atc4 VARCHAR(32),
         atc3 VARCHAR(16) NOT NULL,
         fill_date DATE NOT NULL,
         days_supply INTEGER NOT NULL,
         episode_start DATE NOT NULL,
         episode_end DATE NOT NULL,
         episode_start_clipped DATE NOT NULL,
         episode_end_clipped DATE NOT NULL,
         mapping_source VARCHAR(128) NOT NULL,
         mapping_version_date VARCHAR(64) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, patid, episode_start_clipped, atc3);"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$episodes_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "pharmacy_event_id",
    "ndc11",
    "atc3",
    "episode_start_clipped",
    "episode_end_clipped"
  )
)

execute_sql_with_retry(
  con,
  paste0(
    "DELETE FROM ", episodes_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");

     INSERT INTO ", episodes_identifier, " (
       patid,
       patient_id,
       analysis_year,
       pharmacy_event_id,
       ndc11,
       atc4,
       atc3,
       fill_date,
       days_supply,
       episode_start,
       episode_end,
       episode_start_clipped,
       episode_end_clipped,
       mapping_source,
       mapping_version_date
     )
     SELECT DISTINCT
       f.patid,
       f.patient_id,
       f.analysis_year,
       f.pharmacy_event_id,
       f.ndc11,
       cw.atc4,
       cw.atc3,
       f.fill_date,
       f.days_supply,
       f.fill_date AS episode_start,
       DATEADD(day, f.days_supply - 1, f.fill_date) AS episode_end,
       CASE
         WHEN f.fill_date > f.analysis_start_date THEN f.fill_date
         ELSE f.analysis_start_date
       END AS episode_start_clipped,
       CASE
         WHEN DATEADD(day, f.days_supply - 1, f.fill_date) < f.analysis_end_date
         THEN DATEADD(day, f.days_supply - 1, f.fill_date)
         ELSE f.analysis_end_date
       END AS episode_end_clipped,
       cw.mapping_source,
       cw.mapping_version_date
     FROM ", fills_identifier, " f
     INNER JOIN ", crosswalk_identifier, " cw
       ON f.ndc11 = cw.ndc11
      AND cw.mapping_source = ", sql_string(config$mapping_source), "
      AND cw.mapping_version_date = ", sql_string(config$mapping_version_date), "
      AND cw.mapping_status = 'mapped'
      AND cw.atc3 IS NOT NULL
      AND cw.atc3 <> ''
     WHERE f.analysis_year IN (", sql_values(config$analysis_years), ")
       AND DATEADD(day, f.days_supply - 1, f.fill_date) >= f.analysis_start_date
       AND f.fill_date <= f.analysis_end_date;"
  ),
  label = "polypharmacy exposure episode build"
)

invalid_episode_qa <- print_query(
  con,
  "Checking polypharmacy exposure episode intervals.",
  paste0(
    "SELECT analysis_year,
       COUNT(*)::BIGINT AS episode_rows,
       SUM(CASE WHEN episode_end_clipped < episode_start_clipped
         THEN 1 ELSE 0 END)::BIGINT AS invalid_clipped_intervals,
       COUNT(DISTINCT patid)::BIGINT AS patient_years_with_mapped_episodes,
       COUNT(DISTINCT atc3)::BIGINT AS distinct_atc3
     FROM ", episodes_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

if (any(invalid_episode_qa$invalid_clipped_intervals != 0)) {
  stop("Polypharmacy exposure episode table contains invalid clipped intervals.")
}

message(
  config$workflow_label,
  " exposure episode build complete: ",
  write_schema,
  ".",
  config$episodes_table,
  "."
)

disconnect_komodo(con)
