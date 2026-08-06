source("Code/2_variable construction/6.0_annual_healthcare_utilization_helpers.R")

# Project: Frailty_Komoto healthcare utilization
# Author: Nemo Zhou
# Date started: 2026-07-17
# Date last updated: 2026-07-31
#
# ---- Purpose ----
# Write aggregate-only healthcare-utilization report inputs for the complete
# eligible 2022 cohort. The report inputs cover overall Komodo, medical
# insurance group, medical insurance segment, age group, sex, and CFI level.
# Visit prevalence is reported as the number and percentage of eligible
# patient-years with at least one visit. Duration summaries use one prepared
# utilization event per row, not raw claim service lines. A separate
# aggregate duration-frequency file supports weighted density plots without
# writing patient-level or utilization-level rows locally.

config <- get_annual_healthcare_utilization_config()
con <- connect_komodo()

for (item in list(
  list(
    schema = write_schema,
    table = config$combined_table,
    columns = c(
      "patient_id", "analysis_year", "mx_insurance_group",
      "mx_insurance_segment", "age", "patient_gender", "cfi_score"
    )
  ),
  list(
    schema = write_schema,
    table = config$events_table,
    columns = c("patient_id", "analysis_year", "utilization_category", "duration_days")
  )
)) {
  if (!table_exists(con, item$schema, item$table)) {
    stop("Required table was not found: ", item$schema, ".", item$table)
  }
  table_has_columns(con, item$schema, item$table, item$columns)
}

if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

combined_identifier <- qualified_identifier(write_schema, config$combined_table)
events_identifier <- qualified_identifier(write_schema, config$events_table)

category_values <- paste(
  vapply(
    utilization_categories$utilization_category,
    function(category) {
      paste0("SELECT ", sql_string(category), " AS utilization_category")
    },
    character(1)
  ),
  collapse = "\nUNION ALL\n"
)

normalized_text_sql <- function(column_sql) {
  paste0(
    "CASE WHEN ", column_sql, " IS NULL OR NULLIF(TRIM(", column_sql,
    "), '') IS NULL OR UPPER(TRIM(", column_sql,
    ")) IN ('UNKNOWN', 'UNK') THEN 'Unknown' ELSE UPPER(TRIM(",
    column_sql, ")) END"
  )
}

age_group_sql <- paste0(
  "CASE\n",
  "  WHEN cohort.age BETWEEN 40 AND 49 THEN '40-49'\n",
  "  WHEN cohort.age BETWEEN 50 AND 64 THEN '50-64'\n",
  "  WHEN cohort.age BETWEEN 65 AND 74 THEN '65-74'\n",
  "  WHEN cohort.age BETWEEN 75 AND 84 THEN '75-84'\n",
  "  WHEN cohort.age >= 85 THEN '85+'\n",
  "  ELSE 'Unknown'\n",
  "END"
)

cfi_level_sql <- paste0(
  "CASE\n",
  "  WHEN cohort.cfi_score < 0.15 THEN 'Robust'\n",
  "  WHEN cohort.cfi_score < 0.25 THEN 'Prefrail'\n",
  "  WHEN cohort.cfi_score >= 0.25 THEN 'Frail'\n",
  "  ELSE 'Unknown'\n",
  "END"
)

insurance_group_sql <- normalized_text_sql("cohort.mx_insurance_group")
insurance_segment_sql <- normalized_text_sql("cohort.mx_insurance_segment")
sex_sql <- normalized_text_sql("cohort.patient_gender")

common_ctes <- paste0(
  "WITH cohort AS (\n",
  "  SELECT DISTINCT\n",
  "    patient_id, analysis_year, age, patient_gender, cfi_score,\n",
  "    mx_insurance_group, mx_insurance_segment\n",
  "  FROM ", combined_identifier, "\n",
  "  WHERE analysis_year = ", config$analysis_year, "\n",
  "), strata AS (\n",
  "  SELECT patient_id, analysis_year, 'overall' AS stratification,\n",
  "         'Komodo' AS stratum_value\n",
  "  FROM cohort\n",
  "  UNION ALL\n",
  "  SELECT patient_id, analysis_year, 'mx_insurance_group' AS stratification,\n",
  "         ", insurance_group_sql, " AS stratum_value\n",
  "  FROM cohort\n",
  "  UNION ALL\n",
  "  SELECT patient_id, analysis_year, 'mx_insurance_segment' AS stratification,\n",
  "         ", insurance_segment_sql, " AS stratum_value\n",
  "  FROM cohort\n",
  "  UNION ALL\n",
  "  SELECT patient_id, analysis_year, 'age_group' AS stratification,\n",
  "         ", age_group_sql, " AS stratum_value\n",
  "  FROM cohort\n",
  "  UNION ALL\n",
  "  SELECT patient_id, analysis_year, 'sex' AS stratification,\n",
  "         ", sex_sql, " AS stratum_value\n",
  "  FROM cohort\n",
  "  UNION ALL\n",
  "  SELECT patient_id, analysis_year, 'cfi_level' AS stratification,\n",
  "         ", cfi_level_sql, " AS stratum_value\n",
  "  FROM cohort\n",
  "), strata_summary AS (\n",
  "  SELECT stratification, stratum_value,\n",
  "         COUNT(DISTINCT patient_id)::BIGINT AS n_eligible_patient_years\n",
  "  FROM strata\n",
  "  GROUP BY stratification, stratum_value\n",
  "), categories AS (\n",
  category_values, "\n",
  "), event_rows AS (\n",
  "  SELECT s.stratification, s.stratum_value, events.patient_id,\n",
  "         events.utilization_category, events.duration_days\n",
  "  FROM strata s\n",
  "  INNER JOIN ", events_identifier, " events\n",
  "    ON s.patient_id = events.patient_id\n",
  "   AND s.analysis_year = events.analysis_year\n",
  ")"
)

summary_sql <- paste0(
  common_ctes, ", event_counts AS (\n",
  "  SELECT stratification, stratum_value, utilization_category,\n",
  "         COUNT(DISTINCT patient_id)::BIGINT AS n_patients_with_visit,\n",
  "         COUNT(*)::BIGINT AS n_visits,\n",
  "         SUM(CASE WHEN duration_days IS NULL THEN 1 ELSE 0 END)::BIGINT\n",
  "           AS duration_missing_n\n",
  "  FROM event_rows\n",
  "  GROUP BY stratification, stratum_value, utilization_category\n",
  "), duration_summary AS (\n",
  "  SELECT stratification, stratum_value, utilization_category,\n",
  "         COUNT(duration_days)::BIGINT AS duration_n,\n",
  "         SUM(duration_days)::BIGINT AS total_duration_days,\n",
  "         AVG(duration_days::DOUBLE PRECISION) AS duration_mean_days,\n",
  "         STDDEV_SAMP(duration_days::DOUBLE PRECISION) AS duration_sd_days,\n",
  "         MIN(duration_days)::DOUBLE PRECISION AS duration_min_days,\n",
  "         PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY duration_days)\n",
  "           AS duration_p25_days,\n",
  "         PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY duration_days)\n",
  "           AS duration_median_days,\n",
  "         PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY duration_days)\n",
  "           AS duration_p75_days,\n",
  "         MAX(duration_days)::DOUBLE PRECISION AS duration_max_days\n",
  "  FROM event_rows\n",
  "  WHERE duration_days IS NOT NULL\n",
  "  GROUP BY stratification, stratum_value, utilization_category\n",
  "), event_summary AS (\n",
  "  SELECT ec.stratification, ec.stratum_value, ec.utilization_category,\n",
  "         ec.n_patients_with_visit, ec.n_visits,\n",
  "         COALESCE(ds.duration_n, 0)::BIGINT AS duration_n,\n",
  "         ec.duration_missing_n, ds.total_duration_days,\n",
  "         ds.duration_mean_days, ds.duration_sd_days,\n",
  "         ds.duration_min_days, ds.duration_p25_days,\n",
  "         ds.duration_median_days, ds.duration_p75_days,\n",
  "         ds.duration_max_days\n",
  "  FROM event_counts ec\n",
  "  LEFT JOIN duration_summary ds\n",
  "    ON ec.stratification = ds.stratification\n",
  "   AND ec.stratum_value = ds.stratum_value\n",
  "   AND ec.utilization_category = ds.utilization_category\n",
  ")\n",
  "SELECT ", config$analysis_year, "::INTEGER AS analysis_year,\n",
  "  ss.stratification, ss.stratum_value, categories.utilization_category,\n",
  "  ss.n_eligible_patient_years,\n",
  "  COALESCE(es.n_patients_with_visit, 0)::BIGINT AS n_patients_with_visit,\n",
  "  100.0 * COALESCE(es.n_patients_with_visit, 0)::DOUBLE PRECISION /\n",
  "    NULLIF(ss.n_eligible_patient_years, 0) AS pct_patient_years_with_visit,\n",
  "  COALESCE(es.n_visits, 0)::BIGINT AS n_visits,\n",
  "  COALESCE(es.duration_n, 0)::BIGINT AS duration_n,\n",
  "  COALESCE(es.duration_missing_n, 0)::BIGINT AS duration_missing_n,\n",
  "  100.0 * COALESCE(es.duration_missing_n, 0)::DOUBLE PRECISION /\n",
  "    NULLIF(COALESCE(es.n_visits, 0), 0) AS duration_missing_pct,\n",
  "  es.total_duration_days, es.duration_mean_days, es.duration_sd_days,\n",
  "  es.duration_min_days, es.duration_p25_days, es.duration_median_days,\n",
  "  es.duration_p75_days, es.duration_max_days,\n",
  "  es.duration_p75_days - es.duration_p25_days AS duration_iqr_days\n",
  "FROM strata_summary ss\n",
  "CROSS JOIN categories\n",
  "LEFT JOIN event_summary es\n",
  "  ON ss.stratification = es.stratification\n",
  " AND ss.stratum_value = es.stratum_value\n",
  " AND categories.utilization_category = es.utilization_category\n",
  "WHERE ss.n_eligible_patient_years >= ", config$min_count, "\n",
  "ORDER BY ss.stratification, ss.stratum_value, categories.utilization_category"
)

report_inputs <- DBI::dbGetQuery(con, summary_sql)

small_cell <- (
  report_inputs$n_eligible_patient_years < config$min_count |
    (
      report_inputs$n_patients_with_visit >= 1L &
        report_inputs$n_patients_with_visit < config$min_count
    ) |
    (
      report_inputs$n_visits >= 1L &
        report_inputs$n_visits < config$min_count
    )
)
report_inputs$suppression_applied <- ifelse(small_cell, "yes", "no")

suppressed_columns <- c(
  "n_patients_with_visit", "pct_patient_years_with_visit", "n_visits",
  "duration_n", "duration_missing_n", "duration_missing_pct",
  "total_duration_days", "duration_mean_days", "duration_sd_days",
  "duration_min_days", "duration_p25_days", "duration_median_days",
  "duration_p75_days", "duration_max_days", "duration_iqr_days"
)
for (column in suppressed_columns) {
  report_inputs[[column]][small_cell] <- NA
}

duration_distribution_sql <- paste0(
  common_ctes,
  "\nSELECT ", config$analysis_year, "::INTEGER AS analysis_year,\n",
  "  stratification, stratum_value, utilization_category, duration_days,\n",
  "  COUNT(*)::BIGINT AS n_duration_events\n",
  "FROM event_rows\n",
  "WHERE duration_days IS NOT NULL\n",
  "GROUP BY stratification, stratum_value, utilization_category, duration_days\n",
  "ORDER BY stratification, stratum_value, utilization_category, duration_days"
)
duration_distribution <- DBI::dbGetQuery(con, duration_distribution_sql)

summary_key <- paste(
  report_inputs$stratification,
  report_inputs$stratum_value,
  report_inputs$utilization_category,
  sep = "\r"
)
distribution_key <- paste(
  duration_distribution$stratification,
  duration_distribution$stratum_value,
  duration_distribution$utilization_category,
  sep = "\r"
)
distribution_suppressed <- (
  report_inputs$suppression_applied[match(distribution_key, summary_key)] == "yes" |
    duration_distribution$n_duration_events < config$min_count
)
duration_distribution$suppression_applied <- ifelse(distribution_suppressed, "yes", "no")
duration_distribution$n_duration_events[distribution_suppressed] <- NA
duration_distribution <- duration_distribution[
  !is.na(duration_distribution$n_duration_events),
  ,
  drop = FALSE
]

overall_inputs <- report_inputs[
  report_inputs$stratification %in% c(
    "overall", "mx_insurance_group", "mx_insurance_segment"
  ),
  ,
  drop = FALSE
]
subgroup_inputs <- report_inputs[
  report_inputs$stratification %in% c("age_group", "sex", "cfi_level"),
  ,
  drop = FALSE
]

utilization_write_csv(
  overall_inputs,
  file.path(
    config$output_dir,
    paste0("6.4_healthcare_utilization_summary_", config$analysis_year, ".csv")
  )
)
utilization_write_csv(
  subgroup_inputs,
  file.path(
    config$output_dir,
    paste0(
      "6.4_healthcare_utilization_summary_by_subgroup_",
      config$analysis_year,
      ".csv"
    )
  )
)
utilization_write_csv(
  duration_distribution,
  file.path(
    config$output_dir,
    paste0(
      "6.4_healthcare_utilization_duration_distribution_",
      config$analysis_year,
      ".csv"
    )
  )
)

message("Healthcare utilization report-input CSV generation complete.")
disconnect_komodo(con)
