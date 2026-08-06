source("Code/2_variable construction/6.0_annual_healthcare_utilization_helpers.R")

# Project: Frailty_Komoto healthcare utilization
# Author: Nemo Zhou
# Date started: 2026-07-17
# Date last updated: 2026-08-04
#
# ---- Purpose ----
# Produce aggregate-only quality assurance for the 2022 healthcare-utilization
# workflow. The checks verify cohort restriction, event collapse, dates and
# duration behavior, source classifications, validation fields, and final
# patient-year completeness. No patient-level rows are written locally.

config <- get_annual_healthcare_utilization_config()
con <- connect_komodo()

required_tables <- list(
  list(schema = write_schema, table = config$cohort_table),
  list(schema = write_schema, table = config$events_table),
  list(schema = write_schema, table = config$metrics_table),
  list(schema = write_schema, table = config$combined_table),
  list(schema = komodo_schema, table = config$inpatient_table),
  list(schema = komodo_schema, table = config$non_inpatient_table)
)
for (item in required_tables) {
  if (!table_exists(con, item$schema, item$table)) {
    stop("Required table was not found: ", item$schema, ".", item$table)
  }
}
table_has_columns(
  con,
  komodo_schema,
  config$non_inpatient_table,
  c("patient_id", "utilization_id", "service_date", "procedure_code")
)

if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

cohort_sql <- utilization_cohort_sql(config)
events_identifier <- qualified_identifier(write_schema, config$events_table)
metrics_identifier <- qualified_identifier(write_schema, config$metrics_table)
combined_identifier <- qualified_identifier(write_schema, config$combined_table)
inpatient_identifier <- qualified_identifier(komodo_schema, config$inpatient_table)
non_inpatient_identifier <- qualified_identifier(komodo_schema, config$non_inpatient_table)
year_start <- sql_string(utilization_year_start(config))
year_end <- sql_string(utilization_year_end(config))

metric_table_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", metrics_identifier, " LIMIT 0")
)))
combined_table_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", combined_identifier, " LIMIT 0")
)))
required_ed_metric_columns <- c(
  "ed_visit_type_any_visit",
  "ed_service_subcategory_any_visit",
  "ed_revenue_code_any_visit"
)
legacy_ed_metric_columns <- c(
  "ed_any_visit",
  "ed_n_visits",
  "ed_total_duration_days",
  "ed_duration_n",
  "ed_duration_missing_n"
)
missing_ed_metric_columns <- setdiff(required_ed_metric_columns, metric_table_columns)
required_long_term_care_columns <- "long_term_care_any_visit"
missing_long_term_care_columns <- unique(c(
  setdiff(required_long_term_care_columns, metric_table_columns),
  setdiff(required_long_term_care_columns, combined_table_columns)
))
stale_ed_metric_columns <- intersect(legacy_ed_metric_columns, metric_table_columns)
stale_ed_combined_columns <- intersect(legacy_ed_metric_columns, combined_table_columns)
if (length(missing_ed_metric_columns) > 0L) {
  stop(
    "Healthcare utilization metrics are missing required ED prevalence columns: ",
    paste(missing_ed_metric_columns, collapse = ", ")
  )
}
if (length(missing_long_term_care_columns) > 0L) {
  stop(
    "Healthcare utilization outputs are missing the long-term-care indicator: ",
    paste(missing_long_term_care_columns, collapse = ", ")
  )
}
if (length(stale_ed_metric_columns) > 0L || length(stale_ed_combined_columns) > 0L) {
  stop(
    "Legacy ED utilization columns remain in the active output tables: ",
    paste(unique(c(stale_ed_metric_columns, stale_ed_combined_columns)), collapse = ", ")
  )
}

run_qa <- function(section, sql) {
  data <- DBI::dbGetQuery(con, sql)
  data$qa_section <- section
  data
}

qa_results <- list(
  cohort = run_qa(
    "cohort",
    paste0(
      "SELECT mx_insurance_segment, COUNT(*)::BIGINT AS n_patient_years\n",
      "FROM (", cohort_sql, ") cohort\n",
      "GROUP BY mx_insurance_segment\n",
      "ORDER BY mx_insurance_segment"
    )
  ),
  event_counts = run_qa(
    "event_counts",
    paste0(
      "SELECT utilization_category, source_table, COUNT(*)::BIGINT AS n_events,\n",
      "       COUNT(DISTINCT patient_id)::BIGINT AS n_patients,\n",
      "       SUM(n_source_lines)::BIGINT AS n_source_lines\n",
      "FROM ", events_identifier, "\n",
      "WHERE analysis_year = ", config$analysis_year, "\n",
      "GROUP BY utilization_category, source_table\n",
      "ORDER BY utilization_category"
    )
  ),
  missing_utilization_ids = run_qa(
    "missing_utilization_ids",
    paste0(
      "WITH cohort AS (\n", cohort_sql, "\n)\n",
      "SELECT 'inpatient_events' AS source_table, COUNT(*)::BIGINT AS n_source_rows\n",
      "FROM ", inpatient_identifier, " inp\n",
      "INNER JOIN cohort ON inp.patient_id = cohort.patient_id\n",
      "WHERE inp.utilization_id IS NULL\n",
      "  AND inp.claim_from_date >= ", year_start, "::DATE\n",
      "  AND inp.claim_from_date < ", year_end, "::DATE\n",
      "  AND UPPER(TRIM(inp.visit_type)) IN (\n",
      "    'ACUTE INPATIENT', 'HOSPICE (INPATIENT)', 'SKILLED NURSING',\n",
      "    'IPF'\n",
      "  )\n",
      "UNION ALL\n",
      "SELECT 'non_inpatient_ed_source_lines' AS source_table, COUNT(*)::BIGINT AS n_source_rows\n",
      "FROM ", non_inpatient_identifier, " nie\n",
      "INNER JOIN cohort ON nie.patient_id = cohort.patient_id\n",
      "WHERE nie.utilization_id IS NULL\n",
      "  AND nie.service_date < ", year_end, "::DATE\n",
      "  AND (\n",
      "    nie.service_to_date >= ", year_start, "::DATE\n",
      "    OR (nie.service_to_date IS NULL AND nie.service_date >= ", year_start, "::DATE)\n",
      "  )\n",
      "  AND (\n",
      "    UPPER(REPLACE(TRIM(nie.visit_type), ' ', '_')) = 'OUTPATIENT_ED'\n",
      "    OR UPPER(TRIM(nie.service_subcategory)) = 'EMERGENCY DEPT ENCOUNTER'\n",
      "    OR LPAD(TRIM(nie.revenue_code), 4, '0') IN\n",
      "      ('0450', '0451', '0452', '0456', '0459', '0981')\n",
      "  )"
    )
  ),
  event_cohort_membership = run_qa(
    "event_cohort_membership",
    paste0(
      "WITH cohort AS (\n", cohort_sql, "\n)\n",
      "SELECT SUM(CASE WHEN cohort.patient_id IS NULL THEN 1 ELSE 0 END)::BIGINT\n",
      "  AS n_events_outside_cohort\n",
      "FROM ", events_identifier, " events\n",
      "LEFT JOIN cohort\n",
      "  ON events.patient_id = cohort.patient_id\n",
      " AND events.analysis_year = cohort.analysis_year\n",
      "WHERE events.analysis_year = ", config$analysis_year
    )
  ),
  observed_visit_types = run_qa(
    "observed_visit_types",
    paste0(
      "WITH cohort AS (\n", cohort_sql, "\n)\n",
      "SELECT 'inpatient_events' AS source_table,\n",
      "       COALESCE(NULLIF(TRIM(inp.visit_type), ''), 'MISSING') AS visit_type,\n",
      "       COUNT(*)::BIGINT AS n_rows\n",
      "FROM ", inpatient_identifier, " inp\n",
      "INNER JOIN cohort ON inp.patient_id = cohort.patient_id\n",
      "WHERE inp.claim_from_date >= ", year_start, "::DATE\n",
      "  AND inp.claim_from_date < ", year_end, "::DATE\n",
      "GROUP BY 1, 2\n",
      "UNION ALL\n",
      "SELECT 'non_inpatient_events' AS source_table,\n",
      "       COALESCE(NULLIF(TRIM(nie.visit_type), ''), 'MISSING') AS visit_type,\n",
      "       COUNT(*)::BIGINT AS n_rows\n",
      "FROM ", non_inpatient_identifier, " nie\n",
      "INNER JOIN cohort ON nie.patient_id = cohort.patient_id\n",
      "WHERE nie.service_date < ", year_end, "::DATE\n",
      "  AND (\n",
      "    nie.service_to_date >= ", year_start, "::DATE\n",
      "    OR (nie.service_to_date IS NULL AND nie.service_date >= ", year_start, "::DATE)\n",
      "  )\n",
      "GROUP BY 1, 2"
    )
  ),
  ed_validation = run_qa(
    "ed_validation",
    paste0(
      "SELECT ed_classification_pattern, ed_visit_type_flag,\n",
      "       ed_service_subcategory_flag, ed_revenue_code_flag,\n",
      "       COALESCE(place_of_service, 'MISSING') AS place_of_service,\n",
      "       COUNT(*)::BIGINT AS n_events, SUM(n_source_lines)::BIGINT AS n_source_lines\n",
      "FROM ", events_identifier, "\n",
      "WHERE analysis_year = ", config$analysis_year, "\n",
      "  AND utilization_category = 'Emergency Department'\n",
      "GROUP BY ed_classification_pattern, ed_visit_type_flag,\n",
      "         ed_service_subcategory_flag, ed_revenue_code_flag,\n",
      "         COALESCE(place_of_service, 'MISSING')\n",
      "ORDER BY ed_classification_pattern, n_events DESC"
    )
  ),
  ed_definition_combinations = run_qa(
    "ed_definition_combinations",
    paste0(
      "WITH flag_combinations AS (\n",
      "  SELECT 0 AS ed_visit_type_flag, 0 AS ed_service_subcategory_flag, 0 AS ed_revenue_code_flag\n",
      "  UNION ALL SELECT 0, 0, 1\n",
      "  UNION ALL SELECT 0, 1, 0\n",
      "  UNION ALL SELECT 0, 1, 1\n",
      "  UNION ALL SELECT 1, 0, 0\n",
      "  UNION ALL SELECT 1, 0, 1\n",
      "  UNION ALL SELECT 1, 1, 0\n",
      "  UNION ALL SELECT 1, 1, 1\n",
      "), observed AS (\n",
      "  SELECT ed_visit_type_flag, ed_service_subcategory_flag,\n",
      "         ed_revenue_code_flag, ed_classification_pattern,\n",
      "         COUNT(*)::BIGINT AS n_events,\n",
      "         COUNT(DISTINCT patient_id)::BIGINT AS n_patients\n",
      "  FROM ", events_identifier, "\n",
      "  WHERE analysis_year = ", config$analysis_year, "\n",
      "    AND utilization_category = 'Emergency Department'\n",
      "  GROUP BY ed_visit_type_flag, ed_service_subcategory_flag,\n",
      "           ed_revenue_code_flag, ed_classification_pattern\n",
      ")\n",
      "SELECT flags.ed_visit_type_flag, flags.ed_service_subcategory_flag,\n",
      "       flags.ed_revenue_code_flag,\n",
      "       COALESCE(observed.ed_classification_pattern, 'none') AS ed_classification_pattern,\n",
      "       COALESCE(observed.n_events, 0)::BIGINT AS n_events,\n",
      "       COALESCE(observed.n_patients, 0)::BIGINT AS n_patients\n",
      "FROM flag_combinations flags\n",
      "LEFT JOIN observed\n",
      "  ON flags.ed_visit_type_flag = observed.ed_visit_type_flag\n",
      " AND flags.ed_service_subcategory_flag = observed.ed_service_subcategory_flag\n",
      " AND flags.ed_revenue_code_flag = observed.ed_revenue_code_flag\n",
      "ORDER BY flags.ed_visit_type_flag, flags.ed_service_subcategory_flag,\n",
      "         flags.ed_revenue_code_flag"
    )
  ),
  skilled_nursing_validation = run_qa(
    "skilled_nursing_validation",
    paste0(
      "SELECT COALESCE(bill_type_code, 'MISSING') AS bill_type_code,\n",
      "       COALESCE(facility_type_class_code_desc, 'MISSING') AS facility_type_class_code_desc,\n",
      "       COALESCE(facility_npi_classification, 'MISSING') AS facility_npi_classification,\n",
      "       COUNT(*)::BIGINT AS n_events\n",
      "FROM ", events_identifier, "\n",
      "WHERE analysis_year = ", config$analysis_year, "\n",
      "  AND utilization_category = 'Skilled Nursing'\n",
      "GROUP BY 1, 2, 3\n",
      "ORDER BY n_events DESC"
    )
  ),
  long_term_care_validation = run_qa(
    "long_term_care_validation",
    paste0(
      "WITH cohort AS (\n", cohort_sql, "\n), qualifying_visits AS (\n",
      "  SELECT DISTINCT nie.patient_id, nie.utilization_id\n",
      "  FROM ", non_inpatient_identifier, " nie\n",
      "  INNER JOIN cohort ON nie.patient_id = cohort.patient_id\n",
      "  WHERE nie.utilization_id IS NOT NULL\n",
      "    AND nie.service_date >= ", year_start, "::DATE\n",
      "    AND nie.service_date < ", year_end, "::DATE\n",
      "    AND TRIM(nie.procedure_code) IN ('99304', '99305', '99306', '99307', '99308', '99309', '99310')\n",
      ")\n",
      "SELECT (SELECT COUNT(*)::BIGINT FROM qualifying_visits) AS n_qualifying_visits,\n",
      "       (SELECT COUNT(*)::BIGINT FROM ", events_identifier, "\n",
      "         WHERE analysis_year = ", config$analysis_year, "\n",
      "           AND utilization_category = 'Long-term care') AS n_prepared_events"
    )
  ),
  duration_quality = run_qa(
    "duration_quality",
    paste0(
      "SELECT utilization_category,\n",
      "       COUNT(*)::BIGINT AS n_events,\n",
      "       SUM(duration_missing_flag)::BIGINT AS n_source_missing_duration,\n",
      "       SUM(duration_invalid_flag)::BIGINT AS n_invalid_duration,\n",
      "       SUM(CASE WHEN duration_days IS NULL THEN 1 ELSE 0 END)::BIGINT AS n_duration_not_summarized,\n",
      "       SUM(CASE WHEN source_table = 'inpatient_events'\n",
      "                 AND total_los IS NOT NULL AND date_duration_days IS NOT NULL\n",
      "                 AND total_los <> date_duration_days THEN 1 ELSE 0 END)::BIGINT\n",
      "         AS n_inpatient_los_date_disagreements\n",
      "FROM ", events_identifier, "\n",
      "WHERE analysis_year = ", config$analysis_year, "\n",
      "GROUP BY utilization_category\n",
      "ORDER BY utilization_category"
    )
  ),
  ed_line_collapse = run_qa(
    "ed_line_collapse",
    paste0(
      "SELECT n_source_lines, COUNT(*)::BIGINT AS n_ed_events\n",
      "FROM ", events_identifier, "\n",
      "WHERE analysis_year = ", config$analysis_year, "\n",
      "  AND utilization_category = 'Emergency Department'\n",
      "GROUP BY n_source_lines\n",
      "ORDER BY n_source_lines"
    )
  ),
  completeness = run_qa(
    "completeness",
    paste0(
      "WITH cohort AS (\n", cohort_sql, "\n)\n",
      "SELECT\n",
      "  COUNT(*)::BIGINT AS n_cohort_rows,\n",
      "  COUNT(metrics.patient_id)::BIGINT AS n_metric_rows,\n",
      "  COUNT(combined.patient_id)::BIGINT AS n_combined_rows,\n",
      "  SUM(CASE WHEN metrics.patient_id IS NULL THEN 1 ELSE 0 END)::BIGINT AS missing_metric_rows,\n",
      "  SUM(CASE WHEN combined.patient_id IS NULL THEN 1 ELSE 0 END)::BIGINT AS missing_combined_rows\n",
      "FROM cohort\n",
      "LEFT JOIN ", metrics_identifier, " metrics\n",
      "  ON cohort.patient_id = metrics.patient_id\n",
      " AND cohort.analysis_year = metrics.analysis_year\n",
      "LEFT JOIN ", combined_identifier, " combined\n",
      "  ON cohort.patient_id = combined.patient_id\n",
      " AND cohort.analysis_year = combined.analysis_year"
    )
  ),
  duplicates = run_qa(
    "duplicates",
    paste0(
      "SELECT 'events' AS table_name,\n",
      "  COUNT(*) - COUNT(DISTINCT patient_id || '|' || utilization_id || '|' || utilization_category)\n",
      "    AS duplicate_rows\n",
      "FROM ", events_identifier, " WHERE analysis_year = ", config$analysis_year, "\n",
      "UNION ALL\n",
      "SELECT 'metrics' AS table_name,\n",
      "  COUNT(*) - COUNT(DISTINCT patient_id || '|' || analysis_year::VARCHAR) AS duplicate_rows\n",
      "FROM ", metrics_identifier, " WHERE analysis_year = ", config$analysis_year, "\n",
      "UNION ALL\n",
      "SELECT 'combined' AS table_name,\n",
      "  COUNT(*) - COUNT(DISTINCT patient_id || '|' || analysis_year::VARCHAR) AS duplicate_rows\n",
      "FROM ", combined_identifier, " WHERE analysis_year = ", config$analysis_year
    )
  )
)

qa_frames <- lapply(qa_results, function(data) {
  data[] <- lapply(data, as.character)
  data
})
all_columns <- unique(unlist(lapply(qa_frames, names)))
qa_frames <- lapply(qa_frames, function(data) {
  missing_columns <- setdiff(all_columns, names(data))
  for (column in missing_columns) {
    data[[column]] <- NA_character_
  }
  data[, all_columns, drop = FALSE]
})
qa_output <- do.call(rbind, qa_frames)
qa_count_columns <- grep(
  "(^n_|_rows$|duplicate_rows$)",
  names(qa_output),
  value = TRUE
)
qa_small_cell <- rep(FALSE, nrow(qa_output))
for (column in qa_count_columns) {
  values <- suppressWarnings(as.numeric(qa_output[[column]]))
  qa_small_cell <- qa_small_cell | (values >= 1 & values < config$min_count)
}
qa_output$suppression_applied <- ifelse(qa_small_cell, "yes", "no")
for (column in qa_count_columns) {
  qa_output[[column]][qa_small_cell] <- NA_character_
}
utilization_write_csv(
  qa_output,
  file.path(config$output_dir, paste0("6.5_healthcare_utilization_qa_", config$analysis_year, ".csv"))
)

duplicate_rows <- as.numeric(qa_results$duplicates$duplicate_rows)
completeness <- qa_results$completeness
if (any(duplicate_rows != 0)) {
  stop("Healthcare utilization QA failed: duplicate output keys were found.")
}
if (
  completeness$missing_metric_rows[[1]] != 0 ||
    completeness$missing_combined_rows[[1]] != 0
) {
  stop("Healthcare utilization QA failed: the final tables are incomplete for the cohort.")
}
if (qa_results$event_cohort_membership$n_events_outside_cohort[[1]] != 0) {
  stop("Healthcare utilization QA failed: prepared events were found outside the cohort.")
}
if (
  qa_results$long_term_care_validation$n_qualifying_visits[[1]] !=
    qa_results$long_term_care_validation$n_prepared_events[[1]]
) {
  stop("Healthcare utilization QA failed: long-term-care visits did not reconcile.")
}

message("Healthcare utilization QA complete.")
disconnect_komodo(con)
