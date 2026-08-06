source("Code/2_variable construction/6.0_annual_healthcare_utilization_helpers.R")

# Project: Frailty_Komoto healthcare utilization
# Author: Nemo Zhou
# Date started: 2026-07-17
# Date last updated: 2026-08-04
#
# ---- Purpose ----
# Build a reusable, 2022 event-level utilization table for every eligible
# patient-year. Inpatient events use CLAIM_FROM_DATE for
# annual assignment and TOTAL_LOS for duration. Emergency-department source
# lines are collapsed to one PATIENT_ID + UTILIZATION_ID event. ED events use
# separate visit-type, service-subcategory, and reviewed-revenue-code flags;
# CPT and HCPCS codes are not used for ED. Long-term-care events use only CPT
# 99304-99310 in PROCEDURE_CODE. ED intervals overlapping 2022 are clipped to
# the calendar-year boundaries before deriving inclusive calendar-day duration,
# so a same-day encounter is one day.

config <- get_annual_healthcare_utilization_config()
con <- connect_komodo()

if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

for (item in list(
  list(schema = write_schema, table = config$cohort_table, columns = c(
    "patient_id", "analysis_year", "mx_insurance_group", "mx_insurance_segment"
  )),
  list(schema = komodo_schema, table = config$inpatient_table, columns = c(
    "patient_id", "utilization_id", "visit_type", "claim_from_date",
    "claim_through_date", "admit_date", "total_los", "bill_type_code",
    "facility_type_class_code_desc", "facility_npi_classification"
  )),
  list(schema = komodo_schema, table = config$non_inpatient_table, columns = c(
    "patient_id", "utilization_id", "line_number", "visit_type",
    "service_subcategory", "revenue_code", "procedure_code", "service_date",
    "service_to_date", "place_of_service"
  ))
)) {
  if (!table_exists(con, item$schema, item$table)) {
    stop("Required table was not found: ", item$schema, ".", item$table)
  }
  table_has_columns(con, item$schema, item$table, item$columns)
}

cohort_sql <- utilization_cohort_sql(config)
inpatient_identifier <- qualified_identifier(komodo_schema, config$inpatient_table)
non_inpatient_identifier <- qualified_identifier(komodo_schema, config$non_inpatient_table)
events_identifier <- qualified_identifier(write_schema, config$events_table)
year_start <- sql_string(utilization_year_start(config))
year_end <- sql_string(utilization_year_end(config))

preflight <- DBI::dbGetQuery(
  con,
  paste0(
    "WITH sampled_inpatient AS (\n",
    "  SELECT visit_type\n",
    "  FROM ", inpatient_identifier, "\n",
    "  WHERE claim_from_date >= ", year_start, "::DATE\n",
    "    AND claim_from_date < ", year_end, "::DATE\n",
    "  LIMIT ", config$preflight_n, "\n",
    "), sampled_non_inpatient AS (\n",
    "  SELECT visit_type, service_subcategory, revenue_code\n",
    "  FROM ", non_inpatient_identifier, "\n",
    "  WHERE service_date < ", year_end, "::DATE\n",
    "    AND (\n",
    "      service_to_date >= ", year_start, "::DATE\n",
    "      OR (service_to_date IS NULL AND service_date >= ", year_start, "::DATE)\n",
    "    )\n",
    "  LIMIT ", config$preflight_n, "\n",
    ")\n",
    "SELECT 'inpatient_sample' AS source, COUNT(*)::BIGINT AS n_rows\n",
    "FROM sampled_inpatient\n",
    "UNION ALL\n",
    "SELECT 'non_inpatient_sample' AS source, COUNT(*)::BIGINT AS n_rows\n",
    "FROM sampled_non_inpatient"
  )
)
message("Aggregate source preflight:")
print(preflight)

if (table_exists(con, write_schema, config$events_table)) {
  existing_event_columns <- tolower(names(DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", events_identifier, " LIMIT 0")
  )))
  required_event_columns <- c(
    "patient_id", "utilization_id", "analysis_year", "utilization_category",
    "ed_visit_type_flag", "ed_service_subcategory_flag", "ed_revenue_code_flag",
    "ed_classification_pattern", "duration_days", "duration_missing_flag",
    "duration_invalid_flag"
  )
  legacy_event_columns <- intersect(
    c("ed_any_visit", "ed_n_visits", "ed_total_duration_days"),
    existing_event_columns
  )
  missing_event_columns <- setdiff(required_event_columns, existing_event_columns)
  if (length(legacy_event_columns) > 0L || length(missing_event_columns) > 0L) {
    message(
      "Recreating active utilization events table for the three-definition ED schema."
    )
    DatabaseConnector::executeSql(
      con,
      paste0("DROP TABLE IF EXISTS ", events_identifier, ";")
    )
  }
}

if (!table_exists(con, write_schema, config$events_table)) {
  utilization_run_sql_stage(
    con,
    "create healthcare utilization events table",
    paste0(
      "CREATE TABLE ", events_identifier, " (\n",
      "  patient_id VARCHAR(256) NOT NULL,\n",
      "  utilization_id VARCHAR(256) NOT NULL,\n",
      "  analysis_year INTEGER NOT NULL,\n",
      "  utilization_category VARCHAR(80) NOT NULL,\n",
      "  source_table VARCHAR(64) NOT NULL,\n",
      "  visit_type VARCHAR(256),\n",
      "  ed_visit_type_flag INTEGER,\n",
      "  ed_service_subcategory_flag INTEGER,\n",
      "  ed_revenue_code_flag INTEGER,\n",
      "  ed_classification_pattern VARCHAR(64),\n",
      "  event_start_date DATE,\n",
      "  event_end_date DATE,\n",
      "  claim_from_date DATE,\n",
      "  claim_through_date DATE,\n",
      "  admit_date DATE,\n",
      "  total_los INTEGER,\n",
      "  duration_days INTEGER,\n",
      "  duration_missing_flag INTEGER NOT NULL,\n",
      "  duration_invalid_flag INTEGER NOT NULL,\n",
      "  date_duration_days INTEGER,\n",
      "  n_source_lines INTEGER NOT NULL,\n",
      "  n_missing_service_to_date_lines INTEGER,\n",
      "  bill_type_code VARCHAR(64),\n",
      "  facility_type_class_code_desc VARCHAR(1024),\n",
      "  facility_npi_classification VARCHAR(256),\n",
      "  place_of_service VARCHAR(64)\n",
      ") DISTKEY(patient_id) SORTKEY(analysis_year, patient_id, utilization_category);"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$events_table,
  c(
    "patient_id", "utilization_id", "analysis_year", "utilization_category",
    "ed_visit_type_flag", "ed_service_subcategory_flag", "ed_revenue_code_flag",
    "ed_classification_pattern", "duration_days", "duration_missing_flag",
    "duration_invalid_flag"
  )
)

inpatient_sql <- paste0(
  "WITH cohort AS (\n", cohort_sql, "\n),\n",
  "inpatient_events AS (\n",
  "  SELECT\n",
  "    inp.patient_id,\n",
  "    inp.utilization_id,\n",
  "    CASE UPPER(TRIM(inp.visit_type))\n",
  "      WHEN 'ACUTE INPATIENT' THEN 'Acute Inpatient'\n",
  "      WHEN 'HOSPICE (INPATIENT)' THEN 'Hospice (Inpatient)'\n",
  "      WHEN 'SKILLED NURSING' THEN 'Skilled Nursing'\n",
  "      WHEN 'IPF' THEN 'IPF'\n",
  "    END AS utilization_category,\n",
  "    inp.visit_type, inp.claim_from_date, inp.claim_through_date, inp.admit_date,\n",
  "    inp.total_los, inp.bill_type_code, inp.facility_type_class_code_desc,\n",
  "    inp.facility_npi_classification\n",
  "  FROM ", inpatient_identifier, " inp\n",
  "  INNER JOIN cohort\n",
  "    ON inp.patient_id = cohort.patient_id\n",
  "  WHERE inp.utilization_id IS NOT NULL\n",
  "    AND inp.claim_from_date >= ", year_start, "::DATE\n",
  "    AND inp.claim_from_date < ", year_end, "::DATE\n",
  "    AND UPPER(TRIM(inp.visit_type)) IN (\n",
  "      'ACUTE INPATIENT', 'HOSPICE (INPATIENT)', 'SKILLED NURSING',\n",
  "      'IPF'\n",
  "    )\n",
  ")\n",
  "SELECT\n",
  "  patient_id, utilization_id, ", config$analysis_year, " AS analysis_year,\n",
  "  utilization_category, 'inpatient_events' AS source_table, visit_type,\n",
  "  0::INTEGER AS ed_visit_type_flag,\n",
  "  0::INTEGER AS ed_service_subcategory_flag,\n",
  "  0::INTEGER AS ed_revenue_code_flag,\n",
  "  NULL::VARCHAR(64) AS ed_classification_pattern,\n",
  "  claim_from_date AS event_start_date, claim_through_date AS event_end_date,\n",
  "  claim_from_date, claim_through_date, admit_date, total_los,\n",
  "  CASE WHEN total_los IS NOT NULL AND total_los >= 0 THEN total_los ELSE NULL END AS duration_days,\n",
  "  CASE WHEN total_los IS NULL THEN 1 ELSE 0 END AS duration_missing_flag,\n",
  "  CASE WHEN total_los < 0 THEN 1 ELSE 0 END AS duration_invalid_flag,\n",
  "  CASE\n",
  "    WHEN claim_from_date IS NOT NULL AND claim_through_date IS NOT NULL\n",
  "      THEN DATEDIFF(day, claim_from_date, claim_through_date)::INTEGER\n",
  "    ELSE NULL\n",
  "  END AS date_duration_days,\n",
  "  1 AS n_source_lines, NULL::INTEGER AS n_missing_service_to_date_lines,\n",
  "  bill_type_code, facility_type_class_code_desc, facility_npi_classification,\n",
  "  NULL::VARCHAR(64) AS place_of_service\n",
  "FROM inpatient_events"
)

ed_sql <- paste0(
  "WITH cohort AS (\n", cohort_sql, "\n),\n",
  "ed_source_lines AS (\n",
  "  SELECT\n",
  "    nie.patient_id, nie.utilization_id, nie.visit_type, nie.service_subcategory,\n",
  "    nie.revenue_code,\n",
  "    nie.service_date, nie.service_to_date, nie.place_of_service,\n",
  "    CASE WHEN UPPER(REPLACE(TRIM(nie.visit_type), ' ', '_')) = 'OUTPATIENT_ED'\n",
  "      THEN 1 ELSE 0 END AS ed_visit_type_flag,\n",
  "    CASE WHEN UPPER(TRIM(nie.service_subcategory)) = 'EMERGENCY DEPT ENCOUNTER'\n",
  "      THEN 1 ELSE 0 END AS ed_service_subcategory_flag,\n",
  "    CASE WHEN LPAD(TRIM(nie.revenue_code), 4, '0') IN\n",
  "      ('0450', '0451', '0452', '0456', '0459', '0981')\n",
  "      THEN 1 ELSE 0 END AS ed_revenue_code_flag\n",
  "  FROM ", non_inpatient_identifier, " nie\n",
  "  INNER JOIN cohort\n",
  "    ON nie.patient_id = cohort.patient_id\n",
  "  WHERE nie.utilization_id IS NOT NULL\n",
  "    AND nie.service_date < ", year_end, "::DATE\n",
  "    AND (\n",
  "      nie.service_to_date >= ", year_start, "::DATE\n",
  "      OR (nie.service_to_date IS NULL AND nie.service_date >= ", year_start, "::DATE)\n",
  "    )\n",
  "),\n",
  "collapsed_ed_events AS (\n",
  "  SELECT\n",
  "    patient_id, utilization_id,\n",
  "    MAX(ed_visit_type_flag)::INTEGER AS ed_visit_type_flag,\n",
  "    MAX(ed_service_subcategory_flag)::INTEGER AS ed_service_subcategory_flag,\n",
  "    MAX(ed_revenue_code_flag)::INTEGER AS ed_revenue_code_flag,\n",
  "    MIN(service_date) AS raw_event_start_date,\n",
  "    MAX(service_to_date) AS raw_event_end_date,\n",
  "    COUNT(*)::INTEGER AS n_source_lines,\n",
  "    SUM(CASE WHEN service_to_date IS NULL THEN 1 ELSE 0 END)::INTEGER\n",
  "      AS n_missing_service_to_date_lines,\n",
  "    CASE WHEN COUNT(DISTINCT NULLIF(TRIM(place_of_service), '')) = 1\n",
  "      THEN MIN(NULLIF(TRIM(place_of_service), ''))\n",
  "      WHEN COUNT(DISTINCT NULLIF(TRIM(place_of_service), '')) > 1 THEN 'MULTIPLE'\n",
  "      ELSE NULL END AS place_of_service\n",
  "  FROM ed_source_lines\n",
  "  GROUP BY patient_id, utilization_id\n",
  "  HAVING MAX(ed_visit_type_flag) = 1\n",
  "      OR MAX(ed_service_subcategory_flag) = 1\n",
  "      OR MAX(ed_revenue_code_flag) = 1\n",
  "),\n",
  "clipped_ed_events AS (\n",
  "  SELECT\n",
  "    patient_id, utilization_id, ed_visit_type_flag,\n",
  "    ed_service_subcategory_flag, ed_revenue_code_flag,\n",
  "    CASE\n",
  "      WHEN raw_event_start_date < ", year_start, "::DATE THEN ", year_start, "::DATE\n",
  "      ELSE raw_event_start_date\n",
  "    END AS event_start_date,\n",
  "    CASE\n",
  "      WHEN raw_event_end_date >= ", year_end, "::DATE\n",
  "        THEN DATEADD(day, -1, ", year_end, "::DATE)\n",
  "      ELSE raw_event_end_date\n",
  "    END AS event_end_date,\n",
  "    n_source_lines, n_missing_service_to_date_lines, place_of_service\n",
  "  FROM collapsed_ed_events\n",
  ")\n",
  "SELECT\n",
  "  patient_id, utilization_id, ", config$analysis_year, " AS analysis_year,\n",
  "  'Emergency Department' AS utilization_category,\n",
  "  'non_inpatient_events' AS source_table,\n",
  "  NULL::VARCHAR(256) AS visit_type,\n",
  "  ed_visit_type_flag, ed_service_subcategory_flag, ed_revenue_code_flag,\n",
  "  CASE\n",
  "    WHEN ed_visit_type_flag = 1 AND ed_service_subcategory_flag = 1\n",
  "     AND ed_revenue_code_flag = 1 THEN 'all_three'\n",
  "    WHEN ed_visit_type_flag = 1 AND ed_service_subcategory_flag = 1\n",
  "     THEN 'visit_type_service_subcategory'\n",
  "    WHEN ed_visit_type_flag = 1 AND ed_revenue_code_flag = 1\n",
  "     THEN 'visit_type_revenue_code'\n",
  "    WHEN ed_service_subcategory_flag = 1 AND ed_revenue_code_flag = 1\n",
  "     THEN 'service_subcategory_revenue_code'\n",
  "    WHEN ed_visit_type_flag = 1 THEN 'visit_type_only'\n",
  "    WHEN ed_service_subcategory_flag = 1 THEN 'service_subcategory_only'\n",
  "    ELSE 'revenue_code_only'\n",
  "  END AS ed_classification_pattern,\n",
  "  event_start_date, event_end_date,\n",
  "  NULL::DATE AS claim_from_date, NULL::DATE AS claim_through_date,\n",
  "  NULL::DATE AS admit_date, NULL::INTEGER AS total_los,\n",
  "  CASE\n",
  "    WHEN event_start_date IS NOT NULL AND event_end_date IS NOT NULL\n",
  "     AND event_end_date >= event_start_date\n",
  "      THEN (DATEDIFF(day, event_start_date, event_end_date) + 1)::INTEGER\n",
  "    ELSE NULL\n",
  "  END AS duration_days,\n",
  "  CASE WHEN event_start_date IS NULL OR event_end_date IS NULL THEN 1 ELSE 0 END\n",
  "    AS duration_missing_flag,\n",
  "  CASE WHEN event_start_date IS NOT NULL AND event_end_date IS NOT NULL\n",
  "     AND event_end_date < event_start_date THEN 1 ELSE 0 END AS duration_invalid_flag,\n",
  "  CASE WHEN event_start_date IS NOT NULL AND event_end_date IS NOT NULL\n",
  "    THEN DATEDIFF(day, event_start_date, event_end_date)::INTEGER ELSE NULL END\n",
  "    AS date_duration_days,\n",
  "  n_source_lines, n_missing_service_to_date_lines,\n",
  "  NULL::VARCHAR(64) AS bill_type_code,\n",
  "  NULL::VARCHAR(1024) AS facility_type_class_code_desc,\n",
  "  NULL::VARCHAR(256) AS facility_npi_classification,\n",
  "  place_of_service\n",
  "FROM clipped_ed_events"
)

long_term_care_sql <- paste0(
  "WITH cohort AS (\n", cohort_sql, "\n),\n",
  "long_term_care_source_lines AS (\n",
  "  SELECT\n",
  "    nie.patient_id, nie.utilization_id, nie.service_date, nie.service_to_date,\n",
  "    nie.place_of_service\n",
  "  FROM ", non_inpatient_identifier, " nie\n",
  "  INNER JOIN cohort\n",
  "    ON nie.patient_id = cohort.patient_id\n",
  "  WHERE nie.utilization_id IS NOT NULL\n",
  "    AND nie.service_date >= ", year_start, "::DATE\n",
  "    AND nie.service_date < ", year_end, "::DATE\n",
  "    AND TRIM(nie.procedure_code) IN ('99304', '99305', '99306', '99307', '99308', '99309', '99310')\n",
  "),\n",
  "collapsed_long_term_care_events AS (\n",
  "  SELECT\n",
  "    patient_id, utilization_id, MIN(service_date) AS event_start_date,\n",
  "    MAX(service_to_date) AS event_end_date, COUNT(*)::INTEGER AS n_source_lines,\n",
  "    SUM(CASE WHEN service_to_date IS NULL THEN 1 ELSE 0 END)::INTEGER\n",
  "      AS n_missing_service_to_date_lines,\n",
  "    CASE WHEN COUNT(DISTINCT NULLIF(TRIM(place_of_service), '')) = 1\n",
  "      THEN MIN(NULLIF(TRIM(place_of_service), ''))\n",
  "      WHEN COUNT(DISTINCT NULLIF(TRIM(place_of_service), '')) > 1 THEN 'MULTIPLE'\n",
  "      ELSE NULL END AS place_of_service\n",
  "  FROM long_term_care_source_lines\n",
  "  GROUP BY patient_id, utilization_id\n",
  ")\n",
  "SELECT\n",
  "  patient_id, utilization_id, ", config$analysis_year, " AS analysis_year,\n",
  "  'Long-term care' AS utilization_category,\n",
  "  'non_inpatient_events' AS source_table, NULL::VARCHAR(256) AS visit_type,\n",
  "  0::INTEGER AS ed_visit_type_flag, 0::INTEGER AS ed_service_subcategory_flag,\n",
  "  0::INTEGER AS ed_revenue_code_flag, NULL::VARCHAR(64) AS ed_classification_pattern,\n",
  "  event_start_date, event_end_date, NULL::DATE AS claim_from_date,\n",
  "  NULL::DATE AS claim_through_date, NULL::DATE AS admit_date, NULL::INTEGER AS total_los,\n",
  "  CASE WHEN event_start_date IS NOT NULL AND event_end_date IS NOT NULL\n",
  "       AND event_end_date >= event_start_date\n",
  "    THEN (DATEDIFF(day, event_start_date, event_end_date) + 1)::INTEGER ELSE NULL END\n",
  "    AS duration_days,\n",
  "  CASE WHEN event_start_date IS NULL OR event_end_date IS NULL THEN 1 ELSE 0 END\n",
  "    AS duration_missing_flag,\n",
  "  CASE WHEN event_start_date IS NOT NULL AND event_end_date IS NOT NULL\n",
  "       AND event_end_date < event_start_date THEN 1 ELSE 0 END AS duration_invalid_flag,\n",
  "  CASE WHEN event_start_date IS NOT NULL AND event_end_date IS NOT NULL\n",
  "    THEN DATEDIFF(day, event_start_date, event_end_date)::INTEGER ELSE NULL END\n",
  "    AS date_duration_days,\n",
  "  n_source_lines, n_missing_service_to_date_lines, NULL::VARCHAR(64) AS bill_type_code,\n",
  "  NULL::VARCHAR(1024) AS facility_type_class_code_desc,\n",
  "  NULL::VARCHAR(256) AS facility_npi_classification, place_of_service\n",
  "FROM collapsed_long_term_care_events"
)

inpatient_temp_identifier <- quote_identifier("tmp_6_1_inpatient_events")
ed_temp_identifier <- quote_identifier("tmp_6_1_ed_events")
long_term_care_temp_identifier <- quote_identifier("tmp_6_1_long_term_care_events")

utilization_run_sql_stage(
  con,
  "refresh 2022 healthcare utilization events",
  paste0(
    "DROP TABLE IF EXISTS ", inpatient_temp_identifier, ";\n",
    "CREATE TEMP TABLE ", inpatient_temp_identifier, " AS\n",
    inpatient_sql, ";\n\n",
    "DROP TABLE IF EXISTS ", ed_temp_identifier, ";\n",
    "CREATE TEMP TABLE ", ed_temp_identifier, " AS\n",
    ed_sql, ";\n\n",
    "DROP TABLE IF EXISTS ", long_term_care_temp_identifier, ";\n",
    "CREATE TEMP TABLE ", long_term_care_temp_identifier, " AS\n",
    long_term_care_sql, ";\n\n",
    "DELETE FROM ", events_identifier, "\n",
    "WHERE analysis_year = ", config$analysis_year, ";\n\n",
    "INSERT INTO ", events_identifier, " (\n",
    "  patient_id, utilization_id, analysis_year, utilization_category, source_table,\n",
    "  visit_type, ed_visit_type_flag, ed_service_subcategory_flag, ed_revenue_code_flag,\n",
    "  ed_classification_pattern,\n",
    "  event_start_date, event_end_date, claim_from_date, claim_through_date, admit_date,\n",
    "  total_los, duration_days, duration_missing_flag, duration_invalid_flag,\n",
    "  date_duration_days, n_source_lines, n_missing_service_to_date_lines,\n",
    "  bill_type_code, facility_type_class_code_desc, facility_npi_classification, place_of_service\n",
    ")\n",
    "SELECT * FROM ", inpatient_temp_identifier, "\n",
    "UNION ALL\n",
    "SELECT * FROM ", ed_temp_identifier, "\n",
    "UNION ALL\n",
    "SELECT * FROM ", long_term_care_temp_identifier, ";"
  )
)

event_key_check <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) - COUNT(DISTINCT patient_id || '|' || utilization_id || '|' || utilization_category)\n",
    "  AS duplicate_event_rows\n",
    "FROM ", events_identifier, "\n",
    "WHERE analysis_year = ", config$analysis_year
  )
)
if (event_key_check$duplicate_event_rows[[1]] != 0) {
  stop("Prepared healthcare utilization events contain duplicate event keys.")
}

message("Healthcare utilization event preparation complete.")
disconnect_komodo(con)
