library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto
# Author: Nemo Zhou
# Date started: 2026-07-31
# Date last updated: 2026-08-04
#
# ---- Purpose ----
# Build a fixed 2022 Komodo Medicare-comparison cohort directly from KRD source
# tables. The cohort output, temporary flag table, output directory, and
# same-year non-inpatient requirement can be overridden through
# frailty.2022_komodo_medicare_comparison_cohort.config. The direct default
# builds the broader comparison denominator; the restricted CFI-source table
# requires an explicit configuration with the event requirement set to TRUE.
# For an explicit restricted CFI-source build, set cohort_table to
# "1_2022_komodo_medicare_comparison_cohort", use a separate temporary flags
# table and output directory, and set require_same_year_non_inpatient = TRUE.
#
# Common eligibility is applied in this order:
#   1. known age >=65 years on 2022-01-01;
#   2. full-year, gap-free medical insurance coverage;
#   3. alive on 2022-12-31; and
#   4. optionally, at least one same-year NON_INPATIENT_EVENTS record for the
#      restricted CFI-source output only.
#
# Overall eligibility does not require stable primary or secondary fields, so
# valid insurance group/segment switchers remain in the broader denominator.
# Plan-specific MA/FFS eligibility is derived by pooling valid Medicare segment
# evidence from both primary and secondary fields, retaining exactly one
# distinct segment, and requiring that segment to cover the full year without
# a gap.
#
# The script uses a session-scoped temporary flags table to calculate the
# aggregate overall and Medicare plan-comparison flows. It permanently writes
# only the configured final cohort table; flow and QA outputs are aggregate CSVs.
#
# Race/ethnicity and annual patient residence geography are joined without
# excluding missing values. The output also carries a 2022 dual-coverage flag
# for a Medicare-and-Medicaid primary/secondary pair on any overlapping span;
# this descriptive variable is used only in the plan-specific Table 2.
# Prescription-insurance fields are retained as NULL compatibility columns
# because 3.1_prepare_annual_metric_ids.R requires their names, but they play
# no role in eligibility.

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

redshift_server <- paste0(
  "ohdsi-lab-redshift-cluster-prod.clsyktjhufn7.us-east-1.redshift.amazonaws.com",
  "/ohdsi_lab"
)
redshift_port <- 5439L
analysis_year <- 2022L
minimum_age <- 65L
# The current project source schema is komodo_202606. The option/environment
# override is useful when the connected Redshift account exposes the same KRD
# tables under a different schema. The preflight probes the configured source
# table directly because Redshift external/Spectrum schemas may not appear in
# information_schema metadata views.
configured_komodo_schema <- getOption(
  "komodo.schema",
  Sys.getenv("KOMODO_SCHEMA", unset = "komodo_202606")
)

cohort_build_config <- utils::modifyList(
  list(
    cohort_table = "1_2022_komodo_medicare_comparison_cohort_all_eligible",
    temp_flags_table = "tmp_2022_komodo_medicare_comparison_all_eligible_flags",
    output_dir = file.path(getwd(), "Outputs", "1_eligibility_2022_all_eligible"),
    require_same_year_non_inpatient = FALSE
  ),
  getOption("frailty.2022_komodo_medicare_comparison_cohort.config", list())
)
cohort_table <- as.character(cohort_build_config$cohort_table)
temp_flags_table <- as.character(cohort_build_config$temp_flags_table)
output_dir <- as.character(cohort_build_config$output_dir)
require_same_year_non_inpatient <- isTRUE(
  cohort_build_config$require_same_year_non_inpatient
)
retained_stage_order <- if (require_same_year_non_inpatient) 5L else 4L
flow_output_path <- file.path(
  output_dir,
  "1.1_2022_komodo_medicare_cohort_flow.csv"
)
plan_flow_output_path <- file.path(
  output_dir,
  "1.1_2022_komodo_medicare_plan_flow.csv"
)
insurance_output_path <- file.path(
  output_dir,
  "1.1_2022_komodo_medicare_final_insurance_counts.csv"
)
qa_output_path <- file.path(
  output_dir,
  "1.1_2022_komodo_medicare_cohort_qa.csv"
)

quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

qualified_identifier <- function(schema, table) {
  paste(quote_identifier(schema), quote_identifier(table), sep = ".")
}

disconnect_2022_cohort_connection <- function(con) {
  if (is.null(con)) {
    return(invisible(NULL))
  }

  # This entrypoint is intentionally standalone. It may be sourced before the
  # 3.x helper file, where disconnect_komodo() is defined.
  if (exists("disconnect_komodo", mode = "function", inherits = TRUE)) {
    try(disconnect_komodo(con), silent = TRUE)
  } else if (requireNamespace("DatabaseConnector", quietly = TRUE)) {
    try(DatabaseConnector::disconnect(con), silent = TRUE)
  }
  invisible(NULL)
}

connect_2022_cohort_database <- function() {
  # Use the same explicit Redshift target as Code/0_test/0.1_connect to
  # Komodo.R. This avoids relying on ohdsilab_connect() defaults, which may
  # authenticate successfully against a connection that cannot see the current
  # KRD source schema.
  DatabaseConnector::connect(
    dbms = "redshift",
    server = redshift_server,
    port = redshift_port,
    user = keyring::key_get("db_username"),
    password = keyring::key_get("db_password")
  )
}

resolve_komodo_source_schema <- function(con, configured_schema, source_tables) {
  configured_schema <- trimws(configured_schema)
  if (!nzchar(configured_schema)) {
    stop(
      "The configured Komodo source schema is empty. Set options(komodo.schema = ",
      "'your_schema') or Sys.setenv(KOMODO_SCHEMA = 'your_schema')."
    )
  }

  direct_probe <- tryCatch(
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT * FROM ",
        qualified_identifier(configured_schema, source_tables[[1]]),
        " LIMIT 0"
      )
    ),
    error = function(e) NULL
  )
  if (!is.null(direct_probe)) {
    return(configured_schema)
  }

  source_table_sql <- paste(
    "'", tolower(source_tables), "'", sep = "", collapse = ", "
  )
  candidate_schemas <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT table_schema\n",
      "FROM information_schema.tables\n",
      "WHERE LOWER(table_name) IN (", source_table_sql, ")\n",
      "GROUP BY table_schema\n",
      "HAVING COUNT(DISTINCT LOWER(table_name)) = ", length(source_tables), "\n",
      "ORDER BY table_schema"
    )
  )$table_schema

  if (length(candidate_schemas) == 1L) {
    message(
      "Configured source schema '", configured_schema,
      "' is not visible. Using uniquely discovered KRD schema '",
      candidate_schemas[[1]], "'."
    )
    return(candidate_schemas[[1]])
  }

  visible_table_schemas <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT table_schema, COUNT(DISTINCT LOWER(table_name)) AS n_required_tables\n",
      "FROM information_schema.tables\n",
      "WHERE LOWER(table_name) IN (", source_table_sql, ")\n",
      "GROUP BY table_schema\n",
      "ORDER BY table_schema"
    )
  )
  diagnostic <- if (nrow(visible_table_schemas) == 0L) {
    "No required source tables were visible in information_schema.tables."
  } else {
    paste(
      paste0(
        visible_table_schemas$table_schema, " (",
        visible_table_schemas$n_required_tables, " of ",
        length(source_tables), " required tables)"
      ),
      collapse = "; "
    )
  }
  connection_context <- tryCatch(
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT current_database() AS database_name,\n",
        "       current_user AS user_name,\n",
        "       current_schema() AS current_schema,\n",
        "       current_setting('search_path') AS search_path"
      )
    ),
    error = function(e) NULL
  )
  context_text <- if (is.null(connection_context) || nrow(connection_context) == 0L) {
    "Connection metadata could not be queried."
  } else {
    paste(
      paste0(
        "database=", connection_context$database_name[[1]],
        ", user=", connection_context$user_name[[1]],
        ", current_schema=", connection_context$current_schema[[1]],
        ", search_path=", connection_context$search_path[[1]]
      ),
      collapse = "; "
    )
  }
  stop(
    "Configured Komodo source schema '", configured_schema,
    "' could not be read from the connected account. ",
    diagnostic, " ", context_text,
    " Set options(komodo.schema = 'your_schema') or Sys.setenv(KOMODO_SCHEMA = 'your_schema') ",
    "and rerun the builder."
  )
}

require_table_columns <- function(con, schema, table, required_columns) {
  identifier <- qualified_identifier(schema, table)
  available_columns <- tolower(names(DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", identifier, " LIMIT 0")
  )))
  missing_columns <- setdiff(tolower(required_columns), available_columns)
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns in ", schema, ".", table, ": ",
      paste(missing_columns, collapse = ", "), "."
    )
  }
}

run_sql_stage <- function(con, label, sql) {
  started_at <- Sys.time()
  message(format(started_at, "[%Y-%m-%d %H:%M:%S] "), "START: ", label)
  DatabaseConnector::executeSql(
    con,
    sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  finished_at <- Sys.time()
  message(
    format(finished_at, "[%Y-%m-%d %H:%M:%S] "),
    "DONE: ", label,
    ". Elapsed minutes: ",
    round(as.numeric(difftime(finished_at, started_at, units = "mins")), 2),
    "."
  )
}

  write_aggregate_csv <- function(data, path) {
    output <- data
    output[] <- lapply(output, as.character)
    utils::write.csv(output, path, row.names = FALSE, na = "")
    message("Wrote ", path)
  }

  suppress_aggregate_counts <- function(data, count_columns, min_count = 11L) {
    output <- data
    for (column in intersect(count_columns, names(output))) {
      numeric_values <- suppressWarnings(as.numeric(output[[column]]))
      output[[column]] <- ifelse(
        !is.na(numeric_values) & numeric_values > 0 & numeric_values < min_count,
        "[suppressed]",
        as.character(output[[column]])
      )
    }
    output
  }

build_2022_komodo_medicare_comparison_cohort <- function() {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  write_schema <- paste0("work_", keyring::key_get("db_username"))
  con <- connect_2022_cohort_database()
  on.exit(disconnect_2022_cohort_connection(con), add = TRUE)

  options(con.default.value = con)
  options(write_schema.default.value = write_schema)

  source_requirements <- list(
    patient_demographics = c("patient_id", "patient_dob", "patient_gender"),
    patient_insurance = c(
      "patient_id", "row_valid_start", "row_valid_end",
      "mx_insurance_group", "mx_insurance_segment",
      "mx_secondary_insurance_group", "mx_secondary_insurance_segment"
    ),
    patient_mortality = c("patient_id", "patient_death_date"),
    non_inpatient_events = c("patient_id", "service_date"),
    patient_race_ethnicity = c("patient_id", "patient_race_ethnicity"),
    patient_geography = c(
      "patient_id", "valid_from_date", "valid_to_date", "patient_state",
      "patient_zip"
    )
  )
  komodo_schema <- resolve_komodo_source_schema(
    con,
    configured_schema = configured_komodo_schema,
    source_tables = names(source_requirements)
  )
  options(schema.default.value = komodo_schema)
  message("Using Komodo source schema: ", komodo_schema)

  for (table in names(source_requirements)) {
    require_table_columns(con, komodo_schema, table, source_requirements[[table]])
  }

  cohort_identifier <- qualified_identifier(write_schema, cohort_table)
  temp_flags_identifier <- quote_identifier(temp_flags_table)

  non_inpatient_stage_sql <- if (require_same_year_non_inpatient) {
    paste0(
      "      WHEN e.has_same_year_non_inpatient_event = 0 THEN 4\n",
      "      ELSE 5\n"
    )
  } else {
    "      ELSE 4\n"
  }
  non_inpatient_label_sql <- if (require_same_year_non_inpatient) {
    paste0(
      "      WHEN e.has_same_year_non_inpatient_event = 0\n",
      "        THEN 'No same-year non-inpatient event'\n"
    )
  } else {
    ""
  }

  flags_sql <- paste0(
    "CREATE TEMP TABLE ", temp_flags_identifier, "\n",
    "DISTKEY(patient_id)\n",
    "SORTKEY(patient_id) AS\n",
    "WITH parameters AS (\n",
    "  SELECT ", analysis_year, "::INTEGER AS analysis_year,\n",
    "         TO_DATE('", analysis_year, "-01-01', 'YYYY-MM-DD') AS year_start,\n",
    "         TO_DATE('", analysis_year, "-12-31', 'YYYY-MM-DD') AS year_end\n",
    "), insurance_overlaps AS (\n",
    "  SELECT\n",
    "    pi.patient_id, p.analysis_year, p.year_start, p.year_end,\n",
    "    GREATEST(CAST(pi.row_valid_start AS DATE), p.year_start) AS span_start,\n",
    "    LEAST(CAST(pi.row_valid_end AS DATE), p.year_end) AS span_end,\n",
    "    NULLIF(TRIM(pi.mx_insurance_group), '') AS mx_insurance_group,\n",
    "    NULLIF(TRIM(pi.mx_insurance_segment), '') AS mx_insurance_segment,\n",
    "    NULLIF(TRIM(pi.mx_secondary_insurance_group), '')\n",
    "      AS mx_secondary_insurance_group,\n",
    "    NULLIF(TRIM(pi.mx_secondary_insurance_segment), '')\n",
    "      AS mx_secondary_insurance_segment\n",
    "  FROM ", qualified_identifier(komodo_schema, "patient_insurance"), " pi\n",
    "  CROSS JOIN parameters p\n",
    "  WHERE pi.patient_id IS NOT NULL\n",
    "    AND pi.row_valid_start IS NOT NULL\n",
    "    AND pi.row_valid_end IS NOT NULL\n",
    "    AND CAST(pi.row_valid_start AS DATE) <= CAST(pi.row_valid_end AS DATE)\n",
    "    AND CAST(pi.row_valid_start AS DATE) <= p.year_end\n",
    "    AND CAST(pi.row_valid_end AS DATE) >= p.year_start\n",
    "), demographics AS (\n",
    "  SELECT\n",
    "    d.patient_id,\n",
    "    MIN(CAST(d.patient_dob AS DATE)) AS patient_dob,\n",
    "    CASE\n",
    "      WHEN COUNT(DISTINCT NULLIF(TRIM(d.patient_gender), '')) = 1\n",
    "        THEN MAX(NULLIF(TRIM(d.patient_gender), ''))\n",
    "      ELSE 'UNKNOWN'\n",
    "    END AS patient_gender\n",
    "  FROM ", qualified_identifier(komodo_schema, "patient_demographics"), " d\n",
    "  WHERE d.patient_id IS NOT NULL\n",
    "  GROUP BY d.patient_id\n",
    "), candidate_ids AS (\n",
    "  SELECT patient_id FROM demographics\n",
    "  UNION\n",
    "  SELECT patient_id FROM insurance_overlaps\n",
    "), demographic_candidates AS (\n",
    "  SELECT\n",
    "    c.patient_id, p.analysis_year, p.year_start, p.year_end,\n",
    "    d.patient_dob, d.patient_gender,\n",
    "    CASE WHEN d.patient_dob IS NULL THEN NULL\n",
    "      ELSE DATEDIFF(year, d.patient_dob, p.year_start)\n",
    "        - CASE WHEN DATEADD(year,\n",
    "            DATEDIFF(year, d.patient_dob, p.year_start),\n",
    "            d.patient_dob) > p.year_start THEN 1 ELSE 0 END\n",
    "    END::INTEGER AS age\n",
    "  FROM candidate_ids c\n",
    "  CROSS JOIN parameters p\n",
    "  LEFT JOIN demographics d ON c.patient_id = d.patient_id\n",
    "), insurance_ordered AS (\n",
    "  SELECT\n",
    "    io.*,\n",
    "    MAX(io.span_end) OVER (\n",
    "      PARTITION BY io.patient_id, io.analysis_year\n",
    "      ORDER BY io.span_start, io.span_end\n",
    "      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING\n",
    "    ) AS previous_max_end\n",
    "  FROM insurance_overlaps io\n",
    "), insurance_summary AS (\n",
    "  SELECT\n",
    "    patient_id, analysis_year, MIN(year_start) AS year_start,\n",
    "    MAX(year_end) AS year_end, MIN(span_start) AS first_span_start,\n",
    "    MAX(span_end) AS last_span_end, COUNT(*)::INTEGER AS n_insurance_rows,\n",
    "    CASE WHEN COUNT(mx_insurance_group) = 0 THEN NULL\n",
    "      WHEN COUNT(DISTINCT mx_insurance_group) = 1 THEN MIN(mx_insurance_group)\n",
    "      ELSE 'MULTIPLE' END AS mx_insurance_group,\n",
    "    CASE WHEN COUNT(mx_insurance_segment) = 0 THEN NULL\n",
    "      WHEN COUNT(DISTINCT mx_insurance_segment) = 1 THEN MIN(mx_insurance_segment)\n",
    "      ELSE 'MULTIPLE' END AS mx_insurance_segment,\n",
    "    CASE WHEN COUNT(mx_secondary_insurance_group) = 0 THEN NULL\n",
    "      WHEN COUNT(DISTINCT mx_secondary_insurance_group) = 1 THEN MIN(mx_secondary_insurance_group)\n",
    "      ELSE 'MULTIPLE' END AS mx_secondary_insurance_group,\n",
    "    CASE WHEN COUNT(mx_secondary_insurance_segment) = 0 THEN NULL\n",
    "      WHEN COUNT(DISTINCT mx_secondary_insurance_segment) = 1 THEN MIN(mx_secondary_insurance_segment)\n",
    "      ELSE 'MULTIPLE' END AS mx_secondary_insurance_segment,\n",
    "    MAX(CASE\n",
    "      WHEN previous_max_end IS NOT NULL\n",
    "       AND span_start > DATEADD(day, 1, previous_max_end) THEN 1\n",
    "      ELSE 0\n",
    "    END)::INTEGER AS has_insurance_gap\n",
    "  FROM insurance_ordered\n",
    "  GROUP BY patient_id, analysis_year\n",
    "), primary_classification_summary AS (\n",
    "  SELECT\n",
    "    patient_id, analysis_year,\n",
    "    COUNT(DISTINCT CASE WHEN UPPER(mx_insurance_group) IN\n",
    "          ('COMMERCIAL', 'MEDICAID', 'MEDICARE')\n",
    "       AND NULLIF(TRIM(mx_insurance_segment), '') IS NOT NULL\n",
    "       AND UPPER(mx_insurance_segment) <> 'UNKNOWN'\n",
    "       AND (UPPER(mx_insurance_group) <> 'MEDICARE'\n",
    "            OR UPPER(mx_insurance_segment) IN ('ADVANTAGE', 'FFS'))\n",
    "      THEN UPPER(mx_insurance_group) END)::INTEGER\n",
    "      AS n_valid_primary_groups,\n",
    "    COUNT(DISTINCT CASE WHEN UPPER(mx_insurance_group) = 'MEDICARE'\n",
    "       AND UPPER(mx_insurance_segment) IN ('ADVANTAGE', 'FFS')\n",
    "      THEN UPPER(mx_insurance_segment) END)::INTEGER\n",
    "      AS n_valid_primary_medicare_segments,\n",
    "    MIN(CASE WHEN UPPER(mx_insurance_group) IN\n",
    "          ('COMMERCIAL', 'MEDICAID', 'MEDICARE')\n",
    "       AND NULLIF(TRIM(mx_insurance_segment), '') IS NOT NULL\n",
    "       AND UPPER(mx_insurance_segment) <> 'UNKNOWN'\n",
    "       AND (UPPER(mx_insurance_group) <> 'MEDICARE'\n",
    "            OR UPPER(mx_insurance_segment) IN ('ADVANTAGE', 'FFS'))\n",
    "      THEN UPPER(mx_insurance_group) END) AS primary_classification_group,\n",
    "    MIN(CASE WHEN UPPER(mx_insurance_group) = 'MEDICARE'\n",
    "       AND UPPER(mx_insurance_segment) IN ('ADVANTAGE', 'FFS')\n",
    "      THEN UPPER(mx_insurance_segment) END)\n",
    "      AS primary_medicare_segment\n",
    "  FROM insurance_overlaps\n",
    "  GROUP BY patient_id, analysis_year\n",
    "), medicare_evidence AS (\n",
    "  SELECT patient_id, analysis_year, span_start, span_end,\n",
    "    UPPER(mx_insurance_segment) AS medicare_segment\n",
    "  FROM insurance_overlaps\n",
    "  WHERE UPPER(mx_insurance_group) = 'MEDICARE'\n",
    "    AND UPPER(mx_insurance_segment) IN ('ADVANTAGE', 'FFS')\n",
    "  UNION\n",
    "  SELECT patient_id, analysis_year, span_start, span_end,\n",
    "    UPPER(mx_secondary_insurance_segment)\n",
    "    AS medicare_segment\n",
    "  FROM insurance_overlaps\n",
    "  WHERE UPPER(mx_secondary_insurance_group) = 'MEDICARE'\n",
    "    AND UPPER(mx_secondary_insurance_segment) IN ('ADVANTAGE', 'FFS')\n",
    "), medicare_evidence_ordered AS (\n",
    "  SELECT\n",
    "    me.*,\n",
    "    MAX(me.span_end) OVER (\n",
    "      PARTITION BY me.patient_id, me.analysis_year\n",
    "      ORDER BY me.span_start, me.span_end, me.medicare_segment\n",
    "      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING\n",
    "    ) AS previous_max_medicare_end\n",
    "  FROM medicare_evidence me\n",
    "), medicare_summary AS (\n",
    "  SELECT patient_id, analysis_year,\n",
    "    COUNT(DISTINCT medicare_segment)::INTEGER AS n_valid_medicare_segments,\n",
    "    MIN(medicare_segment) AS pooled_medicare_segment,\n",
    "    MIN(span_start) AS first_medicare_span_start,\n",
    "    MAX(span_end) AS last_medicare_span_end,\n",
    "    MAX(CASE\n",
    "      WHEN previous_max_medicare_end IS NOT NULL\n",
    "       AND span_start > DATEADD(day, 1, previous_max_medicare_end) THEN 1\n",
    "      ELSE 0\n",
    "    END)::INTEGER AS has_medicare_gap\n",
    "  FROM medicare_evidence_ordered\n",
    "  GROUP BY patient_id, analysis_year\n",
    "), mortality_flags AS (\n",
    "  SELECT mortality.patient_id, 1::INTEGER AS has_death_by_year_end\n",
    "  FROM ", qualified_identifier(komodo_schema, "patient_mortality"), " mortality\n",
    "  CROSS JOIN parameters p\n",
    "  WHERE mortality.patient_id IS NOT NULL\n",
    "    AND mortality.patient_death_date IS NOT NULL\n",
    "    AND CAST(mortality.patient_death_date AS DATE) <= p.year_end\n",
    "  GROUP BY mortality.patient_id\n",
    "), non_inpatient_flags AS (\n",
    "  SELECT nie.patient_id, 1::INTEGER AS has_same_year_non_inpatient_event\n",
    "  FROM ", qualified_identifier(komodo_schema, "non_inpatient_events"), " nie\n",
    "  CROSS JOIN parameters p\n",
    "  WHERE nie.patient_id IS NOT NULL\n",
    "    AND nie.service_date IS NOT NULL\n",
    "    AND CAST(nie.service_date AS DATE) >= p.year_start\n",
    "    AND CAST(nie.service_date AS DATE) < DATEADD(day, 1, p.year_end)\n",
    "  GROUP BY nie.patient_id\n",
    "), dual_coverage_flags AS (\n",
    "  SELECT patient_id, analysis_year,\n",
    "    1::INTEGER AS has_medicare_medicaid_dual_coverage\n",
    "  FROM insurance_overlaps\n",
    "  WHERE (\n",
    "    (UPPER(mx_insurance_group) = 'MEDICARE'\n",
    "     AND UPPER(mx_secondary_insurance_group) = 'MEDICAID')\n",
    "    OR (UPPER(mx_insurance_group) = 'MEDICAID'\n",
    "        AND UPPER(mx_secondary_insurance_group) = 'MEDICARE')\n",
    "  )\n",
    "  GROUP BY patient_id, analysis_year\n",
    "), eligibility_flags AS (\n",
    "  SELECT\n",
    "    d.*,\n",
    "    s.mx_insurance_group, s.mx_insurance_segment,\n",
    "    s.mx_secondary_insurance_group, s.mx_secondary_insurance_segment,\n",
    "    CASE WHEN s.patient_id IS NOT NULL\n",
    "       AND s.first_span_start <= d.year_start\n",
    "       AND s.last_span_end >= d.year_end\n",
    "       AND s.has_insurance_gap = 0 THEN 1 ELSE 0 END\n",
    "      AS has_full_year_medical_coverage,\n",
    "    COALESCE(q.n_valid_primary_groups, 0)\n",
    "      AS n_valid_primary_groups,\n",
    "    COALESCE(q.n_valid_primary_medicare_segments, 0)\n",
    "      AS n_valid_primary_medicare_segments,\n",
    "    q.primary_classification_group,\n",
    "    q.primary_medicare_segment,\n",
    "    COALESCE(m.n_valid_medicare_segments, 0)\n",
    "      AS n_valid_medicare_segments,\n",
    "    m.pooled_medicare_segment,\n",
    "    CASE WHEN m.patient_id IS NOT NULL\n",
    "       AND m.first_medicare_span_start <= d.year_start\n",
    "       AND m.last_medicare_span_end >= d.year_end\n",
    "       AND m.has_medicare_gap = 0 THEN 1 ELSE 0 END\n",
    "      AS has_full_year_medicare_coverage,\n",
    "    CASE WHEN mortality_flags.patient_id IS NULL THEN 1 ELSE 0 END\n",
    "      AS alive_at_year_end,\n",
    "    COALESCE(non_inpatient_flags.has_same_year_non_inpatient_event, 0)\n",
    "      AS has_same_year_non_inpatient_event,\n",
    "    COALESCE(dual_coverage_flags.has_medicare_medicaid_dual_coverage, 0)\n",
    "      AS has_medicare_medicaid_dual_coverage\n",
    "  FROM demographic_candidates d\n",
    "  LEFT JOIN insurance_summary s\n",
    "    ON d.patient_id = s.patient_id\n",
    "   AND d.analysis_year = s.analysis_year\n",
    "  LEFT JOIN primary_classification_summary q\n",
    "    ON d.patient_id = q.patient_id\n",
    "   AND d.analysis_year = q.analysis_year\n",
    "  LEFT JOIN medicare_summary m\n",
    "    ON d.patient_id = m.patient_id\n",
    "   AND d.analysis_year = m.analysis_year\n",
    "  LEFT JOIN mortality_flags\n",
    "    ON d.patient_id = mortality_flags.patient_id\n",
    "  LEFT JOIN non_inpatient_flags\n",
    "    ON d.patient_id = non_inpatient_flags.patient_id\n",
    "  LEFT JOIN dual_coverage_flags\n",
    "    ON d.patient_id = dual_coverage_flags.patient_id\n",
    "   AND d.analysis_year = dual_coverage_flags.analysis_year\n",
    "), classified_flags AS (\n",
    "  SELECT\n",
    "    e.*,\n",
    "    CASE WHEN e.has_full_year_medical_coverage = 1\n",
    "       AND e.alive_at_year_end = 1 THEN 1 ELSE 0 END\n",
    "      AS overall_comparison_eligible,\n",
    "    CASE WHEN e.has_full_year_medical_coverage = 1\n",
    "       AND e.alive_at_year_end = 1\n",
    "       AND e.n_valid_medicare_segments = 1\n",
    "       AND e.has_full_year_medicare_coverage = 1 THEN 1 ELSE 0 END\n",
    "      AS plan_comparison_eligible,\n",
    "    CASE\n",
    "      WHEN e.patient_dob IS NULL OR e.age < ", minimum_age, " THEN 1\n",
    "      WHEN e.has_full_year_medical_coverage = 0 THEN 2\n",
    "      WHEN e.alive_at_year_end = 0 THEN 3\n",
    non_inpatient_stage_sql,
    "    END AS exclusion_stage_order,\n",
    "    CASE\n",
    "      WHEN e.patient_dob IS NULL OR e.age < ", minimum_age, "\n",
    "        THEN 'Missing DOB or age under 65'\n",
    "      WHEN e.has_full_year_medical_coverage = 0\n",
    "        THEN 'No full-year, gap-free medical coverage'\n",
    "      WHEN e.alive_at_year_end = 0 THEN 'Not alive at year end'\n",
    non_inpatient_label_sql,
    "      ELSE 'Retained in final cohort'\n",
    "    END AS exclusion_stage\n",
    "  FROM eligibility_flags e\n",
    ")\n",
    "SELECT * FROM classified_flags;"
  )
  run_sql_stage(con, "Build temporary 2022 cohort eligibility flags", flags_sql)

  final_cohort_sql <- paste0(
    "DROP TABLE IF EXISTS ", cohort_identifier, ";\n",
    "CREATE TABLE ", cohort_identifier, "\n",
    "DISTKEY(patient_id)\n",
    "SORTKEY(analysis_year, patient_id) AS\n",
    "WITH final_flags AS (\n",
    "  SELECT * FROM ", temp_flags_identifier, "\n",
    "  WHERE exclusion_stage_order = ", retained_stage_order, "\n",
    "), race_by_patient AS (\n",
    "  SELECT\n",
    "    patient_id,\n",
    "    CASE\n",
    "      WHEN COUNT(DISTINCT NULLIF(TRIM(patient_race_ethnicity), '')) = 0\n",
    "        THEN 'UNKNOWN'\n",
    "      WHEN COUNT(DISTINCT NULLIF(TRIM(patient_race_ethnicity), '')) = 1\n",
    "        THEN MAX(NULLIF(TRIM(patient_race_ethnicity), ''))\n",
    "      ELSE 'OTHER'\n",
    "    END AS patient_race_ethnicity\n",
    "  FROM ", qualified_identifier(komodo_schema, "patient_race_ethnicity"), "\n",
    "  WHERE patient_id IS NOT NULL\n",
    "  GROUP BY patient_id\n",
    "), geography_overlaps AS (\n",
    "  SELECT\n",
    "    f.patient_id, f.analysis_year,\n",
    "    NULLIF(TRIM(pg.patient_state), '') AS patient_state,\n",
    "    NULLIF(TRIM(pg.patient_zip), '') AS patient_zip,\n",
    "    GREATEST(CAST(pg.valid_from_date AS DATE), f.year_start)\n",
    "      AS clipped_start_date,\n",
    "    LEAST(CAST(COALESCE(pg.valid_to_date, f.year_end) AS DATE), f.year_end)\n",
    "      AS clipped_end_date\n",
    "  FROM final_flags f\n",
    "  INNER JOIN ", qualified_identifier(komodo_schema, "patient_geography"), " pg\n",
    "    ON pg.patient_id = f.patient_id\n",
    "   AND pg.valid_from_date IS NOT NULL\n",
    "   AND CAST(pg.valid_from_date AS DATE) <= f.year_end\n",
    "   AND CAST(COALESCE(pg.valid_to_date, f.year_end) AS DATE) >= f.year_start\n",
    "   AND NULLIF(TRIM(pg.patient_zip), '') IS NOT NULL\n",
    "), geography_ordered AS (\n",
    "  SELECT\n",
    "    go.*,\n",
    "    MAX(go.clipped_end_date) OVER (\n",
    "      PARTITION BY go.patient_id, go.analysis_year, go.patient_state, go.patient_zip\n",
    "      ORDER BY go.clipped_start_date, go.clipped_end_date\n",
    "      ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING\n",
    "    ) AS previous_max_end\n",
    "  FROM geography_overlaps go\n",
    "  WHERE go.clipped_start_date <= go.clipped_end_date\n",
    "), geography_marked AS (\n",
    "  SELECT geography_ordered.*,\n",
    "    CASE WHEN previous_max_end IS NULL\n",
    "       OR clipped_start_date > DATEADD(day, 1, previous_max_end)\n",
    "      THEN 1 ELSE 0 END AS new_geography_island\n",
    "  FROM geography_ordered\n",
    "), geography_islands AS (\n",
    "  SELECT geography_marked.*,\n",
    "    SUM(new_geography_island) OVER (\n",
    "      PARTITION BY patient_id, analysis_year, patient_state, patient_zip\n",
    "      ORDER BY clipped_start_date, clipped_end_date\n",
    "      ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW\n",
    "    ) AS geography_island_id\n",
    "  FROM geography_marked\n",
    "), geography_union AS (\n",
    "  SELECT patient_id, analysis_year, patient_state, patient_zip,\n",
    "    geography_island_id, MIN(clipped_start_date) AS island_start_date,\n",
    "    MAX(clipped_end_date) AS island_end_date\n",
    "  FROM geography_islands\n",
    "  GROUP BY patient_id, analysis_year, patient_state, patient_zip,\n",
    "    geography_island_id\n",
    "), geography_days AS (\n",
    "  SELECT patient_id, analysis_year, patient_state, patient_zip,\n",
    "    MIN(island_start_date) AS geography_valid_from_date,\n",
    "    MAX(island_end_date) AS geography_valid_to_date,\n",
    "    SUM(DATEDIFF(day, island_start_date, island_end_date) + 1)::INTEGER\n",
    "      AS geography_days_in_year\n",
    "  FROM geography_union\n",
    "  GROUP BY patient_id, analysis_year, patient_state, patient_zip\n",
    "), geography_ranked AS (\n",
    "  SELECT\n",
    "    geography_days.*,\n",
    "    ROW_NUMBER() OVER (\n",
    "      PARTITION BY patient_id, analysis_year\n",
    "      ORDER BY geography_days_in_year DESC, geography_valid_to_date DESC,\n",
    "               geography_valid_from_date ASC, patient_zip ASC, patient_state ASC\n",
    "    ) AS geography_rank\n",
    "  FROM geography_days\n",
    ")\n",
    "SELECT\n",
    "  f.patient_id, f.analysis_year, CAST(f.year_start AS DATE) AS index_date,\n",
    "  f.age, COALESCE(f.patient_gender, 'UNKNOWN') AS patient_gender,\n",
    "  COALESCE(r.patient_race_ethnicity, 'UNKNOWN') AS patient_race_ethnicity,\n",
    "  f.mx_insurance_group, f.mx_insurance_segment,\n",
    "  f.mx_secondary_insurance_group, f.mx_secondary_insurance_segment,\n",
    "  f.overall_comparison_eligible, f.plan_comparison_eligible,\n",
    "  f.pooled_medicare_segment, f.n_valid_medicare_segments,\n",
    "  f.has_same_year_non_inpatient_event, f.has_medicare_medicaid_dual_coverage,\n",
    "  CAST(NULL AS VARCHAR(128)) AS rx_insurance_group,\n",
    "  CAST(NULL AS VARCHAR(128)) AS rx_insurance_segment,\n",
    "  CAST(NULL AS VARCHAR(128)) AS rx_secondary_insurance_group,\n",
    "  CAST(NULL AS VARCHAR(128)) AS rx_secondary_insurance_segment,\n",
    "  g.patient_state, g.patient_zip, g.geography_days_in_year,\n",
    "  g.geography_valid_from_date, g.geography_valid_to_date,\n",
    "  CASE\n",
    "    WHEN f.n_valid_primary_groups > 1 THEN 'Multiple'\n",
    "    WHEN f.primary_classification_group = 'COMMERCIAL' THEN 'Commercial'\n",
    "    WHEN f.primary_classification_group = 'MEDICAID' THEN 'Medicaid'\n",
    "    WHEN f.primary_classification_group = 'MEDICARE'\n",
    "     AND f.n_valid_primary_medicare_segments = 1\n",
    "     AND f.primary_medicare_segment = 'ADVANTAGE' THEN 'MA'\n",
    "    WHEN f.primary_classification_group = 'MEDICARE'\n",
    "     AND f.n_valid_primary_medicare_segments = 1\n",
    "     AND f.primary_medicare_segment = 'FFS' THEN 'FFS'\n",
    "    WHEN f.primary_classification_group = 'MEDICARE' THEN 'Medicare mixed'\n",
    "    ELSE 'Other'\n",
    "  END AS comparison_insurance_group,\n",
    "  CASE WHEN f.plan_comparison_eligible = 1\n",
    "    THEN CASE WHEN f.pooled_medicare_segment = 'ADVANTAGE' THEN 'MA'\n",
    "              WHEN f.pooled_medicare_segment = 'FFS' THEN 'FFS' END\n",
    "    ELSE NULL END AS plan_comparison_group\n",
    "FROM final_flags f\n",
    "LEFT JOIN race_by_patient r\n",
    "  ON f.patient_id = r.patient_id\n",
    "LEFT JOIN geography_ranked g\n",
    "  ON f.patient_id = g.patient_id\n",
    " AND f.analysis_year = g.analysis_year\n",
    " AND g.geography_rank = 1;"
  )
  run_sql_stage(con, "Create final 2022 Komodo Medicare comparison cohort", final_cohort_sql)

  stage_definitions_sql <- paste0(
    "  SELECT 1 AS stage_order, 'Age >=65 years on 2022-01-01' AS stage\n",
    "  UNION ALL SELECT 2, 'Full-year gap-free medical coverage'\n",
    "  UNION ALL SELECT 3, 'Alive on 2022-12-31'\n",
    if (require_same_year_non_inpatient) {
      "  UNION ALL SELECT 4, 'At least one same-year non-inpatient event'\n"
    } else {
      ""
    }
  )
  flow_terminal_sql <- if (require_same_year_non_inpatient) {
    paste0(
      "UNION ALL\n",
       "SELECT 5, 'Final 2022 primary cohort',\n",
       "       SUM(CASE WHEN exclusion_stage_order = 5 THEN 1 ELSE 0 END)::BIGINT,\n",
       "       COUNT(DISTINCT CASE WHEN exclusion_stage_order = 5 THEN patient_id END)::BIGINT,\n",
      "       0::BIGINT,\n",
      "       0::BIGINT,\n",
       "       SUM(CASE WHEN exclusion_stage_order = 5 THEN 1 ELSE 0 END)::BIGINT,\n",
       "       COUNT(DISTINCT CASE WHEN exclusion_stage_order = 5 THEN patient_id END)::BIGINT\n",
      "FROM ", temp_flags_identifier, "\n"
    )
  } else {
    paste0(
      "UNION ALL\n",
       "SELECT 4, 'Final 2022 comparison cohort (non-inpatient rule not applied)',\n",
       "       SUM(CASE WHEN exclusion_stage_order = 4 THEN 1 ELSE 0 END)::BIGINT,\n",
       "       COUNT(DISTINCT CASE WHEN exclusion_stage_order = 4 THEN patient_id END)::BIGINT,\n",
      "       0::BIGINT,\n",
      "       0::BIGINT,\n",
       "       SUM(CASE WHEN exclusion_stage_order = 4 THEN 1 ELSE 0 END)::BIGINT,\n",
       "       COUNT(DISTINCT CASE WHEN exclusion_stage_order = 4 THEN patient_id END)::BIGINT\n",
      "FROM ", temp_flags_identifier, "\n",
      "UNION ALL\n",
       "SELECT 5, 'CFI-eligible subset with same-year non-inpatient event',\n",
       "       SUM(CASE WHEN exclusion_stage_order = 4\n",
      "                   AND has_same_year_non_inpatient_event = 1 THEN 1 ELSE 0 END)::BIGINT,\n",
       "       COUNT(DISTINCT CASE WHEN exclusion_stage_order = 4\n",
      "                   AND has_same_year_non_inpatient_event = 1 THEN patient_id END)::BIGINT,\n",
      "       0::BIGINT,\n",
      "       0::BIGINT,\n",
       "       SUM(CASE WHEN exclusion_stage_order = 4\n",
      "                   AND has_same_year_non_inpatient_event = 1 THEN 1 ELSE 0 END)::BIGINT,\n",
       "       COUNT(DISTINCT CASE WHEN exclusion_stage_order = 4\n",
      "                   AND has_same_year_non_inpatient_event = 1 THEN patient_id END)::BIGINT\n",
      "FROM ", temp_flags_identifier, "\n"
    )
  }
  flow_counts <- DBI::dbGetQuery(
    con,
    paste0(
      "WITH stage_definitions AS (\n",
      stage_definitions_sql,
      ")\n",
      "SELECT 0 AS stage_order, '2022 candidate person-years' AS stage,\n",
      "       COUNT(*)::BIGINT AS n_person_years_entering_stage,\n",
      "       COUNT(DISTINCT patient_id)::BIGINT AS n_distinct_patients_entering_stage,\n",
      "       0::BIGINT AS n_person_years_excluded_at_stage,\n",
      "       0::BIGINT AS n_distinct_patients_excluded_at_stage,\n",
      "       COUNT(*)::BIGINT AS n_person_years_retained_after_stage,\n",
      "       COUNT(DISTINCT patient_id)::BIGINT AS n_distinct_patients_retained_after_stage\n",
      "FROM ", temp_flags_identifier, "\n",
      "UNION ALL\n",
      "SELECT d.stage_order, d.stage,\n",
      "       SUM(CASE WHEN f.exclusion_stage_order >= d.stage_order THEN 1 ELSE 0 END)::BIGINT,\n",
      "       COUNT(DISTINCT CASE WHEN f.exclusion_stage_order >= d.stage_order THEN f.patient_id END)::BIGINT,\n",
      "       SUM(CASE WHEN f.exclusion_stage_order = d.stage_order THEN 1 ELSE 0 END)::BIGINT,\n",
      "       COUNT(DISTINCT CASE WHEN f.exclusion_stage_order = d.stage_order THEN f.patient_id END)::BIGINT,\n",
      "       SUM(CASE WHEN f.exclusion_stage_order > d.stage_order THEN 1 ELSE 0 END)::BIGINT,\n",
      "       COUNT(DISTINCT CASE WHEN f.exclusion_stage_order > d.stage_order THEN f.patient_id END)::BIGINT\n",
      "FROM stage_definitions d\n",
      "CROSS JOIN ", temp_flags_identifier, " f\n",
      "GROUP BY d.stage_order, d.stage\n",
      flow_terminal_sql,
      "ORDER BY stage_order"
    )
  )
  write_aggregate_csv(
    suppress_aggregate_counts(
      flow_counts,
      c(
        "n_person_years_entering_stage",
        "n_distinct_patients_entering_stage",
        "n_person_years_excluded_at_stage",
        "n_distinct_patients_excluded_at_stage",
        "n_person_years_retained_after_stage",
        "n_distinct_patients_retained_after_stage"
      )
    ),
    flow_output_path
  )
  print(flow_counts)

  plan_flow_counts <- DBI::dbGetQuery(
    con,
    paste0(
      "WITH plan_flags AS (\n",
      "  SELECT f.patient_id, f.pooled_medicare_segment,\n",
      "    CASE\n",
      "      WHEN f.patient_dob IS NULL OR f.age < ", minimum_age, " THEN 1\n",
      "      WHEN f.has_full_year_medical_coverage = 0\n",
      "        OR f.n_valid_medicare_segments <> 1\n",
      "        OR f.has_full_year_medicare_coverage = 0 THEN 2\n",
      "      WHEN f.alive_at_year_end = 0 THEN 3\n",
      "      ELSE 4\n",
      "    END AS plan_flow_stage_order\n",
      "  FROM ", temp_flags_identifier, " f\n",
      "), plan_aggregate AS (\n",
      "  SELECT\n",
      "    COUNT(*)::BIGINT AS candidate_person_years,\n",
      "    COUNT(DISTINCT patient_id)::BIGINT AS candidate_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order >= 1 THEN 1 ELSE 0 END)::BIGINT AS age_entering_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order >= 1 THEN patient_id END)::BIGINT AS age_entering_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order = 1 THEN 1 ELSE 0 END)::BIGINT AS age_excluded_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order = 1 THEN patient_id END)::BIGINT AS age_excluded_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order > 1 THEN 1 ELSE 0 END)::BIGINT AS age_retained_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order > 1 THEN patient_id END)::BIGINT AS age_retained_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order >= 2 THEN 1 ELSE 0 END)::BIGINT AS coverage_entering_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order >= 2 THEN patient_id END)::BIGINT AS coverage_entering_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order = 2 THEN 1 ELSE 0 END)::BIGINT AS coverage_excluded_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order = 2 THEN patient_id END)::BIGINT AS coverage_excluded_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order > 2 THEN 1 ELSE 0 END)::BIGINT AS coverage_retained_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order > 2 THEN patient_id END)::BIGINT AS coverage_retained_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order >= 3 THEN 1 ELSE 0 END)::BIGINT AS alive_entering_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order >= 3 THEN patient_id END)::BIGINT AS alive_entering_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order = 3 THEN 1 ELSE 0 END)::BIGINT AS alive_excluded_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order = 3 THEN patient_id END)::BIGINT AS alive_excluded_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order > 3 THEN 1 ELSE 0 END)::BIGINT AS alive_retained_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order > 3 THEN patient_id END)::BIGINT AS alive_retained_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order = 4 THEN 1 ELSE 0 END)::BIGINT AS final_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order = 4 THEN patient_id END)::BIGINT AS final_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order = 4 AND pooled_medicare_segment = 'ADVANTAGE' THEN 1 ELSE 0 END)::BIGINT AS ma_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order = 4 AND pooled_medicare_segment = 'ADVANTAGE' THEN patient_id END)::BIGINT AS ma_patients,\n",
      "    SUM(CASE WHEN plan_flow_stage_order = 4 AND pooled_medicare_segment = 'FFS' THEN 1 ELSE 0 END)::BIGINT AS ffs_person_years,\n",
      "    COUNT(DISTINCT CASE WHEN plan_flow_stage_order = 4 AND pooled_medicare_segment = 'FFS' THEN patient_id END)::BIGINT AS ffs_patients\n",
      "  FROM plan_flags\n",
      "), flow_counts AS (\n",
      "  SELECT 0 AS stage_order, '2022 candidate person-years' AS stage,\n",
      "         candidate_person_years AS n_person_years_entering_stage, candidate_patients AS n_distinct_patients_entering_stage,\n",
      "         0::BIGINT AS n_person_years_excluded_at_stage, 0::BIGINT AS n_distinct_patients_excluded_at_stage,\n",
      "         candidate_person_years AS n_person_years_retained_after_stage, candidate_patients AS n_distinct_patients_retained_after_stage\n",
      "  FROM plan_aggregate\n",
      "  UNION ALL SELECT 1, 'Age >=65 years on 2022-01-01', age_entering_person_years, age_entering_patients, age_excluded_person_years, age_excluded_patients, age_retained_person_years, age_retained_patients FROM plan_aggregate\n",
      "  UNION ALL SELECT 2, 'Full-year, gap-free, stable Medicare FFS or MA medical coverage', coverage_entering_person_years, coverage_entering_patients, coverage_excluded_person_years, coverage_excluded_patients, coverage_retained_person_years, coverage_retained_patients FROM plan_aggregate\n",
      "  UNION ALL SELECT 3, 'Alive on 2022-12-31', alive_entering_person_years, alive_entering_patients, alive_excluded_person_years, alive_excluded_patients, alive_retained_person_years, alive_retained_patients FROM plan_aggregate\n",
      "  UNION ALL SELECT 4, 'Final Komodo Medicare plan cohort (MA or FFS)', final_person_years, final_patients, 0::BIGINT, 0::BIGINT, final_person_years, final_patients FROM plan_aggregate\n",
      "  UNION ALL SELECT 5, 'Komodo MA', ma_person_years, ma_patients, 0::BIGINT, 0::BIGINT, ma_person_years, ma_patients FROM plan_aggregate\n",
      "  UNION ALL SELECT 6, 'Komodo FFS', ffs_person_years, ffs_patients, 0::BIGINT, 0::BIGINT, ffs_person_years, ffs_patients FROM plan_aggregate\n",
      ")\n",
      "SELECT stage_order, stage,\n",
      "       n_person_years_entering_stage, n_distinct_patients_entering_stage,\n",
      "       n_person_years_excluded_at_stage, n_distinct_patients_excluded_at_stage,\n",
      "       n_person_years_retained_after_stage, n_distinct_patients_retained_after_stage\n",
      "FROM flow_counts\n",
      "ORDER BY stage_order"
    )
  )
  write_aggregate_csv(
    suppress_aggregate_counts(
      plan_flow_counts,
      c(
        "n_person_years_entering_stage",
        "n_distinct_patients_entering_stage",
        "n_person_years_excluded_at_stage",
        "n_distinct_patients_excluded_at_stage",
        "n_person_years_retained_after_stage",
        "n_distinct_patients_retained_after_stage"
      )
    ),
    plan_flow_output_path
  )
  print(plan_flow_counts)

  insurance_counts <- DBI::dbGetQuery(
    con,
    paste0(
      "WITH grouped AS (\n",
      "  SELECT comparison_insurance_group, COUNT(*)::BIGINT AS n_person_years,\n",
      "         COUNT(DISTINCT patient_id)::BIGINT AS n_distinct_patients\n",
      "  FROM ", cohort_identifier, "\n",
      "  GROUP BY comparison_insurance_group\n",
      ")\n",
       "SELECT CASE comparison_insurance_group\n",
       "         WHEN 'Commercial' THEN 1 WHEN 'MA' THEN 2 WHEN 'FFS' THEN 3\n",
       "         WHEN 'Medicaid' THEN 4 WHEN 'Medicare mixed' THEN 5\n",
       "         WHEN 'Multiple' THEN 6 WHEN 'Other' THEN 7 END AS display_order,\n",
      "       comparison_insurance_group, n_person_years, n_distinct_patients\n",
      "FROM grouped\n",
      "UNION ALL\n",
       "SELECT 8, 'Total', COUNT(*)::BIGINT, COUNT(DISTINCT patient_id)::BIGINT\n",
      "FROM ", cohort_identifier, "\n",
      "ORDER BY display_order"
    )
  )
  write_aggregate_csv(
    suppress_aggregate_counts(
      insurance_counts,
      c("n_person_years", "n_distinct_patients")
    ),
    insurance_output_path
  )
  print(insurance_counts)

  cohort_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT\n",
      "  COUNT(*)::BIGINT AS n_person_years,\n",
      "  COUNT(DISTINCT patient_id)::BIGINT AS n_distinct_patients,\n",
      "  MIN(age)::INTEGER AS minimum_age,\n",
      "  SUM(CASE WHEN analysis_year <> ", analysis_year, " THEN 1 ELSE 0 END)::BIGINT\n",
      "    AS n_non_2022_rows,\n",
      "  SUM(CASE WHEN patient_id IS NULL THEN 1 ELSE 0 END)::BIGINT\n",
      "    AS n_missing_patient_id,\n",
      "  SUM(CASE WHEN age < ", minimum_age, " OR age IS NULL THEN 1 ELSE 0 END)::BIGINT\n",
      "    AS n_invalid_age,\n",
       "  SUM(CASE WHEN geography_days_in_year > 366 THEN 1 ELSE 0 END)::BIGINT\n",
       "    AS n_geography_over_366_days\n",
      "FROM ", cohort_identifier
    )
  )
  final_flag_count <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::BIGINT AS n_final_flags\n",
      "FROM ", temp_flags_identifier, "\n",
      "WHERE exclusion_stage_order = ", retained_stage_order
    )
  )$n_final_flags[[1]]

  group_count_total <- sum(
    insurance_counts$n_person_years[
      insurance_counts$comparison_insurance_group != "Total"
    ]
  )
  qa_checks <- data.frame(
    check = c(
      "final cohort is nonempty",
      "final person-years equal distinct patients",
      "final cohort contains only 2022",
      "final cohort has no missing patient IDs",
      "final cohort minimum age is at least 65",
      "geography intervals do not exceed 366 unioned days",
      "final cohort rows equal retained temporary flags",
      "insurance-group counts reconcile to final cohort",
      "dual-coverage flag is binary"
    ),
    observed = c(
      as.integer(cohort_qa$n_person_years[[1]] == 0),
      cohort_qa$n_person_years[[1]] - cohort_qa$n_distinct_patients[[1]],
      cohort_qa$n_non_2022_rows[[1]],
      cohort_qa$n_missing_patient_id[[1]],
      cohort_qa$n_invalid_age[[1]],
      cohort_qa$n_geography_over_366_days[[1]],
      cohort_qa$n_person_years[[1]] - final_flag_count,
      cohort_qa$n_person_years[[1]] - group_count_total,
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT SUM(CASE WHEN has_medicare_medicaid_dual_coverage NOT IN (0, 1) ",
          "OR has_medicare_medicaid_dual_coverage IS NULL THEN 1 ELSE 0 END)::BIGINT ",
          "AS n_invalid_dual_coverage_flags FROM ", cohort_identifier
        )
      )$n_invalid_dual_coverage_flags[[1]]
    ),
    expected = rep(0, 9L),
    stringsAsFactors = FALSE
  )
  write_aggregate_csv(qa_checks, qa_output_path)
  print(qa_checks)

  if (any(as.numeric(qa_checks$observed) != as.numeric(qa_checks$expected))) {
    stop("2022 Komodo Medicare comparison cohort QA failed. See ", qa_output_path)
  }

  required_output_columns <- c(
    "patient_id", "analysis_year", "index_date", "age", "patient_gender",
    "patient_race_ethnicity", "mx_insurance_group", "mx_insurance_segment",
    "mx_secondary_insurance_group", "mx_secondary_insurance_segment",
    "overall_comparison_eligible", "plan_comparison_eligible",
    "pooled_medicare_segment", "n_valid_medicare_segments",
    "has_same_year_non_inpatient_event", "has_medicare_medicaid_dual_coverage",
    "rx_insurance_group", "rx_insurance_segment",
    "rx_secondary_insurance_group", "rx_secondary_insurance_segment",
    "patient_state", "patient_zip", "geography_days_in_year",
    "geography_valid_from_date", "geography_valid_to_date",
    "comparison_insurance_group", "plan_comparison_group"
  )
  require_table_columns(con, write_schema, cohort_table, required_output_columns)

  message("2022 Komodo Medicare comparison cohort complete: ", write_schema, ".", cohort_table)
  message(
    "Next required metric run: set frailty.normalized_clinical_metrics.config with ",
    "analysis_years = 2022, id_years = 2022, eligibility_table = '", cohort_table,
    "', then source ",
    "Code/2_variable construction/3.13_run_normalized_annual_clinical_metrics.R."
  )
}

build_2022_komodo_medicare_comparison_cohort()
