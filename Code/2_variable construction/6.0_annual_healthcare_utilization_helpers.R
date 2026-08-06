source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto healthcare utilization
# Author: Nemo Zhou
# Date started: 2026-07-17
# Date last updated: 2026-08-04
#
# ---- Purpose ----
# Provide the shared configuration and utility functions for the 2022
# healthcare-utilization workflow. The 6.x scripts use every eligible
# patient-year in the 2022 comparison clinical-metrics table, retain medical
# insurance group and segment values, prepare event-level utilization records,
# construct patient-year variables, and append those variables to a new
# derivative clinical-metrics table. This helper does not write tables.

default_annual_healthcare_utilization_config <- list(
  analysis_year = 2022L,
  cohort_table = "6_2022_komodo_medicare_comparison_clinical_metrics_shared",
  inpatient_table = "inpatient_events",
  non_inpatient_table = "non_inpatient_events",
  events_table = "6_2022_komodo_medicare_comparison_healthcare_utilization_events",
  metrics_table = "6_2022_komodo_medicare_comparison_healthcare_utilization_metrics",
  combined_table = "6_2022_komodo_medicare_comparison_clinical_metrics_with_utilization",
  output_root = file.path(getwd(), "Outputs"),
  output_dir = NULL,
  min_count = 11L,
  preflight_n = 10000L
)

protected_annual_healthcare_utilization_tables <- c(
  "6_annual_healthcare_utilization_events",
  "6_annual_healthcare_utilization_metrics",
  "6_annual_clinical_metrics_shared_2022",
  "6_annual_clinical_metrics_shared_2022_with_utilization"
)

validate_annual_healthcare_utilization_table_names <- function(config) {
  output_names <- unname(unlist(
    config[c("events_table", "metrics_table", "combined_table")],
    use.names = FALSE
  ))
  protected_outputs <- intersect(output_names, protected_annual_healthcare_utilization_tables)
  if (length(protected_outputs) > 0L) {
    stop(
      "Protected original utilization table name(s) were configured as outputs: ",
      paste(protected_outputs, collapse = ", "),
      ". Use the 2022 comparison-specific table names."
    )
  }
  if (anyDuplicated(output_names) > 0L) {
    duplicated_outputs <- unique(output_names[duplicated(output_names)])
    stop(
      "Healthcare-utilization output table names must be unique: ",
      paste(duplicated_outputs, collapse = ", "), "."
    )
  }
  invisible(config)
}

get_annual_healthcare_utilization_config <- function() {
  config <- utils::modifyList(
    default_annual_healthcare_utilization_config,
    getOption("frailty.annual_healthcare_utilization.config", list())
  )

  config$analysis_year <- as.integer(config$analysis_year)
  if (
    length(config$analysis_year) != 1L ||
      is.na(config$analysis_year) ||
      config$analysis_year < 2016L ||
      config$analysis_year > 2025L
  ) {
    stop("analysis_year must be one integer year from 2016 through 2025.")
  }

  config$min_count <- as.integer(config$min_count)
  if (length(config$min_count) != 1L || is.na(config$min_count) || config$min_count < 1L) {
    stop("min_count must be one positive integer.")
  }

  config$preflight_n <- as.integer(config$preflight_n)
  if (length(config$preflight_n) != 1L || is.na(config$preflight_n) || config$preflight_n < 1L) {
    stop("preflight_n must be one positive integer.")
  }

  required_names <- c(
    "cohort_table", "inpatient_table", "non_inpatient_table", "events_table",
    "metrics_table", "combined_table", "output_root"
  )
  for (name in required_names) {
    value <- as.character(config[[name]])
    if (length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop(name, " must be one nonempty value.")
    }
    config[[name]] <- value
  }

  if (is.null(config$output_dir)) {
    config$output_dir <- file.path(
      config$output_root,
      paste0("6.x_annual_healthcare_utilization_", config$analysis_year)
    )
  }
  config$output_dir <- as.character(config$output_dir)
  if (length(config$output_dir) != 1L || is.na(config$output_dir) || !nzchar(config$output_dir)) {
    stop("output_dir must be one nonempty path.")
  }

  validate_annual_healthcare_utilization_table_names(config)
  config
}

utilization_categories <- data.frame(
  utilization_category = c(
    "Acute Inpatient",
    "Hospice (Inpatient)",
    "Skilled Nursing",
    "IPF",
    "Long-term care",
    "Emergency Department"
  ),
  metric_prefix = c(
    "acute_inpatient",
    "inpatient_hospice",
    "skilled_nursing",
    "ipf",
    "long_term_care",
    "ed"
  ),
  stringsAsFactors = FALSE
)

utilization_ed_definitions <- data.frame(
  definition = c(
    "visit_type",
    "service_subcategory",
    "revenue_code"
  ),
  metric_prefix = c(
    "ed_visit_type",
    "ed_service_subcategory",
    "ed_revenue_code"
  ),
  event_flag = c(
    "ed_visit_type_flag",
    "ed_service_subcategory_flag",
    "ed_revenue_code_flag"
  ),
  stringsAsFactors = FALSE
)

utilization_year_start <- function(config) {
  sprintf("%04d-01-01", config$analysis_year)
}

utilization_year_end <- function(config) {
  sprintf("%04d-01-01", config$analysis_year + 1L)
}

utilization_cohort_sql <- function(config, alias = "cohort") {
  paste0(
    "SELECT DISTINCT\n",
    "       patient_id,\n",
    "       analysis_year,\n",
    "       mx_insurance_group,\n",
    "       mx_insurance_segment\n",
    "FROM ", qualified_identifier(write_schema, config$cohort_table), "\n",
    "WHERE analysis_year = ", config$analysis_year
  )
}

utilization_run_sql_stage <- function(con, label, sql) {
  started_at <- Sys.time()
  message(format(started_at, "[%Y-%m-%d %H:%M:%S] "), "START ", label)
  execute_sql_with_retry(con, sql, label = label)
  finished_at <- Sys.time()
  message(
    format(finished_at, "[%Y-%m-%d %H:%M:%S] "),
    "DONE  ", label, " (",
    round(as.numeric(difftime(finished_at, started_at, units = "mins")), 1),
    " min)"
  )
  invisible(NULL)
}

utilization_write_csv <- function(data, path) {
  output <- data
  output[] <- lapply(output, as.character)
  utils::write.csv(output, path, row.names = FALSE, na = "")
  message("Wrote ", path)
  invisible(path)
}

utilization_metric_column_definitions <- function() {
  standard_categories <- utilization_categories[
    utilization_categories$utilization_category != "Emergency Department",
    ,
    drop = FALSE
  ]
  standard_columns <- unlist(lapply(standard_categories$metric_prefix, function(prefix) {
    c(
      paste0(prefix, "_any_visit INTEGER NOT NULL"),
      paste0(prefix, "_n_visits INTEGER NOT NULL"),
      paste0(prefix, "_total_duration_days INTEGER"),
      paste0(prefix, "_duration_n INTEGER NOT NULL"),
      paste0(prefix, "_duration_missing_n INTEGER NOT NULL")
    )
  }))
  ed_columns <- paste0(
    utilization_ed_definitions$metric_prefix,
    "_any_visit INTEGER NOT NULL"
  )
  c(standard_columns, ed_columns)
}

utilization_metric_select_sql <- function(events_alias = "events") {
  standard_categories <- utilization_categories[
    utilization_categories$utilization_category != "Emergency Department",
    ,
    drop = FALSE
  ]
  standard_sql <- unlist(lapply(seq_len(nrow(standard_categories)), function(index) {
    category <- standard_categories$utilization_category[[index]]
    prefix <- standard_categories$metric_prefix[[index]]
    category_sql <- sql_string(category)
    c(
      paste0(
        "CASE WHEN COUNT(CASE WHEN ", events_alias, ".utilization_category = ", category_sql,
        " THEN 1 END) > 0 THEN 1 ELSE 0 END AS ", prefix, "_any_visit"
      ),
      paste0(
        "COUNT(CASE WHEN ", events_alias, ".utilization_category = ", category_sql,
        " THEN 1 END)::INTEGER AS ", prefix, "_n_visits"
      ),
      paste0(
        "CASE\n",
        "  WHEN COUNT(CASE WHEN ", events_alias, ".utilization_category = ", category_sql, " THEN 1 END) = 0 THEN 0\n",
        "  WHEN COUNT(CASE WHEN ", events_alias, ".utilization_category = ", category_sql,
        " AND ", events_alias, ".duration_days IS NOT NULL THEN 1 END) = 0 THEN NULL\n",
        "  ELSE SUM(CASE WHEN ", events_alias, ".utilization_category = ", category_sql,
        " AND ", events_alias, ".duration_days IS NOT NULL THEN ", events_alias,
        ".duration_days ELSE 0 END)::INTEGER\n",
        "END AS ", prefix, "_total_duration_days"
      ),
      paste0(
        "COUNT(CASE WHEN ", events_alias, ".utilization_category = ", category_sql,
        " AND ", events_alias, ".duration_days IS NOT NULL THEN 1 END)::INTEGER AS ", prefix, "_duration_n"
      ),
      paste0(
        "COUNT(CASE WHEN ", events_alias, ".utilization_category = ", category_sql,
        " AND ", events_alias, ".duration_days IS NULL THEN 1 END)::INTEGER AS ", prefix, "_duration_missing_n"
      )
    )
  }))
  ed_sql <- unlist(lapply(seq_len(nrow(utilization_ed_definitions)), function(index) {
    prefix <- utilization_ed_definitions$metric_prefix[[index]]
    event_flag <- utilization_ed_definitions$event_flag[[index]]
    paste0(
      "CASE WHEN COUNT(CASE WHEN ", events_alias,
      ".utilization_category = 'Emergency Department' AND ", events_alias,
      ".", event_flag, " = 1 THEN 1 END) > 0 THEN 1 ELSE 0 END AS ",
      prefix, "_any_visit"
    )
  }))
  paste(c(standard_sql, ed_sql), collapse = ",\n       ")
}

utilization_metric_columns <- function() {
  standard_categories <- utilization_categories[
    utilization_categories$utilization_category != "Emergency Department",
    ,
    drop = FALSE
  ]
  standard_columns <- unlist(lapply(standard_categories$metric_prefix, function(prefix) {
    c(
      paste0(prefix, "_any_visit"),
      paste0(prefix, "_n_visits"),
      paste0(prefix, "_total_duration_days"),
      paste0(prefix, "_duration_n"),
      paste0(prefix, "_duration_missing_n")
    )
  }))
  ed_columns <- paste0(utilization_ed_definitions$metric_prefix, "_any_visit")
  c(standard_columns, ed_columns)
}
