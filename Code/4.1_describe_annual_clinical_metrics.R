source("Code/3.0_normalized_clinical_metrics_helpers.R")

suppressPackageStartupMessages(library(ggplot2))

# Project: Frailty_Komoto annual clinical metrics descriptive analysis
# Author: Nemo Zhou
# Date started: 2026-07-02
# Date last updated: 2026-07-03
#
# ---- Purpose ----
# Generate aggregate descriptive summaries and plots for the normalized annual
# clinical metrics table. Outputs cover CFI, CCW, Gagne combined comorbidity
# score, and HIV-related CCW burden summaries. The script keeps patient-level
# rows in Redshift and only writes aggregate CSV and PNG files.

config <- get_normalized_clinical_metrics_config()
con <- connect_komodo()

min_count <- 11L
cfi_prefrail_cutpoint <- 0.15
cfi_frail_cutpoint <- 0.25
cfi_histogram_bin_width <- 0.01
gagne_histogram_bin_width <- 1

descriptive_dir <- config$descriptive_output_dir
if (is.null(descriptive_dir) || !nzchar(descriptive_dir)) {
  descriptive_dir <- file.path(
    config$output_dir,
    "4.1_annual_clinical_metrics_descriptive"
  )
}
if (!dir.exists(descriptive_dir)) {
  dir.create(descriptive_dir, recursive = TRUE)
}

final_identifier <- qualified_identifier(write_schema, config$final_table)
final_columns <- tolower(names(DBI::dbGetQuery(
  con,
  paste0("SELECT * FROM ", final_identifier, " LIMIT 0")
)))

required_columns <- c(
  "patid",
  "analysis_year",
  "age",
  "patient_gender",
  "patient_race_ethnicity",
  "mx_insurance_group",
  "mx_insurance_segment",
  "cfi_score",
  "ccw_condition_count",
  "ccw_total_condition_count",
  "gagne_score",
  "hiv_status"
)
missing_columns <- setdiff(required_columns, final_columns)
if (length(missing_columns) > 0L) {
  stop(
    "The final clinical metrics table is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

analysis_year_filter <- paste0(
  "analysis_year IN (",
  sql_values(config$analysis_years),
  ")"
)

sanitize_identifier <- function(value, prefix) {
  value <- tolower(value)
  value <- gsub("[^a-z0-9]+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  paste0(prefix, value)
}

write_output <- function(data, filename) {
  path <- file.path(descriptive_dir, filename)
  csv_data <- data
  csv_data[] <- lapply(csv_data, as.character)
  utils::write.csv(csv_data, path, row.names = FALSE, na = "")
  message("Wrote ", path)
  invisible(path)
}

save_plot <- function(plot, filename, width = 9, height = 6) {
  path <- file.path(descriptive_dir, filename)
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = 300)
  message("Wrote ", path)
  invisible(path)
}

db_query <- function(label, sql) {
  message(label)
  DBI::dbGetQuery(con, sql)
}

to_numeric_columns <- function(data, columns) {
  for (column in intersect(columns, names(data))) {
    data[[column]] <- as.numeric(data[[column]])
  }
  data
}

plot_histogram <- function(data, metric_label, filename) {
  plot_data <- to_numeric_columns(
    data,
    c("analysis_year", "bin_start", "bin_end", "n_person_years")
  )
  plot_data <- plot_data[!is.na(plot_data$n_person_years), , drop = FALSE]
  if (nrow(plot_data) == 0L) {
    return(invisible(NULL))
  }

  plot <- ggplot(
    plot_data,
    aes(x = bin_start, y = n_person_years, width = bin_end - bin_start)
  ) +
    geom_col(fill = "#3b6ea8", color = "white", linewidth = 0.1) +
    facet_wrap(~ analysis_year, scales = "free_y") +
    labs(x = metric_label, y = "Person-years") +
    theme_minimal(base_size = 12)

  save_plot(plot, filename)
}

plot_box_summary <- function(data, metric_label, filename) {
  plot_data <- to_numeric_columns(
    data,
    c("minimum", "p25", "median", "p75", "maximum", "n_person_years")
  )
  plot_data <- plot_data[
    !is.na(plot_data$n_person_years) & plot_data$n_person_years >= min_count,
    ,
    drop = FALSE
  ]
  if (nrow(plot_data) == 0L) {
    return(invisible(NULL))
  }

  plot_data$facet_label <- if (length(unique(plot_data$analysis_year)) > 1L) {
    paste(plot_data$stratification, plot_data$analysis_year, sep = " / ")
  } else {
    plot_data$stratification
  }
  plot_data$stratum_value <- stats::reorder(
    plot_data$stratum_value,
    plot_data$median
  )

  plot <- ggplot(
    plot_data,
    aes(
      x = stratum_value,
      ymin = minimum,
      lower = p25,
      middle = median,
      upper = p75,
      ymax = maximum
    )
  ) +
    geom_boxplot(stat = "identity", fill = "#86b6d9", color = "#1f2933", width = 0.5) +
    coord_flip() +
    facet_wrap(~ facet_label, scales = "free_y") +
    labs(
      title = paste(metric_label, "distribution by subgroup"),
      x = NULL,
      y = metric_label
    ) +
    theme_minimal(base_size = 10)

  save_plot(plot, filename, width = 12, height = 8)
}

age_group_expr <- paste0(
  "CASE
     WHEN age BETWEEN 40 AND 49 THEN '40-49'
     WHEN age BETWEEN 50 AND 64 THEN '50-64'
     WHEN age BETWEEN 65 AND 74 THEN '65-74'
     WHEN age BETWEEN 75 AND 84 THEN '75-84'
     WHEN age >= 85 THEN '85+'
     ELSE 'UNKNOWN'
   END"
)
sex_expr <- "COALESCE(NULLIF(TRIM(patient_gender), ''), 'UNKNOWN')"
insurance_expr <- "COALESCE(NULLIF(TRIM(mx_insurance_group), ''), 'UNKNOWN')"
insurance_segment_expr <- paste0(
  "COALESCE(NULLIF(TRIM(mx_insurance_group), ''), 'UNKNOWN') || ' / ' ||
   COALESCE(NULLIF(TRIM(mx_insurance_segment), ''), 'UNKNOWN')"
)
race_expr <- "COALESCE(NULLIF(TRIM(patient_race_ethnicity), ''), 'UNKNOWN')"
hiv_status_expr <- "CASE WHEN hiv_status = 1 THEN 'HIV positive' ELSE 'HIV negative' END"
frailty_expr <- paste0(
  "CASE
     WHEN cfi_score < ", cfi_prefrail_cutpoint, " THEN 'Robust (<0.15)'
     WHEN cfi_score < ", cfi_frail_cutpoint, " THEN 'Prefrail (0.15-<0.25)'
     ELSE 'Frail (>=0.25)'
   END"
)

strata <- list(
  age_group = age_group_expr,
  sex = sex_expr,
  insurance_type = insurance_expr,
  insurance_segment = insurance_segment_expr,
  race_ethnicity = race_expr
)

metric_summary_sql <- function(metric_name, value_expr, where_sql, group_sql = NULL) {
  group_select <- ""
  group_by <- ""
  if (!is.null(group_sql)) {
    group_select <- paste0(",\n       ", group_sql)
    group_by <- paste0("\n     GROUP BY analysis_year, ", group_sql)
  } else {
    group_by <- "\n     GROUP BY analysis_year"
  }

  paste0(
    "SELECT
       analysis_year,
       ", sql_string(metric_name), " AS metric",
    group_select,
    ",
       COUNT(*)::BIGINT AS n_person_years,
       AVG(", value_expr, ")::DOUBLE PRECISION AS mean_value,
       STDDEV_SAMP(", value_expr, ")::DOUBLE PRECISION AS sd_value,
       PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY ", value_expr, ") AS p25,
       PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY ", value_expr, ") AS median,
       PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY ", value_expr, ") AS p75,
       MIN(", value_expr, ")::DOUBLE PRECISION AS minimum,
       MAX(", value_expr, ")::DOUBLE PRECISION AS maximum
     FROM ", final_identifier, "
     WHERE ", analysis_year_filter, "
       AND ", where_sql,
    group_by,
    "
     HAVING COUNT(*) >= ", min_count, "
     ORDER BY analysis_year"
  )
}

stratified_metric_sql <- function(metric_name, value_expr, where_sql) {
  parts <- lapply(
    names(strata),
    function(stratum_name) {
      paste0(
        "SELECT
           analysis_year,
           ", sql_string(metric_name), " AS metric,
           ", sql_string(stratum_name), " AS stratification,
           ", strata[[stratum_name]], " AS stratum_value,
           COUNT(*)::BIGINT AS n_person_years,
           AVG(", value_expr, ")::DOUBLE PRECISION AS mean_value,
           STDDEV_SAMP(", value_expr, ")::DOUBLE PRECISION AS sd_value,
           PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY ", value_expr, ") AS p25,
           PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY ", value_expr, ") AS median,
           PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY ", value_expr, ") AS p75,
           MIN(", value_expr, ")::DOUBLE PRECISION AS minimum,
           MAX(", value_expr, ")::DOUBLE PRECISION AS maximum
         FROM ", final_identifier, "
         WHERE ", analysis_year_filter, "
           AND ", where_sql, "
         GROUP BY analysis_year, ", strata[[stratum_name]], "
         HAVING COUNT(*) >= ", min_count
      )
    }
  )

  paste(paste("(", unlist(parts), ")", sep = ""), collapse = "\nUNION ALL\n")
}

histogram_sql <- function(metric_name, value_expr, where_sql, bin_width) {
  paste0(
    "SELECT
       analysis_year,
       ", sql_string(metric_name), " AS metric,
       FLOOR(", value_expr, " / ", bin_width, ") * ", bin_width, " AS bin_start,
       (FLOOR(", value_expr, " / ", bin_width, ") + 1) * ", bin_width, " AS bin_end,
       COUNT(*)::BIGINT AS n_person_years
     FROM ", final_identifier, "
     WHERE ", analysis_year_filter, "
       AND ", where_sql, "
     GROUP BY analysis_year, FLOOR(", value_expr, " / ", bin_width, ")
     HAVING COUNT(*) >= ", min_count, "
     ORDER BY analysis_year, bin_start"
  )
}

cfi_overall <- db_query(
  "Summarizing CFI overall distribution.",
  metric_summary_sql("CFI", "cfi_score", "cfi_score IS NOT NULL")
)
write_output(cfi_overall, "4.1_cfi_overall_summary.csv")

cfi_by_strata <- db_query(
  "Summarizing CFI distribution by age, sex, insurance, and race/ethnicity.",
  stratified_metric_sql("CFI", "cfi_score", "cfi_score IS NOT NULL")
)
write_output(cfi_by_strata, "4.1_cfi_distribution_by_strata.csv")

cfi_histogram <- db_query(
  "Building CFI histogram bins.",
  histogram_sql("CFI", "cfi_score", "cfi_score IS NOT NULL", cfi_histogram_bin_width)
)
write_output(cfi_histogram, "4.1_cfi_histogram_bins.csv")
plot_histogram(cfi_histogram, "CFI score", "4.1_cfi_histogram.png")

plot_box_summary(
  cfi_by_strata,
  "CFI score",
  "4.1_cfi_boxplot_by_subgroup.png"
)

gagne_overall <- db_query(
  "Summarizing Gagne overall distribution.",
  metric_summary_sql("Gagne", "gagne_score", "gagne_score IS NOT NULL")
)
write_output(gagne_overall, "4.1_gagne_overall_summary.csv")

gagne_by_strata <- db_query(
  "Summarizing Gagne distribution by age, sex, insurance, and race/ethnicity.",
  stratified_metric_sql("Gagne", "gagne_score", "gagne_score IS NOT NULL")
)
write_output(gagne_by_strata, "4.1_gagne_distribution_by_strata.csv")

gagne_histogram <- db_query(
  "Building Gagne histogram bins.",
  histogram_sql("Gagne", "gagne_score", "gagne_score IS NOT NULL", gagne_histogram_bin_width)
)
write_output(gagne_histogram, "4.1_gagne_histogram_bins.csv")
plot_histogram(gagne_histogram, "Gagne score", "4.1_gagne_histogram.png")

plot_box_summary(
  gagne_by_strata,
  "Gagne score",
  "4.1_gagne_boxplot_by_subgroup.png"
)

ccw_lookup_path <- file.path(config$lookup_dir, "0.6_ccw_diagnosis_lookup.csv")
if (!file.exists(ccw_lookup_path)) {
  stop("Missing CCW lookup: ", ccw_lookup_path)
}
ccw_lookup <- read_lookup_csv(ccw_lookup_path)
require_columns(
  ccw_lookup,
  c("ccw_condition_id", "ccw_condition_name", "ccw_group"),
  "CCW lookup"
)

ccw_conditions <- unique(ccw_lookup[, c(
  "ccw_condition_id",
  "ccw_condition_name",
  "ccw_group"
)])
ccw_conditions <- ccw_conditions[
  !is.na(ccw_conditions$ccw_condition_id) &
    ccw_conditions$ccw_condition_id != "" &
    !is.na(ccw_conditions$ccw_group) &
    ccw_conditions$ccw_group != "",
]
ccw_conditions$indicator_column <- vapply(
  ccw_conditions$ccw_condition_id,
  sanitize_identifier,
  character(1),
  prefix = "ccw_"
)
ccw_conditions <- ccw_conditions[
  ccw_conditions$indicator_column %in% final_columns,
  ,
  drop = FALSE
]
if (nrow(ccw_conditions) == 0L) {
  stop("No CCW condition indicator columns were found in the final table.")
}

ccw_prevalence_parts <- apply(
  ccw_conditions,
  1L,
  function(row) {
    numerator <- paste0(
      "SUM(CASE WHEN ",
      quote_identifier(row[["indicator_column"]]),
      " = 1 THEN 1 ELSE 0 END)"
    )
    paste0(
      "SELECT
         analysis_year,
         ", sql_string(row[["ccw_condition_id"]]), " AS ccw_condition_id,
         ", sql_string(row[["ccw_condition_name"]]), " AS ccw_condition_name,
         ", sql_string(row[["ccw_group"]]), " AS ccw_group,
         COUNT(*)::BIGINT AS denominator_person_years,
         CASE WHEN ", numerator, " BETWEEN 1 AND ", min_count - 1L, "
              THEN NULL ELSE ", numerator, "::BIGINT END AS n_with_condition,
         CASE WHEN ", numerator, " BETWEEN 1 AND ", min_count - 1L, "
              THEN NULL ELSE ", numerator, "::DOUBLE PRECISION / COUNT(*) END
              AS prevalence,
         CASE WHEN ", numerator, " BETWEEN 1 AND ", min_count - 1L, "
              THEN TRUE ELSE FALSE END AS small_cell_suppressed
       FROM ", final_identifier, "
       WHERE ", analysis_year_filter, "
       GROUP BY analysis_year
       HAVING COUNT(*) >= ", min_count
    )
  }
)
ccw_condition_prevalence <- db_query(
  "Calculating CCW condition prevalence.",
  paste(paste0("(", ccw_prevalence_parts, ")"), collapse = "\nUNION ALL\n")
)
write_output(ccw_condition_prevalence, "4.1_ccw_condition_prevalence.csv")

ccw_group_columns <- grep("^index_", final_columns, value = TRUE)
if (length(ccw_group_columns) == 0L) {
  stop("No CCW group indicator columns were found in the final table.")
}
ccw_group_parts <- lapply(
  ccw_group_columns,
  function(column) {
    group_name <- toupper(sub("^index_", "", column))
    numerator <- paste0(
      "SUM(CASE WHEN ",
      quote_identifier(column),
      " > 0 THEN 1 ELSE 0 END)"
    )
    paste0(
      "SELECT
         analysis_year,
         ", sql_string(group_name), " AS ccw_group,
         COUNT(*)::BIGINT AS denominator_person_years,
         CASE WHEN ", numerator, " BETWEEN 1 AND ", min_count - 1L, "
              THEN NULL ELSE ", numerator, "::BIGINT END AS n_with_group,
         CASE WHEN ", numerator, " BETWEEN 1 AND ", min_count - 1L, "
              THEN NULL ELSE ", numerator, "::DOUBLE PRECISION / COUNT(*) END
              AS prevalence,
         CASE WHEN ", numerator, " BETWEEN 1 AND ", min_count - 1L, "
              THEN TRUE ELSE FALSE END AS small_cell_suppressed
       FROM ", final_identifier, "
       WHERE ", analysis_year_filter, "
       GROUP BY analysis_year
       HAVING COUNT(*) >= ", min_count
    )
  }
)
ccw_group_prevalence <- db_query(
  "Calculating CCW group prevalence.",
  paste(paste0("(", unlist(ccw_group_parts), ")"), collapse = "\nUNION ALL\n")
)
write_output(ccw_group_prevalence, "4.1_ccw_group_prevalence.csv")

ccw_burden_parts <- lapply(
  names(strata),
  function(stratum_name) {
    paste0(
      "SELECT
         analysis_year,
         ", sql_string(stratum_name), " AS stratification,
         ", strata[[stratum_name]], " AS stratum_value,
         COUNT(*)::BIGINT AS n_person_years,
         AVG(ccw_condition_count)::DOUBLE PRECISION AS mean_condition_count,
         AVG(ccw_total_condition_count)::DOUBLE PRECISION AS mean_group_condition_count,
         SUM(CASE WHEN ccw_condition_count = 0 THEN 1 ELSE 0 END)::DOUBLE PRECISION /
           COUNT(*) AS zero_condition_percent,
         SUM(CASE WHEN ccw_condition_count >= 5 THEN 1 ELSE 0 END)::DOUBLE PRECISION /
           COUNT(*) AS five_plus_condition_percent
       FROM ", final_identifier, "
       WHERE ", analysis_year_filter, "
       GROUP BY analysis_year, ", strata[[stratum_name]], "
       HAVING COUNT(*) >= ", min_count
    )
  }
)
ccw_burden_by_strata <- db_query(
  "Summarizing CCW burden by age, sex, payer, and race/ethnicity.",
  paste(paste0("(", unlist(ccw_burden_parts), ")"), collapse = "\nUNION ALL\n")
)
write_output(ccw_burden_by_strata, "4.1_ccw_burden_by_strata.csv")

hiv_ccw_strata <- list(
  age_group = age_group_expr,
  sex = sex_expr,
  payer = insurance_expr
)
hiv_ccw_parts <- lapply(
  names(hiv_ccw_strata),
  function(stratum_name) {
    paste0(
      "SELECT
         analysis_year,
         ", sql_string(stratum_name), " AS stratification,
         ", hiv_ccw_strata[[stratum_name]], " AS stratum_value,
         ", hiv_status_expr, " AS hiv_status_group,
         COUNT(*)::BIGINT AS n_person_years,
         AVG(ccw_condition_count)::DOUBLE PRECISION AS mean_condition_count,
         AVG(ccw_total_condition_count)::DOUBLE PRECISION AS mean_group_condition_count,
         SUM(CASE WHEN ccw_condition_count = 0 THEN 1 ELSE 0 END)::DOUBLE PRECISION /
           COUNT(*) AS zero_condition_percent,
         SUM(CASE WHEN ccw_condition_count >= 5 THEN 1 ELSE 0 END)::DOUBLE PRECISION /
           COUNT(*) AS five_plus_condition_percent
       FROM ", final_identifier, "
       WHERE ", analysis_year_filter, "
       GROUP BY analysis_year, ", hiv_ccw_strata[[stratum_name]], ", ", hiv_status_expr, "
       HAVING COUNT(*) >= ", min_count
    )
  }
)
hiv_ccw_burden <- db_query(
  "Summarizing CCW burden by HIV status, age, sex, and payer.",
  paste(paste0("(", unlist(hiv_ccw_parts), ")"), collapse = "\nUNION ALL\n")
)
write_output(hiv_ccw_burden, "4.1_hiv_ccw_burden_by_strata.csv")

table_one_category_exprs <- lapply(
  list(
    age_group = age_group_expr,
    sex = sex_expr,
    insurance_type = insurance_expr,
    insurance_segment = insurance_segment_expr,
    race_ethnicity = race_expr,
    hiv_status = hiv_status_expr
  ),
  function(category_expr) {
    names(category_expr) <- NULL
    category_expr
  }
)

build_categorical_table_one <- function(
  group_expr,
  group_column,
  denominator_column,
  percent_column,
  where_sql,
  message_label,
  category_exprs = table_one_category_exprs
) {
  table_one_categorical_parts <- Map(
    function(variable_name, category_expr) {
      paste0(
        "SELECT
           analysis_year,
           ", group_expr, " AS ", group_column, ",
           ", sql_string(variable_name), " AS variable,
           ", category_expr, " AS category,
           COUNT(*)::BIGINT AS n_person_years
         FROM ", final_identifier, "
         WHERE ", analysis_year_filter, "
           AND ", where_sql, "
         GROUP BY analysis_year, ", group_expr, ", ", category_expr
      )
    },
    names(category_exprs),
    category_exprs
  )

  db_query(
    message_label,
    paste0(
      "WITH categorical AS (
         ",
      paste(unlist(table_one_categorical_parts), collapse = "\nUNION ALL\n"),
      "
       ),
       denominators AS (
         SELECT
           analysis_year,
           ", group_expr, " AS ", group_column, ",
           COUNT(*)::BIGINT AS ", denominator_column, "
         FROM ", final_identifier, "
         WHERE ", analysis_year_filter, "
           AND ", where_sql, "
         GROUP BY analysis_year, ", group_expr, "
       )
       SELECT
         c.analysis_year,
         c.", group_column, ",
         c.variable,
         c.category,
         d.", denominator_column, ",
         CASE WHEN c.n_person_years BETWEEN 1 AND ", min_count - 1L, "
              THEN NULL ELSE c.n_person_years END AS n_person_years,
         CASE WHEN c.n_person_years BETWEEN 1 AND ", min_count - 1L, "
              THEN NULL ELSE c.n_person_years::DOUBLE PRECISION / d.", denominator_column, " END
              AS ", percent_column, ",
         CASE WHEN c.n_person_years BETWEEN 1 AND ", min_count - 1L, "
              THEN TRUE ELSE FALSE END AS small_cell_suppressed
       FROM categorical c
       INNER JOIN denominators d
         ON c.analysis_year = d.analysis_year
        AND c.", group_column, " = d.", group_column, "
       WHERE d.", denominator_column, " >= ", min_count, "
       ORDER BY c.analysis_year, c.", group_column, ", c.variable, c.category"
    )
  )
}

gagne_level_expr <- "CASE
  WHEN gagne_score < 0 THEN 'Gagne <0'
  WHEN gagne_score = 0 THEN 'Gagne 0'
  WHEN gagne_score BETWEEN 1 AND 2 THEN 'Gagne 1-2'
  WHEN gagne_score BETWEEN 3 AND 5 THEN 'Gagne 3-5'
  ELSE 'Gagne 6+'
END"

table_one_categorical <- build_categorical_table_one(
  group_expr = frailty_expr,
  group_column = "frailty_level",
  denominator_column = "frailty_denominator",
  percent_column = "percent_within_frailty_level",
  where_sql = "cfi_score IS NOT NULL",
  message_label = "Building categorical Table 1 by frailty level."
)
write_output(table_one_categorical, "4.1_table_one_by_frailty_level_categorical.csv")

table_one_gagne_categorical <- build_categorical_table_one(
  group_expr = gagne_level_expr,
  group_column = "gagne_level",
  denominator_column = "gagne_denominator",
  percent_column = "percent_within_gagne_level",
  where_sql = "gagne_score IS NOT NULL",
  message_label = "Building categorical Table 1 by Gagne score level."
)
write_output(
  table_one_gagne_categorical,
  "4.1_table_one_by_gagne_level_categorical.csv"
)

table_one_hiv_categorical <- build_categorical_table_one(
  group_expr = hiv_status_expr,
  group_column = "hiv_status_group",
  denominator_column = "hiv_status_denominator",
  percent_column = "percent_within_hiv_status",
  where_sql = "hiv_status IS NOT NULL",
  message_label = "Building categorical Table 1 by HIV status.",
  category_exprs = table_one_category_exprs[
    names(table_one_category_exprs) != "hiv_status"
  ]
)
write_output(
  table_one_hiv_categorical,
  "4.1_table_one_by_hiv_status_categorical.csv"
)

table_one_continuous_parts <- list(
  age = "age",
  cfi_score = "cfi_score",
  gagne_score = "gagne_score",
  ccw_condition_count = "ccw_condition_count",
  ccw_total_condition_count = "ccw_total_condition_count"
)
table_one_continuous_parts <- Map(
  function(variable_name, value_expr) {
    paste0(
      "SELECT
         analysis_year,
         ", frailty_expr, " AS frailty_level,
         ", sql_string(variable_name), " AS variable,
         COUNT(*)::BIGINT AS n_person_years,
         AVG(", value_expr, ")::DOUBLE PRECISION AS mean_value,
         STDDEV_SAMP(", value_expr, ")::DOUBLE PRECISION AS sd_value,
         PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY ", value_expr, ") AS p25,
         PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY ", value_expr, ") AS median,
         PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY ", value_expr, ") AS p75,
         MIN(", value_expr, ")::DOUBLE PRECISION AS minimum,
         MAX(", value_expr, ")::DOUBLE PRECISION AS maximum
       FROM ", final_identifier, "
       WHERE ", analysis_year_filter, "
         AND cfi_score IS NOT NULL
         AND ", value_expr, " IS NOT NULL
       GROUP BY analysis_year, ", frailty_expr, "
       HAVING COUNT(*) >= ", min_count
    )
  },
  names(table_one_continuous_parts),
  table_one_continuous_parts
)
table_one_continuous <- db_query(
  "Building continuous Table 1 by frailty level.",
  paste(paste0("(", unlist(table_one_continuous_parts), ")"), collapse = "\nUNION ALL\n")
)
write_output(table_one_continuous, "4.1_table_one_by_frailty_level_continuous.csv")

message(
  "Annual clinical metrics descriptive analysis complete. Aggregate outputs are in: ",
  descriptive_dir
)

disconnect_komodo(con)
