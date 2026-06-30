library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto 2016 CFI subgroup summaries
# Author: Nemo Zhou
# Date started: 2026-06-15
# Date last updated: 2026-06-15
#
# ---- Purpose ----
# Summarize 2016 Claims-Based Frailty Index (CFI) scores overall and by
# demographic and insurance subgroups. This script uses the patient-year score
# table created by Code/3.3_compute_2016_cfi_scores.R and writes aggregate-only
# CSV outputs to Outputs.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Analysis parameters ----
analysis_year <- 2016L
model_intercept <- 0.10288
intercept_tolerance <- 1e-10
min_count <- 11L

scores_table <- "cfi_2016_scores"
ids_table <- "cfi_2016_ids"
race_table <- "patient_race_ethnicity"

output_dir <- file.path(getwd(), "Outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

summary_output_file <- file.path(
  output_dir,
  "3.4_cfi_2016_subgroup_summary.csv"
)
category_output_file <- file.path(
  output_dir,
  "3.4_cfi_2016_subgroup_category_summary.csv"
)

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Helpers ----
quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

qualified_identifier <- function(schema, table) {
  paste(
    quote_identifier(schema),
    quote_identifier(table),
    sep = "."
  )
}

sql_string <- function(value) {
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

sql_number <- function(value) {
  ifelse(is.na(value), "NULL", as.character(value))
}

table_exists <- function(schema, table) {
  result <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::INTEGER AS table_count
       FROM information_schema.tables
       WHERE table_schema = ", sql_string(schema), "
         AND table_name = ", sql_string(table)
    )
  )

  nrow(result) == 1L &&
    !is.na(result$table_count[[1]]) &&
    result$table_count[[1]] == 1L
}

table_has_columns <- function(schema, table, columns) {
  result <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT LOWER(column_name) AS column_name
       FROM information_schema.columns
       WHERE table_schema = ", sql_string(schema), "
         AND table_name = ", sql_string(table)
    )
  )

  missing_columns <- setdiff(tolower(columns), result$column_name)
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns in ",
      schema,
      ".",
      table,
      ": ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

format_output_value <- function(value) {
  if (is.na(value)) {
    return(NA_character_)
  }

  formatted <- format(value, scientific = FALSE, trim = TRUE)
  if (grepl("^0(\\.0+)?$", formatted)) {
    return("0")
  }

  formatted <- sub("(\\.\\d*?)0+$", "\\1", formatted)
  sub("\\.$", "", formatted)
}

clean_summary_values <- function(data) {
  numeric_columns <- c(
    "mean_cfi", "minimum_cfi", "q1_cfi", "median_cfi", "q3_cfi",
    "maximum_cfi", "category_percent"
  )
  count_columns <- c("n_patient_years", "n_intercept_value", "category_count")

  for (column in intersect(numeric_columns, names(data))) {
    data[[column]] <- vapply(data[[column]], format_output_value, character(1))
  }
  for (column in intersect(count_columns, names(data))) {
    data[[column]] <- as.character(data[[column]])
  }

  data
}

# ---- Validate source tables ----
if (!table_exists(write_schema, scores_table)) {
  stop(
    "Missing ",
    write_schema,
    ".",
    scores_table,
    ". Run Code/3.3_compute_2016_cfi_scores.R first."
  )
}

if (!table_exists(write_schema, ids_table)) {
  stop(
    "Missing ",
    write_schema,
    ".",
    ids_table,
    ". Run Code/3.1_prepare_2016_cfi_inputs.R first."
  )
}

include_race <- table_exists(komodo_schema, race_table)
if (!include_race) {
  warning(
    "Missing ",
    komodo_schema,
    ".",
    race_table,
    ". Race/ethnicity summaries will be skipped."
  )
}

table_has_columns(
  write_schema,
  scores_table,
  c("patid", "patient_id", "analysis_year", "frailty_index")
)
table_has_columns(
  write_schema,
  ids_table,
  c(
    "patid", "analysis_year", "age", "patient_gender",
    "mx_insurance_group", "rx_insurance_group"
  )
)
if (include_race) {
  table_has_columns(
    komodo_schema,
    race_table,
    c("patient_id", "patient_race_ethnicity")
  )
}

scores_table_identifier <- qualified_identifier(write_schema, scores_table)
ids_table_identifier <- qualified_identifier(write_schema, ids_table)
race_table_identifier <- qualified_identifier(komodo_schema, race_table)

# ---- Shared subgroup dataset SQL ----
race_cte_sql <- if (include_race) {
  paste0(
    "race_by_patient AS (
     SELECT
       patient_id,
       CASE
         WHEN COUNT(DISTINCT NULLIF(TRIM(patient_race_ethnicity), '')) = 0
           THEN 'UNKNOWN'
         WHEN COUNT(DISTINCT NULLIF(TRIM(patient_race_ethnicity), '')) = 1
           THEN MAX(NULLIF(TRIM(patient_race_ethnicity), ''))
         ELSE 'MULTIPLE'
     END AS patient_race_ethnicity
     FROM ", race_table_identifier, "
     GROUP BY patient_id
   ),"
  )
} else {
  ""
}

race_select_sql <- if (include_race) {
  "COALESCE(
         NULLIF(TRIM(race.patient_race_ethnicity), ''),
         'UNKNOWN'
       )"
} else {
  "'NOT_AVAILABLE'"
}

race_join_sql <- if (include_race) {
  "LEFT JOIN race_by_patient race
       ON score.patient_id = race.patient_id"
} else {
  ""
}

race_subgroup_sql <- if (include_race) {
  "UNION ALL
  SELECT 'Race/ethnicity' AS stratifier, race_ethnicity AS stratum,
         frailty_index, has_intercept_value, frailty_category,
         frailty_category_order
  FROM base_population"
} else {
  ""
}

base_population_sql <- paste0(
  "WITH ", race_cte_sql, "
   base_population AS (
     SELECT
       score.patid,
       score.patient_id,
       score.analysis_year,
       score.frailty_index,
       CASE
         WHEN ABS(score.frailty_index - ", sql_number(model_intercept), ")
              <= ", sql_number(intercept_tolerance), " THEN 1
         ELSE 0
       END AS has_intercept_value,
       CASE
         WHEN ids.age < 50 THEN '40-49'
         WHEN ids.age < 65 THEN '50-64'
         WHEN ids.age < 75 THEN '65-74'
         WHEN ids.age < 85 THEN '75-84'
         ELSE '85+'
       END AS age_group,
       COALESCE(NULLIF(TRIM(ids.patient_gender), ''), 'UNKNOWN') AS sex,
       ", race_select_sql, " AS race_ethnicity,
       COALESCE(NULLIF(TRIM(ids.mx_insurance_group), ''), 'UNKNOWN')
         AS mx_insurance_group,
       COALESCE(NULLIF(TRIM(ids.rx_insurance_group), ''), 'UNKNOWN')
         AS rx_insurance_group,
       CASE
         WHEN score.frailty_index < 0.15 THEN 'Non-frail'
         WHEN score.frailty_index < 0.25 THEN 'Prefrail'
         WHEN score.frailty_index < 0.35 THEN 'Mildly frail'
         WHEN score.frailty_index < 0.45 THEN 'Moderately frail'
         ELSE 'Severely frail'
       END AS frailty_category,
       CASE
         WHEN score.frailty_index < 0.15 THEN 1
         WHEN score.frailty_index < 0.25 THEN 2
         WHEN score.frailty_index < 0.35 THEN 3
         WHEN score.frailty_index < 0.45 THEN 4
         ELSE 5
       END AS frailty_category_order
     FROM ", scores_table_identifier, " score
     INNER JOIN ", ids_table_identifier, " ids
       ON score.patid = ids.patid
     ", race_join_sql, "
     WHERE score.analysis_year = ", analysis_year, "
       AND ids.analysis_year = ", analysis_year, "
   )"
)

subgroup_select_sql <- paste0(
  "
  SELECT 'Overall' AS stratifier, 'Overall' AS stratum, frailty_index,
         has_intercept_value, frailty_category, frailty_category_order
  FROM base_population
  UNION ALL
  SELECT 'Age group' AS stratifier, age_group AS stratum, frailty_index,
         has_intercept_value, frailty_category, frailty_category_order
  FROM base_population
  UNION ALL
  SELECT 'Sex' AS stratifier, sex AS stratum, frailty_index,
         has_intercept_value, frailty_category, frailty_category_order
  FROM base_population
  ",
  race_subgroup_sql,
  "
  UNION ALL
  SELECT 'Mx insurance group' AS stratifier, mx_insurance_group AS stratum,
         frailty_index, has_intercept_value, frailty_category,
         frailty_category_order
  FROM base_population
  UNION ALL
  SELECT 'Rx insurance group' AS stratifier, rx_insurance_group AS stratum,
         frailty_index, has_intercept_value, frailty_category,
         frailty_category_order
  FROM base_population
"
)

# ---- Summary statistics by subgroup ----
message("Generating 2016 CFI subgroup summary statistics.")

summary_stats <- DBI::dbGetQuery(
  con,
  paste0(
    base_population_sql,
    ",
    subgroup_population AS (
      ", subgroup_select_sql, "
    )
    SELECT DISTINCT
      stratifier,
      stratum,
      CAST(COUNT(*) OVER (
        PARTITION BY stratifier, stratum
      ) AS INTEGER) AS n_patient_years,
      CASE
        WHEN SUM(has_intercept_value) OVER (
          PARTITION BY stratifier, stratum
        ) > 0
         AND SUM(has_intercept_value) OVER (
          PARTITION BY stratifier, stratum
        ) < ", min_count, " THEN NULL
        ELSE CAST(SUM(has_intercept_value) OVER (
          PARTITION BY stratifier, stratum
        ) AS INTEGER)
      END AS n_intercept_value,
      CAST(AVG(frailty_index) OVER (
        PARTITION BY stratifier, stratum
      ) AS DOUBLE PRECISION) AS mean_cfi,
      CAST(MIN(frailty_index) OVER (
        PARTITION BY stratifier, stratum
      ) AS DOUBLE PRECISION) AS minimum_cfi,
      CAST(PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY frailty_index)
        OVER (PARTITION BY stratifier, stratum) AS DOUBLE PRECISION) AS q1_cfi,
      CAST(PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY frailty_index)
        OVER (PARTITION BY stratifier, stratum) AS DOUBLE PRECISION)
        AS median_cfi,
      CAST(PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY frailty_index)
        OVER (PARTITION BY stratifier, stratum) AS DOUBLE PRECISION) AS q3_cfi,
      CAST(MAX(frailty_index) OVER (
        PARTITION BY stratifier, stratum
      ) AS DOUBLE PRECISION) AS maximum_cfi
    FROM subgroup_population
    ORDER BY stratifier, stratum"
  )
)

summary_stats <- summary_stats[summary_stats$n_patient_years >= min_count, ]
summary_stats <- clean_summary_values(summary_stats)

write.csv(
  summary_stats,
  summary_output_file,
  row.names = FALSE,
  na = ""
)

# ---- Frailty category counts by subgroup ----
message("Generating 2016 CFI frailty category summaries.")

category_summary <- DBI::dbGetQuery(
  con,
  paste0(
    base_population_sql,
    ",
    subgroup_population AS (
      ", subgroup_select_sql, "
    ),
    subgroup_denominator AS (
      SELECT
        stratifier,
        stratum,
        COUNT(*) AS n_patient_years
      FROM subgroup_population
      GROUP BY stratifier, stratum
    ),
    category_counts AS (
      SELECT
        stratifier,
        stratum,
        frailty_category,
        frailty_category_order,
        COUNT(*) AS category_count
      FROM subgroup_population
      GROUP BY stratifier, stratum, frailty_category, frailty_category_order
    )
    SELECT
      category.stratifier,
      category.stratum,
      denominator.n_patient_years::INTEGER AS n_patient_years,
      category.frailty_category,
      CASE
        WHEN category.category_count < ", min_count, " THEN NULL
        ELSE category.category_count::INTEGER
      END AS category_count,
      CASE
        WHEN category.category_count < ", min_count, " THEN NULL
        ELSE
          100.0 * category.category_count / denominator.n_patient_years
      END::DOUBLE PRECISION AS category_percent,
      CASE
        WHEN category.category_count < ", min_count, " THEN TRUE
        ELSE FALSE
      END AS category_count_suppressed
    FROM category_counts category
    INNER JOIN subgroup_denominator denominator
      ON category.stratifier = denominator.stratifier
     AND category.stratum = denominator.stratum
    WHERE denominator.n_patient_years >= ", min_count, "
    ORDER BY
      category.stratifier,
      category.stratum,
      category.frailty_category_order"
  )
)

category_summary <- clean_summary_values(category_summary)

write.csv(
  category_summary,
  category_output_file,
  row.names = FALSE,
  na = ""
)

message(
  "2016 CFI subgroup summaries complete. Files written:\n",
  " - ",
  normalizePath(summary_output_file, winslash = "/", mustWork = FALSE),
  "\n - ",
  normalizePath(category_output_file, winslash = "/", mustWork = FALSE)
)
