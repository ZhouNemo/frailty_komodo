library(ohdsilab)
library(DatabaseConnector)
library(DBI)
library(dplyr)
library(keyring)
library(readr)
library(tidyr)

# Project: Frailty_Komoto 2016 CFI descriptive analysis extraction
# Author: Nemo Zhou
# Date started: 2026-06-17
# Date last updated: 2026-06-17
#
# ---- Purpose ----
# Extract aggregate-only 2016 Claims-Based Frailty Index (CFI) descriptive
# outputs from Redshift and save them as CSV files in Outputs. This script does
# the slow database work for Code/4.2_visualize_2016_cfi_descriptive_outputs.Rmd
# so the visualization report can be knitted quickly without reconnecting to
# Redshift.
#
# Outputs include a frailty-group Table 1, CFI summary statistics by stratifier,
# overall histogram bins, box-plot statistics, and analysis metadata. No
# patient-level rows are collected or written locally.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Analysis parameters ----
analysis_year <- 2016L
min_count <- 11L

scores_table <- "cfi_2016_scores"
ids_table <- "cfi_2016_ids"
eligibility_table <- "1_annual_eligible_cohort"
race_table <- "patient_race_ethnicity"

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
if (
  !file.exists(file.path(project_root, "Frailty_Komoto.Rproj")) &&
    file.exists(file.path(project_root, "..", "Frailty_Komoto.Rproj"))
) {
  project_root <- normalizePath(
    file.path(project_root, ".."),
    winslash = "/",
    mustWork = TRUE
  )
}

output_dir <- file.path(project_root, "Outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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
  paste(quote_identifier(schema), quote_identifier(table), sep = ".")
}

table_is_accessible <- function(schema, table) {
  table_identifier <- qualified_identifier(schema, table)

  tryCatch(
    {
      DBI::dbGetQuery(
        con,
        paste0("SELECT * FROM ", table_identifier, " LIMIT 0")
      )
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}

table_columns <- function(schema, table) {
  table_identifier <- qualified_identifier(schema, table)

  names(DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", table_identifier, " LIMIT 0")
  )) |>
    tolower()
}

assert_table <- function(schema, table, note) {
  if (!table_is_accessible(schema, table)) {
    stop("Missing required table: ", schema, ".", table, ". ", note)
  }
}

format_count_percent <- function(n, denominator) {
  n_numeric <- as.numeric(n)
  denominator_numeric <- as.numeric(denominator)

  ifelse(
    is.na(n_numeric) | is.na(denominator_numeric) | denominator_numeric == 0,
    NA_character_,
    paste0(
      format(
        round(n_numeric),
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      ),
      " (",
      sprintf("%.1f", 100 * n_numeric / denominator_numeric),
      "%)"
    )
  )
}

scores_identifier <- qualified_identifier(write_schema, scores_table)
ids_identifier <- qualified_identifier(write_schema, ids_table)
eligibility_identifier <- qualified_identifier(write_schema, eligibility_table)
race_identifier <- qualified_identifier(komodo_schema, race_table)

# ---- Validate source tables ----
assert_table(
  write_schema,
  scores_table,
  "Run Code/3.3_compute_2016_cfi_scores.R first."
)
assert_table(
  write_schema,
  ids_table,
  "Run Code/3.1_prepare_2016_cfi_inputs.R first."
)

ids_columns <- table_columns(write_schema, ids_table)
required_ids_columns <- c(
  "patid", "patient_id", "analysis_year", "age", "patient_gender",
  "mx_insurance_group", "rx_insurance_group",
  "mx_insurance_segment", "rx_insurance_segment"
)

missing_ids_columns <- setdiff(required_ids_columns, ids_columns)
if (length(missing_ids_columns) > 0L) {
  stop(
    "Missing columns in ",
    write_schema,
    ".",
    ids_table,
    ": ",
    paste(missing_ids_columns, collapse = ", ")
  )
}

use_eligibility_race <- table_is_accessible(write_schema, eligibility_table) &&
  "patient_race_ethnicity" %in% table_columns(write_schema, eligibility_table)

use_krd_race <- !use_eligibility_race &&
  table_is_accessible(komodo_schema, race_table)

race_source_note <- if (use_eligibility_race) {
  paste0(write_schema, ".", eligibility_table)
} else if (use_krd_race) {
  paste0(komodo_schema, ".", race_table)
} else {
  "No race source available; race/ethnicity labeled NOT_AVAILABLE"
}

# ---- Shared base-population SQL ----
race_cte_sql <- if (use_krd_race) {
  paste0(
    "race_by_patient AS (
       SELECT
         patient_id,
         CASE
           WHEN COUNT(DISTINCT NULLIF(TRIM(patient_race_ethnicity), '')) = 0
             THEN 'UNKNOWN'
           WHEN COUNT(DISTINCT NULLIF(TRIM(patient_race_ethnicity), '')) = 1
             THEN MAX(NULLIF(TRIM(patient_race_ethnicity), ''))
           ELSE 'MULTIPLE VALUES'
         END AS patient_race_ethnicity
       FROM ", race_identifier, "
       GROUP BY patient_id
     ),"
  )
} else {
  ""
}

race_select_sql <- if (use_eligibility_race) {
  "COALESCE(NULLIF(TRIM(eligible.patient_race_ethnicity), ''), 'UNKNOWN')"
} else if (use_krd_race) {
  "COALESCE(NULLIF(TRIM(race.patient_race_ethnicity), ''), 'UNKNOWN')"
} else {
  "'NOT_AVAILABLE'"
}

race_join_sql <- if (use_eligibility_race) {
  paste0(
    "LEFT JOIN ", eligibility_identifier, " eligible
       ON ids.patient_id = eligible.patient_id
      AND ids.analysis_year = eligible.analysis_year"
  )
} else if (use_krd_race) {
  "LEFT JOIN race_by_patient race
       ON ids.patient_id = race.patient_id"
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
         WHEN score.frailty_index < 0.15 THEN 'Non-frail'
         WHEN score.frailty_index < 0.25 THEN 'Prefrail'
         ELSE 'Frail'
       END AS frailty_group,
       CASE
         WHEN score.frailty_index < 0.15 THEN 1
         WHEN score.frailty_index < 0.25 THEN 2
         ELSE 3
       END AS frailty_group_order,
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
       COALESCE(NULLIF(TRIM(ids.mx_insurance_segment), ''), 'UNKNOWN')
         AS mx_insurance_segment,
       COALESCE(NULLIF(TRIM(ids.rx_insurance_segment), ''), 'UNKNOWN')
         AS rx_insurance_segment
     FROM ", scores_identifier, " score
     INNER JOIN ", ids_identifier, " ids
       ON score.patid = ids.patid
     ", race_join_sql, "
     WHERE score.analysis_year = ", analysis_year, "
       AND ids.analysis_year = ", analysis_year, "
       AND score.frailty_index IS NOT NULL
   )"
)

# ---- Table 1 by three-level frailty group ----
message("Extracting Table 1 by frailty group.")

table_one_sql <- paste0(
  base_population_sql,
  ",
  variable_population AS (
    SELECT 'Overall' AS variable, 'Overall' AS level,
           frailty_group, frailty_group_order
    FROM base_population
    UNION ALL
    SELECT 'Age group' AS variable, age_group AS level,
           frailty_group, frailty_group_order
    FROM base_population
    UNION ALL
    SELECT 'Sex' AS variable, sex AS level,
           frailty_group, frailty_group_order
    FROM base_population
    UNION ALL
    SELECT 'Race/ethnicity' AS variable, race_ethnicity AS level,
           frailty_group, frailty_group_order
    FROM base_population
    UNION ALL
    SELECT 'Mx insurance group' AS variable, mx_insurance_group AS level,
           frailty_group, frailty_group_order
    FROM base_population
    UNION ALL
    SELECT 'Rx insurance group' AS variable, rx_insurance_group AS level,
           frailty_group, frailty_group_order
    FROM base_population
    UNION ALL
    SELECT 'Mx insurance segment' AS variable, mx_insurance_segment AS level,
           frailty_group, frailty_group_order
    FROM base_population
    UNION ALL
    SELECT 'Rx insurance segment' AS variable, rx_insurance_segment AS level,
           frailty_group, frailty_group_order
    FROM base_population
  ),
  frailty_denominators AS (
    SELECT frailty_group, COUNT(*)::BIGINT AS frailty_n
    FROM base_population
    GROUP BY frailty_group
  ),
  table_one_counts AS (
    SELECT
      variable,
      level,
      frailty_group,
      MIN(frailty_group_order) AS frailty_group_order,
      COUNT(*)::BIGINT AS n
    FROM variable_population
    GROUP BY variable, level, frailty_group
  )
  SELECT
    c.variable,
    c.level,
    c.frailty_group,
    c.frailty_group_order,
    c.n,
    d.frailty_n
  FROM table_one_counts c
  INNER JOIN frailty_denominators d
    ON c.frailty_group = d.frailty_group
  WHERE c.n >= ", min_count, "
  ORDER BY c.variable, c.level, c.frailty_group_order"
)

table_one_raw <- DBI::dbGetQuery(con, table_one_sql)

table_one_display <- table_one_raw |>
  mutate(
    frailty_group_header = paste0(
      frailty_group,
      " (N=",
      format(
        round(as.numeric(frailty_n)),
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      ),
      ")"
    ),
    value = paste0(sprintf("%.1f", 100 * as.numeric(n) / as.numeric(frailty_n)), "%")
  ) |>
  select(variable, level, frailty_group_header, value) |>
  tidyr::pivot_wider(
    names_from = frailty_group_header,
    values_from = value
  ) |>
  arrange(variable, level)

readr::write_csv(
  table_one_display,
  file.path(output_dir, "4.1_cfi_2016_table1_by_frailty_group.csv")
)

# ---- CFI summary statistics by subgroup ----
message("Extracting CFI summary statistics by subgroup.")

subgroup_summary_sql <- paste0(
  base_population_sql,
  ",
  subgroup_population AS (
    SELECT 'Overall' AS stratifier, 'Overall' AS stratum, frailty_index
    FROM base_population
    UNION ALL
    SELECT 'Age group' AS stratifier, age_group AS stratum, frailty_index
    FROM base_population
    UNION ALL
    SELECT 'Sex' AS stratifier, sex AS stratum, frailty_index
    FROM base_population
    UNION ALL
    SELECT 'Race/ethnicity' AS stratifier, race_ethnicity AS stratum,
           frailty_index
    FROM base_population
    UNION ALL
    SELECT 'Mx insurance group' AS stratifier, mx_insurance_group AS stratum,
           frailty_index
    FROM base_population
    UNION ALL
    SELECT 'Rx insurance group' AS stratifier, rx_insurance_group AS stratum,
           frailty_index
    FROM base_population
    UNION ALL
    SELECT 'Mx insurance segment' AS stratifier,
           mx_insurance_segment AS stratum, frailty_index
    FROM base_population
    UNION ALL
    SELECT 'Rx insurance segment' AS stratifier,
           rx_insurance_segment AS stratum, frailty_index
    FROM base_population
  )
  SELECT DISTINCT
    stratifier,
    stratum,
    CAST(COUNT(*) OVER (
      PARTITION BY stratifier, stratum
    ) AS BIGINT) AS n_patient,
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

subgroup_summary <- DBI::dbGetQuery(con, subgroup_summary_sql) |>
  mutate(n_patient = as.numeric(n_patient)) |>
  filter(n_patient >= min_count)

readr::write_csv(
  subgroup_summary,
  file.path(output_dir, "4.1_cfi_2016_subgroup_summary.csv")
)

# ---- Overall histogram bins ----
message("Extracting histogram bins.")

histogram_sql <- paste0(
  base_population_sql,
  ",
  histogram_population AS (
    SELECT 'Overall' AS stratifier, 'Overall' AS stratum, frailty_index
    FROM base_population
  )
  SELECT
    stratifier,
    stratum,
    FLOOR(frailty_index / 0.01) * 0.01 AS cfi_bin_start,
    COUNT(*)::BIGINT AS n_patient_years
  FROM histogram_population
  GROUP BY stratifier, stratum, FLOOR(frailty_index / 0.01) * 0.01
  HAVING COUNT(*) >= ", min_count, "
  ORDER BY stratifier, stratum, cfi_bin_start"
)

histogram_data <- DBI::dbGetQuery(con, histogram_sql) |>
  mutate(n_patient_years = as.numeric(n_patient_years))

readr::write_csv(
  histogram_data,
  file.path(output_dir, "4.1_cfi_2016_histogram_bins.csv")
)

# ---- Box-plot statistics by subgroup ----
message("Writing box-plot statistics.")

boxplot_data <- subgroup_summary |>
  transmute(
    stratifier,
    stratum,
    n_patient,
    ymin = minimum_cfi,
    lower = q1_cfi,
    middle = median_cfi,
    upper = q3_cfi,
    ymax = maximum_cfi
  )

readr::write_csv(
  boxplot_data,
  file.path(output_dir, "4.1_cfi_2016_boxplot_statistics.csv")
)

# ---- Metadata for the visualization report ----
metadata <- tibble::tibble(
  field = c(
    "analysis_year",
    "min_count",
    "write_schema",
    "scores_table",
    "ids_table",
    "race_source",
    "histogram_note"
  ),
  value = c(
    as.character(analysis_year),
    as.character(min_count),
    write_schema,
    scores_table,
    ids_table,
    race_source_note,
    "Histogram output includes the overall CFI distribution only."
  )
)

readr::write_csv(
  metadata,
  file.path(output_dir, "4.1_cfi_2016_metadata.csv")
)

message("2016 CFI descriptive aggregate extraction complete.")
