source("Code/2_variable construction/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy insurance-subgroup inputs
# Author: Nemo Zhou
# Date started: 2026-07-10
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Build the lightweight aggregate CSV needed by
# Code/2_EDA/5.10_visualize_polypharmacy_by_insurance_groups.Rmd. This script only
# reads the selected-year denominator table and completed annual polypharmacy
# metrics table. It does not rescan pharmacy fills, rebuild exposures, rerun NDC
# mapping QA, or perform the full 5.6 QA workflow.

config <- get_annual_polypharmacy_config()
con <- connect_komodo()
min_count <- 11L

if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
final_identifier <- qualified_identifier(write_schema, config$final_table)

required_write_tables <- c(config$ids_table, config$final_table)
missing_write_tables <- required_write_tables[
  !vapply(
    required_write_tables,
    function(table) table_exists(con, write_schema, table),
    logical(1)
  )
]
if (length(missing_write_tables) > 0L) {
  stop(
    "Missing required polypharmacy tables in ",
    write_schema,
    ": ",
    paste(missing_write_tables, collapse = ", ")
  )
}

table_has_columns(
  con,
  write_schema,
  config$ids_table,
  c(
    "patid",
    "analysis_year",
    "age",
    "patient_gender",
    "patient_race_ethnicity",
    "rx_insurance_group",
    "rx_insurance_segment"
  )
)
table_has_columns(
  con,
  write_schema,
  config$final_table,
  c("patid", "analysis_year", "polypharmacy")
)

message(
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
  "START polypharmacy insurance-subgroup aggregate for years ",
  paste(config$analysis_years, collapse = ", ")
)

age_group_expr <- "CASE
  WHEN ids.age BETWEEN 40 AND 49 THEN '40-49'
  WHEN ids.age BETWEEN 50 AND 64 THEN '50-64'
  WHEN ids.age BETWEEN 65 AND 74 THEN '65-74'
  WHEN ids.age BETWEEN 75 AND 84 THEN '75-84'
  WHEN ids.age >= 85 THEN '85+'
  ELSE 'Unknown'
END"
sex_expr <- "COALESCE(NULLIF(ids.patient_gender, ''), 'Unknown')"
race_expr <- "COALESCE(NULLIF(ids.patient_race_ethnicity, ''), 'Unknown')"
rx_insurance_comparison_expr <- "CASE
  WHEN UPPER(TRIM(ids.rx_insurance_group)) = 'COMMERCIAL' THEN 'Commercial'
  WHEN UPPER(TRIM(ids.rx_insurance_group)) = 'MEDICAID' THEN 'Medicaid'
  WHEN UPPER(TRIM(ids.rx_insurance_group)) = 'MEDICARE'
    AND (
      UPPER(TRIM(ids.rx_insurance_segment)) LIKE '%ADVANTAGE%' OR
      UPPER(TRIM(ids.rx_insurance_segment)) LIKE '%ADV%' OR
      UPPER(TRIM(ids.rx_insurance_segment)) IN ('MA', 'MAPD')
    )
    THEN 'Medicare Advantage'
  WHEN UPPER(TRIM(ids.rx_insurance_group)) = 'MEDICARE'
    AND (
      UPPER(TRIM(ids.rx_insurance_segment)) LIKE '%FFS%' OR
      UPPER(TRIM(ids.rx_insurance_segment)) LIKE '%FEE%' OR
      UPPER(TRIM(ids.rx_insurance_segment)) LIKE '%TRADITIONAL%' OR
      UPPER(TRIM(ids.rx_insurance_segment)) IN ('PDP')
    )
    THEN 'Medicare FFS'
  ELSE NULL
END"
polypharmacy_group_expr <- "CASE
  WHEN final.polypharmacy = 1 THEN 'With polypharmacy'
  ELSE 'Without polypharmacy'
END"

insurance_subgroup_exprs <- list(
  age_group = age_group_expr,
  sex = sex_expr,
  race_ethnicity = race_expr
)

polypharmacy_insurance_subgroup_sql <- function(variable_name, category_expr) {
  paste0(
    "SELECT
       ids.analysis_year,
       ", rx_insurance_comparison_expr, " AS rx_insurance_comparison_group,
       ", sql_string(variable_name), " AS stratification,
       ", category_expr, " AS stratum_value,
       ", polypharmacy_group_expr, " AS polypharmacy_group,
       COUNT(*)::BIGINT AS n_patients
     FROM ", ids_identifier, " ids
     INNER JOIN ", final_identifier, " final
       ON ids.patid = final.patid
      AND ids.analysis_year = final.analysis_year
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
       AND ", rx_insurance_comparison_expr, " IS NOT NULL
     GROUP BY
       ids.analysis_year,
       ", rx_insurance_comparison_expr, ",
       ", category_expr, ",
       ", polypharmacy_group_expr
  )
}

polypharmacy_insurance_subgroup <- DBI::dbGetQuery(
  con,
  paste0(
    "WITH subgroup_counts AS (
       ",
    paste(
      unlist(Map(
        polypharmacy_insurance_subgroup_sql,
        names(insurance_subgroup_exprs),
        insurance_subgroup_exprs
      )),
      collapse = "\nUNION ALL\n"
    ),
    "
     ),
     denominators AS (
       SELECT
         analysis_year,
         rx_insurance_comparison_group,
         stratification,
         stratum_value,
         SUM(n_patients)::BIGINT AS subgroup_denominator
       FROM subgroup_counts
       GROUP BY
         analysis_year,
         rx_insurance_comparison_group,
         stratification,
         stratum_value
     )
     SELECT
       c.analysis_year,
       c.rx_insurance_comparison_group,
       c.stratification,
       c.stratum_value,
       c.polypharmacy_group,
       d.subgroup_denominator,
       CASE WHEN c.n_patients BETWEEN 1 AND ", min_count - 1L, "
            THEN NULL ELSE c.n_patients END AS n_patients,
       CASE WHEN c.n_patients BETWEEN 1 AND ", min_count - 1L, "
            THEN NULL ELSE c.n_patients::DOUBLE PRECISION /
              d.subgroup_denominator END AS percent_within_subgroup,
       CASE WHEN c.n_patients BETWEEN 1 AND ", min_count - 1L, "
            THEN TRUE ELSE FALSE END AS small_cell_suppressed
     FROM subgroup_counts c
     INNER JOIN denominators d
       ON c.analysis_year = d.analysis_year
      AND c.rx_insurance_comparison_group = d.rx_insurance_comparison_group
      AND c.stratification = d.stratification
      AND c.stratum_value = d.stratum_value
     WHERE d.subgroup_denominator >= ", min_count, "
     ORDER BY
       c.analysis_year,
       c.rx_insurance_comparison_group,
       c.stratification,
       c.stratum_value,
       c.polypharmacy_group"
  )
)

output_path <- file.path(
  config$output_dir,
  "5.6_polypharmacy_level_by_rx_insurance_subgroup.csv"
)
csv_data <- polypharmacy_insurance_subgroup
csv_data[] <- lapply(csv_data, as.character)
utils::write.csv(csv_data, output_path, row.names = FALSE, na = "")

message(
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
  "DONE  wrote ",
  output_path
)

disconnect_komodo(con)
