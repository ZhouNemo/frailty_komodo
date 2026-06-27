library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto annual shared clinical metrics QA
# Author: Nemo Zhou
# Date started: 2026-06-27
# Date last updated: 2026-06-27
#
# ---- Purpose ----
# Run aggregate QA checks for the shared annual clinical-metric pipeline. This
# script does not rescan raw KRD claims and does not print patient-level rows. It
# verifies selected-year row counts, duplicate keys, core metric invariants, and
# final-table completeness for:
#   - 2_annual_metric_ids
#   - 2_annual_diagnosis_matches
#   - 2_annual_procedure_matches
#   - annual_cfi_scores
#   - annual_ccw_condition_indicators
#   - annual_ccw_group_counts
#   - annual_gagne_scores
#   - annual_hiv_status
#   - annual_clinical_metrics_shared
#
# Aggregate QA results are written to Outputs/5.8_annual_clinical_metrics_qa.csv.

Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

default_config <- list(
  analysis_years = 2016L,
  ids_table = "2_annual_metric_ids",
  diagnosis_matches_table = "2_annual_diagnosis_matches",
  procedure_matches_table = "2_annual_procedure_matches",
  cfi_scores_table = "annual_cfi_scores",
  ccw_condition_indicators_table = "annual_ccw_condition_indicators",
  ccw_group_counts_table = "annual_ccw_group_counts",
  gagne_scores_table = "annual_gagne_scores",
  hiv_status_table = "annual_hiv_status",
  final_table = "annual_clinical_metrics_shared",
  output_dir = file.path(getwd(), "Outputs")
)

config <- utils::modifyList(
  default_config,
  getOption("frailty.clinical_metrics.config", list())
)

analysis_years <- sort(unique(as.integer(config$analysis_years)))
if (
  length(analysis_years) == 0L ||
    any(is.na(analysis_years)) ||
    any(analysis_years < 2016L | analysis_years > 2025L)
) {
  stop("analysis_years must contain years from 2016 through 2025.")
}

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

qualified_identifier <- function(schema, table) {
  paste(quote_identifier(schema), quote_identifier(table), sep = ".")
}

sql_string <- function(value) {
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

sql_values <- function(values) {
  paste(values, collapse = ", ")
}

table_exists <- function(schema, table) {
  table_identifier <- qualified_identifier(schema, table)
  isTRUE(
    tryCatch(
      {
        DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_identifier, " LIMIT 0"))
        TRUE
      },
      error = function(e) FALSE
    )
  )
}

get_columns <- function(schema, table) {
  table_identifier <- qualified_identifier(schema, table)
  result <- tryCatch(
    DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_identifier, " LIMIT 0")),
    error = function(e) {
      stop(
        "Required table was not accessible: ",
        schema,
        ".",
        table,
        ". ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  tolower(names(result))
}

query_metric <- function(section, sql) {
  result <- DBI::dbGetQuery(con, sql)
  data.frame(section = section, result, stringsAsFactors = FALSE)
}

combine_qa_tables <- function(tables) {
  all_columns <- unique(unlist(lapply(tables, names), use.names = FALSE))
  aligned <- lapply(tables, function(table) {
    missing_columns <- setdiff(all_columns, names(table))
    for (column in missing_columns) {
      table[[column]] <- NA
    }
    table[, all_columns, drop = FALSE]
  })
  do.call(rbind, aligned)
}

required_tables <- c(
  config$ids_table,
  config$diagnosis_matches_table,
  config$procedure_matches_table,
  config$cfi_scores_table,
  config$ccw_condition_indicators_table,
  config$ccw_group_counts_table,
  config$gagne_scores_table,
  config$hiv_status_table,
  config$final_table
)

missing_tables <- required_tables[
  !vapply(required_tables, function(table) table_exists(write_schema, table), logical(1))
]
if (length(missing_tables) > 0L) {
  stop(
    "Missing required shared clinical metric tables in ",
    write_schema,
    ": ",
    paste(missing_tables, collapse = ", ")
  )
}

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
diagnosis_matches_identifier <- qualified_identifier(
  write_schema,
  config$diagnosis_matches_table
)
procedure_matches_identifier <- qualified_identifier(
  write_schema,
  config$procedure_matches_table
)
cfi_identifier <- qualified_identifier(write_schema, config$cfi_scores_table)
ccw_ind_identifier <- qualified_identifier(
  write_schema,
  config$ccw_condition_indicators_table
)
ccw_group_identifier <- qualified_identifier(
  write_schema,
  config$ccw_group_counts_table
)
gagne_identifier <- qualified_identifier(write_schema, config$gagne_scores_table)
hiv_identifier <- qualified_identifier(write_schema, config$hiv_status_table)
final_identifier <- qualified_identifier(write_schema, config$final_table)

gagne_indicator_columns <- grep(
  "^gagne_group_[0-9][0-9]_",
  get_columns(write_schema, config$gagne_scores_table),
  value = TRUE
)
ccw_indicator_columns <- setdiff(
  grep(
    "^ccw_",
    get_columns(write_schema, config$ccw_condition_indicators_table),
    value = TRUE
  ),
  c("ccw_condition_count")
)
ccw_group_columns <- grep(
  "^index_",
  get_columns(write_schema, config$ccw_group_counts_table),
  value = TRUE
)

if (length(gagne_indicator_columns) == 0L) {
  stop("No Gagne indicator columns were found.")
}
if (length(ccw_indicator_columns) == 0L) {
  stop("No CCW indicator columns were found.")
}
if (length(ccw_group_columns) == 0L) {
  stop("No CCW group-count columns were found.")
}

gagne_indicator_sum <- paste(
  paste0("COALESCE(", quote_identifier(gagne_indicator_columns), ", 0)"),
  collapse = " + "
)
ccw_indicator_sum <- paste(
  paste0("COALESCE(", quote_identifier(ccw_indicator_columns), ", 0)"),
  collapse = " + "
)
ccw_group_sum <- paste(
  paste0("COALESCE(", quote_identifier(ccw_group_columns), ", 0)"),
  collapse = " + "
)

qa_tables <- list()

qa_tables[["row_counts"]] <- query_metric(
  "row_counts",
  paste0(
    "WITH denominator AS (
       SELECT analysis_year, COUNT(*)::BIGINT AS n_rows
       FROM ", ids_identifier, "
       WHERE analysis_year IN (", sql_values(analysis_years), ")
       GROUP BY analysis_year
     )
     SELECT
       d.analysis_year,
       d.n_rows AS denominator_rows,
       (SELECT COUNT(*) FROM ", cfi_identifier, " t
        WHERE t.analysis_year = d.analysis_year)::BIGINT AS cfi_rows,
       (SELECT COUNT(*) FROM ", ccw_ind_identifier, " t
        WHERE t.analysis_year = d.analysis_year)::BIGINT AS ccw_indicator_rows,
       (SELECT COUNT(*) FROM ", ccw_group_identifier, " t
        WHERE t.analysis_year = d.analysis_year)::BIGINT AS ccw_group_rows,
       (SELECT COUNT(*) FROM ", gagne_identifier, " t
        WHERE t.analysis_year = d.analysis_year)::BIGINT AS gagne_rows,
       (SELECT COUNT(*) FROM ", hiv_identifier, " t
        WHERE t.analysis_year = d.analysis_year)::BIGINT AS hiv_rows,
       (SELECT COUNT(*) FROM ", final_identifier, " t
        WHERE t.analysis_year = d.analysis_year)::BIGINT AS final_rows
     FROM denominator d
     ORDER BY d.analysis_year"
  )
)

qa_tables[["match_counts"]] <- query_metric(
  "match_counts",
  paste0(
    "SELECT
       analysis_year,
       metric,
       match_type,
       COUNT(*)::BIGINT AS matched_rows
     FROM ", diagnosis_matches_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY analysis_year, metric, match_type
     UNION ALL
     SELECT
       analysis_year,
       metric,
       match_type,
       COUNT(*)::BIGINT AS matched_rows
     FROM ", procedure_matches_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY analysis_year, metric, match_type
     ORDER BY analysis_year, metric, match_type"
  )
)

qa_tables[["duplicate_keys"]] <- query_metric(
  "duplicate_keys",
  paste0(
    "SELECT
       ", sql_string(config$ids_table), " AS table_name,
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
         AS duplicate_rows
     FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$cfi_scores_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", cfi_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$ccw_condition_indicators_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", ccw_ind_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$ccw_group_counts_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", ccw_group_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$gagne_scores_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", gagne_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$hiv_status_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", hiv_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$final_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", final_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")"
  )
)

qa_tables[["metric_invariants"]] <- query_metric(
  "metric_invariants",
  paste0(
    "SELECT
       'cfi_missing_or_bad_intercept_flag' AS check_name,
       COUNT(*)::BIGINT AS n_rows
     FROM ", cfi_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
       AND (
         cfi_score IS NULL
         OR cfi_feature_count IS NULL
         OR cfi_intercept_only_flag IS NULL
         OR (cfi_feature_count = 0 AND cfi_intercept_only_flag <> 1)
         OR (cfi_feature_count > 0 AND cfi_intercept_only_flag <> 0)
       )
     UNION ALL
     SELECT
       'ccw_indicator_count_mismatch',
       COUNT(*)::BIGINT
     FROM ", ccw_ind_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
       AND (", ccw_indicator_sum, ") <> ccw_condition_count
     UNION ALL
     SELECT
       'ccw_group_count_mismatch',
       COUNT(*)::BIGINT
     FROM ", ccw_group_identifier, " grp
     INNER JOIN ", ccw_ind_identifier, " ind
       ON grp.patid = ind.patid
      AND grp.analysis_year = ind.analysis_year
     WHERE grp.analysis_year IN (", sql_values(analysis_years), ")
       AND (
         (", ccw_group_sum, ") <> ind.ccw_condition_count
         OR grp.ccw_total_condition_count <> ind.ccw_condition_count
       )
     UNION ALL
     SELECT
       'gagne_group_count_mismatch',
       COUNT(*)::BIGINT
     FROM ", gagne_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
       AND (", gagne_indicator_sum, ") <> gagne_group_count
     UNION ALL
     SELECT
       'hiv_confirmation_rule_violation',
       COUNT(*)::BIGINT
     FROM ", hiv_identifier, "
     WHERE analysis_year IN (", sql_values(analysis_years), ")
       AND (
         (hiv_status = 1
          AND hiv_inpatient_evidence = 0
          AND hiv_non_inpatient_second_date IS NULL)
         OR
         (hiv_status = 0
          AND (
            hiv_inpatient_evidence = 1
            OR hiv_non_inpatient_second_date IS NOT NULL
          ))
       )"
  )
)

qa_tables[["metric_distributions"]] <- query_metric(
  "metric_distributions",
  paste0(
    "SELECT
       ids.analysis_year,
       SUM(CASE WHEN cfi.cfi_intercept_only_flag = 1 THEN 1 ELSE 0 END)::BIGINT
         AS cfi_intercept_only_rows,
       SUM(CASE WHEN ccw.ccw_condition_count = 0 THEN 1 ELSE 0 END)::BIGINT
         AS zero_ccw_condition_rows,
       SUM(CASE WHEN gagne.gagne_group_count = 0 THEN 1 ELSE 0 END)::BIGINT
         AS zero_gagne_group_rows,
       SUM(CASE WHEN hiv.hiv_status = 1 THEN 1 ELSE 0 END)::BIGINT
         AS hiv_status_rows,
       MIN(cfi.cfi_score) AS min_cfi_score,
       MAX(cfi.cfi_score) AS max_cfi_score,
       MIN(gagne.gagne_score) AS min_gagne_score,
       MAX(gagne.gagne_score) AS max_gagne_score
     FROM ", ids_identifier, " ids
     INNER JOIN ", cfi_identifier, " cfi
       ON ids.patid = cfi.patid
      AND ids.analysis_year = cfi.analysis_year
     INNER JOIN ", ccw_ind_identifier, " ccw
       ON ids.patid = ccw.patid
      AND ids.analysis_year = ccw.analysis_year
     INNER JOIN ", gagne_identifier, " gagne
       ON ids.patid = gagne.patid
      AND ids.analysis_year = gagne.analysis_year
     INNER JOIN ", hiv_identifier, " hiv
       ON ids.patid = hiv.patid
      AND ids.analysis_year = hiv.analysis_year
     WHERE ids.analysis_year IN (", sql_values(analysis_years), ")
     GROUP BY ids.analysis_year
     ORDER BY ids.analysis_year"
  )
)

qa_output <- combine_qa_tables(qa_tables)
write.csv(
  qa_output,
  file.path(config$output_dir, "5.8_annual_clinical_metrics_qa.csv"),
  row.names = FALSE,
  na = ""
)

print(qa_output)

row_counts <- qa_tables[["row_counts"]]
row_count_columns <- setdiff(names(row_counts), c("section", "analysis_year"))
row_count_failures <- row_counts[
  apply(row_counts[, row_count_columns, drop = FALSE], 1L, function(row) {
    any(row != row[["denominator_rows"]])
  }),
]

duplicate_failures <- qa_tables[["duplicate_keys"]][
  qa_tables[["duplicate_keys"]]$duplicate_rows != 0,
]
invariant_failures <- qa_tables[["metric_invariants"]][
  qa_tables[["metric_invariants"]]$n_rows != 0,
]

if (
  nrow(row_count_failures) > 0L ||
    nrow(duplicate_failures) > 0L ||
    nrow(invariant_failures) > 0L
) {
  stop("One or more annual clinical metrics QA checks failed.")
}

message(
  "Annual clinical metrics QA complete. Aggregate results written to ",
  file.path(config$output_dir, "5.8_annual_clinical_metrics_qa.csv"),
  "."
)
