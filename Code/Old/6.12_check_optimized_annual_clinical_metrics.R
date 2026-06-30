source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual clinical metrics QA
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Run aggregate QA for the optimized 6.x clinical-metrics pipeline. It checks
# row counts, duplicate keys, compact extraction counts, core metric invariants,
# and final-table completeness without printing patient-level rows. Aggregate
# QA is written to:
#   - Outputs/6.12_optimized_annual_clinical_metrics_qa.csv

config <- get_optimized_clinical_metrics_config()
con <- connect_komodo()

dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
inpatient_candidate_identifier <- qualified_identifier(
  write_schema,
  config$inpatient_candidate_table
)
non_inpatient_candidate_identifier <- qualified_identifier(
  write_schema,
  config$non_inpatient_candidate_table
)
diagnosis_presence_identifier <- qualified_identifier(
  write_schema,
  config$diagnosis_presence_table
)
procedure_presence_identifier <- qualified_identifier(
  write_schema,
  config$procedure_presence_table
)
hiv_evidence_identifier <- qualified_identifier(write_schema, config$hiv_evidence_table)
cfi_matches_identifier <- qualified_identifier(write_schema, config$cfi_feature_matches_table)
ccw_matches_identifier <- qualified_identifier(write_schema, config$ccw_feature_matches_table)
gagne_matches_identifier <- qualified_identifier(write_schema, config$gagne_feature_matches_table)
cfi_identifier <- qualified_identifier(write_schema, config$cfi_scores_table)
ccw_ind_identifier <- qualified_identifier(write_schema, config$ccw_condition_indicators_table)
ccw_group_identifier <- qualified_identifier(write_schema, config$ccw_group_counts_table)
gagne_identifier <- qualified_identifier(write_schema, config$gagne_scores_table)
hiv_identifier <- qualified_identifier(write_schema, config$hiv_status_table)
final_identifier <- qualified_identifier(write_schema, config$final_table)

candidate_required_tables <- if (config$use_candidate_event_stage) {
  c(config$inpatient_candidate_table, config$non_inpatient_candidate_table)
} else {
  character()
}

required_tables <- c(
  config$ids_table,
  candidate_required_tables,
  config$diagnosis_presence_table,
  config$procedure_presence_table,
  config$hiv_evidence_table,
  config$cfi_feature_matches_table,
  config$ccw_feature_matches_table,
  config$gagne_feature_matches_table,
  config$cfi_scores_table,
  config$ccw_condition_indicators_table,
  config$ccw_group_counts_table,
  config$gagne_scores_table,
  config$hiv_status_table,
  config$final_table
)

missing_tables <- required_tables[
  !vapply(required_tables, function(table) table_exists(con, write_schema, table), logical(1))
]
if (length(missing_tables) > 0L) {
  stop(
    "Missing required optimized clinical metric tables in ",
    write_schema,
    ": ",
    paste(missing_tables, collapse = ", ")
  )
}

get_columns <- function(table) {
  result <- DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", qualified_identifier(write_schema, table), " LIMIT 0")
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

gagne_indicator_columns <- grep(
  "^gagne_group_[0-9][0-9]_",
  get_columns(config$gagne_scores_table),
  value = TRUE
)
ccw_indicator_columns <- setdiff(
  grep("^ccw_", get_columns(config$ccw_condition_indicators_table), value = TRUE),
  c("ccw_condition_count")
)
ccw_group_columns <- grep(
  "^index_",
  get_columns(config$ccw_group_counts_table),
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
       WHERE analysis_year IN (", sql_values(config$analysis_years), ")
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

candidate_count_sql <- if (config$use_candidate_event_stage) {
  paste0(
    "SELECT
       analysis_year,
       'inpatient_candidate_events' AS layer,
       COUNT(*)::BIGINT AS rows
     FROM ", inpatient_candidate_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT analysis_year, 'non_inpatient_candidate_events', COUNT(*)::BIGINT
     FROM ", non_inpatient_candidate_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     "
  )
} else {
  ""
}

qa_tables[["compact_extraction_counts"]] <- query_metric(
  "compact_extraction_counts",
  paste0(
    candidate_count_sql,
    "SELECT
       analysis_year,
       'diagnosis_presence' AS layer,
       COUNT(*)::BIGINT AS rows
     FROM ", diagnosis_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT analysis_year, 'procedure_presence', COUNT(*)::BIGINT
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT analysis_year, 'hiv_evidence', COUNT(*)::BIGINT
     FROM ", hiv_evidence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT analysis_year, 'cfi_feature_matches', COUNT(*)::BIGINT
     FROM ", cfi_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT analysis_year, 'ccw_feature_matches', COUNT(*)::BIGINT
     FROM ", ccw_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT analysis_year, 'gagne_feature_matches', COUNT(*)::BIGINT
     FROM ", gagne_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year, layer"
  )
)

qa_tables[["duplicate_keys"]] <- query_metric(
  "duplicate_keys",
  paste0(
    "SELECT ", sql_string(config$ids_table), " AS table_name,
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
         AS duplicate_rows
     FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$diagnosis_presence_table), ",
       COUNT(*) - COUNT(DISTINCT
         patid || '|' || analysis_year::VARCHAR || '|' || diagnosis_code)
     FROM ", diagnosis_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$procedure_presence_table), ",
       COUNT(*) - COUNT(DISTINCT
         patid || '|' || analysis_year::VARCHAR || '|' || procedure_code)
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$cfi_scores_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", cfi_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$ccw_condition_indicators_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", ccw_ind_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$ccw_group_counts_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", ccw_group_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$gagne_scores_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", gagne_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$hiv_status_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", hiv_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$final_table), ",
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
     FROM ", final_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")"
  )
)

qa_tables[["code_format"]] <- query_metric(
  "code_format",
  paste0(
    "SELECT ", sql_string(config$diagnosis_presence_table), " AS table_name,
       COUNT(*)::BIGINT AS total_rows,
       SUM(CASE WHEN diagnosis_code IS NULL
                  OR diagnosis_code = ''
                  OR diagnosis_code !~ '^[A-Z0-9]+$'
                THEN 1 ELSE 0 END)::BIGINT AS invalid_code_rows
     FROM ", diagnosis_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$procedure_presence_table), ",
       COUNT(*)::BIGINT AS total_rows,
       SUM(CASE WHEN procedure_code IS NULL
                  OR procedure_code = ''
                  OR procedure_code !~ '^[A-Z0-9]{5}$'
                  OR procedure_code !~ '[0-9]$'
                THEN 1 ELSE 0 END)::BIGINT AS invalid_code_rows
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT ", sql_string(config$hiv_evidence_table), ",
       COUNT(*)::BIGINT AS total_rows,
       SUM(CASE WHEN diagnosis_code IS NULL
                  OR diagnosis_code = ''
                  OR diagnosis_code !~ '^[A-Z0-9]+$'
                THEN 1 ELSE 0 END)::BIGINT AS invalid_code_rows
     FROM ", hiv_evidence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")"
  )
)

qa_tables[["metric_invariants"]] <- query_metric(
  "metric_invariants",
  paste0(
    "SELECT
       'cfi_missing_or_bad_intercept_flag' AS check_name,
       COUNT(*)::BIGINT AS n_rows
     FROM ", cfi_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
       AND (
         cfi_score IS NULL
         OR cfi_feature_count IS NULL
         OR cfi_intercept_only_flag IS NULL
         OR (cfi_feature_count = 0 AND cfi_intercept_only_flag <> 1)
         OR (cfi_feature_count > 0 AND cfi_intercept_only_flag <> 0)
       )
     UNION ALL
     SELECT 'ccw_indicator_count_mismatch', COUNT(*)::BIGINT
     FROM ", ccw_ind_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
       AND (", ccw_indicator_sum, ") <> ccw_condition_count
     UNION ALL
     SELECT 'ccw_group_count_mismatch', COUNT(*)::BIGINT
     FROM ", ccw_group_identifier, " grp
     INNER JOIN ", ccw_ind_identifier, " ind
       ON grp.patid = ind.patid
      AND grp.analysis_year = ind.analysis_year
     WHERE grp.analysis_year IN (", sql_values(config$analysis_years), ")
       AND (
         (", ccw_group_sum, ") <> ind.ccw_condition_count
         OR grp.ccw_total_condition_count <> ind.ccw_condition_count
       )
     UNION ALL
     SELECT 'gagne_group_count_mismatch', COUNT(*)::BIGINT
     FROM ", gagne_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
       AND (", gagne_indicator_sum, ") <> gagne_group_count
     UNION ALL
     SELECT 'hiv_confirmation_rule_violation', COUNT(*)::BIGINT
     FROM ", hiv_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
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

if (
  isTRUE(config$run_cfi_2016_parity_check) &&
    2016L %in% config$analysis_years &&
    table_exists(con, write_schema, "cfi_2016_scores")
) {
  cfi_2016_identifier <- qualified_identifier(write_schema, "cfi_2016_scores")
  qa_tables[["cfi_2016_score_parity"]] <- query_metric(
    "cfi_2016_score_parity",
    paste0(
      "SELECT
         COUNT(old_score.patid)::BIGINT AS old_rows,
         COUNT(new_score.patid)::BIGINT AS new_matched_rows,
         SUM(CASE WHEN new_score.patid IS NULL THEN 1 ELSE 0 END)::BIGINT
           AS missing_new_rows,
         SUM(CASE WHEN new_score.patid IS NOT NULL
                    AND ABS(old_score.cfi_score - new_score.cfi_score) > 0.0000001
                  THEN 1 ELSE 0 END)::BIGINT AS changed_score_rows,
         MAX(ABS(old_score.cfi_score - new_score.cfi_score))::DOUBLE PRECISION
           AS max_abs_score_difference
       FROM ", cfi_2016_identifier, " old_score
       LEFT JOIN ", cfi_identifier, " new_score
         ON old_score.patid = new_score.patid
        AND new_score.analysis_year = 2016"
    )
  )
}

qa_output <- combine_qa_tables(qa_tables)
qa_path <- file.path(config$output_dir, "6.12_optimized_annual_clinical_metrics_qa.csv")
write.csv(qa_output, qa_path, row.names = FALSE, na = "")
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
code_format_failures <- qa_tables[["code_format"]][
  qa_tables[["code_format"]]$invalid_code_rows != 0,
]
invariant_failures <- qa_tables[["metric_invariants"]][
  qa_tables[["metric_invariants"]]$n_rows != 0,
]
cfi_parity_failures <- if ("cfi_2016_score_parity" %in% names(qa_tables)) {
  qa_tables[["cfi_2016_score_parity"]][
    qa_tables[["cfi_2016_score_parity"]]$missing_new_rows != 0 |
      qa_tables[["cfi_2016_score_parity"]]$changed_score_rows != 0,
  ]
} else {
  data.frame()
}

if (
  nrow(row_count_failures) > 0L ||
    nrow(duplicate_failures) > 0L ||
    nrow(code_format_failures) > 0L ||
    nrow(invariant_failures) > 0L ||
    nrow(cfi_parity_failures) > 0L
) {
  stop("One or more optimized annual clinical metrics QA checks failed.")
}

message("Optimized annual clinical metrics QA complete. Results written to ", qa_path, ".")
