source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual clinical metrics QA
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-08-06
#
# ---- Purpose ----
# Run aggregate QA for the normalized 3.x clinical-metrics pipeline. The checks
# validate source table schemas, selected-year output completeness, duplicate
# keys, compact lookup-filtered match counts, HIV confirmation consistency, and
# final-table completeness. Patient-level rows are not printed.

config <- get_normalized_clinical_metrics_config()
con <- connect_komodo()
# Do NOT register on.exit(disconnect_komodo(con)) here. At the top level of a
# source()d script, on.exit() fires early and closes the connection before the
# script can query it. The connection is disconnected explicitly at the end.

if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
procedure_presence_identifier <- qualified_identifier(write_schema, config$procedure_presence_table)
hiv_evidence_identifier <- qualified_identifier(write_schema, config$hiv_evidence_table)
cfi_matches_identifier <- qualified_identifier(write_schema, config$cfi_feature_matches_table)
ccw_matches_identifier <- qualified_identifier(write_schema, config$ccw_feature_matches_table)
ccw_long_identifier <- qualified_identifier(write_schema, config$ccw_conditions_long_table)
gagne_matches_identifier <- qualified_identifier(write_schema, config$gagne_feature_matches_table)
cfi_scores_identifier <- qualified_identifier(write_schema, config$cfi_scores_table)
ccw_indicators_identifier <- qualified_identifier(write_schema, config$ccw_condition_indicators_table)
ccw_groups_identifier <- qualified_identifier(write_schema, config$ccw_group_counts_table)
gagne_scores_identifier <- qualified_identifier(write_schema, config$gagne_scores_table)
hiv_status_identifier <- qualified_identifier(write_schema, config$hiv_status_table)
final_identifier <- qualified_identifier(write_schema, config$final_table)
dx_identifier <- qualified_identifier(komodo_schema, config$normalized_dx_table)
procedure_identifier <- qualified_identifier(komodo_schema, config$normalized_procedure_table)

table_has_columns(
  con,
  komodo_schema,
  config$normalized_dx_table,
  c("patient_id", "event_date", "visit_id", "source_table", "source_field", "dx_code")
)
table_has_columns(
  con,
  komodo_schema,
  config$normalized_procedure_table,
  c("patient_id", "event_date", "visit_id", "source_table", "source_field", "procedure_code")
)

required_write_tables <- c(
  config$ids_table,
  config$procedure_presence_table,
  config$hiv_evidence_table,
  config$cfi_feature_matches_table,
  config$ccw_feature_matches_table,
  config$ccw_conditions_long_table,
  config$gagne_feature_matches_table,
  config$cfi_scores_table,
  config$ccw_condition_indicators_table,
  config$ccw_group_counts_table,
  config$gagne_scores_table,
  config$hiv_status_table,
  config$final_table
)

missing_write_tables <- required_write_tables[
  !vapply(
    required_write_tables,
    function(table) table_exists(con, write_schema, table),
    logical(1)
  )
]
if (length(missing_write_tables) > 0L) {
  stop(
    "Missing required normalized-flow tables in ",
    write_schema,
    ": ",
    paste(missing_write_tables, collapse = ", ")
  )
}

if (identical(config$ccw_algorithm, "ccw30_normalized")) {
  table_has_columns(
    con, write_schema, config$ccw_feature_matches_table,
    c("patid", "patient_id", "analysis_year", "feature_id", "event_date", "source_table", "claim_setting", "dx_code", "match_type")
  )
  table_has_columns(
    con, write_schema, config$ccw_condition_indicators_table,
    c(
      "patid", "patient_id", "analysis_year", "ccw_condition_count", "ccw_cancer",
      "ccw_acute_myocardial_infarction", "ccw_diabetes", "ccw_heart_failure",
      "ccw_hypertension", "ccw_ischemic_heart_disease", "ccw_alzheimers_disease",
      "ccw_chronic_kidney_disease", "ccw_copd", "ccw_depressive_mood_disorders",
      "ccw_hip_and_pelvic_fracture"
    )
  )
  table_has_columns(
    con, write_schema, config$ccw_group_counts_table,
    c("patid", "patient_id", "analysis_year", "ccw_total_condition_count", "lookup_version")
  )
  ccw_group_columns <- tolower(names(DBI::dbGetQuery(
    con,
    paste0("SELECT * FROM ", ccw_groups_identifier, " LIMIT 0")
  )))
  if (length(grep("^index_", ccw_group_columns, value = TRUE)) > 0L) {
    stop("CCW-30 total-count table must not contain fabricated index_* group columns.")
  }
  table_has_columns(
    con, write_schema, config$final_table,
    c(
      "ccw_total_condition_count", "ccw_cancer", "ccw_acute_myocardial_infarction",
      "ccw_diabetes", "ccw_heart_failure", "ccw_hypertension", "ccw_ischemic_heart_disease",
      "ccw_alzheimers_disease", "ccw_chronic_kidney_disease", "ccw_copd",
      "ccw_depressive_mood_disorders", "ccw_hip_and_pelvic_fracture"
    )
  )
}

qa_results <- list()

qa_results$denominator <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       'denominator' AS qa_check,
       analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_distinct_patient_years
     FROM ", ids_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

qa_results$code_presence <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT 'procedure_code_presence' AS qa_check, analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_patient_years
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT 'hiv_diagnosis_evidence' AS qa_check, analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_patient_years
     FROM ", hiv_evidence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year, qa_check"
  )
)

qa_results$feature_matches <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT 'cfi_feature_matches' AS qa_check, analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_patient_years
     FROM ", cfi_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT 'ccw_condition_matches' AS qa_check, analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_patient_years
     FROM ", ccw_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     UNION ALL
     SELECT 'gagne_group_matches' AS qa_check, analysis_year,
       COUNT(*)::BIGINT AS n_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_patient_years
     FROM ", gagne_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year, qa_check"
  )
)

if (identical(config$ccw_algorithm, "ccw30_normalized")) {
  qa_results$ccw30_hiv_evidence_by_setting <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT 'CCW30' AS metric, source_table, claim_setting, COUNT(*)::BIGINT AS evidence_rows\n",
      "FROM ", ccw_matches_identifier, "\n",
      "WHERE analysis_year IN (", sql_values(config$analysis_years), ")\n",
      "GROUP BY source_table, claim_setting\n",
      "UNION ALL\n",
      "SELECT 'HIV' AS metric, NULL::VARCHAR(64) AS source_table, claim_setting, COUNT(*)::BIGINT AS evidence_rows\n",
      "FROM ", hiv_evidence_identifier, "\n",
      "WHERE analysis_year IN (", sql_values(config$analysis_years), ")\n",
      "GROUP BY claim_setting\n",
      "ORDER BY metric, source_table, claim_setting"
    )
  )
  qa_results$ccw30_qualification <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT qualification_group, COUNT(*)::BIGINT AS qualified_conditions\n",
      "FROM ", ccw_long_identifier, "\n",
      "WHERE analysis_year IN (", sql_values(config$analysis_years), ")\n",
      "GROUP BY qualification_group\nORDER BY qualification_group"
    )
  )
  qa_results$ccw30_total_integrity <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::BIGINT AS rows,\n",
      "       SUM(CASE WHEN ind.ccw_condition_count = totals.ccw_total_condition_count THEN 0 ELSE 1 END)::BIGINT AS unequal_counts\n",
      "FROM ", ccw_indicators_identifier, " ind\n",
      "INNER JOIN ", ccw_groups_identifier, " totals\n",
      "  ON ind.patid = totals.patid AND ind.analysis_year = totals.analysis_year\n",
      "WHERE ind.analysis_year IN (", sql_values(config$analysis_years), ")"
    )
  )
}

qa_results$final_completeness <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       ids.analysis_year,
       COUNT(ids.patid)::BIGINT AS denominator_rows,
       COUNT(cfi.patid)::BIGINT AS cfi_rows,
       COUNT(ccw_ind.patid)::BIGINT AS ccw_indicator_rows,
       COUNT(ccw_group.patid)::BIGINT AS ccw_group_rows,
       COUNT(gagne.patid)::BIGINT AS gagne_rows,
       COUNT(hiv.patid)::BIGINT AS hiv_rows,
       COUNT(final.patid)::BIGINT AS final_rows,
       (SELECT COUNT(*)::BIGINT
        FROM ", cfi_scores_identifier, " source_cfi
         WHERE source_cfi.analysis_year = ids.analysis_year
       ) AS cfi_source_rows
     FROM ", ids_identifier, " ids
     LEFT JOIN ", cfi_scores_identifier, " cfi
       ON ids.patid = cfi.patid AND ids.analysis_year = cfi.analysis_year
     LEFT JOIN ", ccw_indicators_identifier, " ccw_ind
       ON ids.patid = ccw_ind.patid AND ids.analysis_year = ccw_ind.analysis_year
     LEFT JOIN ", ccw_groups_identifier, " ccw_group
       ON ids.patid = ccw_group.patid AND ids.analysis_year = ccw_group.analysis_year
     LEFT JOIN ", gagne_scores_identifier, " gagne
       ON ids.patid = gagne.patid AND ids.analysis_year = gagne.analysis_year
     LEFT JOIN ", hiv_status_identifier, " hiv
       ON ids.patid = hiv.patid AND ids.analysis_year = hiv.analysis_year
     LEFT JOIN ", final_identifier, " final
       ON ids.patid = final.patid AND ids.analysis_year = final.analysis_year
     WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY ids.analysis_year
     ORDER BY ids.analysis_year"
  )
)

qa_results$duplicates <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT 'cfi_scores' AS qa_check,
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
         AS duplicate_rows
     FROM ", cfi_scores_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT 'procedure_code_presence' AS qa_check,
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR || '|' || procedure_code)
         AS duplicate_rows
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     UNION ALL
     SELECT 'final_table' AS qa_check,
       COUNT(*) - COUNT(DISTINCT patid || '|' || analysis_year::VARCHAR)
         AS duplicate_rows
     FROM ", final_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")"
  )
)

qa_results$hiv_consistency <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       analysis_year,
       SUM(CASE
         WHEN hiv_status = 1
          AND hiv_inpatient_evidence = 0
          AND hiv_non_inpatient_distinct_dates < ",
    config$hiv_non_inpatient_min_distinct_dates, "
         THEN 1 ELSE 0 END)::BIGINT AS invalid_positive_hiv_rows,
       SUM(CASE
         WHEN hiv_status = 0
          AND (
            hiv_inpatient_evidence = 1
            OR hiv_non_inpatient_distinct_dates >= ",
    config$hiv_non_inpatient_min_distinct_dates, "
          )
         THEN 1 ELSE 0 END)::BIGINT AS invalid_negative_hiv_rows
     FROM ", hiv_status_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

for (name in names(qa_results)) {
  message("QA: ", name)
  print(qa_results[[name]])
}

final_completeness <- qa_results$final_completeness
if (
  (config$calculate_cfi &&
    any(final_completeness$denominator_rows != final_completeness$cfi_rows)) ||
    (!config$calculate_cfi &&
      any(final_completeness$cfi_rows != final_completeness$cfi_source_rows)) ||
    any(final_completeness$cfi_rows > final_completeness$denominator_rows) ||
    any(final_completeness$denominator_rows != final_completeness$ccw_indicator_rows) ||
    any(final_completeness$denominator_rows != final_completeness$ccw_group_rows) ||
    any(final_completeness$denominator_rows != final_completeness$gagne_rows) ||
    any(final_completeness$denominator_rows != final_completeness$hiv_rows) ||
    any(final_completeness$denominator_rows != final_completeness$final_rows)
) {
  stop("Normalized final-table completeness checks failed.")
}

if (any(qa_results$duplicates$duplicate_rows != 0)) {
  stop("Normalized pipeline duplicate checks failed.")
}

if (identical(config$ccw_algorithm, "ccw30_normalized") &&
    qa_results$ccw30_total_integrity$unequal_counts[[1]] != 0) {
  stop("CCW-30 total condition counts do not equal the 30-base-condition indicators.")
}

if (
  any(qa_results$hiv_consistency$invalid_positive_hiv_rows != 0) ||
    any(qa_results$hiv_consistency$invalid_negative_hiv_rows != 0)
) {
  stop("Normalized HIV consistency checks failed.")
}

qa_frames <- lapply(
  names(qa_results),
  function(name) {
    data <- qa_results[[name]]
    data$qa_section <- name
    data[] <- lapply(data, as.character)
    data
  }
)
all_columns <- unique(unlist(lapply(qa_frames, names)))
qa_frames <- lapply(
  qa_frames,
  function(data) {
    missing_columns <- setdiff(all_columns, names(data))
    for (column in missing_columns) {
      data[[column]] <- NA_character_
    }
    data[, all_columns, drop = FALSE]
  }
)
qa_output <- do.call(rbind, qa_frames)

qa_path <- file.path(config$output_dir, "3.12_normalized_annual_clinical_metrics_qa.csv")
utils::write.csv(qa_output, qa_path, row.names = FALSE)

message(
  config$workflow_label,
  " QA complete. Aggregate QA written to: ",
  qa_path
)

# Release the Redshift connection now that the script has completed.
disconnect_komodo(con)


