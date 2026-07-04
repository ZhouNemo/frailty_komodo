source("Code/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-03
#
# ---- Purpose ----
# Build the selected-year unique NDC11 table from cleaned polypharmacy pharmacy
# fills and export one NDC11 per line for external n2c/RxNav mapping. The local
# export contains drug codes only, not patient-level rows. The script writes:
#   - 2_polypharmacy_unique_ndc11
#   - Outputs/5.2_polypharmacy_unique_ndc11_<years>.txt

config <- get_annual_polypharmacy_config()
con <- connect_komodo()

if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

fills_identifier <- qualified_identifier(write_schema, config$fills_table)
unique_ndc_identifier <- qualified_identifier(write_schema, config$unique_ndc_table)
export_path <- if (is.null(config$unique_ndc_export_path)) {
  polypharmacy_default_unique_ndc_export_path(config)
} else {
  config$unique_ndc_export_path
}

if (!table_exists(con, write_schema, config$fills_table)) {
  stop("Required pharmacy fill table was not found: ", write_schema, ".", config$fills_table)
}
table_has_columns(
  con,
  write_schema,
  config$fills_table,
  c("patid", "analysis_year", "fill_date", "ndc11")
)

if (!table_exists(con, write_schema, config$unique_ndc_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", unique_ndc_identifier, " (
         ndc11 VARCHAR(32) NOT NULL,
         n_fill_rows BIGINT,
         n_patient_years BIGINT,
         first_fill_date DATE,
         last_fill_date DATE,
         analysis_years VARCHAR(128),
         exported_at TIMESTAMP
       )
       DISTSTYLE ALL
       SORTKEY(ndc11);"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$unique_ndc_table,
  c("ndc11", "n_fill_rows", "n_patient_years", "first_fill_date", "last_fill_date")
)

analysis_years_label <- paste(config$analysis_years, collapse = ",")

DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", unique_ndc_identifier, ";

     INSERT INTO ", unique_ndc_identifier, " (
       ndc11,
       n_fill_rows,
       n_patient_years,
       first_fill_date,
       last_fill_date,
       analysis_years,
       exported_at
     )
     SELECT
       ndc11,
       COUNT(*)::BIGINT AS n_fill_rows,
       COUNT(DISTINCT patid)::BIGINT AS n_patient_years,
       MIN(fill_date) AS first_fill_date,
       MAX(fill_date) AS last_fill_date,
       ", sql_string(analysis_years_label), " AS analysis_years,
       GETDATE() AS exported_at
     FROM ", fills_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
       AND ndc11 IS NOT NULL
       AND LEN(ndc11) = 11
     GROUP BY ndc11;"
  ),
  progressBar = FALSE,
  reportOverallTime = FALSE
)

unique_ndc <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT ndc11
     FROM ", unique_ndc_identifier, "
     ORDER BY ndc11"
  )
)

if (nrow(unique_ndc) == 0L) {
  stop("No unique NDC11 values were found for the selected years.")
}

utils::write.table(
  unique_ndc$ndc11,
  file = export_path,
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE
)

print_query(
  con,
  "Checking selected-year unique NDC11 export summary.",
  paste0(
    "SELECT
       COUNT(*)::BIGINT AS unique_ndc11,
       SUM(n_fill_rows)::BIGINT AS represented_fill_rows,
       SUM(n_patient_years)::BIGINT AS summed_patient_year_counts
     FROM ", unique_ndc_identifier
  )
)

message(
  config$workflow_label,
  " unique NDC11 export complete: ",
  export_path,
  ". Run n2c on this file, save the crosswalk CSV to ",
  config$crosswalk_input_path,
  ", then run Code/5.3_stage_polypharmacy_ndc11_atc_crosswalk.R."
)

disconnect_komodo(con)
