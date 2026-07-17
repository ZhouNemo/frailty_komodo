source("Code/2_variable construction/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy
# Author: Nemo Zhou
# Date started: 2026-07-03
# Date last updated: 2026-07-16
#
# ---- Purpose ----
# Stage a versioned NDC11-to-ATC4/ATC3 crosswalk for the selected
# polypharmacy run. The input is expected to be an n2c/RxNav output CSV or a
# reviewed crosswalk CSV with NDC and ATC code columns. Unmapped selected-year
# NDC11 values are retained with `mapping_status = 'unmapped'` for coverage QA.
# Multiple ATC codes packed into one input cell are expanded to separate rows.
# ATC4 inputs are converted to ATC3; already-ATC3 inputs are labeled as ATC3
# source mappings rather than being stored as ATC4 values.
# The script writes:
#   - 2_polypharmacy_ndc11_atc_crosswalk

config <- get_annual_polypharmacy_config()
con <- connect_komodo()

unique_ndc_identifier <- qualified_identifier(write_schema, config$unique_ndc_table)
crosswalk_identifier <- qualified_identifier(write_schema, config$crosswalk_table)

if (!file.exists(config$crosswalk_input_path)) {
  stop(
    "Missing crosswalk input CSV: ",
    config$crosswalk_input_path,
    ". Run Code/2_variable construction/5.2_export_polypharmacy_unique_ndc11.R, map the exported NDC11 ",
    "file with n2c, and save the mapping CSV to this path or override ",
    "crosswalk_input_path in frailty.annual_polypharmacy.config."
  )
}
if (!table_exists(con, write_schema, config$unique_ndc_table)) {
  stop("Required unique NDC11 table was not found: ", write_schema, ".", config$unique_ndc_table)
}
table_has_columns(con, write_schema, config$unique_ndc_table, c("ndc11"))

raw_crosswalk <- utils::read.csv(
  config$crosswalk_input_path,
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE,
  na.strings = c("", "NA")
)

ndc_column <- find_first_column(raw_crosswalk, c("ndc11", "ndc", "NDC"), "NDC")
atc_column <- find_first_column(
  raw_crosswalk,
  c(
    "atc4",
    "ATC4 Class",
    "ATC4",
    "ATC4_Class",
    "atc3",
    "ATC3 Class",
    "ATC3",
    "ATC3_Class",
    "atc",
    "ATC",
    "ATC Class",
    "ATC_Class"
  ),
  "ATC"
)

extract_atc_codes <- function(values) {
  values <- toupper(trimws(as.character(values)))
  matches <- gregexpr("\\b[A-Z][0-9]{2}[A-Z][A-Z0-9]?\\b", values, perl = TRUE)
  regmatches(values, matches)
}

ndc_values <- gsub("[^0-9]", "", trimws(raw_crosswalk[[ndc_column]]))
atc_code_list <- extract_atc_codes(raw_crosswalk[[atc_column]])
n_atc_codes <- lengths(atc_code_list)
expanded_atc_codes <- unlist(atc_code_list, use.names = FALSE)
if (is.null(expanded_atc_codes)) {
  expanded_atc_codes <- character()
}
multi_atc_cell_count <- sum(n_atc_codes > 1L)
if (multi_atc_cell_count > 0L) {
  message(
    "Expanded ",
    multi_atc_cell_count,
    " crosswalk input cells containing multiple ATC codes."
  )
}

mapped_crosswalk <- data.frame(
  ndc11 = rep(ndc_values, n_atc_codes),
  atc_code = expanded_atc_codes,
  stringsAsFactors = FALSE
)
mapped_crosswalk <- mapped_crosswalk[
  !is.na(mapped_crosswalk$ndc11) &
    nchar(mapped_crosswalk$ndc11) == 11L &
    !is.na(mapped_crosswalk$atc_code) &
    nchar(mapped_crosswalk$atc_code) >= 4L,
]
mapped_crosswalk$atc4 <- ifelse(
  nchar(mapped_crosswalk$atc_code) >= 5L,
  mapped_crosswalk$atc_code,
  NA_character_
)
mapped_crosswalk$atc3 <- substr(mapped_crosswalk$atc_code, 1L, 4L)
mapped_crosswalk$mapping_level_observed <- ifelse(
  is.na(mapped_crosswalk$atc4),
  "atc3",
  "atc4_to_atc3"
)
mapped_crosswalk$atc_code <- NULL
mapped_crosswalk <- unique(mapped_crosswalk)

selected_ndc <- DBI::dbGetQuery(
  con,
  paste0("SELECT ndc11 FROM ", unique_ndc_identifier, " ORDER BY ndc11")
)

crosswalk <- merge(
  selected_ndc,
  mapped_crosswalk,
  by = "ndc11",
  all.x = TRUE,
  sort = FALSE
)
crosswalk$mapping_source <- config$mapping_source
crosswalk$mapping_level <- ifelse(
  is.na(crosswalk$mapping_level_observed),
  config$mapping_level,
  crosswalk$mapping_level_observed
)
crosswalk$mapping_level_observed <- NULL
crosswalk$mapping_version_date <- config$mapping_version_date
crosswalk$n2c_run_date <- as.character(Sys.Date())
crosswalk$n2c_commit_or_download_date <- config$n2c_commit_or_download_date
crosswalk$cache_file_name <- config$cache_file_name
crosswalk$mapping_status <- ifelse(
  is.na(crosswalk$atc3) | crosswalk$atc3 == "",
  "unmapped",
  "mapped"
)

crosswalk <- crosswalk[
  ,
  c(
    "ndc11",
    "atc4",
    "atc3",
    "mapping_source",
    "mapping_level",
    "mapping_version_date",
    "n2c_run_date",
    "n2c_commit_or_download_date",
    "cache_file_name",
    "mapping_status"
  ),
  drop = FALSE
]

if (!table_exists(con, write_schema, config$crosswalk_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", crosswalk_identifier, " (
         ndc11 VARCHAR(32) NOT NULL,
         atc4 VARCHAR(32),
         atc3 VARCHAR(16),
         mapping_source VARCHAR(128) NOT NULL,
         mapping_level VARCHAR(64) NOT NULL,
         mapping_version_date VARCHAR(64) NOT NULL,
         n2c_run_date VARCHAR(64),
         n2c_commit_or_download_date VARCHAR(128),
         cache_file_name VARCHAR(512),
         mapping_status VARCHAR(32) NOT NULL
       )
       DISTSTYLE ALL
       SORTKEY(mapping_source, mapping_version_date, ndc11);"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$crosswalk_table,
  c("ndc11", "atc4", "atc3", "mapping_source", "mapping_version_date", "mapping_status")
)

DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", crosswalk_identifier, "
     WHERE mapping_source = ", sql_string(config$mapping_source), "
       AND mapping_version_date = ", sql_string(config$mapping_version_date), ";"
  ),
  progressBar = FALSE,
  reportOverallTime = FALSE
)

execute_insert_batches(
  con,
  crosswalk_identifier,
  names(crosswalk),
  crosswalk
)

print_query(
  con,
  "Checking staged polypharmacy NDC11-to-ATC mapping coverage.",
  paste0(
    "SELECT mapping_status,
       COUNT(DISTINCT ndc11)::BIGINT AS unique_ndc11,
       COUNT(*)::BIGINT AS mapping_rows
     FROM ", crosswalk_identifier, "
     WHERE mapping_source = ", sql_string(config$mapping_source), "
       AND mapping_version_date = ", sql_string(config$mapping_version_date), "
     GROUP BY mapping_status
     ORDER BY mapping_status"
  )
)

message(
  config$workflow_label,
  " NDC11-to-ATC crosswalk staging complete: ",
  write_schema,
  ".",
  config$crosswalk_table,
  "."
)

disconnect_komodo(con)
