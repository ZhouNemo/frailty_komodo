source("Code/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual clinical metrics
# Author: Nemo Zhou
# Date started: 2026-06-30
# Date last updated: 2026-07-02
#
# ---- Purpose ----
# Match normalized diagnosis events to reviewed CFI, CCW, and Gagne lookup
# rules without materializing all diagnosis code presence. This script first
# stages selected-year eligible patient IDs by `patient_id`, builds a
# lookup-candidate diagnosis stage from `komodo_ext.normalized_dx_events`, then
# matches compact features from that smaller stage. It also matches compact
# patient-year procedure code presence to CFI procedure ranges.
#
# The lookup-candidate diagnosis stage is a persistent, restartable write-schema
# table (2_annual_dx_candidate_stage). The expensive external Spectrum scan is
# built into a TEMP staging table and validated first; only then are the
# persistent year slices replaced, so a failed scan can never erase a previously
# banked stage. The candidate scan runs in configurable retry-wrapped chunks,
# defaulting to one chunk per year, and each chunk is split by literal prefix
# length so Redshift can hash-join each prefix branch instead of evaluating a
# variable-length prefix join against the external diagnosis stream. After the
# candidate stage is ready, compact feature
# matching uses distinct-code-to-feature maps and hash-joins those maps back to
# patient-year rows. A companion manifest (2_annual_dx_candidate_stage_manifest)
# records the scan window, prefix length, prefix set, and lookup version(s) per
# built year; reuse_candidate_stage = TRUE reuses the banked stage only when
# every requested year's manifest matches this run. The manifest does not
# fingerprint every row in 2_annual_metric_ids, so candidate-stage reuse assumes
# the selected-year denominator has not changed since the stage was banked. The
# scan window uses bare-column, half-open date literals (see event_window_sql)
# so the external Parquet scan stays prunable. It writes selected years to:
#   - 2_annual_dx_candidate_stage
#   - 2_annual_dx_candidate_stage_manifest
#   - 2_annual_cfi_feature_matches
#   - 2_annual_ccw_condition_matches
#   - 2_annual_gagne_group_matches

config <- get_normalized_clinical_metrics_config()
con <- connect_komodo()
# Do NOT register on.exit(disconnect_komodo(con)) here. At the top level of a
# source()d script, on.exit() fires early and closes the connection before the
# script can query it. The connection is disconnected explicitly at the end.

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
dx_identifier <- qualified_identifier(komodo_schema, config$normalized_dx_table)
procedure_presence_identifier <- qualified_identifier(
  write_schema,
  config$procedure_presence_table
)
cfi_matches_identifier <- qualified_identifier(
  write_schema,
  config$cfi_feature_matches_table
)
ccw_matches_identifier <- qualified_identifier(
  write_schema,
  config$ccw_feature_matches_table
)
gagne_matches_identifier <- qualified_identifier(
  write_schema,
  config$gagne_feature_matches_table
)

diagnosis_lookup_path <- file.path(
  config$lookup_dir,
  "0.6_unified_diagnosis_rule_lookup.csv"
)
procedure_lookup_path <- file.path(
  config$lookup_dir,
  "0.6_cfi_procedure_lookup.csv"
)
diagnosis_stage_table <- "clinical_metric_normalized_diagnosis_lookup_stage"
procedure_stage_table <- "clinical_metric_normalized_procedure_lookup_stage"
ids_stage_table <- "clinical_metric_normalized_ids_stage"
diagnosis_candidate_prefix_table <- "clinical_metric_normalized_dx_candidate_prefix"
diagnosis_stage_identifier <- quote_identifier(diagnosis_stage_table)
procedure_stage_identifier <- quote_identifier(procedure_stage_table)
ids_stage_identifier <- quote_identifier(ids_stage_table)
diagnosis_candidate_prefix_identifier <- quote_identifier(diagnosis_candidate_prefix_table)
# The lookup-candidate diagnosis stage is a persistent write-schema table so a
# expensive Spectrum scan of the external diagnosis table is chunked, banked,
# and kept out of downstream matching. See the candidate-stage build below.
diagnosis_candidate_identifier <- qualified_identifier(
  write_schema,
  config$candidate_stage_table
)
# Manifest of what each banked candidate-stage year slice was built from, so a
# reuse only happens when the run parameters still match.
candidate_manifest_identifier <- qualified_identifier(
  write_schema,
  config$candidate_stage_manifest_table
)
candidate_build_table <- "clinical_metric_normalized_dx_candidate_build"
candidate_build_raw_table <- "clinical_metric_normalized_dx_candidate_build_raw"
candidate_build_identifier <- quote_identifier(candidate_build_table)
candidate_build_raw_identifier <- quote_identifier(candidate_build_raw_table)
diagnosis_distinct_code_table <- "clinical_metric_normalized_dx_distinct_code"
diagnosis_feature_map_table <- "clinical_metric_normalized_dx_feature_map"
procedure_distinct_code_table <- "clinical_metric_normalized_procedure_distinct_code"
procedure_feature_map_table <- "clinical_metric_normalized_procedure_feature_map"
diagnosis_distinct_code_identifier <- quote_identifier(diagnosis_distinct_code_table)
diagnosis_feature_map_identifier <- quote_identifier(diagnosis_feature_map_table)
procedure_distinct_code_identifier <- quote_identifier(procedure_distinct_code_table)
procedure_feature_map_identifier <- quote_identifier(procedure_feature_map_table)

missing_lookup_files <- c(
  diagnosis_lookup_path,
  procedure_lookup_path
)[!file.exists(c(diagnosis_lookup_path, procedure_lookup_path))]
if (length(missing_lookup_files) > 0L) {
  stop(
    "Missing normalized clinical metric lookup files:\n",
    paste(" -", missing_lookup_files, collapse = "\n")
  )
}

table_has_columns(
  con,
  write_schema,
  config$ids_table,
  c("patid", "patient_id", "analysis_year", "analysis_start_date", "analysis_end_date")
)
table_has_columns(
  con,
  komodo_schema,
  config$normalized_dx_table,
  c("patient_id", "event_date", "dx_code")
)
table_has_columns(
  con,
  write_schema,
  config$procedure_presence_table,
  c("patid", "analysis_year", "procedure_code")
)

diagnosis_lookup <- read_lookup_csv(diagnosis_lookup_path)
require_columns(
  diagnosis_lookup,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "code_system",
    "match_value",
    "range_start",
    "range_end",
    "range_end_inclusive",
    "match_type",
    "final_match_after_flattening"
  ),
  "Unified diagnosis lookup"
)

diagnosis_lookup$metric <- toupper(diagnosis_lookup$metric)
diagnosis_lookup$match_type <- tolower(diagnosis_lookup$match_type)
diagnosis_lookup$lookup_row_id <- seq_len(nrow(diagnosis_lookup))

active_diagnosis_lookup <- diagnosis_lookup[
  diagnosis_lookup$code_system %in% "ICD10CM" &
    diagnosis_lookup$metric %in% c("CFI", "CCW", "GAGNE") &
    !is.na(diagnosis_lookup$final_match_after_flattening) &
    toupper(diagnosis_lookup$final_match_after_flattening) == "TRUE",
  c(
    "lookup_row_id",
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "match_value",
    "range_start",
    "range_end",
    "range_end_inclusive",
    "match_type"
  )
]

if (nrow(active_diagnosis_lookup) == 0L) {
  stop("No active ICD-10-CM diagnosis lookup rows were found for CFI/CCW/Gagne.")
}

candidate_prefix_length <- if (is.null(config$diagnosis_candidate_prefix_length)) {
  3L
} else {
  as.integer(config$diagnosis_candidate_prefix_length)
}
if (
  length(candidate_prefix_length) != 1L ||
    is.na(candidate_prefix_length) ||
    candidate_prefix_length < 1L
) {
  stop("diagnosis_candidate_prefix_length must be a positive integer.")
}

candidate_prefixes_for_range <- function(start_value, end_value) {
  if (is.na(start_value) || is.na(end_value)) {
    return(character())
  }
  start_chars <- strsplit(start_value, "", fixed = TRUE)[[1]]
  end_chars <- strsplit(end_value, "", fixed = TRUE)[[1]]
  max_length <- min(length(start_chars), length(end_chars))
  if (max_length == 0L) {
    return(character())
  }
  same <- start_chars[seq_len(max_length)] == end_chars[seq_len(max_length)]
  if (!any(same)) {
    leading_codes <- c(LETTERS, as.character(0:9))
    start_lead <- start_chars[[1]]
    end_lead <- end_chars[[1]]
    start_index <- match(start_lead, leading_codes)
    end_index <- match(end_lead, leading_codes)
    if (is.na(start_index) || is.na(end_index) || start_index > end_index) {
      stop(
        "Cannot derive candidate prefixes for cross-leading-character range: ",
        start_value,
        "-",
        end_value,
        "."
      )
    }
    return(leading_codes[start_index:end_index])
  }
  mismatch <- which(!same)
  prefix_length <- if (length(mismatch) == 0L) max_length else mismatch[[1]] - 1L
  if (prefix_length < 1L) {
    prefix_length <- 1L
  }
  substr(start_value, 1L, min(candidate_prefix_length, prefix_length))
}

candidate_prefixes_for_lookup <- function(row_index) {
  row <- active_diagnosis_lookup[row_index, , drop = FALSE]
  if (row$match_type %in% c("exact", "prefix")) {
    values <- row$match_value
  } else if (row$match_type == "range") {
    values <- candidate_prefixes_for_range(row$range_start, row$range_end)
  } else {
    values <- character()
  }

  values <- values[!is.na(values) & values != ""]
  if (length(values) == 0L) {
    return(character())
  }
  unique(substr(values, 1L, pmin(candidate_prefix_length, nchar(values))))
}

candidate_prefixes_by_row <- lapply(
  seq_len(nrow(active_diagnosis_lookup)),
  candidate_prefixes_for_lookup
)
candidate_prefix_counts <- lengths(candidate_prefixes_by_row)
active_diagnosis_lookup <- active_diagnosis_lookup[
  rep(seq_len(nrow(active_diagnosis_lookup)), candidate_prefix_counts),
]
active_diagnosis_lookup$candidate_prefix <- unlist(
  candidate_prefixes_by_row,
  use.names = FALSE
)
active_diagnosis_lookup$candidate_prefix_length <- nchar(
  active_diagnosis_lookup$candidate_prefix
)
active_diagnosis_lookup <- active_diagnosis_lookup[
  !is.na(active_diagnosis_lookup$candidate_prefix) &
    active_diagnosis_lookup$candidate_prefix != "",
]

if (nrow(active_diagnosis_lookup) == 0L) {
  stop("No diagnosis lookup rows produced usable candidate prefixes.")
}

candidate_prefix_lookup <- unique(active_diagnosis_lookup[
  ,
  c("candidate_prefix", "candidate_prefix_length")
])

procedure_lookup <- read_lookup_csv(procedure_lookup_path)
require_columns(
  procedure_lookup,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "code_system",
    "range_start",
    "range_end",
    "match_type"
  ),
  "CFI procedure lookup"
)

procedure_lookup$metric <- toupper(procedure_lookup$metric)
procedure_lookup$match_type <- tolower(procedure_lookup$match_type)

active_procedure_lookup <- procedure_lookup[
  procedure_lookup$code_system %in% c("CPT_HCPCS", "CPTHCPCS") &
    procedure_lookup$metric %in% "CFI" &
    procedure_lookup$match_type %in% "range" &
    !is.na(procedure_lookup$range_start) &
    procedure_lookup$range_start != "" &
    !is.na(procedure_lookup$range_end) &
    procedure_lookup$range_end != "",
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "range_start",
    "range_end",
    "match_type"
  )
]

if (nrow(active_procedure_lookup) == 0L) {
  stop("No active CFI CPT/HCPCS procedure lookup rows were found.")
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", diagnosis_stage_identifier, ";
     CREATE TEMP TABLE ", diagnosis_stage_identifier, " (
       lookup_row_id INTEGER NOT NULL,
       lookup_version VARCHAR(128) NOT NULL,
       metric VARCHAR(32) NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       feature_name VARCHAR(256) NOT NULL,
       match_value VARCHAR(64),
       range_start VARCHAR(64),
       range_end VARCHAR(64),
       range_end_inclusive VARCHAR(16),
       match_type VARCHAR(32) NOT NULL,
       candidate_prefix VARCHAR(64) NOT NULL,
       candidate_prefix_length INTEGER NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(metric, match_type, candidate_prefix, lookup_row_id);

     DROP TABLE IF EXISTS ", diagnosis_candidate_prefix_identifier, ";
     CREATE TEMP TABLE ", diagnosis_candidate_prefix_identifier, " (
       candidate_prefix VARCHAR(64) NOT NULL,
       candidate_prefix_length INTEGER NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(candidate_prefix_length, candidate_prefix);

     DROP TABLE IF EXISTS ", procedure_stage_identifier, ";
     CREATE TEMP TABLE ", procedure_stage_identifier, " (
       lookup_version VARCHAR(128) NOT NULL,
       metric VARCHAR(32) NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       feature_name VARCHAR(256) NOT NULL,
       range_start VARCHAR(64) NOT NULL,
       range_end VARCHAR(64) NOT NULL,
       match_type VARCHAR(32) NOT NULL
     )
     DISTSTYLE ALL
     SORTKEY(range_start, range_end);"
  )
)

execute_insert_batches(
  con,
  diagnosis_stage_identifier,
  c(
    "lookup_row_id",
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "match_value",
    "range_start",
    "range_end",
    "range_end_inclusive",
    "match_type",
    "candidate_prefix",
    "candidate_prefix_length"
  ),
  active_diagnosis_lookup,
  numeric_columns = c("lookup_row_id", "candidate_prefix_length")
)
execute_insert_batches(
  con,
  diagnosis_candidate_prefix_identifier,
  c("candidate_prefix", "candidate_prefix_length"),
  candidate_prefix_lookup,
  numeric_columns = "candidate_prefix_length"
)
execute_insert_batches(
  con,
  procedure_stage_identifier,
  c(
    "lookup_version",
    "metric",
    "feature_id",
    "feature_name",
    "range_start",
    "range_end",
    "match_type"
  ),
  active_procedure_lookup
)

feature_table_sql <- function(identifier) {
  paste0(
    "CREATE TABLE ", identifier, " (
       patid VARCHAR(256) NOT NULL,
       analysis_year INTEGER NOT NULL,
       metric VARCHAR(32) NOT NULL,
       feature_id VARCHAR(128) NOT NULL,
       feature_name VARCHAR(256) NOT NULL,
       match_type VARCHAR(32) NOT NULL,
       lookup_version VARCHAR(128) NOT NULL
     )
     DISTKEY(patid)
     SORTKEY(analysis_year, patid, feature_id);"
  )
}

if (!table_exists(con, write_schema, config$cfi_feature_matches_table)) {
  DatabaseConnector::executeSql(con, feature_table_sql(cfi_matches_identifier))
}
if (!table_exists(con, write_schema, config$ccw_feature_matches_table)) {
  DatabaseConnector::executeSql(con, feature_table_sql(ccw_matches_identifier))
}
if (!table_exists(con, write_schema, config$gagne_feature_matches_table)) {
  DatabaseConnector::executeSql(con, feature_table_sql(gagne_matches_identifier))
}

for (table in c(
  config$cfi_feature_matches_table,
  config$ccw_feature_matches_table,
  config$gagne_feature_matches_table
)) {
  table_has_columns(
    con,
    write_schema,
    table,
    c(
      "patid",
      "analysis_year",
      "metric",
      "feature_id",
      "feature_name",
      "match_type",
      "lookup_version"
    )
  )
}

# ---- Persistent, restartable candidate stage with a build manifest ----
# Fingerprint the parameters that determine the banked candidate-stage content
# so a reuse only happens when the current run matches: scan window, candidate
# prefix length, prefix set, and lookup version(s).
event_start_repr <- if (is.null(config$event_start_date)) {
  "correlated"
} else {
  as.character(config$event_start_date)
}
event_end_repr <- if (is.null(config$event_end_date)) {
  "correlated"
} else {
  as.character(config$event_end_date)
}
candidate_prefix_count <- nrow(candidate_prefix_lookup)
lookup_versions_repr <- paste(
  sort(unique(active_diagnosis_lookup$lookup_version)),
  collapse = "|"
)
candidate_stage_signature <- string_fingerprint(c(
  paste0("prefix_length=", candidate_prefix_length),
  paste0("lookup_versions=", lookup_versions_repr),
  paste0(
    "prefixes=",
    paste(
      sort(paste0(
        candidate_prefix_lookup$candidate_prefix,
        ":",
        candidate_prefix_lookup$candidate_prefix_length
      )),
      collapse = ","
    )
  )
))
candidate_prefix_scan_lengths <- sort(unique(candidate_prefix_lookup$candidate_prefix_length))

candidate_prefix_scan_branch <- function(prefix_length, dx_window_sql) {
  paste0(
    "SELECT
         ids_stage.patid,
         ids_stage.analysis_year,
         dx.dx_code
       FROM ", ids_stage_identifier, " ids_stage
       INNER JOIN ", dx_identifier, " dx
         ON dx.patient_id = ids_stage.patient_id
       INNER JOIN ", diagnosis_candidate_prefix_identifier, " p
         ON p.candidate_prefix_length = ", prefix_length, "
        AND p.candidate_prefix = LEFT(dx.dx_code, ", prefix_length, ")
       WHERE ids_stage.analysis_year IN (", sql_values(config$analysis_years), ")
         AND ", dx_window_sql, "
         AND dx.dx_code IS NOT NULL
         AND dx.dx_code <> ''"
  )
}

candidate_prefix_scan_sql <- function(dx_window_sql) {
  paste(
    vapply(
      candidate_prefix_scan_lengths,
      candidate_prefix_scan_branch,
      character(1),
      dx_window_sql = dx_window_sql
    ),
    collapse = "
       UNION ALL
       "
  )
}

# Ensure the persistent candidate stage and its build manifest exist before
# deciding whether to rebuild. The manifest records how each year slice was
# built and is written only after a successful, validated replace.
if (!table_exists(con, write_schema, config$candidate_stage_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", diagnosis_candidate_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         dx_code VARCHAR(64) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, dx_code, patid);"
    )
  )
}
if (!table_exists(con, write_schema, config$candidate_stage_manifest_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", candidate_manifest_identifier, " (
         analysis_year INTEGER NOT NULL,
         event_start_date VARCHAR(32) NOT NULL,
         event_end_date VARCHAR(32) NOT NULL,
         candidate_prefix_length INTEGER NOT NULL,
         candidate_prefix_count INTEGER NOT NULL,
         lookup_versions VARCHAR(512) NOT NULL,
         candidate_stage_signature VARCHAR(64) NOT NULL,
         completed_flag INTEGER NOT NULL,
         built_at TIMESTAMP NOT NULL
       )
       DISTSTYLE ALL
       SORTKEY(analysis_year);"
    )
  )
}

table_has_columns(
  con,
  write_schema,
  config$candidate_stage_table,
  c("patid", "analysis_year", "dx_code")
)

# Reuse only when every requested year has a completed manifest row whose build
# parameters match this run. A stale, smoke, or differently-parameterized stage
# will not match and is rebuilt.
rebuild_candidate_stage <- TRUE
if (isTRUE(config$reuse_candidate_stage)) {
  matching_manifest_years <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT DISTINCT analysis_year
       FROM ", candidate_manifest_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ")
         AND completed_flag = 1
         AND event_start_date = ", sql_string(event_start_repr), "
         AND event_end_date = ", sql_string(event_end_repr), "
         AND candidate_prefix_length = ", candidate_prefix_length, "
         AND candidate_prefix_count = ", candidate_prefix_count, "
         AND lookup_versions = ", sql_string(lookup_versions_repr), "
         AND candidate_stage_signature = ", sql_string(candidate_stage_signature)
    )
  )$analysis_year
  if (length(setdiff(config$analysis_years, matching_manifest_years)) == 0L) {
    rebuild_candidate_stage <- FALSE
    message(
      format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
      "Reusing banked lookup-candidate diagnosis stage (manifest match) for years: ",
      paste(config$analysis_years, collapse = ", "),
      "."
    )
  }
}

if (rebuild_candidate_stage) {
  message(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
    "Building selected-year ID stage for normalized diagnosis join."
  )
  flush.console()
  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", ids_stage_identifier, ";
       CREATE TEMP TABLE ", ids_stage_identifier, "
       DISTKEY(patient_id)
       SORTKEY(patient_id, analysis_year) AS
       SELECT DISTINCT
         patid,
         patient_id,
         analysis_year,
         analysis_start_date,
         analysis_end_date
       FROM ", ids_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ")
         AND patient_id IS NOT NULL;"
    )
  )

  print_query(
    con,
    "Checking selected-year ID stage counts for normalized diagnosis join.",
    paste0(
      "SELECT
         analysis_year,
         COUNT(*)::BIGINT AS rows,
         COUNT(DISTINCT patient_id)::BIGINT AS patient_ids,
         COUNT(DISTINCT patid)::BIGINT AS patids
       FROM ", ids_stage_identifier, "
       GROUP BY analysis_year
       ORDER BY analysis_year"
    )
  )

  # Build the fragile external Spectrum scan into TEMP staging tables first.
  # The persistent stage is only mutated after all chunks succeed and validate,
  # so a failed scan can never erase a previously banked candidate stage. Each
  # configured chunk is split by literal prefix length to avoid a variable-length
  # nested-loop prefix join over the external diagnosis stream.
  message(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
    "Scanning normalized diagnosis events into candidate build stage using ",
    nrow(candidate_prefix_lookup),
    " candidate prefixes."
  )
  flush.console()
  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", candidate_build_raw_identifier, ";
       CREATE TEMP TABLE ", candidate_build_raw_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         dx_code VARCHAR(64) NOT NULL
       )
       DISTKEY(patid)
       SORTKEY(analysis_year, dx_code, patid);"
    )
  )

  diagnosis_scan_chunks <- event_scan_chunks(config)
  for (chunk_row in seq_len(nrow(diagnosis_scan_chunks))) {
    chunk <- diagnosis_scan_chunks[chunk_row, ]
    dx_window_sql <- event_chunk_window_sql(
      config,
      "ids_stage",
      "dx.event_date",
      chunk$chunk_start_date,
      chunk$chunk_end_date
    )
    message(
      format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
      "Scanning lookup-candidate diagnosis chunk ",
      chunk$chunk_id,
      " of ",
      nrow(diagnosis_scan_chunks),
      " (",
      chunk$chunk_start_date,
      " to ",
      chunk$chunk_end_date,
      ")."
    )
    flush.console()
    execute_sql_with_retry(
      con,
      paste0(
        "INSERT INTO ", candidate_build_raw_identifier, " (
           patid,
           analysis_year,
           dx_code
         )
         SELECT DISTINCT
           patid,
           analysis_year,
           dx_code
         FROM (
         ", candidate_prefix_scan_sql(dx_window_sql), "
         ) candidate_rows;"
      ),
      label = paste0("lookup-candidate diagnosis stage scan chunk ", chunk$chunk_id)
    )
  }

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", candidate_build_identifier, ";
       CREATE TEMP TABLE ", candidate_build_identifier, "
       DISTKEY(patid)
       SORTKEY(analysis_year, dx_code, patid) AS
       SELECT DISTINCT
         patid,
         analysis_year,
         dx_code
       FROM ", candidate_build_raw_identifier, ";

       DROP TABLE IF EXISTS ", candidate_build_raw_identifier, ";"
    )
  )

  # Validate the staged scan before touching the persistent table.
  build_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         COUNT(*)::BIGINT AS n_rows,
         SUM(CASE WHEN patid IS NULL OR dx_code IS NULL THEN 1 ELSE 0 END)::BIGINT
           AS n_bad
       FROM ", candidate_build_identifier
    )
  )
  if (build_qa$n_bad[[1]] != 0) {
    stop("Candidate build stage contains NULL patid or dx_code rows.")
  }
  message(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
    "Candidate build stage validated (", build_qa$n_rows[[1]], " rows)."
  )

  # Replace the persistent year slices and refresh the manifest. Manifest rows
  # are invalidated before the data slice is touched, so a mid-batch failure
  # cannot leave a completed manifest pointing at missing or partial data.
  manifest_values <- paste(
    vapply(
      config$analysis_years,
      function(year) {
        paste0(
          "(", year, ", ",
          sql_string(event_start_repr), ", ",
          sql_string(event_end_repr), ", ",
          candidate_prefix_length, ", ",
          candidate_prefix_count, ", ",
          sql_string(lookup_versions_repr), ", ",
          sql_string(candidate_stage_signature), ", ",
          "1, GETDATE())"
        )
      },
      character(1)
    ),
    collapse = ", "
  )
  execute_sql_with_retry(
    con,
    paste0(
      "DELETE FROM ", candidate_manifest_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ");

       DELETE FROM ", diagnosis_candidate_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ");

       INSERT INTO ", diagnosis_candidate_identifier, " (patid, analysis_year, dx_code)
       SELECT patid, analysis_year, dx_code FROM ", candidate_build_identifier, ";

       INSERT INTO ", candidate_manifest_identifier, " (
         analysis_year,
         event_start_date,
         event_end_date,
         candidate_prefix_length,
         candidate_prefix_count,
         lookup_versions,
         candidate_stage_signature,
         completed_flag,
         built_at
       ) VALUES ", manifest_values, ";

       DROP TABLE IF EXISTS ", candidate_build_identifier, ";"
    ),
    label = "candidate stage persistent replace"
  )
}

candidate_stage_counts <- print_query(
  con,
  "Checking selected-year lookup-candidate diagnosis stage counts.",
  paste0(
    "SELECT
       analysis_year,
       COUNT(*)::BIGINT AS rows,
       COUNT(DISTINCT patid)::BIGINT AS patient_years,
       COUNT(DISTINCT dx_code)::BIGINT AS distinct_dx_codes
     FROM ", diagnosis_candidate_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year
     ORDER BY analysis_year"
  )
)

missing_candidate_years <- setdiff(
  config$analysis_years,
  candidate_stage_counts$analysis_year
)
zero_candidate_years <- candidate_stage_counts$analysis_year[
  candidate_stage_counts$rows == 0
]
if (length(missing_candidate_years) > 0L || length(zero_candidate_years) > 0L) {
  stop(
    "Lookup-candidate diagnosis stage has no rows for selected year(s): ",
    paste(sort(unique(c(missing_candidate_years, zero_candidate_years))), collapse = ", "),
    "."
  )
}

DatabaseConnector::executeSql(
  con,
  paste0(
    "DELETE FROM ", cfi_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");
     DELETE FROM ", ccw_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");
     DELETE FROM ", gagne_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");"
  )
)

message(
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
  "Building diagnosis code-to-feature map from distinct candidate codes."
)
flush.console()
DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", diagnosis_distinct_code_identifier, ";
     CREATE TEMP TABLE ", diagnosis_distinct_code_identifier, "
     DISTSTYLE ALL
     SORTKEY(dx_code) AS
     SELECT DISTINCT dx_code
     FROM ", diagnosis_candidate_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");

     DROP TABLE IF EXISTS ", diagnosis_feature_map_identifier, ";
     CREATE TEMP TABLE ", diagnosis_feature_map_identifier, "
     DISTSTYLE ALL
     SORTKEY(dx_code, metric, feature_id) AS
     SELECT DISTINCT
       d.dx_code,
       l.metric,
       l.feature_id,
       l.feature_name,
       l.match_type,
       l.lookup_version
     FROM ", diagnosis_distinct_code_identifier, " d
     INNER JOIN ", diagnosis_stage_identifier, " l
       ON LEFT(d.dx_code, l.candidate_prefix_length) = l.candidate_prefix
      AND (
        (
          l.match_type = 'exact'
          AND d.dx_code = l.match_value
        )
        OR (
          l.match_type = 'prefix'
          AND LEFT(d.dx_code, LENGTH(l.match_value)) = l.match_value
        )
        OR (
          l.match_type = 'range'
          AND d.dx_code >= l.range_start
          AND (
            (
              COALESCE(UPPER(l.range_end_inclusive), 'TRUE') = 'FALSE'
              AND d.dx_code < l.range_end
            )
            OR (
              COALESCE(UPPER(l.range_end_inclusive), 'TRUE') <> 'FALSE'
              AND d.dx_code <= l.range_end
            )
          )
        )
      );"
  )
)

insert_diagnosis_metric_matches <- function(metric, target_identifier) {
  message(
    format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
    "Hash-joining diagnosis feature map for ",
    metric,
    "."
  )
  flush.console()
  DatabaseConnector::executeSql(
    con,
    paste0(
      "INSERT INTO ", target_identifier, " (
         patid,
         analysis_year,
         metric,
         feature_id,
         feature_name,
         match_type,
         lookup_version
       )
       SELECT DISTINCT
         cand.patid,
         cand.analysis_year,
         m.metric,
         m.feature_id,
         m.feature_name,
         m.match_type,
         m.lookup_version
       FROM ", diagnosis_candidate_identifier, " cand
       INNER JOIN ", diagnosis_feature_map_identifier, " m
         ON cand.dx_code = m.dx_code
       WHERE cand.analysis_year IN (", sql_values(config$analysis_years), ")
         AND m.metric = ", sql_string(metric), ";"
    )
  )
}

insert_diagnosis_metric_matches("CFI", cfi_matches_identifier)
insert_diagnosis_metric_matches("CCW", ccw_matches_identifier)
insert_diagnosis_metric_matches("GAGNE", gagne_matches_identifier)

message(
  format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "),
  "Building CFI procedure code-to-feature map from distinct procedure codes."
)
flush.console()
DatabaseConnector::executeSql(
  con,
  paste0(
    "DROP TABLE IF EXISTS ", procedure_distinct_code_identifier, ";
     CREATE TEMP TABLE ", procedure_distinct_code_identifier, "
     DISTSTYLE ALL
     SORTKEY(procedure_code) AS
     SELECT DISTINCT procedure_code
     FROM ", procedure_presence_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ");

     DROP TABLE IF EXISTS ", procedure_feature_map_identifier, ";
     CREATE TEMP TABLE ", procedure_feature_map_identifier, "
     DISTSTYLE ALL
     SORTKEY(procedure_code, metric, feature_id) AS
     SELECT DISTINCT
       p.procedure_code,
       l.metric,
       l.feature_id,
       l.feature_name,
       l.match_type,
       l.lookup_version
     FROM ", procedure_distinct_code_identifier, " p
     INNER JOIN ", procedure_stage_identifier, " l
       ON p.procedure_code >= l.range_start
      AND p.procedure_code <= l.range_end;"
  )
)

message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "Hash-joining CFI procedure feature map.")
flush.console()
DatabaseConnector::executeSql(
  con,
  paste0(
    "INSERT INTO ", cfi_matches_identifier, " (
         patid,
         analysis_year,
         metric,
         feature_id,
         feature_name,
         match_type,
         lookup_version
       )
     SELECT DISTINCT
       p.patid,
       p.analysis_year,
       m.metric,
       m.feature_id,
       m.feature_name,
       m.match_type,
       m.lookup_version
     FROM ", procedure_presence_identifier, " p
     INNER JOIN ", procedure_feature_map_identifier, " m
       ON p.procedure_code = m.procedure_code
     WHERE p.analysis_year IN (", sql_values(config$analysis_years), ");"
  )
)

check_feature_duplicates <- function(table_identifier, label) {
  duplicate_qa <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COALESCE(SUM(row_count - 1), 0)::BIGINT AS duplicate_rows
       FROM (
         SELECT
           patid,
           analysis_year,
           metric,
           feature_id,
           feature_name,
           match_type,
           lookup_version,
           COUNT(*)::BIGINT AS row_count
         FROM ", table_identifier, "
         WHERE analysis_year IN (", sql_values(config$analysis_years), ")
         GROUP BY
           patid,
           analysis_year,
           metric,
           feature_id,
           feature_name,
           match_type,
           lookup_version
         HAVING COUNT(*) > 1
       ) duplicates"
    )
  )
  if (duplicate_qa$duplicate_rows[[1]] != 0) {
    stop(label, " contains duplicate selected-year feature rows.")
  }
}

check_feature_duplicates(cfi_matches_identifier, "CFI feature matches")
check_feature_duplicates(ccw_matches_identifier, "CCW condition matches")
check_feature_duplicates(gagne_matches_identifier, "Gagne group matches")

print_query(
  con,
  "Checking normalized compact feature match counts.",
  paste0(
    "SELECT analysis_year, metric, match_type, COUNT(*)::BIGINT AS matched_rows
     FROM ", cfi_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, metric, match_type
     UNION ALL
     SELECT analysis_year, metric, match_type, COUNT(*)::BIGINT AS matched_rows
     FROM ", ccw_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, metric, match_type
     UNION ALL
     SELECT analysis_year, metric, match_type, COUNT(*)::BIGINT AS matched_rows
     FROM ", gagne_matches_identifier, "
     WHERE analysis_year IN (", sql_values(config$analysis_years), ")
     GROUP BY analysis_year, metric, match_type
     ORDER BY analysis_year, metric, match_type"
  )
)

message(
  config$workflow_label,
  " compact feature matching complete: ",
  paste(
    paste0(write_schema, ".", c(
      config$cfi_feature_matches_table,
      config$ccw_feature_matches_table,
      config$gagne_feature_matches_table
    )),
    collapse = ", "
  ),
  "."
)

# Release the Redshift connection now that the script has completed.
disconnect_komodo(con)


