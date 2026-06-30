source("Code/6.0_optimized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto optimized annual candidate event staging
# Author: Nemo Zhou
# Date started: 2026-06-29
# Date last updated: 2026-06-30
#
# ---- Purpose ----
# Apply conservative lookup-derived candidate prefilters to the staged annual
# inpatient and non-inpatient event tables before any diagnosis or procedure
# arrays are flattened. This script writes selected years to:
#   - 2_annual_inpatient_candidate_event_stage
#   - 2_annual_non_inpatient_candidate_event_stage
#
# Candidate filtering is a performance gate only. It may keep false-positive
# rows, but it must not remove rows that can contain valid CFI, CCW, Gagne, or
# HIV evidence. Final exact, prefix, and range matching occurs after flattening
# in downstream scripts.

config <- get_optimized_clinical_metrics_config()
con <- connect_komodo()

inpatient_identifier <- qualified_identifier(write_schema, config$inpatient_stage_table)
non_inpatient_identifier <- qualified_identifier(
  write_schema,
  config$non_inpatient_stage_table
)
inpatient_candidate_identifier <- qualified_identifier(
  write_schema,
  config$inpatient_candidate_table
)
non_inpatient_candidate_identifier <- qualified_identifier(
  write_schema,
  config$non_inpatient_candidate_table
)

diagnosis_lookup_path <- file.path(
  config$lookup_dir,
  "0.6_unified_diagnosis_rule_lookup.csv"
)
procedure_lookup_path <- file.path(
  config$lookup_dir,
  "0.6_cfi_procedure_lookup.csv"
)
hiv_lookup_path <- file.path(config$lookup_dir, "0.6_hiv_diagnosis_lookup.csv")

missing_lookup_files <- c(
  diagnosis_lookup_path,
  procedure_lookup_path,
  hiv_lookup_path
)[!file.exists(c(diagnosis_lookup_path, procedure_lookup_path, hiv_lookup_path))]
if (length(missing_lookup_files) > 0L) {
  stop(
    "Missing optimized candidate lookup files:\n",
    paste(" -", missing_lookup_files, collapse = "\n")
  )
}

for (table in c(config$inpatient_stage_table, config$non_inpatient_stage_table)) {
  if (!table_exists(con, write_schema, table)) {
    stop("Required staged event table was not found: ", write_schema, ".", table)
  }
}

diagnosis_lookup <- read_lookup_csv(diagnosis_lookup_path)
require_columns(
  diagnosis_lookup,
  c(
    "metric",
    "code_system",
    "match_value",
    "range_start",
    "range_end",
    "match_type",
    "final_match_after_flattening"
  ),
  "Unified diagnosis lookup"
)

diagnosis_lookup$metric <- toupper(diagnosis_lookup$metric)
diagnosis_lookup$match_type <- tolower(diagnosis_lookup$match_type)
active_metric_dx <- diagnosis_lookup[
  diagnosis_lookup$code_system %in% "ICD10CM" &
    diagnosis_lookup$metric %in% c("CFI", "CCW", "GAGNE") &
    !is.na(diagnosis_lookup$final_match_after_flattening) &
    toupper(diagnosis_lookup$final_match_after_flattening) == "TRUE",
  ,
  drop = FALSE
]

make_candidate_prefix <- function(values, prefix_length) {
  values <- values[!is.na(values) & values != ""]
  substr(values, 1L, pmin(prefix_length, nchar(values)))
}

metric_exact_prefix <- make_candidate_prefix(
  active_metric_dx$match_value[
  active_metric_dx$match_type %in% c("exact", "prefix") &
    !is.na(active_metric_dx$match_value) &
    active_metric_dx$match_value != ""
  ],
  config$diagnosis_candidate_prefix_length
)
metric_range_rows <- active_metric_dx[
  active_metric_dx$match_type %in% "range" &
    !is.na(active_metric_dx$range_start) &
    !is.na(active_metric_dx$range_end) &
    active_metric_dx$range_start != "" &
    active_metric_dx$range_end != "",
  ,
  drop = FALSE
]
metric_range_prefix <- character()
metric_range_unsafe <- FALSE
if (nrow(metric_range_rows) > 0L) {
  range_start_short <- make_candidate_prefix(
    metric_range_rows$range_start,
    config$diagnosis_candidate_prefix_length
  )
  range_end_short <- make_candidate_prefix(
    metric_range_rows$range_end,
    config$diagnosis_candidate_prefix_length
  )
  range_start_one <- substr(metric_range_rows$range_start, 1L, 1L)
  range_end_one <- substr(metric_range_rows$range_end, 1L, 1L)
  same_short_prefix <- range_start_short == range_end_short
  same_one_prefix <- range_start_one == range_end_one
  metric_range_prefix <- ifelse(
    same_short_prefix,
    range_start_short,
    ifelse(same_one_prefix, range_start_one, NA_character_)
  )
  metric_range_unsafe <- any(is.na(metric_range_prefix))
}
metric_dx_prefixes <- unique(c(metric_exact_prefix, metric_range_prefix))
metric_dx_prefixes <- metric_dx_prefixes[
  !is.na(metric_dx_prefixes) & metric_dx_prefixes != ""
]

procedure_lookup <- read_lookup_csv(procedure_lookup_path)
require_columns(
  procedure_lookup,
  c("metric", "code_system", "range_start", "range_end", "match_type"),
  "CFI procedure lookup"
)

procedure_lookup$metric <- toupper(procedure_lookup$metric)
procedure_lookup$match_type <- tolower(procedure_lookup$match_type)
active_px <- procedure_lookup[
  procedure_lookup$code_system %in% c("CPT_HCPCS", "CPTHCPCS") &
    procedure_lookup$metric %in% "CFI" &
    procedure_lookup$match_type %in% "range" &
    !is.na(procedure_lookup$range_start) &
    !is.na(procedure_lookup$range_end) &
    procedure_lookup$range_start != "" &
    procedure_lookup$range_end != "",
  ,
  drop = FALSE
]
procedure_prefixes <- make_candidate_prefix(
  active_px$range_start,
  config$procedure_candidate_prefix_length
)
procedure_unsafe <- any(
  is.na(active_px$range_start) |
    is.na(active_px$range_end) |
    make_candidate_prefix(
      active_px$range_start,
      config$procedure_candidate_prefix_length
    ) !=
      make_candidate_prefix(
        active_px$range_end,
        config$procedure_candidate_prefix_length
      )
)
procedure_prefixes <- unique(procedure_prefixes[
  !is.na(procedure_prefixes) & procedure_prefixes != ""
])

hiv_lookup <- read_lookup_csv(hiv_lookup_path)
require_columns(
  hiv_lookup,
  c("code_system", "match_value", "match_type"),
  "HIV diagnosis lookup"
)
hiv_lookup$match_type <- tolower(hiv_lookup$match_type)
hiv_codes <- unique(hiv_lookup$match_value[
  hiv_lookup$code_system %in% "ICD10CM" &
    hiv_lookup$match_type %in% "exact" &
    !is.na(hiv_lookup$match_value) &
    hiv_lookup$match_value != ""
])

if (length(metric_dx_prefixes) == 0L && !metric_range_unsafe) {
  stop("No candidate diagnosis prefixes were available for CFI/CCW/Gagne.")
}
if (length(procedure_prefixes) == 0L && !procedure_unsafe) {
  stop("No candidate procedure prefixes were available for CFI.")
}
if (length(hiv_codes) == 0L) {
  stop("No candidate ICD-10-CM HIV diagnosis codes were available.")
}

or_conditions <- function(conditions) {
  conditions <- conditions[!is.na(conditions) & conditions != ""]
  if (length(conditions) == 0L) {
    return("FALSE")
  }
  paste(conditions, collapse = "\n              OR ")
}

like_prefix_any <- function(field, prefixes) {
  or_conditions(paste0(field, " LIKE ", sql_string(paste0(prefixes, "%"))))
}

like_contains_any <- function(field, values) {
  or_conditions(paste0(field, " LIKE ", sql_string(paste0("%", values, "%"))))
}

equals_any <- function(field, values) {
  or_conditions(paste0(field, " IN (", paste(vapply(values, sql_string, character(1)), collapse = ", "), ")"))
}

as_not_null_boolean <- function(condition) {
  if (identical(condition, "TRUE") || identical(condition, "FALSE")) {
    return(condition)
  }
  paste0("COALESCE(", condition, ", FALSE)")
}

array_head_text_sql <- function(field) {
  pieces <- vapply(
    seq.int(0L, config$array_code_limit - 1L),
    function(array_index) {
      paste0(
        "COALESCE(JSON_EXTRACT_ARRAY_ELEMENT_TEXT(",
        field,
        ", ",
        array_index,
        ", TRUE), '')"
      )
    },
    character(1)
  )
  paste0("(", paste(pieces, collapse = " || '|' || "), ")")
}

metric_dx_inpatient_condition <- if (isTRUE(metric_range_unsafe)) {
  "TRUE"
} else {
  as_not_null_boolean(
    paste0(
      "(",
      or_conditions(c(
        like_prefix_any("admission_diagnosis_code", metric_dx_prefixes),
        like_prefix_any("primary_diagnosis_code", metric_dx_prefixes),
        like_contains_any("secondary_diagnosis_codes_head", metric_dx_prefixes)
      )),
      ")"
    )
  )
}
metric_dx_non_inpatient_condition <- if (isTRUE(metric_range_unsafe)) {
  "TRUE"
} else {
  as_not_null_boolean(
    paste0("(", like_contains_any("diagnosis_codes_head", metric_dx_prefixes), ")")
  )
}
hiv_dx_inpatient_condition <- as_not_null_boolean(
  paste0(
    "(",
    or_conditions(c(
      equals_any("admission_diagnosis_code", hiv_codes),
      equals_any("primary_diagnosis_code", hiv_codes),
      like_contains_any("secondary_diagnosis_codes_head", hiv_codes)
    )),
    ")"
  )
)
hiv_dx_non_inpatient_condition <- as_not_null_boolean(
  paste0(
    "(",
    like_contains_any("diagnosis_codes_head", hiv_codes),
    ")"
  )
)
procedure_inpatient_condition <- if (isTRUE(procedure_unsafe)) {
  "TRUE"
} else {
  as_not_null_boolean(
    paste0("(", like_contains_any("cpt_hcpcs_codes_head", procedure_prefixes), ")")
  )
}
procedure_non_inpatient_condition <- if (isTRUE(procedure_unsafe)) {
  "TRUE"
} else {
  as_not_null_boolean(
    paste0("(", like_prefix_any("procedure_code", procedure_prefixes), ")")
  )
}

if (!table_exists(con, write_schema, config$inpatient_candidate_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", inpatient_candidate_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         event_date DATE NOT NULL,
         admission_diagnosis_code VARCHAR(64),
         primary_diagnosis_code VARCHAR(64),
         secondary_diagnosis_codes VARCHAR(65535),
         cpt_hcpcs_codes VARCHAR(65535),
         has_metric_diagnosis_candidate BOOLEAN NOT NULL,
         has_hiv_diagnosis_candidate BOOLEAN NOT NULL,
         has_procedure_candidate BOOLEAN NOT NULL
       )
       DISTKEY(patient_id)
       SORTKEY(analysis_year, event_date);"
    )
  )
}

if (!table_exists(con, write_schema, config$non_inpatient_candidate_table)) {
  DatabaseConnector::executeSql(
    con,
    paste0(
      "CREATE TABLE ", non_inpatient_candidate_identifier, " (
         patid VARCHAR(256) NOT NULL,
         analysis_year INTEGER NOT NULL,
         patient_id VARCHAR(256) NOT NULL,
         event_date DATE NOT NULL,
         diagnosis_codes VARCHAR(65535),
         procedure_code VARCHAR(64),
         has_metric_diagnosis_candidate BOOLEAN NOT NULL,
         has_hiv_diagnosis_candidate BOOLEAN NOT NULL,
         has_procedure_candidate BOOLEAN NOT NULL
       )
       DISTKEY(patient_id)
       SORTKEY(analysis_year, event_date);"
    )
  )
}

for (analysis_year in config$analysis_years) {
  message("Preparing optimized candidate event stages for ", analysis_year, ".")

  array_validity <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT
         (SELECT COUNT(*) FROM ", inpatient_identifier, "
          WHERE analysis_year = ", analysis_year, "
            AND secondary_diagnosis_codes IS NOT NULL
            AND secondary_diagnosis_codes <> ''
            AND JSON_ARRAY_LENGTH(secondary_diagnosis_codes, TRUE) IS NULL
         )::BIGINT AS invalid_secondary_diagnosis_arrays,
         (SELECT COUNT(*) FROM ", inpatient_identifier, "
          WHERE analysis_year = ", analysis_year, "
            AND cpt_hcpcs_codes IS NOT NULL
            AND cpt_hcpcs_codes <> ''
            AND JSON_ARRAY_LENGTH(cpt_hcpcs_codes, TRUE) IS NULL
         )::BIGINT AS invalid_cpt_hcpcs_arrays,
         (SELECT COUNT(*) FROM ", non_inpatient_identifier, "
          WHERE analysis_year = ", analysis_year, "
            AND diagnosis_codes IS NOT NULL
            AND diagnosis_codes <> ''
            AND JSON_ARRAY_LENGTH(diagnosis_codes, TRUE) IS NULL
         )::BIGINT AS invalid_non_inpatient_diagnosis_arrays"
    )
  )

  if (sum(unlist(array_validity), na.rm = TRUE) > 0) {
    stop("Invalid JSON-style staged arrays were found for ", analysis_year, ".")
  }

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DELETE FROM ", inpatient_candidate_identifier, "
       WHERE analysis_year = ", analysis_year, ";
       DELETE FROM ", non_inpatient_candidate_identifier, "
       WHERE analysis_year = ", analysis_year, ";

       INSERT INTO ", inpatient_candidate_identifier, " (
         patid,
         analysis_year,
         patient_id,
         event_date,
         admission_diagnosis_code,
         primary_diagnosis_code,
         secondary_diagnosis_codes,
         cpt_hcpcs_codes,
         has_metric_diagnosis_candidate,
         has_hiv_diagnosis_candidate,
         has_procedure_candidate
       )
       WITH inpatient_base AS (
         SELECT
           patid,
           analysis_year,
           patient_id,
           event_date,
           admission_diagnosis_code,
           primary_diagnosis_code,
           secondary_diagnosis_codes,
           cpt_hcpcs_codes,
           ", array_head_text_sql("secondary_diagnosis_codes"), "
             AS secondary_diagnosis_codes_head,
           ", array_head_text_sql("cpt_hcpcs_codes"), "
             AS cpt_hcpcs_codes_head
         FROM ", inpatient_identifier, "
         WHERE analysis_year = ", analysis_year, "
       ),
       inpatient_flags AS (
         SELECT
           patid,
           analysis_year,
           patient_id,
           event_date,
           admission_diagnosis_code,
           primary_diagnosis_code,
           secondary_diagnosis_codes,
           cpt_hcpcs_codes,
           ", metric_dx_inpatient_condition, " AS has_metric_diagnosis_candidate,
           ", hiv_dx_inpatient_condition, " AS has_hiv_diagnosis_candidate,
           ", procedure_inpatient_condition, " AS has_procedure_candidate
         FROM inpatient_base
       )
       SELECT
         patid,
         analysis_year,
         patient_id,
         event_date,
         admission_diagnosis_code,
         primary_diagnosis_code,
         secondary_diagnosis_codes,
         cpt_hcpcs_codes,
         has_metric_diagnosis_candidate,
         has_hiv_diagnosis_candidate,
         has_procedure_candidate
       FROM inpatient_flags
       WHERE has_metric_diagnosis_candidate
          OR has_hiv_diagnosis_candidate
          OR has_procedure_candidate;

       INSERT INTO ", non_inpatient_candidate_identifier, " (
         patid,
         analysis_year,
         patient_id,
         event_date,
         diagnosis_codes,
         procedure_code,
         has_metric_diagnosis_candidate,
         has_hiv_diagnosis_candidate,
         has_procedure_candidate
       )
       WITH non_inpatient_base AS (
         SELECT
           patid,
           analysis_year,
           patient_id,
           event_date,
           diagnosis_codes,
           procedure_code,
           ", array_head_text_sql("diagnosis_codes"), " AS diagnosis_codes_head
         FROM ", non_inpatient_identifier, "
         WHERE analysis_year = ", analysis_year, "
       ),
       non_inpatient_flags AS (
         SELECT
           patid,
           analysis_year,
           patient_id,
           event_date,
           diagnosis_codes,
           procedure_code,
           ", metric_dx_non_inpatient_condition, " AS has_metric_diagnosis_candidate,
           ", hiv_dx_non_inpatient_condition, " AS has_hiv_diagnosis_candidate,
           ", procedure_non_inpatient_condition, " AS has_procedure_candidate
         FROM non_inpatient_base
       )
       SELECT
         patid,
         analysis_year,
         patient_id,
         event_date,
         diagnosis_codes,
         procedure_code,
         has_metric_diagnosis_candidate,
         has_hiv_diagnosis_candidate,
         has_procedure_candidate
       FROM non_inpatient_flags
       WHERE has_metric_diagnosis_candidate
          OR has_hiv_diagnosis_candidate
          OR has_procedure_candidate;"
    )
  )

  print_query(
    con,
    paste0("Checking ", analysis_year, " optimized candidate event counts."),
    paste0(
      "SELECT
         'inpatient' AS source_table,
         COUNT(*)::BIGINT AS candidate_rows,
         SUM(CASE WHEN has_metric_diagnosis_candidate THEN 1 ELSE 0 END)::BIGINT
           AS metric_diagnosis_candidate_rows,
         SUM(CASE WHEN has_hiv_diagnosis_candidate THEN 1 ELSE 0 END)::BIGINT
           AS hiv_diagnosis_candidate_rows,
         SUM(CASE WHEN has_procedure_candidate THEN 1 ELSE 0 END)::BIGINT
           AS procedure_candidate_rows
       FROM ", inpatient_candidate_identifier, "
       WHERE analysis_year = ", analysis_year, "
       UNION ALL
       SELECT
         'non_inpatient' AS source_table,
         COUNT(*)::BIGINT AS candidate_rows,
         SUM(CASE WHEN has_metric_diagnosis_candidate THEN 1 ELSE 0 END)::BIGINT,
         SUM(CASE WHEN has_hiv_diagnosis_candidate THEN 1 ELSE 0 END)::BIGINT,
         SUM(CASE WHEN has_procedure_candidate THEN 1 ELSE 0 END)::BIGINT
       FROM ", non_inpatient_candidate_identifier, "
       WHERE analysis_year = ", analysis_year, "
       ORDER BY source_table"
    )
  )
}

message(
  config$workflow_label,
  " candidate event staging complete: ",
  paste(
    paste0(write_schema, ".", c(
      config$inpatient_candidate_table,
      config$non_inpatient_candidate_table
    )),
    collapse = ", "
  ),
  "."
)
