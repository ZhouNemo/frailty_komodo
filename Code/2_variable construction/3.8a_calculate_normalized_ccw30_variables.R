source("Code/2_variable construction/3.0_normalized_clinical_metrics_helpers.R")

# Project: Frailty_Komoto normalized annual clinical metrics
# Author: Nemo Zhou
# Date started: 2026-08-05
# Date last updated: 2026-08-05
#
# ---- Purpose ----
# Build the SAS-derived CCW-30 adaptation for the fixed-2022 Medicare
# comparison pipeline. It exact-matches the checked-in ICD-10-CM lookup to
# normalized_dx_events once for both CCW-30 and HIV. It treats INPATIENT_EVENTS
# as inpatient evidence and NON_INPATIENT_EVENTS as outpatient evidence. It intentionally does not
# approximate the SAS BPH and stroke/TIA claim-level exclusions because the
# normalized source has no reliable claim identifier.

config <- get_normalized_clinical_metrics_config()
if (!identical(config$ccw_algorithm, "ccw30_normalized")) {
  stop("The CCW-30 scorer requires ccw_algorithm = 'ccw30_normalized'.")
}
if (!identical(as.integer(config$analysis_years), 2022L) ||
    !identical(as.character(config$event_start_date), "2022-01-01") ||
    !identical(as.character(config$event_end_date), "2022-12-31")) {
  stop("The SAS-derived CCW-30 branch is defined only for calendar year 2022.")
}

metadata_path <- file.path(config$ccw30_lookup_dir, "0.6_ccw30_condition_metadata.csv")
lookup_path <- file.path(config$ccw30_lookup_dir, "0.6_ccw30_diagnosis_lookup.csv")
hiv_lookup_path <- file.path(config$lookup_dir, "0.6_hiv_diagnosis_lookup.csv")
if (!file.exists(metadata_path) || !file.exists(lookup_path) || !file.exists(hiv_lookup_path)) {
  stop("CCW-30 or HIV lookup CSVs are missing. Run the relevant lookup preparation script first.")
}

metadata <- read_lookup_csv(metadata_path)
lookup <- read_lookup_csv(lookup_path)
hiv_lookup <- read_lookup_csv(hiv_lookup_path)
require_columns(metadata, c("condition_name", "ccw_condition_id", "qualification_group", "lookup_version"), "CCW-30 metadata")
require_columns(lookup, c("lookup_version", "condition_name", "condition_name_long", "ccw_condition_id", "qualification_group", "rule_type", "dx_code", "match_type"), "CCW-30 diagnosis lookup")
require_columns(hiv_lookup, c("lookup_version", "metric", "feature_id", "feature_name", "code_system", "match_value", "match_type"), "HIV diagnosis lookup")
metadata$qualification_group <- as.integer(metadata$qualification_group)
lookup$qualification_group <- as.integer(lookup$qualification_group)
inclusion_lookup <- lookup[lookup$rule_type == "inclusion", , drop = FALSE]
exclusion_lookup <- lookup[lookup$rule_type == "exclusion", , drop = FALSE]
hiv_lookup$metric <- toupper(hiv_lookup$metric)
hiv_lookup$match_type <- tolower(hiv_lookup$match_type)
active_hiv_lookup <- hiv_lookup[
  hiv_lookup$metric == "HIV" &
    hiv_lookup$match_type == "exact" &
    !is.na(hiv_lookup$match_value) &
    hiv_lookup$match_value != "",
  c("lookup_version", "metric", "feature_id", "feature_name", "match_value", "match_type")
]
if (nrow(metadata) != 30L || length(unique(metadata$condition_name)) != 30L) stop("CCW-30 metadata must contain exactly 30 conditions.")
if (any(!metadata$condition_name %in% inclusion_lookup$condition_name)) stop("Each CCW-30 condition must have inclusion codes.")
if (any(inclusion_lookup$match_type != "exact")) stop("CCW-30 inclusion matching must be exact.")
if (nrow(active_hiv_lookup) == 0L || any(active_hiv_lookup$match_type != "exact")) stop("HIV matching in the shared CCW-30 scan must use active exact diagnosis codes.")
if (!identical(sort(unique(exclusion_lookup$condition_name)), c("bph", "stroketia")) || !all(grepl("^omitted_no_reliable_claim_identifier", exclusion_lookup$exclusion_implementation))) stop("The two SAS exclusion lists must be recorded as intentionally omitted.")

con <- connect_komodo()
ids_identifier <- qualified_identifier(write_schema, config$ids_table)
dx_identifier <- qualified_identifier(komodo_schema, config$normalized_dx_table)
matches_identifier <- qualified_identifier(write_schema, config$ccw_feature_matches_table)
long_identifier <- qualified_identifier(write_schema, config$ccw_conditions_long_table)
indicator_identifier <- qualified_identifier(write_schema, config$ccw_condition_indicators_table)
total_identifier <- qualified_identifier(write_schema, config$ccw_group_counts_table)
hiv_evidence_identifier <- qualified_identifier(write_schema, config$hiv_evidence_table)

table_has_columns(con, write_schema, config$ids_table, c("patid", "patient_id", "analysis_year"))
table_has_columns(con, komodo_schema, config$normalized_dx_table, c("patient_id", "event_date", "source_table", "dx_code"))

metadata_stage <- "ccw30_metadata_stage"
shared_lookup_stage <- "ccw30_hiv_exact_lookup_stage"
ids_stage <- "ccw30_ids_stage"
ids_stage_identifier <- quote_identifier(ids_stage)
DatabaseConnector::executeSql(con, paste0("DROP TABLE IF EXISTS ", quote_identifier(metadata_stage), "; CREATE TEMP TABLE ", quote_identifier(metadata_stage), " (condition_name VARCHAR(64), condition_name_long VARCHAR(256), ccw_condition_id VARCHAR(128), qualification_group INTEGER, lookup_version VARCHAR(128));"), progressBar = FALSE, reportOverallTime = FALSE)
DatabaseConnector::executeSql(con, paste0("DROP TABLE IF EXISTS ", quote_identifier(shared_lookup_stage), "; CREATE TEMP TABLE ", quote_identifier(shared_lookup_stage), " (metric VARCHAR(16), lookup_version VARCHAR(128), feature_id VARCHAR(128), feature_name VARCHAR(256), condition_name VARCHAR(64), condition_name_long VARCHAR(256), ccw_condition_id VARCHAR(128), qualification_group INTEGER, dx_code VARCHAR(64), match_type VARCHAR(16)) DISTSTYLE ALL SORTKEY(dx_code);"), progressBar = FALSE, reportOverallTime = FALSE)
DatabaseConnector::executeSql(con, paste0("DROP TABLE IF EXISTS ", ids_stage_identifier, "; CREATE TEMP TABLE ", ids_stage_identifier, " DISTKEY(patient_id) SORTKEY(analysis_year, patient_id) AS SELECT patid, patient_id, analysis_year FROM ", ids_identifier, " WHERE analysis_year = 2022;"), progressBar = FALSE, reportOverallTime = FALSE)
execute_insert_batches(con, quote_identifier(metadata_stage), c("condition_name", "condition_name_long", "ccw_condition_id", "qualification_group", "lookup_version"), metadata[, c("condition_name", "condition_name_long", "ccw_condition_id", "qualification_group", "lookup_version")], numeric_columns = "qualification_group")
ccw_shared_lookup <- inclusion_lookup[, c("lookup_version", "condition_name", "condition_name_long", "ccw_condition_id", "qualification_group", "dx_code", "match_type")]
ccw_shared_lookup <- data.frame(
  metric = "CCW30",
  lookup_version = ccw_shared_lookup$lookup_version,
  feature_id = ccw_shared_lookup$ccw_condition_id,
  feature_name = ccw_shared_lookup$condition_name_long,
  condition_name = ccw_shared_lookup$condition_name,
  condition_name_long = ccw_shared_lookup$condition_name_long,
  ccw_condition_id = ccw_shared_lookup$ccw_condition_id,
  qualification_group = ccw_shared_lookup$qualification_group,
  dx_code = ccw_shared_lookup$dx_code,
  match_type = ccw_shared_lookup$match_type,
  stringsAsFactors = FALSE
)
hiv_shared_lookup <- data.frame(
  metric = active_hiv_lookup$metric,
  lookup_version = active_hiv_lookup$lookup_version,
  feature_id = active_hiv_lookup$feature_id,
  feature_name = active_hiv_lookup$feature_name,
  condition_name = NA_character_,
  condition_name_long = NA_character_,
  ccw_condition_id = NA_character_,
  qualification_group = NA_integer_,
  dx_code = active_hiv_lookup$match_value,
  match_type = active_hiv_lookup$match_type,
  stringsAsFactors = FALSE
)
shared_lookup <- rbind(ccw_shared_lookup, hiv_shared_lookup)
execute_insert_batches(con, quote_identifier(shared_lookup_stage), names(shared_lookup), shared_lookup, numeric_columns = "qualification_group")

base_condition_ids <- metadata$ccw_condition_id
cancer_condition_ids <- metadata$ccw_condition_id[grepl("^cancer_", metadata$ccw_condition_id)]
indicator_columns <- paste0("ccw_", base_condition_ids)
indicator_definition <- paste(paste0(quote_identifier(indicator_columns), " INTEGER"), collapse = ", ")

if (!table_exists(con, write_schema, config$ccw_feature_matches_table)) {
  DatabaseConnector::executeSql(con, paste0("CREATE TABLE ", matches_identifier, " (patid VARCHAR(64), patient_id VARCHAR(128), analysis_year INTEGER, metric VARCHAR(16), feature_id VARCHAR(128), feature_name VARCHAR(256), event_date DATE, source_table VARCHAR(64), claim_setting VARCHAR(16), dx_code VARCHAR(16), match_type VARCHAR(16), lookup_version VARCHAR(128));"), progressBar = FALSE, reportOverallTime = FALSE)
}
if (!table_exists(con, write_schema, config$ccw_conditions_long_table)) {
  DatabaseConnector::executeSql(con, paste0("CREATE TABLE ", long_identifier, " (patid VARCHAR(64), patient_id VARCHAR(128), analysis_year INTEGER, ccw_condition_id VARCHAR(128), condition_name VARCHAR(64), condition_name_long VARCHAR(256), qualification_group INTEGER, lookup_version VARCHAR(128));"), progressBar = FALSE, reportOverallTime = FALSE)
}
if (!table_exists(con, write_schema, config$ccw_condition_indicators_table)) {
  DatabaseConnector::executeSql(con, paste0("CREATE TABLE ", indicator_identifier, " (patid VARCHAR(64), patient_id VARCHAR(128), analysis_year INTEGER, ", indicator_definition, ", ccw_cancer INTEGER, ccw_condition_count INTEGER, lookup_version VARCHAR(128));"), progressBar = FALSE, reportOverallTime = FALSE)
}
if (!table_exists(con, write_schema, config$ccw_group_counts_table)) {
  DatabaseConnector::executeSql(con, paste0("CREATE TABLE ", total_identifier, " (patid VARCHAR(64), patient_id VARCHAR(128), analysis_year INTEGER, ccw_total_condition_count INTEGER, lookup_version VARCHAR(128));"), progressBar = FALSE, reportOverallTime = FALSE)
}
if (!table_exists(con, write_schema, config$hiv_evidence_table)) {
  DatabaseConnector::executeSql(con, paste0("CREATE TABLE ", hiv_evidence_identifier, " (patid VARCHAR(256) NOT NULL, analysis_year INTEGER NOT NULL, diagnosis_date DATE NOT NULL, claim_setting VARCHAR(40) NOT NULL, dx_code VARCHAR(64) NOT NULL, metric VARCHAR(32) NOT NULL, feature_id VARCHAR(128) NOT NULL, feature_name VARCHAR(256) NOT NULL, match_type VARCHAR(32) NOT NULL, lookup_version VARCHAR(128) NOT NULL) DISTKEY(patid) SORTKEY(analysis_year, patid, diagnosis_date);"), progressBar = FALSE, reportOverallTime = FALSE)
}

message("CCW-30/HIV: matching normalized diagnosis evidence in one scan.")
shared_evidence_table <- quote_identifier("ccw30_hiv_matched_evidence")
evidence_table <- quote_identifier("ccw30_matched_evidence")
qualified_table <- quote_identifier("ccw30_qualified_conditions")
DatabaseConnector::executeSql(con, paste0(
  "DROP TABLE IF EXISTS ", shared_evidence_table, "; CREATE TEMP TABLE ", shared_evidence_table, " AS\n",
  "SELECT DISTINCT ids.patid, ids.patient_id, ids.analysis_year, l.metric, l.lookup_version, l.feature_id, dx.event_date::DATE AS event_date, dx.source_table, CASE dx.source_table WHEN 'INPATIENT_EVENTS' THEN 'inpatient' WHEN 'NON_INPATIENT_EVENTS' THEN 'outpatient' END AS claim_setting, dx.dx_code, l.match_type\n",
  "FROM ", ids_stage_identifier, " ids\nJOIN ", dx_identifier, " dx ON dx.patient_id = ids.patient_id\nJOIN ", quote_identifier(shared_lookup_stage), " l ON l.dx_code = dx.dx_code\n",
  "WHERE ids.analysis_year = 2022 AND ", event_literal_window_sql("2022-01-01", "2022-12-31", "dx.event_date"), "\n",
  "  AND dx.source_table IN ('INPATIENT_EVENTS', 'NON_INPATIENT_EVENTS');\n",
  "DROP TABLE IF EXISTS ", evidence_table, "; CREATE TEMP TABLE ", evidence_table, " AS\n",
  "SELECT e.patid, e.patient_id, e.analysis_year, e.metric, e.feature_id, m.condition_name_long AS feature_name, e.event_date, e.source_table, e.claim_setting, e.dx_code, e.match_type, e.lookup_version, m.condition_name, m.condition_name_long, m.ccw_condition_id, m.qualification_group\n",
  "FROM ", shared_evidence_table, " e\nJOIN ", quote_identifier(metadata_stage), " m ON m.ccw_condition_id = e.feature_id\nWHERE e.metric = 'CCW30';\n",
  "DROP TABLE IF EXISTS ", qualified_table, "; CREATE TEMP TABLE ", qualified_table, " AS\n",
  "SELECT patid, patient_id, analysis_year, condition_name, condition_name_long, ccw_condition_id, qualification_group, lookup_version\n",
  "FROM ", evidence_table, "\nGROUP BY patid, patient_id, analysis_year, condition_name, condition_name_long, ccw_condition_id, qualification_group, lookup_version\nHAVING (qualification_group = 1 AND MAX(CASE WHEN claim_setting = 'inpatient' THEN 1 ELSE 0 END) = 1)\n",
  "    OR (qualification_group IN (2, 3) AND (MAX(CASE WHEN claim_setting = 'inpatient' THEN 1 ELSE 0 END) = 1 OR COUNT(DISTINCT CASE WHEN claim_setting = 'outpatient' THEN event_date END) >= 2))\n",
  "    OR (qualification_group = 4 AND MAX(CASE WHEN claim_setting = 'outpatient' THEN 1 ELSE 0 END) = 1)\n",
  "    OR (qualification_group IN (5, 6) AND COUNT(*) > 0);"
), progressBar = FALSE, reportOverallTime = FALSE)

DatabaseConnector::executeSql(con, paste0("DELETE FROM ", matches_identifier, " WHERE analysis_year = 2022; DELETE FROM ", long_identifier, " WHERE analysis_year = 2022; DELETE FROM ", indicator_identifier, " WHERE analysis_year = 2022; DELETE FROM ", total_identifier, " WHERE analysis_year = 2022; DELETE FROM ", hiv_evidence_identifier, " WHERE analysis_year = 2022;"), progressBar = FALSE, reportOverallTime = FALSE)
DatabaseConnector::executeSql(con, paste0("INSERT INTO ", matches_identifier, " SELECT patid, patient_id, analysis_year, metric, feature_id, feature_name, event_date, source_table, claim_setting, dx_code, match_type, lookup_version FROM ", evidence_table, "; INSERT INTO ", long_identifier, " SELECT patid, patient_id, analysis_year, ccw_condition_id, condition_name, condition_name_long, qualification_group, lookup_version FROM ", qualified_table, "; INSERT INTO ", hiv_evidence_identifier, " SELECT e.patid, e.analysis_year, e.event_date AS diagnosis_date, CASE WHEN e.claim_setting = 'inpatient' THEN 'inpatient' ELSE 'non_inpatient' END AS claim_setting, e.dx_code, e.metric, e.feature_id, l.feature_name, e.match_type, e.lookup_version FROM ", shared_evidence_table, " e JOIN ", quote_identifier(shared_lookup_stage), " l ON l.metric = e.metric AND l.feature_id = e.feature_id AND l.dx_code = e.dx_code AND l.lookup_version = e.lookup_version WHERE e.metric = 'HIV';"), progressBar = FALSE, reportOverallTime = FALSE)

flag_sql <- vapply(base_condition_ids, function(id) paste0("MAX(CASE WHEN q.ccw_condition_id = ", sql_string(id), " THEN 1 ELSE 0 END) AS ", quote_identifier(paste0("ccw_", id))), character(1))
count_sql <- "COUNT(DISTINCT q.ccw_condition_id) AS ccw_condition_count"
cancer_sql <- paste0("MAX(CASE WHEN q.ccw_condition_id IN (", paste(vapply(cancer_condition_ids, sql_string, character(1)), collapse = ", "), ") THEN 1 ELSE 0 END) AS ccw_cancer")
indicator_select <- paste(c("ids.patid", "ids.patient_id", "ids.analysis_year", flag_sql, cancer_sql, count_sql, "MAX(meta.lookup_version) AS lookup_version"), collapse = ", ")
DatabaseConnector::executeSql(con, paste0("INSERT INTO ", indicator_identifier, " SELECT ", indicator_select, " FROM ", ids_identifier, " ids CROSS JOIN ", quote_identifier(metadata_stage), " meta LEFT JOIN ", qualified_table, " q ON q.patid = ids.patid AND q.analysis_year = ids.analysis_year AND q.ccw_condition_id = meta.ccw_condition_id WHERE ids.analysis_year = 2022 GROUP BY ids.patid, ids.patient_id, ids.analysis_year; INSERT INTO ", total_identifier, " SELECT patid, patient_id, analysis_year, ccw_condition_count, lookup_version FROM ", indicator_identifier, " WHERE analysis_year = 2022;"), progressBar = FALSE, reportOverallTime = FALSE)

table_has_columns(con, write_schema, config$ccw_feature_matches_table, c("patid", "patient_id", "analysis_year", "event_date", "source_table", "claim_setting", "dx_code", "match_type"))
table_has_columns(con, write_schema, config$ccw_condition_indicators_table, c("patid", "patient_id", "analysis_year", indicator_columns, "ccw_cancer", "ccw_condition_count", "lookup_version"))
table_has_columns(con, write_schema, config$ccw_group_counts_table, c("patid", "patient_id", "analysis_year", "ccw_total_condition_count", "lookup_version"))
table_has_columns(con, write_schema, config$hiv_evidence_table, c("patid", "analysis_year", "diagnosis_date", "claim_setting", "dx_code", "metric", "feature_id", "feature_name", "match_type", "lookup_version"))
print_query(con, "Shared CCW-30/HIV evidence by metric, normalized source table, and setting", paste0("SELECT metric, source_table, claim_setting, COUNT(*) AS matched_evidence_rows FROM ", shared_evidence_table, " GROUP BY metric, source_table, claim_setting ORDER BY metric, source_table, claim_setting;"))
coverage_qa <- print_query(con, "CCW-30 denominator coverage QA", paste0("SELECT COUNT(*) AS rows, COUNT(DISTINCT patid) AS patients, COUNT(*) - COUNT(DISTINCT patid) AS duplicate_patient_years FROM ", indicator_identifier, " WHERE analysis_year = 2022;"))
if (nrow(coverage_qa) != 1L || coverage_qa$duplicate_patient_years[[1]] != 0) stop("CCW-30 indicators contain duplicate patient-years.")
message("CCW-30 complete. BPH and stroke/TIA encounter exclusions are recorded but intentionally not applied.")
disconnect_komodo(con)
