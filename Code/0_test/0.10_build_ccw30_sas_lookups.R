# Project: Frailty_Komoto CCW-30 SAS lookup preparation
# Author: Nemo Zhou
# Date started: 2026-08-05
# Date last updated: 2026-08-05
#
# ---- Purpose ----
# Parse the versioned CCW-30 SAS codelist and metadata supplied for the 2022
# comparison project into checked-in CSV lookups. The resulting files retain
# the 30-condition metadata, exact normalized ICD-10 inclusion codes, and the
# two encounter-level exclusion lists. The normalized-table CCW-30 workflow
# intentionally records those exclusions as unavailable because the source
# diagnosis table has no reliable claim identifier.
#
# Run from the project root:
#   source("Code/0_test/0.10_build_ccw30_sas_lookups.R")

lookup_dir <- file.path(
  getwd(), "Documents", "Clinical Metric Look Up Tables", "CCW30"
)
source_dir <- file.path(lookup_dir, "source")
metadata_path <- file.path(source_dir, "ccw30_meta.sas7bdat")
codelist_path <- file.path(source_dir, "ccw30_codelist.sas")
batch_path <- file.path(source_dir, "ccw30_batch.sas")
metadata_output_path <- file.path(lookup_dir, "0.6_ccw30_condition_metadata.csv")
diagnosis_output_path <- file.path(lookup_dir, "0.6_ccw30_diagnosis_lookup.csv")
provenance_output_path <- file.path(lookup_dir, "0.6_ccw30_source_provenance.csv")

required_sources <- c(metadata_path, codelist_path, batch_path)
missing_sources <- required_sources[!file.exists(required_sources)]
if (length(missing_sources) > 0L) {
  stop(
    "Missing versioned CCW-30 SAS source file(s): ",
    paste(missing_sources, collapse = ", "),
    call. = FALSE
  )
}
if (!requireNamespace("haven", quietly = TRUE)) {
  stop("The renv-managed haven package is required to read ccw30_meta.sas7bdat.")
}

condition_id_map <- c(
  ami = "acute_myocardial_infarction",
  ad = "alzheimers_disease",
  anemia = "anemia",
  asthma = "asthma",
  afib = "atrial_fibrillation_and_flutter",
  bph = "benign_prostatic_hyperplasia",
  cancer_breast = "cancer_breast",
  cancer_colorectal = "cancer_colorectal",
  cancer_endometrial = "cancer_endometrial",
  cancer_lung = "cancer_lung",
  cancer_prostate = "cancer_prostate",
  cancer_urologic = "cancer_urologic",
  cataract = "cataract",
  kidney = "chronic_kidney_disease",
  copd = "copd",
  depression = "depressive_mood_disorders",
  diabetes = "diabetes",
  glaucoma = "glaucoma",
  heartfailure = "heart_failure",
  hipfracture = "hip_and_pelvic_fracture",
  hyperlipidemia = "hyperlipidemia",
  hypertension = "hypertension",
  hypothyroidism = "hypothyroidism",
  ischemicheart = "ischemic_heart_disease",
  dementia = "non_alzheimer_dementia",
  osteoporosis = "osteoporosis",
  parkinson = "parkinsons_disease",
  pneumonia = "pneumonia",
  arthritis = "rheumatoid_arthritis_or_osteoarthritis",
  stroketia = "stroke_or_transient_ischemic_attack"
)

normalize_dx_code <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", trimws(x)))
}

source_md5 <- unname(tools::md5sum(required_sources))
names(source_md5) <- basename(required_sources)
lookup_version <- paste0("ccw30_sas_", source_md5[["ccw30_codelist.sas"]])
source_provenance <- data.frame(
  source_filename = basename(required_sources),
  source_relative_path = file.path("source", basename(required_sources)),
  md5 = unname(source_md5),
  lookup_version = lookup_version,
  stringsAsFactors = FALSE
)

metadata <- as.data.frame(haven::read_sas(metadata_path), stringsAsFactors = FALSE)
required_metadata_columns <- c(
  "condition_name", "condition_name_long", "ref_period",
  "qualification_group", "qualification", "needs_exclusion", "order"
)
missing_metadata_columns <- setdiff(required_metadata_columns, names(metadata))
if (length(missing_metadata_columns) > 0L) {
  stop(
    "CCW-30 metadata is missing required columns: ",
    paste(missing_metadata_columns, collapse = ", "),
    call. = FALSE
  )
}

metadata$condition_name <- tolower(trimws(metadata$condition_name))
metadata$ccw_condition_id <- unname(condition_id_map[metadata$condition_name])
if (anyNA(metadata$ccw_condition_id)) {
  stop(
    "No canonical CCW-30 condition ID was defined for: ",
    paste(metadata$condition_name[is.na(metadata$ccw_condition_id)], collapse = ", "),
    call. = FALSE
  )
}
if (nrow(metadata) != 30L || anyDuplicated(metadata$condition_name) > 0L) {
  stop("Expected exactly 30 unique CCW-30 metadata conditions.", call. = FALSE)
}

codelist_text <- paste(readLines(codelist_path, warn = FALSE), collapse = "\n")
macro_pattern <- "(?is)%let\\s+ccw_([a-z0-9_]+)_dx\\s*=\\s*(.*?);"
macro_matches <- regmatches(codelist_text, gregexpr(macro_pattern, codelist_text, perl = TRUE))[[1]]
if (length(macro_matches) == 0L) {
  stop("No CCW-30 diagnosis macros were found in the supplied codelist.", call. = FALSE)
}

parse_macro <- function(macro_text) {
  parts <- regmatches(macro_text, regexec(macro_pattern, macro_text, perl = TRUE))[[1]]
  macro_name <- tolower(parts[[2]])
  is_exclusion <- grepl("_exclude$", macro_name)
  condition_name <- sub("_exclude$", "", macro_name)
  codes <- regmatches(parts[[3]], gregexpr('"[^"]+"', parts[[3]], perl = TRUE))[[1]]
  data.frame(
    condition_name = condition_name,
    rule_type = if (is_exclusion) "exclusion" else "inclusion",
    dx_code = normalize_dx_code(gsub('^"|"$', "", codes)),
    stringsAsFactors = FALSE
  )
}

diagnosis_lookup <- do.call(rbind, lapply(macro_matches, parse_macro))
diagnosis_lookup <- diagnosis_lookup[
  !is.na(diagnosis_lookup$dx_code) & nzchar(diagnosis_lookup$dx_code),
  , drop = FALSE
]
diagnosis_lookup <- unique(diagnosis_lookup)
diagnosis_lookup$ccw_condition_id <- unname(condition_id_map[diagnosis_lookup$condition_name])
if (anyNA(diagnosis_lookup$ccw_condition_id)) {
  stop(
    "The SAS codelist contains an unmapped condition: ",
    paste(unique(diagnosis_lookup$condition_name[is.na(diagnosis_lookup$ccw_condition_id)]), collapse = ", "),
    call. = FALSE
  )
}

missing_inclusion_conditions <- setdiff(
  metadata$condition_name,
  unique(diagnosis_lookup$condition_name[diagnosis_lookup$rule_type == "inclusion"])
)
if (length(missing_inclusion_conditions) > 0L) {
  stop(
    "CCW-30 metadata conditions without inclusion codes: ",
    paste(missing_inclusion_conditions, collapse = ", "),
    call. = FALSE
  )
}

metadata_output <- metadata[order(metadata$order), required_metadata_columns, drop = FALSE]
metadata_output$lookup_version <- lookup_version
metadata_output$ccw_condition_id <- unname(condition_id_map[metadata_output$condition_name])
metadata_output$exclusion_implementation <- ifelse(
  metadata_output$needs_exclusion == 1,
  "omitted_no_reliable_claim_identifier_in_normalized_dx_events",
  "not_applicable"
)
metadata_output$source_batch_md5 <- source_md5[["ccw30_batch.sas"]]
metadata_output$source_codelist_md5 <- source_md5[["ccw30_codelist.sas"]]
metadata_output$source_metadata_md5 <- source_md5[["ccw30_meta.sas7bdat"]]

diagnosis_output <- merge(
  diagnosis_lookup,
  metadata_output[, c(
    "condition_name", "condition_name_long", "qualification_group",
    "ref_period", "needs_exclusion", "ccw_condition_id", "lookup_version",
    "exclusion_implementation"
  )],
  by = c("condition_name", "ccw_condition_id"),
  all.x = TRUE,
  sort = FALSE
)
diagnosis_output$code_system <- "ICD10CM"
diagnosis_output$match_type <- "exact"
diagnosis_output$source_file <- "ccw30_codelist.sas"
diagnosis_output$source_codelist_md5 <- source_md5[["ccw30_codelist.sas"]]
diagnosis_output <- diagnosis_output[order(
  diagnosis_output$condition_name,
  diagnosis_output$rule_type,
  diagnosis_output$dx_code
), c(
  "lookup_version", "condition_name", "condition_name_long", "ccw_condition_id",
  "qualification_group", "ref_period", "needs_exclusion", "exclusion_implementation",
  "rule_type", "code_system", "dx_code", "match_type", "source_file",
  "source_codelist_md5"
)]

dir.create(lookup_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(metadata_output, metadata_output_path, row.names = FALSE, na = "")
utils::write.csv(diagnosis_output, diagnosis_output_path, row.names = FALSE, na = "")
utils::write.csv(source_provenance, provenance_output_path, row.names = FALSE, na = "")

if (
  nrow(metadata_output) != 30L ||
    length(unique(diagnosis_output$ccw_condition_id[diagnosis_output$rule_type == "inclusion"])) != 30L ||
    !identical(
      sort(unique(diagnosis_output$condition_name[diagnosis_output$rule_type == "exclusion"])),
      c("bph", "stroketia")
    ) ||
    !all(diagnosis_output$match_type == "exact") ||
    nrow(source_provenance) != 3L
) {
  stop("CCW-30 lookup validation failed.", call. = FALSE)
}

message("Wrote CCW-30 metadata lookup: ", metadata_output_path)
message("Wrote CCW-30 diagnosis lookup: ", diagnosis_output_path)
message("Wrote CCW-30 source provenance: ", provenance_output_path)
message("CCW-30 conditions: ", nrow(metadata_output), "; diagnosis rules: ", nrow(diagnosis_output), ".")
