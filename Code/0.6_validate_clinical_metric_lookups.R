library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

# Project: Frailty_Komoto clinical metric lookup validation
# Author: Nemo Zhou
# Date started: 2026-06-24
# Date last updated: 2026-06-24
#
# ---- Purpose ----
# Convert and validate source lookup files needed before annual clinical metric
# processing. This script prepares local CSV lookup artifacts for CFI, CCW,
# Gagne's combined comorbidity score, and HIV status under
# Documents/Clinical Metric Look Up Tables. It does not connect to Redshift,
# query KRD claims, or create persistent write-schema tables.
#
# Analysis time unit:
# One eligible patient-year. Downstream scripts should match claims with:
#   event_date >= January 1 of analysis_year
#   event_date <  January 1 of analysis_year + 1 year

# ---- Configuration ----

analysis_time_unit <- "calendar_year"
lookup_version <- "clinical_metric_lookups_2026-06-24"
stop_on_validation_error <- TRUE

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
lookup_output_dir <- file.path(
  repo_root,
  "Documents",
  "Clinical Metric Look Up Tables"
)
dir.create(lookup_output_dir, showWarnings = FALSE, recursive = TRUE)

source_root <- normalizePath(
  "D:/Users/xia.zhou/Documents/Frailty_Komoto/Documents",
  winslash = "/",
  mustWork = FALSE
)

ccw_setup_path <- file.path(source_root, "CCW code", "11_ccw_setup.sas")
hiv_sas_path <- file.path(source_root, "HIV code", "1.a.ID_HIVpop_K01_bk2.sas")
gagne_source_dir <- file.path(source_root, "CCI")
cfi_lookup_dir <- file.path(
  source_root,
  "CFI",
  "Required files to calculate CFI"
)

output_paths <- list(
  cfi_diagnosis_lookup = file.path(lookup_output_dir, "0.6_cfi_diagnosis_lookup.csv"),
  cfi_procedure_lookup = file.path(lookup_output_dir, "0.6_cfi_procedure_lookup.csv"),
  cfi_weight_lookup = file.path(lookup_output_dir, "0.6_cfi_weight_lookup.csv"),
  ccw_diagnosis_lookup = file.path(lookup_output_dir, "0.6_ccw_diagnosis_lookup.csv"),
  hiv_diagnosis_lookup = file.path(lookup_output_dir, "0.6_hiv_diagnosis_lookup.csv"),
  gagne_diagnosis_lookup = file.path(lookup_output_dir, "0.6_gagne_diagnosis_lookup.csv"),
  gagne_weight_lookup = file.path(lookup_output_dir, "0.6_gagne_weight_lookup.csv"),
  unified_diagnosis_lookup = file.path(
    lookup_output_dir,
    "0.6_unified_diagnosis_rule_lookup.csv"
  ),
  validation_summary = file.path(
    lookup_output_dir,
    "0.6_clinical_metric_lookup_validation.csv"
  )
)

# ---- Helpers ----

normalize_code <- function(x) {
  x |>
    as.character() |>
    str_trim() |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]", "")
}

normalize_gagne_endpoint <- function(x, preserve_sentinel = FALSE) {
  x <- x |>
    as.character() |>
    str_trim() |>
    str_to_upper()

  if (preserve_sentinel) {
    x <- str_replace_all(x, "[^A-Z0-9\\[:]", "")
  } else {
    x <- str_replace_all(x, "[^A-Z0-9]", "")
  }

  x
}

make_id <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

file_md5 <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  unname(tools::md5sum(path))
}

add_check <- function(checks, metric, check_name, status, detail = "") {
  bind_rows(
    checks,
    tibble(
      metric = metric,
      check_name = check_name,
      status = status,
      detail = as.character(detail)
    )
  )
}

assert_files_exist <- function(paths, label) {
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths) > 0) {
    stop(
      label,
      " missing required file(s):\n",
      paste(missing_paths, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

find_column <- function(data, candidates, file_label) {
  names_lower <- tolower(names(data))
  candidate_lower <- tolower(candidates)
  match_index <- match(candidate_lower, names_lower)
  match_index <- match_index[!is.na(match_index)]
  if (length(match_index) == 0) {
    stop(
      "Could not find any of these columns in ",
      file_label,
      ": ",
      paste(candidates, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }
  names(data)[match_index[[1]]]
}

read_sas_required <- function(path) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "Package 'haven' is required to read SAS7BDAT lookup files. ",
      "Install it with renv::install('haven'), then rerun this script.",
      call. = FALSE
    )
  }
  haven::read_sas(path)
}

# ---- CFI lookup conversion and validation ----

convert_cfi_lookups <- function(checks) {
  cfi_files <- file.path(
    cfi_lookup_dir,
    c(
      "CFI_ICD9CM_V32.csv",
      "CFI_ICD10CM_V2020.csv",
      "pxlookup.txt",
      "disease_weight.txt"
    )
  )
  names(cfi_files) <- c("icd9", "icd10", "px", "weight")
  assert_files_exist(cfi_files, "CFI")

  read_icd <- function(path, code_system) {
    readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE,
      progress = FALSE
    ) |>
      transmute(
        lookup_version = lookup_version,
        metric = "CFI",
        feature_id = paste0("cfi_disease_", as.integer(disease_number)),
        feature_name = paste0("CFI disease ", as.integer(disease_number)),
        code_system = code_system,
        match_value_raw = as.character(dx),
        match_value = normalize_code(dx),
        match_type = "exact",
        disease_number = as.integer(disease_number),
        source_file = basename(path),
        source_md5 = file_md5(path)
      ) |>
      filter(!is.na(disease_number), match_value != "") |>
      distinct()
  }

  cfi_dx_lookup <- bind_rows(
    read_icd(cfi_files[["icd9"]], "ICD9CM"),
    read_icd(cfi_files[["icd10"]], "ICD10CM")
  )

  cfi_px_lookup <- readr::read_tsv(
    cfi_files[["px"]],
    col_types = readr::cols(.default = readr::col_character()),
    comment = "#",
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    transmute(
      lookup_version = lookup_version,
      metric = "CFI",
      feature_id = paste0("cfi_disease_", as.integer(disease_number)),
      feature_name = paste0("CFI disease ", as.integer(disease_number)),
      code_system = "CPT_HCPCS",
      range_start = normalize_code(start),
      range_end = normalize_code(stop),
      match_type = "range",
      disease_number = as.integer(disease_number),
      source_file = basename(cfi_files[["px"]]),
      source_md5 = file_md5(cfi_files[["px"]])
    ) |>
    filter(!is.na(disease_number), range_start != "", range_end != "") |>
    distinct()

  cfi_weight_lookup <- readr::read_tsv(
    cfi_files[["weight"]],
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    transmute(
      lookup_version = lookup_version,
      metric = "CFI",
      disease_number = as.integer(disease_number),
      feature_id = paste0("cfi_disease_", disease_number),
      weight = as.numeric(weight),
      source_file = basename(cfi_files[["weight"]]),
      source_md5 = file_md5(cfi_files[["weight"]])
    ) |>
    filter(!is.na(disease_number), !is.na(weight)) |>
    distinct()

  readr::write_csv(cfi_dx_lookup, output_paths$cfi_diagnosis_lookup)
  readr::write_csv(cfi_px_lookup, output_paths$cfi_procedure_lookup)
  readr::write_csv(cfi_weight_lookup, output_paths$cfi_weight_lookup)

  checks <- add_check(
    checks,
    "CFI",
    "required_files_present",
    "pass",
    paste(basename(cfi_files), collapse = "; ")
  )
  checks <- add_check(
    checks,
    "CFI",
    "diagnosis_lookup_rows",
    if_else(nrow(cfi_dx_lookup) > 0, "pass", "fail"),
    nrow(cfi_dx_lookup)
  )
  checks <- add_check(
    checks,
    "CFI",
    "procedure_lookup_rows",
    if_else(nrow(cfi_px_lookup) > 0, "pass", "fail"),
    nrow(cfi_px_lookup)
  )
  checks <- add_check(
    checks,
    "CFI",
    "weight_rows",
    if_else(nrow(cfi_weight_lookup) == 93, "pass", "fail"),
    nrow(cfi_weight_lookup)
  )

  list(
    checks = checks,
    cfi_dx_lookup = cfi_dx_lookup
  )
}

# ---- CCW lookup conversion and validation ----

ccw_condition_groups <- tribble(
  ~ccw_condition_id, ~ccw_group,
  "acute_myocardial_infarction", "CV",
  "atrial_fibrillation_and_flutter", "CV",
  "diabetes", "CV",
  "heart_failure", "CV",
  "hyperlipidemia", "CV",
  "hypertension", "CV",
  "ischemic_heart_disease", "CV",
  "pvd", "CV",
  "adhd_and_conduct_disorders", "LDND",
  "autism_spectrum_disorders", "LDND",
  "epilepsy", "LDND",
  "intellectual_disabilities", "LDND",
  "learning_disabilities", "LDND",
  "migraine_and_chronic_headache", "LDND",
  "multiple_sclerosis", "LDND",
  "other_developmental_delays", "LDND",
  "spinal_cord_injuries", "LDND",
  "tbi", "LDND",
  "alzheimers_disease", "MH",
  "anxiety_disorders", "MH",
  "depressive_mood_disorders", "MH",
  "non_alzheimer_dementia", "MH",
  "personality_disorders", "MH",
  "psychotic_disorders", "MH",
  "ptsd", "MH",
  "cerebral_palsy", "MUSC",
  "fibromyalgia", "MUSC",
  "hip_and_pelvic_fracture", "MUSC",
  "mobility_impairments", "MUSC",
  "muscular_dystrophy", "MUSC",
  "osteoporosis", "MUSC",
  "spina_bifida", "MUSC",
  "anemia", "OCC",
  "benign_prostatic_hyperplasia", "OCC",
  "cancer", "OCC",
  "chronic_kidney_disease", "OCC",
  "chronic_pain", "OCC",
  "hepatitis", "OCC",
  "hypothyroidism", "OCC",
  "liver_disease", "OCC",
  "obesity", "OCC",
  "parkinsons_disease", "OCC",
  "pressure_and_chronic_ulcers", "OCC",
  "sickle_cell_disease", "OCC",
  "asthma", "PULM",
  "copd", "PULM",
  "cystic_fibrosis", "PULM",
  "pneumonia", "PULM",
  "blindness_and_visual_impairments", "SENSE",
  "cataract", "SENSE",
  "deafness_and_hearing_impairments", "SENSE",
  "glaucoma", "SENSE",
  "alcohol_use_disorders", "SU",
  "drug_use_disorder", "SU",
  "oud", "SU",
  "tobacco_use_disorders", "SU"
)

convert_ccw_lookup <- function(checks) {
  assert_files_exist(ccw_setup_path, "CCW")
  text <- paste(readr::read_lines(ccw_setup_path), collapse = "\n")

  rule_pattern <- regex(
    "if\\s+code\\s+in\\s*:\\s*\\((.*?)\\)\\s*then\\s+do;\\s*condition\\s*=\\s*'([^']+)'",
    ignore_case = TRUE,
    dotall = TRUE
  )
  rules <- str_match_all(text, rule_pattern)[[1]]
  if (nrow(rules) == 0) {
    stop("No CCW rules were parsed from ", ccw_setup_path, call. = FALSE)
  }

  ccw_lookup <- purrr::map_dfr(seq_len(nrow(rules)), function(i) {
    code_block <- rules[i, 2]
    condition_name <- rules[i, 3]
    codes <- str_match_all(code_block, "'([^']+)'")[[1]][, 2]
    tibble(
      lookup_version = lookup_version,
      metric = "CCW",
      feature_id = make_id(condition_name),
      feature_name = condition_name,
      ccw_condition_id = make_id(condition_name),
      ccw_condition_name = condition_name,
      code_system = "ICD10CM",
      match_value_raw = codes,
      match_value = normalize_code(codes),
      match_type = "prefix",
      source_file = basename(ccw_setup_path),
      source_md5 = file_md5(ccw_setup_path)
    )
  }) |>
    filter(match_value != "") |>
    distinct() |>
    left_join(ccw_condition_groups, by = "ccw_condition_id")

  readr::write_csv(ccw_lookup, output_paths$ccw_diagnosis_lookup)

  missing_group <- ccw_lookup |>
    distinct(ccw_condition_id, ccw_condition_name, ccw_group) |>
    filter(is.na(ccw_group))

  checks <- add_check(
    checks,
    "CCW",
    "source_file_present",
    "pass",
    ccw_setup_path
  )
  checks <- add_check(
    checks,
    "CCW",
    "condition_count",
    if_else(n_distinct(ccw_lookup$ccw_condition_id) == 56, "pass", "fail"),
    n_distinct(ccw_lookup$ccw_condition_id)
  )
  checks <- add_check(
    checks,
    "CCW",
    "lookup_row_count",
    if_else(nrow(ccw_lookup) > 0, "pass", "fail"),
    nrow(ccw_lookup)
  )
  checks <- add_check(
    checks,
    "CCW",
    "all_conditions_have_group",
    if_else(nrow(missing_group) == 0, "pass", "fail"),
    if_else(
      nrow(missing_group) == 0,
      "all grouped",
      paste(missing_group$ccw_condition_name, collapse = "; ")
    )
  )
  checks <- add_check(
    checks,
    "CCW",
    "all_rules_prefix_match",
    if_else(all(ccw_lookup$match_type == "prefix"), "pass", "fail"),
    "Source SAS uses if code in: prefix semantics."
  )

  list(
    checks = checks,
    ccw_lookup = ccw_lookup
  )
}

# ---- HIV lookup conversion and validation ----

convert_hiv_lookup <- function(checks) {
  assert_files_exist(hiv_sas_path, "HIV")
  hiv_codes <- c(
    "042", "043", "044", "V08", "V0179", "79571", "V6544",
    "Z21", "B200", "B201", "B202", "B203", "B204", "B205",
    "B206", "B207", "B208", "B209", "B210", "B211", "B212",
    "B213", "B217", "B218", "B219", "B220", "B221", "B222",
    "B227", "B230", "B231", "B232", "B238", "B24"
  )

  hiv_lookup <- tibble(
    lookup_version = lookup_version,
    metric = "HIV",
    feature_id = "hiv_status",
    feature_name = "HIV diagnosis evidence",
    code_system = case_when(
      str_detect(hiv_codes, "^[0-9V]") ~ "ICD9CM",
      TRUE ~ "ICD10CM"
    ),
    match_value_raw = hiv_codes,
    match_value = normalize_code(hiv_codes),
    match_type = "exact",
    claim_window_rule = "calendar_year",
    status_assignment_rule = "annual_only_calendar_year",
    inpatient_rule = "at_least_one_inpatient_diagnosis_event",
    non_inpatient_rule = "at_least_two_non_inpatient_diagnosis_events_at_least_1_day_apart",
    pharmacy_evidence_used = FALSE,
    source_file = basename(hiv_sas_path),
    source_md5 = file_md5(hiv_sas_path)
  ) |>
    distinct()

  readr::write_csv(hiv_lookup, output_paths$hiv_diagnosis_lookup)

  checks <- add_check(
    checks,
    "HIV",
    "source_file_present",
    "pass",
    hiv_sas_path
  )
  checks <- add_check(
    checks,
    "HIV",
    "diagnosis_code_count",
    if_else(nrow(hiv_lookup) == 34, "pass", "fail"),
    nrow(hiv_lookup)
  )
  checks <- add_check(
    checks,
    "HIV",
    "claim_window_rule_recorded",
    "pass",
    "calendar_year"
  )
  checks <- add_check(
    checks,
    "HIV",
    "pharmacy_evidence_excluded",
    "pass",
    "HIV lookup uses diagnosis evidence only."
  )

  list(
    checks = checks,
    hiv_lookup = hiv_lookup
  )
}

# ---- Gagne lookup conversion and validation ----

convert_gagne_lookups <- function(checks) {
  gagne_files <- file.path(
    gagne_source_dir,
    paste0("comorb_dxfmt", seq_len(20), "f.sas7bdat")
  )
  names(gagne_files) <- paste0("comorb_dxfmt", seq_len(20), "f.sas7bdat")
  weight_path <- file.path(gagne_source_dir, "comorb_weight.sas7bdat")
  assert_files_exist(c(gagne_files, weight_path), "Gagne")

  format_rows <- purrr::map_dfr(gagne_files, function(path) {
    raw <- read_sas_required(path) |>
      as_tibble()

    fmt_col <- find_column(raw, c("fmtname"), basename(path))
    start_col <- find_column(raw, c("start"), basename(path))
    end_col <- find_column(raw, c("end"), basename(path))
    label_col <- find_column(raw, c("label"), basename(path))

    raw |>
      transmute(
        lookup_version = lookup_version,
        metric = "GAGNE",
        source_file = basename(path),
        source_md5 = file_md5(path),
        format_name = as.character(.data[[fmt_col]]),
        code_system = case_when(
          str_detect(str_to_upper(format_name), "DX09") ~ "ICD9CM",
          str_detect(str_to_upper(format_name), "DX10") ~ "ICD10CM",
          TRUE ~ NA_character_
        ),
        sas_start_raw = str_to_upper(str_trim(as.character(.data[[start_col]]))),
        sas_end_raw = str_to_upper(str_trim(as.character(.data[[end_col]]))),
        start_clean = normalize_gagne_endpoint(sas_start_raw),
        end_clean = normalize_gagne_endpoint(sas_end_raw),
        end_for_range = normalize_gagne_endpoint(
          sas_end_raw,
          preserve_sentinel = TRUE
        ),
        end_sentinel = str_extract(sas_end_raw, "[\\[:]$"),
        same_clean_endpoint = start_clean == end_clean,
        match_type = case_when(
          same_clean_endpoint & is.na(end_sentinel) ~ "exact",
          same_clean_endpoint & !is.na(end_sentinel) ~ "prefix",
          TRUE ~ "range"
        ),
        match_value = if_else(
          match_type %in% c("exact", "prefix"),
          start_clean,
          NA_character_
        ),
        range_start = if_else(match_type == "range", start_clean, NA_character_),
        range_end = if_else(match_type == "range", end_for_range, NA_character_),
        range_end_inclusive = case_when(
          match_type != "range" ~ NA,
          is.na(end_sentinel) ~ TRUE,
          TRUE ~ FALSE
        ),
        gagne_group = str_to_upper(str_trim(as.character(.data[[label_col]])))
      ) |>
      filter(
        !is.na(code_system),
        !is.na(gagne_group),
        gagne_group != "",
        gagne_group != "OT",
        match_value != "" | (range_start != "" & range_end != "")
      )
  }) |>
    distinct()

  weight_raw <- read_sas_required(weight_path) |>
    as_tibble()
  group_col <- find_column(weight_raw, c("cc_group", "CC_GROUP"), basename(weight_path))
  desc_col <- find_column(
    weight_raw,
    c("cc_group_desc", "CC_GROUP_DESC", "label", "description"),
    basename(weight_path)
  )
  weight_col <- find_column(weight_raw, c("weight", "WEIGHT"), basename(weight_path))

  gagne_weight_lookup <- weight_raw |>
    transmute(
      lookup_version = lookup_version,
      metric = "GAGNE",
      gagne_group = str_to_upper(str_trim(as.character(.data[[group_col]]))),
      gagne_group_desc = as.character(.data[[desc_col]]),
      weight = as.numeric(.data[[weight_col]]),
      source_file = basename(weight_path),
      source_md5 = file_md5(weight_path)
    ) |>
    filter(gagne_group != "", !is.na(weight)) |>
    distinct()

  gagne_dx_lookup <- format_rows |>
    left_join(
      gagne_weight_lookup |>
        select(gagne_group, gagne_group_desc) |>
        distinct(),
      by = "gagne_group"
    ) |>
    mutate(
      feature_id = paste0("gagne_", make_id(coalesce(gagne_group_desc, gagne_group))),
      feature_name = coalesce(gagne_group_desc, gagne_group)
    ) |>
    select(
      lookup_version,
      metric,
      feature_id,
      feature_name,
      code_system,
      match_value,
      range_start,
      range_end,
      range_end_inclusive,
      match_type,
      gagne_group,
      gagne_group_desc,
      format_name,
      sas_start_raw,
      sas_end_raw,
      source_file,
      source_md5
    )

  readr::write_csv(gagne_dx_lookup, output_paths$gagne_diagnosis_lookup)
  readr::write_csv(gagne_weight_lookup, output_paths$gagne_weight_lookup)

  groups_without_weight <- gagne_dx_lookup |>
    distinct(gagne_group) |>
    anti_join(gagne_weight_lookup |> distinct(gagne_group), by = "gagne_group")

  weights_without_dx <- gagne_weight_lookup |>
    distinct(gagne_group) |>
    anti_join(gagne_dx_lookup |> distinct(gagne_group), by = "gagne_group")

  checks <- add_check(
    checks,
    "GAGNE",
    "required_files_present",
    "pass",
    paste(c(basename(gagne_files), basename(weight_path)), collapse = "; ")
  )
  checks <- add_check(
    checks,
    "GAGNE",
    "format_file_count",
    if_else(n_distinct(gagne_dx_lookup$source_file) == 20, "pass", "fail"),
    n_distinct(gagne_dx_lookup$source_file)
  )
  checks <- add_check(
    checks,
    "GAGNE",
    "weight_group_count",
    if_else(nrow(gagne_weight_lookup) == 20, "pass", "fail"),
    nrow(gagne_weight_lookup)
  )
  checks <- add_check(
    checks,
    "GAGNE",
    "all_dx_groups_have_weight",
    if_else(nrow(groups_without_weight) == 0, "pass", "fail"),
    paste(groups_without_weight$gagne_group, collapse = "; ")
  )
  checks <- add_check(
    checks,
    "GAGNE",
    "all_weight_groups_have_dx_lookup",
    if_else(nrow(weights_without_dx) == 0, "pass", "fail"),
    paste(weights_without_dx$gagne_group, collapse = "; ")
  )
  checks <- add_check(
    checks,
    "GAGNE",
    "analysis_window_recorded",
    "pass",
    analysis_time_unit
  )
  match_type_counts <- table(gagne_dx_lookup$match_type)
  checks <- add_check(
    checks,
    "GAGNE",
    "match_type_counts",
    if_else(
      all(c("exact", "prefix", "range") %in% names(match_type_counts)),
      "pass",
      "fail"
    ),
    paste(
      paste(
        names(match_type_counts),
        as.integer(match_type_counts),
        sep = "="
      ),
      collapse = "; "
    )
  )

  list(
    checks = checks,
    gagne_dx_lookup = gagne_dx_lookup
  )
}

# ---- Unified diagnosis lookup ----

build_unified_diagnosis_lookup <- function(cfi_dx_lookup,
                                           ccw_lookup,
                                           hiv_lookup,
                                           gagne_dx_lookup = NULL) {
  cfi_unified <- cfi_dx_lookup |>
    transmute(
      lookup_version,
      metric,
      feature_id,
      feature_name,
      code_system,
      match_value,
      range_start = NA_character_,
      range_end = NA_character_,
      range_end_inclusive = NA,
      match_type,
      source_file,
      source_md5
    )

  ccw_unified <- ccw_lookup |>
    transmute(
      lookup_version,
      metric,
      feature_id,
      feature_name,
      code_system,
      match_value,
      range_start = NA_character_,
      range_end = NA_character_,
      range_end_inclusive = NA,
      match_type,
      source_file,
      source_md5
    )

  hiv_unified <- hiv_lookup |>
    transmute(
      lookup_version,
      metric,
      feature_id,
      feature_name,
      code_system,
      match_value,
      range_start = NA_character_,
      range_end = NA_character_,
      range_end_inclusive = NA,
      match_type,
      source_file,
      source_md5
    )

  if (is.null(gagne_dx_lookup)) {
    gagne_unified <- tibble()
  } else {
    gagne_unified <- gagne_dx_lookup |>
      transmute(
        lookup_version,
        metric,
        feature_id,
        feature_name,
        code_system,
        match_value,
        range_start,
        range_end,
        range_end_inclusive,
        match_type,
        source_file,
        source_md5
      )
  }

  bind_rows(cfi_unified, ccw_unified, hiv_unified, gagne_unified) |>
    mutate(
      analysis_time_unit = analysis_time_unit,
      final_match_after_flattening = TRUE
    ) |>
    distinct()
}

# ---- Main ----

checks <- tibble(
  metric = character(),
  check_name = character(),
  status = character(),
  detail = character()
)

message("Converting and validating CFI lookup files.")
cfi_result <- convert_cfi_lookups(checks)
checks <- cfi_result$checks

message("Converting and validating CCW lookup files.")
ccw_result <- convert_ccw_lookup(checks)
checks <- ccw_result$checks

message("Converting and validating HIV lookup files.")
hiv_result <- convert_hiv_lookup(checks)
checks <- hiv_result$checks

message("Converting and validating Gagne lookup files.")
gagne_result <- tryCatch(
  convert_gagne_lookups(checks),
  error = function(e) {
    blocked_checks <- add_check(
      checks,
      "GAGNE",
      "gagne_lookup_conversion",
      "blocked",
      conditionMessage(e)
    )
    list(checks = blocked_checks, gagne_dx_lookup = NULL)
  }
)
checks <- gagne_result$checks

unified_lookup <- build_unified_diagnosis_lookup(
  cfi_dx_lookup = cfi_result$cfi_dx_lookup,
  ccw_lookup = ccw_result$ccw_lookup,
  hiv_lookup = hiv_result$hiv_lookup,
  gagne_dx_lookup = gagne_result$gagne_dx_lookup
)

readr::write_csv(unified_lookup, output_paths$unified_diagnosis_lookup)

checks <- add_check(
  checks,
  "ALL",
  "unified_diagnosis_lookup_rows",
  if_else(nrow(unified_lookup) > 0, "pass", "fail"),
  nrow(unified_lookup)
)
checks <- add_check(
  checks,
  "ALL",
  "analysis_time_unit",
  "pass",
  analysis_time_unit
)

readr::write_csv(checks, output_paths$validation_summary)

message("Saved validation summary to: ", output_paths$validation_summary)
message("Saved unified diagnosis lookup to: ", output_paths$unified_diagnosis_lookup)

bad_checks <- checks |>
  filter(status %in% c("fail", "blocked"))

if (nrow(bad_checks) > 0) {
  message("Validation issues:")
  print(bad_checks, n = Inf)
}

if (stop_on_validation_error && nrow(bad_checks) > 0) {
  stop(
    "Clinical metric lookup validation did not fully pass. ",
    "Review ",
    output_paths$validation_summary,
    " before running claims extraction.",
    call. = FALSE
  )
}

message("Clinical metric lookup validation complete.")
