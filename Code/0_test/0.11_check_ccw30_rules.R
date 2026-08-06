# Project: Frailty_Komoto SAS-derived CCW-30 validation
# Author: Nemo Zhou
# Date started: 2026-08-05
# Date last updated: 2026-08-05
#
# ---- Purpose ----
# Unit-test the simplified CCW-30 qualification rules used by
# 3.8a_calculate_normalized_ccw30_variables.R. These tests run locally and use
# synthetic records only; they do not connect to Redshift or expose patient data.

qualifies_ccw30 <- function(qualification_group, settings, dates) {
  has_inpatient <- any(settings == "inpatient")
  outpatient_dates <- length(unique(dates[settings == "outpatient"]))
  switch(
    as.character(qualification_group),
    "1" = has_inpatient,
    "2" = has_inpatient || outpatient_dates >= 2L,
    "3" = has_inpatient || outpatient_dates >= 2L,
    "4" = outpatient_dates >= 1L,
    "5" = length(settings) > 0L,
    "6" = length(settings) > 0L,
    stop("Unexpected qualification group.")
  )
}

qualifies_hiv <- function(settings, dates) {
  any(settings == "inpatient") ||
    length(unique(dates[settings == "non_inpatient"])) >= 2L
}

stopifnot(
  qualifies_ccw30(1L, "inpatient", as.Date("2022-02-01")),
  !qualifies_ccw30(2L, "outpatient", as.Date("2022-02-01")),
  qualifies_ccw30(2L, c("outpatient", "outpatient"), as.Date(c("2022-02-01", "2022-03-01"))),
  !qualifies_ccw30(2L, c("outpatient", "outpatient"), as.Date(c("2022-02-01", "2022-02-01"))),
  qualifies_ccw30(4L, "outpatient", as.Date("2022-02-01")),
  qualifies_ccw30(5L, "inpatient", as.Date("2022-02-01")),
  qualifies_ccw30(6L, "outpatient", as.Date("2022-02-01"))
)
stopifnot(
  qualifies_hiv("inpatient", as.Date("2022-02-01")),
  !qualifies_hiv("non_inpatient", as.Date("2022-02-01")),
  qualifies_hiv(
    c("non_inpatient", "non_inpatient"),
    as.Date(c("2022-02-01", "2022-03-01"))
  ),
  !qualifies_hiv(
    c("non_inpatient", "non_inpatient"),
    as.Date(c("2022-02-01", "2022-02-01"))
  )
)

conditions <- data.frame(
  patid = c("one_ip", "two_op", "duplicate_op", "all_zero", rep("cancer", 6L)),
  ccw_condition_id = c(
    "diabetes", "diabetes", "diabetes", NA_character_,
    "cancer_breast", "cancer_colorectal", "cancer_endometrial",
    "cancer_lung", "cancer_prostate", "cancer_urologic"
  ),
  stringsAsFactors = FALSE
)
condition_count <- function(patient) {
  length(unique(stats::na.omit(conditions$ccw_condition_id[conditions$patid == patient])))
}
cancer_flag <- function(patient) {
  as.integer(any(grepl("^cancer_", conditions$ccw_condition_id[conditions$patid == patient])))
}

stopifnot(
  condition_count("one_ip") == 1L,
  condition_count("all_zero") == 0L,
  cancer_flag("cancer") == 1L,
  condition_count("cancer") == 6L
)

all_base_conditions <- paste0("condition_", sprintf("%02d", seq_len(30L)))
stopifnot(length(unique(all_base_conditions)) == 30L)

metadata_path <- file.path(
  getwd(), "Documents", "Clinical Metric Look Up Tables", "CCW30",
  "0.6_ccw30_condition_metadata.csv"
)
metadata <- utils::read.csv(metadata_path, stringsAsFactors = FALSE)
hiv_lookup_path <- file.path(
  getwd(), "Documents", "Clinical Metric Look Up Tables",
  "0.6_hiv_diagnosis_lookup.csv"
)
hiv_lookup <- utils::read.csv(hiv_lookup_path, stringsAsFactors = FALSE)
table1_condition_ids <- c(
  "acute_myocardial_infarction", "diabetes", "heart_failure", "hypertension",
  "ischemic_heart_disease", "alzheimers_disease", "chronic_kidney_disease",
  "copd", "depressive_mood_disorders", "hip_and_pelvic_fracture"
)
stopifnot(
  nrow(metadata) == 30L,
  all(table1_condition_ids %in% metadata$ccw_condition_id),
  sum(grepl("^cancer_", metadata$ccw_condition_id)) == 6L,
  any(toupper(hiv_lookup$metric) == "HIV"),
  all(tolower(hiv_lookup$match_type[toupper(hiv_lookup$metric) == "HIV"]) == "exact")
)
message("CCW-30 rule-level tests passed.")
