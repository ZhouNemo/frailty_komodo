source("Code/5.0_annual_polypharmacy_helpers.R")

# Project: Frailty_Komoto annual polypharmacy ATC3 prevalence
# Author: Nemo Zhou
# Date started: 2026-07-04
# Date last updated: 2026-07-05
#
# ---- Purpose ----
# Summarize annual prevalence of the top 20 mapped ATC3 medication classes
# among the selected eligible patient-years. This is a descriptive aggregate
# output, not a new persistent Redshift table. It reads the annual denominator
# and clipped ATC3 exposure episodes, then writes:
#   - Outputs/5.8_annual_polypharmacy_atc3_prevalence.csv
#   - Outputs/5.8_annual_polypharmacy_atc3_prevalence_by_rx_insurance.csv
# These CSVs are report-ready inputs for
# Code/5.9_visualize_annual_polypharmacy_outputs.Rmd.

config <- get_annual_polypharmacy_config()
con <- connect_komodo()
min_count <- 11L
top_atc3_n <- 20L

atc3_class_labels <- c(
  A01A = "Stomatological preparations",
  A02B = "Acid-related disorder drugs",
  A07E = "Intestinal antiinflammatory agents",
  A10B = "Blood-glucose-lowering drugs excluding insulin",
  B01A = "Antithrombotic agents",
  C05A = "Agents for treatment of hemorrhoids and anal fissures for topical use",
  C07A = "Beta-blocking agents",
  C09A = "ACE inhibitors, plain",
  C09C = "Angiotensin II receptor blockers, plain",
  C10A = "Lipid-modifying agents, plain",
  D01A = "Antifungals for topical use",
  D02A = "Emollients and protectives",
  D04A = "Antipruritics, including antihistamines and anesthetics",
  D06A = "Antibiotics for topical use",
  D07A = "Corticosteroids, plain",
  H02A = "Corticosteroids for systemic use, plain",
  J01C = "Beta-lactam antibacterials, penicillins",
  J01D = "Other beta-lactam antibacterials",
  J01F = "Macrolides, lincosamides and streptogramins",
  J01M = "Quinolone antibacterials",
  M01A = "Anti-inflammatory and antirheumatic products, non-steroids",
  M03B = "Muscle relaxants, centrally acting agents",
  N02A = "Opioids",
  N02B = "Other analgesics and antipyretics",
  N03A = "Antiepileptics",
  N05A = "Antipsychotics",
  N05B = "Anxiolytics",
  N05C = "Hypnotics and sedatives",
  N06A = "Antidepressants",
  R01A = "Decongestants and other nasal preparations for topical use",
  R03A = "Adrenergics, inhalants",
  R03B = "Other drugs for obstructive airway diseases, inhalants",
  R05D = "Cough suppressants, excluding combinations with expectorants",
  S01A = "Antiinfectives",
  S01B = "Antiinflammatory agents",
  S01E = "Antiglaucoma preparations and miotics",
  S01X = "Other ophthalmologicals"
)

atc3_label_for <- function(atc3) {
  labels <- unname(atc3_class_labels[atc3])
  ifelse(is.na(labels), paste0(atc3, " (ATC3 name not mapped)"), labels)
}

if (!dir.exists(config$output_dir)) {
  dir.create(config$output_dir, recursive = TRUE)
}

ids_identifier <- qualified_identifier(write_schema, config$ids_table)
episodes_identifier <- qualified_identifier(write_schema, config$episodes_table)

for (table in c(config$ids_table, config$episodes_table)) {
  if (!table_exists(con, write_schema, table)) {
    stop("Required polypharmacy table was not found: ", write_schema, ".", table)
  }
}

table_has_columns(
  con,
  write_schema,
  config$ids_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "rx_insurance_group",
    "rx_insurance_segment"
  )
)
table_has_columns(
  con,
  write_schema,
  config$episodes_table,
  c(
    "patid",
    "patient_id",
    "analysis_year",
    "ndc11",
    "atc3",
    "episode_start_clipped",
    "episode_end_clipped"
  )
)

run_query_stage <- function(label, expr) {
  started_at <- Sys.time()
  message(format(started_at, "[%Y-%m-%d %H:%M:%S] "), "START ", label)
  result <- eval.parent(substitute(expr))
  finished_at <- Sys.time()
  elapsed_minutes <- round(
    as.numeric(difftime(finished_at, started_at, units = "mins")),
    1
  )
  message(
    format(finished_at, "[%Y-%m-%d %H:%M:%S] "),
    "DONE  ",
    label,
    " (",
    elapsed_minutes,
    " min)"
  )
  result
}

suppress_count_columns <- function(data, suppress_rows, columns) {
  for (column in columns) {
    if (column %in% names(data)) {
      data[[column]] <- as.numeric(data[[column]])
      data[[column]][suppress_rows] <- NA_real_
    }
  }
  data
}

atc3_prevalence <- run_query_stage(
  "Build overall ATC3 prevalence summary",
  DBI::dbGetQuery(
    con,
    paste0(
      "WITH denominator AS (
       SELECT
         analysis_year,
         COUNT(*)::BIGINT AS n_eligible_patient_years
       FROM ", ids_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ")
       GROUP BY analysis_year
     ),
     atc3_summary AS (
       SELECT
         analysis_year,
         atc3,
         COUNT(DISTINCT patid)::BIGINT AS n_patient_years_with_atc3,
         COUNT(*)::BIGINT AS n_exposure_episode_rows,
         COUNT(DISTINCT ndc11)::BIGINT AS n_distinct_ndc11
       FROM ", episodes_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ")
         AND atc3 IS NOT NULL
         AND atc3 <> ''
         AND episode_start_clipped IS NOT NULL
         AND episode_end_clipped IS NOT NULL
         AND episode_end_clipped >= episode_start_clipped
       GROUP BY analysis_year, atc3
     ),
     ranked_atc3 AS (
       SELECT
         *,
         ROW_NUMBER() OVER (
           PARTITION BY analysis_year
           ORDER BY n_patient_years_with_atc3 DESC, atc3
         ) AS atc3_rank
       FROM atc3_summary
     )
     SELECT
       s.analysis_year,
       s.atc3,
       s.atc3_rank,
       s.n_patient_years_with_atc3,
       d.n_eligible_patient_years,
       CAST(
         100.0 * s.n_patient_years_with_atc3::DOUBLE PRECISION /
           NULLIF(d.n_eligible_patient_years, 0)
         AS DECIMAL(18, 4)
       ) AS prevalence_pct,
       s.n_exposure_episode_rows,
       s.n_distinct_ndc11
     FROM ranked_atc3 s
     INNER JOIN denominator d
       ON s.analysis_year = d.analysis_year
     WHERE s.atc3_rank <= ", top_atc3_n, "
     ORDER BY s.analysis_year, s.atc3_rank, s.atc3"
    )
  )
)

atc3_prevalence$atc3_label <- atc3_label_for(atc3_prevalence$atc3)
atc3_prevalence <- atc3_prevalence[
  c(
    "analysis_year",
    "atc3",
    "atc3_rank",
    "atc3_label",
    "n_patient_years_with_atc3",
    "n_eligible_patient_years",
    "prevalence_pct",
    "n_exposure_episode_rows",
    "n_distinct_ndc11"
  )
]

atc3_suppressed <- (
  is.na(atc3_prevalence$n_patient_years_with_atc3) |
    atc3_prevalence$n_patient_years_with_atc3 < min_count
)
atc3_prevalence$suppression_applied <- ifelse(
  atc3_suppressed,
  "yes",
  "no"
)
atc3_prevalence <- suppress_count_columns(
  atc3_prevalence,
  atc3_suppressed,
  c(
    "n_patient_years_with_atc3",
    "prevalence_pct",
    "n_exposure_episode_rows",
    "n_distinct_ndc11"
  )
)

insurance_prevalence <- run_query_stage(
  "Build insurance-stratified ATC3 prevalence summary",
  DBI::dbGetQuery(
    con,
    paste0(
      "WITH denominator AS (
       SELECT
         analysis_year,
         rx_insurance_group,
         rx_insurance_segment,
         COUNT(*)::BIGINT AS n_eligible_patient_years
       FROM ", ids_identifier, "
       WHERE analysis_year IN (", sql_values(config$analysis_years), ")
       GROUP BY analysis_year, rx_insurance_group, rx_insurance_segment
     ),
     top_atc3_summary AS (
       SELECT
         analysis_year,
         atc3,
         COUNT(DISTINCT patid)::BIGINT AS n_patient_years_with_atc3
       FROM ", episodes_identifier, "
         WHERE analysis_year IN (", sql_values(config$analysis_years), ")
           AND atc3 IS NOT NULL
           AND atc3 <> ''
           AND episode_start_clipped IS NOT NULL
           AND episode_end_clipped IS NOT NULL
           AND episode_end_clipped >= episode_start_clipped
       GROUP BY analysis_year, atc3
     ),
     top_atc3 AS (
       SELECT analysis_year, atc3
       FROM (
         SELECT
           *,
           ROW_NUMBER() OVER (
             PARTITION BY analysis_year
             ORDER BY n_patient_years_with_atc3 DESC, atc3
           ) AS atc3_rank
         FROM top_atc3_summary
       ) ranked
       WHERE atc3_rank <= ", top_atc3_n, "
     ),
     exposed AS (
       SELECT DISTINCT
         ids.analysis_year,
         ids.rx_insurance_group,
         ids.rx_insurance_segment,
         e.atc3,
         ids.patid
       FROM ", ids_identifier, " ids
       INNER JOIN ", episodes_identifier, " e
         ON ids.patid = e.patid
        AND ids.analysis_year = e.analysis_year
       WHERE ids.analysis_year IN (", sql_values(config$analysis_years), ")
         AND e.atc3 IS NOT NULL
         AND e.atc3 <> ''
         AND e.episode_start_clipped IS NOT NULL
         AND e.episode_end_clipped IS NOT NULL
         AND e.episode_end_clipped >= e.episode_start_clipped
         AND EXISTS (
           SELECT 1
           FROM top_atc3 t
           WHERE t.analysis_year = e.analysis_year
             AND t.atc3 = e.atc3
         )
     ),
     atc3_summary AS (
       SELECT
         analysis_year,
         rx_insurance_group,
         rx_insurance_segment,
         atc3,
         COUNT(DISTINCT patid)::BIGINT AS n_patient_years_with_atc3
       FROM exposed
       GROUP BY analysis_year, rx_insurance_group, rx_insurance_segment, atc3
     )
     SELECT
       s.analysis_year,
       s.rx_insurance_group,
       s.rx_insurance_segment,
       s.atc3,
       s.n_patient_years_with_atc3,
       d.n_eligible_patient_years,
       CAST(
         100.0 * s.n_patient_years_with_atc3::DOUBLE PRECISION /
           NULLIF(d.n_eligible_patient_years, 0)
         AS DECIMAL(18, 4)
       ) AS prevalence_pct
     FROM atc3_summary s
     INNER JOIN denominator d
       ON s.analysis_year = d.analysis_year
      AND COALESCE(s.rx_insurance_group, '<NULL>') =
        COALESCE(d.rx_insurance_group, '<NULL>')
      AND COALESCE(s.rx_insurance_segment, '<NULL>') =
        COALESCE(d.rx_insurance_segment, '<NULL>')
     ORDER BY
       s.analysis_year,
       s.rx_insurance_group,
       s.rx_insurance_segment,
       s.n_patient_years_with_atc3 DESC,
       s.atc3"
    )
  )
)

insurance_prevalence$atc3_label <- atc3_label_for(insurance_prevalence$atc3)
insurance_prevalence <- insurance_prevalence[
  c(
    "analysis_year",
    "rx_insurance_group",
    "rx_insurance_segment",
    "atc3",
    "atc3_label",
    "n_patient_years_with_atc3",
    "n_eligible_patient_years",
    "prevalence_pct"
  )
]

insurance_suppressed <- (
  is.na(insurance_prevalence$n_eligible_patient_years) |
    insurance_prevalence$n_eligible_patient_years < min_count |
    is.na(insurance_prevalence$n_patient_years_with_atc3) |
    insurance_prevalence$n_patient_years_with_atc3 < min_count
)
insurance_prevalence$suppression_applied <- ifelse(
  insurance_suppressed,
  "yes",
  "no"
)
insurance_prevalence <- suppress_count_columns(
  insurance_prevalence,
  insurance_suppressed,
  c(
    "n_patient_years_with_atc3",
    "n_eligible_patient_years",
    "prevalence_pct"
  )
)

atc3_path <- file.path(
  config$output_dir,
  "5.8_annual_polypharmacy_atc3_prevalence.csv"
)
insurance_path <- file.path(
  config$output_dir,
  "5.8_annual_polypharmacy_atc3_prevalence_by_rx_insurance.csv"
)

message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "START Write ATC3 prevalence CSVs")
utils::write.csv(atc3_prevalence, atc3_path, row.names = FALSE)
utils::write.csv(insurance_prevalence, insurance_path, row.names = FALSE)
message(format(Sys.time(), "[%Y-%m-%d %H:%M:%S] "), "DONE  Write ATC3 prevalence CSVs")

message("ATC3 prevalence rows: ", nrow(atc3_prevalence))
message("ATC3 insurance prevalence rows: ", nrow(insurance_prevalence))
message(
  config$workflow_label,
  " report-ready ATC3 prevalence summaries written to: ",
  atc3_path,
  " and ",
  insurance_path
)

disconnect_komodo(con)
