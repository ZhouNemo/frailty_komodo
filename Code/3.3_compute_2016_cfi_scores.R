library(ohdsilab)
library(DatabaseConnector)
library(keyring)
library(DBI)

# Project: Frailty_Komoto 2016 CFI scoring
# Author: Nemo Zhou
# Date started: 2026-06-15
# Date last updated: 2026-06-15
#
# ---- Purpose ----
# Compute Claims-Based Frailty Index (CFI) scores for the 2016 validation
# population prepared by Code/3.1_prepare_2016_cfi_inputs.R.
#
# The script keeps patient-level data in Redshift. It stages the official CFI
# lookup files from the local reference package, maps diagnosis and CPT/HCPCS
# codes to disease numbers, applies the model weights, adds the model
# intercept, and materializes one score per 2016 patient-year in:
#   - cfi_2016_scores
#
# Only aggregate QA and distribution summaries are written to Outputs.

# ---- Connection settings ----
Sys.setenv(
  "DATABASECONNECTOR_JAR_FOLDER" = "D:/Users/xia.zhou/Documents/JDBC Driver"
)

komodo_schema <- "komodo_ext"
write_schema <- paste0("work_", keyring::key_get("db_username"))

# ---- Analysis parameters ----
analysis_year <- 2016L
model_intercept <- 0.10288
min_count <- 11L

ids_table <- "cfi_2016_ids"
dx09_table <- "cfi_2016_dx09"
dx10_table <- "cfi_2016_dx10"
px_table <- "cfi_2016_px"
scores_table <- "cfi_2016_scores"
reuse_existing_scores <- isTRUE(
  getOption("frailty.cfi.reuse_existing_scores", FALSE)
)

lookup_icd9_table <- "cfi_lookup_icd9cm_v32"
lookup_icd10_table <- "cfi_lookup_icd10cm_v2020"
lookup_px_table <- "cfi_lookup_px"
lookup_weight_table <- "cfi_lookup_disease_weight"

cfi_package_dir <- getOption(
  "frailty.cfi.package_dir",
  "D:/Users/xia.zhou/Documents/Frailty_Komoto/Documents/CFI"
)
lookup_dir <- file.path(cfi_package_dir, "Required files to calculate CFI")
output_dir <- file.path(getwd(), "Outputs")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Connect to Redshift ----
con <- ohdsilab_connect(
  username = keyring::key_get("db_username"),
  password = keyring::key_get("db_password")
)

options(con.default.value = con)
options(schema.default.value = komodo_schema)
options(write_schema.default.value = write_schema)

# ---- Helpers ----
quote_identifier <- function(identifier) {
  paste0('"', gsub('"', '""', identifier, fixed = TRUE), '"')
}

qualified_identifier <- function(schema, table) {
  paste(
    quote_identifier(schema),
    quote_identifier(table),
    sep = "."
  )
}

sql_string <- function(value) {
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

sql_number <- function(value) {
  ifelse(is.na(value), "NULL", as.character(value))
}

print_query <- function(label, sql) {
  message(label)
  result <- DBI::dbGetQuery(con, sql)
  print(result)
  invisible(result)
}

table_exists <- function(schema, table) {
  result <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*)::INTEGER AS table_count
       FROM information_schema.tables
       WHERE table_schema = ", sql_string(schema), "
         AND table_name = ", sql_string(table)
    )
  )

  nrow(result) == 1L &&
    !is.na(result$table_count[[1]]) &&
    result$table_count[[1]] == 1L
}

table_has_columns <- function(schema, table, columns) {
  result <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT LOWER(column_name) AS column_name
       FROM information_schema.columns
       WHERE table_schema = ", sql_string(schema), "
         AND table_name = ", sql_string(table)
    )
  )

  missing_columns <- setdiff(tolower(columns), result$column_name)
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns in ",
      schema,
      ".",
      table,
      ": ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

normalize_code <- function(x) {
  gsub("[^A-Z0-9]", "", toupper(trimws(as.character(x))))
}

execute_insert_batches <- function(
  table_identifier,
  columns,
  data,
  numeric_columns = character(),
  chunk_size = 1000L
) {
  if (nrow(data) == 0L) {
    return(invisible(NULL))
  }

  column_sql <- paste(quote_identifier(columns), collapse = ", ")
  starts <- seq.int(1L, nrow(data), by = chunk_size)

  for (start_row in starts) {
    end_row <- min(start_row + chunk_size - 1L, nrow(data))
    chunk <- data[start_row:end_row, columns, drop = FALSE]

    values <- apply(
      chunk,
      1L,
      function(row) {
        paste0(
          "(",
          paste(
            vapply(
              names(row),
              function(column) {
                value <- row[[column]]
                if (is.na(value)) {
                  "NULL"
                } else if (column %in% numeric_columns) {
                  value
                } else {
                  sql_string(value)
                }
              },
              character(1)
            ),
            collapse = ", "
          ),
          ")"
        )
      }
    )

    DatabaseConnector::executeSql(
      con,
      paste0(
        "INSERT INTO ",
        table_identifier,
        " (",
        column_sql,
        ") VALUES ",
        paste(values, collapse = ", "),
        ";"
      )
    )
  }
}

read_icd_lookup <- function(path) {
  lookup <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    colClasses = "character"
  )

  required_columns <- c("disease_number", "dx")
  missing_columns <- setdiff(required_columns, names(lookup))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required ICD lookup columns in ",
      path,
      ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  lookup <- lookup[, required_columns]
  lookup$disease_number <- as.integer(lookup$disease_number)
  lookup$dx <- normalize_code(lookup$dx)
  lookup <- lookup[
    !is.na(lookup$disease_number) &
      !is.na(lookup$dx) &
      lookup$dx != "",
  ]
  unique(lookup)
}

read_px_lookup <- function(path) {
  lookup <- utils::read.delim(
    path,
    sep = "\t",
    header = TRUE,
    comment.char = "#",
    stringsAsFactors = FALSE,
    colClasses = "character"
  )

  required_columns <- c("start", "stop", "disease_number")
  missing_columns <- setdiff(required_columns, names(lookup))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required procedure lookup columns in ",
      path,
      ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  lookup <- lookup[, required_columns]
  lookup$lookup_order <- seq_len(nrow(lookup))
  lookup$start_code <- normalize_code(lookup$start)
  lookup$stop_code <- normalize_code(lookup$stop)
  lookup$disease_number <- as.integer(lookup$disease_number)
  lookup <- lookup[
    !is.na(lookup$disease_number) &
      lookup$start_code != "" &
      lookup$stop_code != "",
    c("lookup_order", "start_code", "stop_code", "disease_number")
  ]
  unique(lookup)
}

read_weight_lookup <- function(path) {
  lookup <- utils::read.delim(
    path,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    colClasses = "character"
  )

  required_columns <- c("disease_number", "weight")
  missing_columns <- setdiff(required_columns, names(lookup))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required weight lookup columns in ",
      path,
      ": ",
      paste(missing_columns, collapse = ", ")
    )
  }

  lookup <- lookup[, required_columns]
  lookup$disease_number <- as.integer(lookup$disease_number)
  lookup$weight <- as.numeric(lookup$weight)
  lookup <- lookup[!is.na(lookup$disease_number) & !is.na(lookup$weight), ]
  unique(lookup)
}

load_icd_lookup <- function(table, data) {
  table_identifier <- qualified_identifier(write_schema, table)

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", table_identifier, ";
       CREATE TABLE ", table_identifier, " (
         disease_number INTEGER NOT NULL,
         dx VARCHAR(32) NOT NULL
       )
       DISTSTYLE ALL
       SORTKEY(dx);"
    )
  )

  data$disease_number <- as.character(data$disease_number)
  execute_insert_batches(
    table_identifier,
    c("disease_number", "dx"),
    data,
    numeric_columns = "disease_number"
  )
}

load_px_lookup <- function(table, data) {
  table_identifier <- qualified_identifier(write_schema, table)

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", table_identifier, ";
       CREATE TABLE ", table_identifier, " (
         lookup_order INTEGER NOT NULL,
         start_code VARCHAR(16) NOT NULL,
         stop_code VARCHAR(16) NOT NULL,
         disease_number INTEGER NOT NULL
       )
       DISTSTYLE ALL
       SORTKEY(start_code, stop_code);"
    )
  )

  data$lookup_order <- as.character(data$lookup_order)
  data$disease_number <- as.character(data$disease_number)
  execute_insert_batches(
    table_identifier,
    c("lookup_order", "start_code", "stop_code", "disease_number"),
    data,
    numeric_columns = c("lookup_order", "disease_number")
  )
}

load_weight_lookup <- function(table, data) {
  table_identifier <- qualified_identifier(write_schema, table)

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", table_identifier, ";
       CREATE TABLE ", table_identifier, " (
         disease_number INTEGER NOT NULL,
         weight DOUBLE PRECISION NOT NULL
       )
       DISTSTYLE ALL
       SORTKEY(disease_number);"
    )
  )

  data$disease_number <- as.character(data$disease_number)
  data$weight <- as.character(data$weight)
  execute_insert_batches(
    table_identifier,
    c("disease_number", "weight"),
    data,
    numeric_columns = c("disease_number", "weight")
  )
}

as_metric_table <- function(result) {
  data.frame(
    metric = names(result),
    value = vapply(
      names(result),
      function(column) as.character(result[[column]][[1]]),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

suppress_small_count <- function(count, min_count) {
  count_numeric <- as.numeric(count)
  ifelse(!is.na(count_numeric) & count_numeric > 0 & count_numeric < min_count,
         NA_real_, count_numeric)
}

format_output_value <- function(value) {
  if (is.na(value)) {
    return(NA_character_)
  }

  formatted <- format(value, scientific = FALSE, trim = TRUE)
  if (grepl("^0(\\.0+)?$", formatted)) {
    return("0")
  }

  formatted <- sub("(\\.\\d*?)0+$", "\\1", formatted)
  sub("\\.$", "", formatted)
}

# ---- Validate inputs ----
required_input_tables <- c(ids_table, dx09_table, dx10_table, px_table)
missing_tables <- required_input_tables[
  !vapply(
    required_input_tables,
    function(table) table_exists(write_schema, table),
    logical(1)
  )
]

if (length(missing_tables) > 0L) {
  stop(
    "Missing required 2016 CFI input tables in ",
    write_schema,
    ": ",
    paste(missing_tables, collapse = ", "),
    ". Run Code/3.1_prepare_2016_cfi_inputs.R first."
  )
}

table_has_columns(
  write_schema,
  ids_table,
  c("patid", "patient_id", "analysis_year", "cfi_index_date")
)
table_has_columns(write_schema, dx09_table, c("patid", "dx"))
table_has_columns(write_schema, dx10_table, c("patid", "dx"))
table_has_columns(write_schema, px_table, c("patid", "px"))

required_lookup_files <- file.path(
  lookup_dir,
  c(
    "CFI_ICD9CM_V32.csv",
    "CFI_ICD10CM_V2020.csv",
    "pxlookup.txt",
    "disease_weight.txt"
  )
)
missing_lookup_files <- required_lookup_files[!file.exists(required_lookup_files)]
if (length(missing_lookup_files) > 0L) {
  stop(
    "Missing required CFI lookup files:\n",
    paste(" -", missing_lookup_files, collapse = "\n")
  )
}

ids_table_identifier <- qualified_identifier(write_schema, ids_table)
dx09_table_identifier <- qualified_identifier(write_schema, dx09_table)
dx10_table_identifier <- qualified_identifier(write_schema, dx10_table)
px_table_identifier <- qualified_identifier(write_schema, px_table)
scores_table_identifier <- qualified_identifier(write_schema, scores_table)
lookup_icd9_identifier <- qualified_identifier(write_schema, lookup_icd9_table)
lookup_icd10_identifier <- qualified_identifier(write_schema, lookup_icd10_table)
lookup_px_identifier <- qualified_identifier(write_schema, lookup_px_table)
lookup_weight_identifier <- qualified_identifier(write_schema, lookup_weight_table)

# ---- Stage lookups and score 2016 CFI in Redshift ----
if (reuse_existing_scores && table_exists(write_schema, scores_table)) {
  message(
    "Reusing existing ",
    write_schema,
    ".",
    scores_table,
    " and regenerating aggregate outputs."
  )
} else {
  message("Loading official CFI lookup files into ", write_schema, ".")

  load_icd_lookup(
    lookup_icd9_table,
    read_icd_lookup(file.path(lookup_dir, "CFI_ICD9CM_V32.csv"))
  )
  load_icd_lookup(
    lookup_icd10_table,
    read_icd_lookup(file.path(lookup_dir, "CFI_ICD10CM_V2020.csv"))
  )
  load_px_lookup(
    lookup_px_table,
    read_px_lookup(file.path(lookup_dir, "pxlookup.txt"))
  )
  load_weight_lookup(
    lookup_weight_table,
    read_weight_lookup(file.path(lookup_dir, "disease_weight.txt"))
  )

  message("Computing 2016 CFI scores in Redshift.")

  DatabaseConnector::executeSql(
    con,
    paste0(
      "DROP TABLE IF EXISTS ", scores_table_identifier, ";
       CREATE TABLE ", scores_table_identifier, "
     DISTKEY(patient_id)
     SORTKEY(patid) AS
     WITH unique_dx09 AS (
       SELECT DISTINCT patid, dx
       FROM ", dx09_table_identifier, "
     ),
     unique_dx10 AS (
       SELECT DISTINCT patid, dx
       FROM ", dx10_table_identifier, "
     ),
     unique_px AS (
       SELECT DISTINCT patid, px
       FROM ", px_table_identifier, "
     ),
     dx09_diseases AS (
       SELECT DISTINCT
         dx.patid,
         COALESCE(lkp.disease_number, 0)::INTEGER AS disease_number
       FROM unique_dx09 dx
       LEFT JOIN ", lookup_icd9_identifier, " lkp
         ON dx.dx = lkp.dx
     ),
     dx10_diseases AS (
       SELECT DISTINCT
         dx.patid,
         COALESCE(lkp.disease_number, 0)::INTEGER AS disease_number
       FROM unique_dx10 dx
       LEFT JOIN ", lookup_icd10_identifier, " lkp
         ON dx.dx = lkp.dx
     ),
     px_matches AS (
       SELECT
         px.patid,
         px.px,
         lkp.disease_number,
         ROW_NUMBER() OVER (
           PARTITION BY px.patid, px.px
           ORDER BY lkp.lookup_order DESC
         ) AS lookup_rank
       FROM unique_px px
       INNER JOIN ", lookup_px_identifier, " lkp
         ON px.px >= lkp.start_code
        AND px.px <= lkp.stop_code
       WHERE px.px ~ '^[A-Z0-9]{4}[0-9]$'
     ),
     px_diseases AS (
       SELECT DISTINCT
         px.patid,
         CASE
           WHEN px.px !~ '^[A-Z0-9]{4}[0-9]$' THEN 0
           ELSE COALESCE(match.disease_number, 0)
         END::INTEGER AS disease_number
       FROM unique_px px
       LEFT JOIN (
         SELECT patid, px, disease_number
         FROM px_matches
         WHERE lookup_rank = 1
       ) match
         ON px.patid = match.patid
        AND px.px = match.px
     ),
     all_diseases AS (
       SELECT patid, disease_number FROM dx09_diseases
       UNION
       SELECT patid, disease_number FROM dx10_diseases
       UNION
       SELECT patid, disease_number FROM px_diseases
       UNION
       SELECT patid, 0::INTEGER AS disease_number
       FROM ", ids_table_identifier, "
       WHERE analysis_year = ", analysis_year, "
     ),
     weighted_scores AS (
       SELECT
         disease.patid,
         COUNT(DISTINCT CASE
           WHEN disease.disease_number > 0 THEN disease.disease_number
           ELSE NULL
         END)::INTEGER AS recognized_disease_count,
         SUM(COALESCE(weight.weight, 0))::DOUBLE PRECISION AS disease_weight_sum
       FROM all_diseases disease
       LEFT JOIN ", lookup_weight_identifier, " weight
         ON disease.disease_number = weight.disease_number
       GROUP BY disease.patid
     )
     SELECT
       ids.patid,
       ids.patient_id,
       ids.analysis_year,
       ids.cfi_index_date,
       COALESCE(score.recognized_disease_count, 0)::INTEGER
         AS recognized_disease_count,
       COALESCE(score.disease_weight_sum, 0)::DOUBLE PRECISION
         AS disease_weight_sum,
       (", sql_number(model_intercept), " +
         COALESCE(score.disease_weight_sum, 0))::DOUBLE PRECISION
         AS frailty_index
     FROM ", ids_table_identifier, " ids
     LEFT JOIN weighted_scores score
       ON ids.patid = score.patid
     WHERE ids.analysis_year = ", analysis_year, ";"
    )
  )
}

# ---- Runtime QA ----
input_counts <- print_query(
  "Checking 2016 CFI input row counts.",
  paste0(
    "SELECT
       (SELECT COUNT(*) FROM ", ids_table_identifier, "
        WHERE analysis_year = ", analysis_year, ")::INTEGER AS id_rows,
       (SELECT COUNT(*) FROM ", dx09_table_identifier, ")::INTEGER AS dx09_rows,
       (SELECT COUNT(*) FROM ", dx10_table_identifier, ")::INTEGER AS dx10_rows,
       (SELECT COUNT(*) FROM ", px_table_identifier, ")::INTEGER AS px_rows"
  )
)

code_mapping_counts <- print_query(
  "Checking 2016 CFI code recognition counts.",
  paste0(
    "WITH unique_dx09 AS (
       SELECT DISTINCT patid, dx
       FROM ", dx09_table_identifier, "
     ),
     unique_dx10 AS (
       SELECT DISTINCT patid, dx
       FROM ", dx10_table_identifier, "
     ),
     unique_px AS (
       SELECT DISTINCT patid, px
       FROM ", px_table_identifier, "
     ),
     px_recognition AS (
       SELECT
         px.patid,
         px.px,
         MAX(CASE
           WHEN px.px ~ '^[A-Z0-9]{4}[0-9]$'
            AND lkp.disease_number IS NOT NULL THEN 1
           ELSE 0
         END) AS recognized
       FROM unique_px px
       LEFT JOIN ", lookup_px_identifier, " lkp
         ON px.px >= lkp.start_code
        AND px.px <= lkp.stop_code
       GROUP BY px.patid, px.px
     )
     SELECT
       (SELECT COUNT(*)
        FROM unique_dx09 dx
        INNER JOIN ", lookup_icd9_identifier, " lkp
          ON dx.dx = lkp.dx)::INTEGER AS recognized_dx09_codes,
       (SELECT COUNT(*)
        FROM unique_dx09 dx
        LEFT JOIN ", lookup_icd9_identifier, " lkp
          ON dx.dx = lkp.dx
        WHERE lkp.dx IS NULL)::INTEGER AS unrecognized_dx09_codes,
       (SELECT COUNT(*)
        FROM unique_dx10 dx
        INNER JOIN ", lookup_icd10_identifier, " lkp
          ON dx.dx = lkp.dx)::INTEGER AS recognized_dx10_codes,
       (SELECT COUNT(*)
        FROM unique_dx10 dx
        LEFT JOIN ", lookup_icd10_identifier, " lkp
          ON dx.dx = lkp.dx
        WHERE lkp.dx IS NULL)::INTEGER AS unrecognized_dx10_codes,
       (SELECT COALESCE(SUM(recognized), 0)::INTEGER
        FROM px_recognition)::INTEGER AS recognized_px_codes,
       (SELECT (COUNT(*) - COALESCE(SUM(recognized), 0))::INTEGER
        FROM px_recognition)::INTEGER AS unrecognized_px_codes"
  )
)

score_integrity <- print_query(
  "Checking 2016 CFI score integrity.",
  paste0(
    "SELECT
       (SELECT COUNT(*) FROM ", ids_table_identifier, "
        WHERE analysis_year = ", analysis_year, ")::INTEGER AS id_rows,
       (SELECT COUNT(*) FROM ", scores_table_identifier, ")::INTEGER
         AS score_rows,
       (SELECT COUNT(*) - COUNT(DISTINCT patid)
        FROM ", scores_table_identifier, ")::INTEGER AS duplicate_score_patids,
       (SELECT COUNT(*) FROM ", scores_table_identifier, "
        WHERE frailty_index IS NULL)::INTEGER AS missing_scores,
       (SELECT COUNT(*) FROM ", scores_table_identifier, "
        WHERE analysis_year <> ", analysis_year, ")::INTEGER AS non_2016_scores"
  )
)

if (
  score_integrity$id_rows[[1]] != score_integrity$score_rows[[1]] ||
    score_integrity$duplicate_score_patids[[1]] != 0 ||
    score_integrity$missing_scores[[1]] != 0 ||
    score_integrity$non_2016_scores[[1]] != 0
) {
  stop("One or more 2016 CFI score integrity checks failed.")
}

disease_count_summary <- print_query(
  "Summarizing recognized disease counts.",
  paste0(
    "SELECT
       recognized_disease_count,
       COUNT(*)::INTEGER AS patient_years
     FROM ", scores_table_identifier, "
     GROUP BY recognized_disease_count
     ORDER BY recognized_disease_count"
  )
)

scoring_qa <- rbind(
  data.frame(section = "input_counts", as_metric_table(input_counts)),
  data.frame(section = "code_mapping", as_metric_table(code_mapping_counts)),
  data.frame(section = "score_integrity", as_metric_table(score_integrity)),
  data.frame(
    section = "recognized_disease_count_distribution",
    metric = paste0(
      "recognized_disease_count_",
      disease_count_summary$recognized_disease_count
    ),
    value = as.character(
      suppress_small_count(disease_count_summary$patient_years, min_count)
    )
  )
)

scoring_qa$suppressed <- is.na(scoring_qa$value)
write.csv(
  scoring_qa,
  file.path(output_dir, "3.3_cfi_2016_scoring_qa.csv"),
  row.names = FALSE,
  na = ""
)

# ---- Aggregate score distribution summaries ----
base_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       COUNT(*)::INTEGER AS total_observations,
       COUNT(frailty_index)::INTEGER AS nonmissing_cfi,
       (COUNT(*) - COUNT(frailty_index))::INTEGER AS missing_cfi,
       AVG(frailty_index)::DOUBLE PRECISION AS mean,
       STDDEV_SAMP(frailty_index)::DOUBLE PRECISION AS standard_deviation,
       MIN(frailty_index)::DOUBLE PRECISION AS minimum,
       MAX(frailty_index)::DOUBLE PRECISION AS maximum
     FROM ", scores_table_identifier
  )
)

maximum_score <- suppressWarnings(as.numeric(base_summary$maximum[[1]]))
if (is.na(maximum_score) || maximum_score <= 0) {
  stop(
    "The maximum CFI score is not positive. This is not compatible with ",
    "the CFI model intercept and indicates that ",
    write_schema,
    ".",
    scores_table,
    " should be checked or recomputed before using the summary outputs."
  )
}

percentile_specs <- data.frame(
  statistic = c(
    "1st percentile", "5th percentile", "10th percentile",
    "25th percentile", "Median", "75th percentile",
    "90th percentile", "95th percentile", "99th percentile"
  ),
  probability = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
  stringsAsFactors = FALSE
)

percentile_values <- vapply(
  percentile_specs$probability,
  function(probability) {
    result <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT DISTINCT CAST(PERCENTILE_CONT(",
        probability,
        ") WITHIN GROUP (ORDER BY frailty_index) OVER ()
          AS DOUBLE PRECISION) AS value
         FROM ",
        scores_table_identifier,
        "
         WHERE frailty_index IS NOT NULL"
      )
    )
    result$value[[1]]
  },
  numeric(1)
)

descriptive_summary <- data.frame(
  statistic = c(
    "Total observations", "Nonmissing CFI", "Missing CFI",
    "Mean", "Standard deviation", "Minimum",
    percentile_specs$statistic,
    "Maximum"
  ),
  value = c(
    as.character(base_summary$total_observations[[1]]),
    as.character(base_summary$nonmissing_cfi[[1]]),
    as.character(base_summary$missing_cfi[[1]]),
    format_output_value(base_summary$mean[[1]]),
    format_output_value(base_summary$standard_deviation[[1]]),
    format_output_value(base_summary$minimum[[1]]),
    vapply(percentile_values, format_output_value, character(1)),
    format_output_value(base_summary$maximum[[1]])
  )
)

write.csv(
  descriptive_summary,
  file.path(output_dir, "3.3_cfi_2016_descriptive_summary.csv"),
  row.names = FALSE,
  na = ""
)

category_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT
       CASE
         WHEN frailty_index < 0.15 THEN 'Non-frail (<0.15)'
         WHEN frailty_index < 0.25 THEN 'Prefrail (0.15 to <0.25)'
         WHEN frailty_index < 0.35 THEN 'Mildly frail (0.25 to <0.35)'
         WHEN frailty_index < 0.45 THEN 'Moderately frail (0.35 to <0.45)'
         ELSE 'Severely frail (>=0.45)'
       END AS frailty_category,
       COUNT(*)::INTEGER AS patient_years,
       100.0 * COUNT(*) / SUM(COUNT(*)) OVER () AS percent_of_patient_years
     FROM ", scores_table_identifier, "
     GROUP BY 1
     ORDER BY
       MIN(CASE
         WHEN frailty_index < 0.15 THEN 1
         WHEN frailty_index < 0.25 THEN 2
         WHEN frailty_index < 0.35 THEN 3
         WHEN frailty_index < 0.45 THEN 4
         ELSE 5
       END)"
  )
)

category_summary$count_suppressed <- (
  category_summary$patient_years > 0 &
    category_summary$patient_years < min_count
)
category_summary$patient_years <- suppress_small_count(
  category_summary$patient_years,
  min_count
)
category_summary$percent_of_patient_years[
  category_summary$count_suppressed
] <- NA_real_

write.csv(
  category_summary,
  file.path(output_dir, "3.3_cfi_2016_category_summary.csv"),
  row.names = FALSE,
  na = ""
)

message(
  "2016 CFI scoring complete. Patient-year scores are in ",
  write_schema,
  ".",
  scores_table,
  "."
)
message(
  "Aggregate QA and summaries written to: ",
  normalizePath(output_dir, winslash = "/", mustWork = FALSE)
)
