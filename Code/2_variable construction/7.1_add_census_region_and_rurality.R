source("Code/2_variable construction/6.0_annual_healthcare_utilization_helpers.R")

# Project: Frailty_Komoto healthcare utilization and geography enrichment
# Author: Nemo Zhou
# Date started: 2026-07-17
# Date last updated: 2026-08-04
#
# ---- Purpose ----
# Add Census region and ZIP3-derived rurality variables to the completed 2022
# comparison healthcare-utilization analysis table in place. Census region is assigned from
# patient residence state. Rurality uses the versioned state-plus-ZIP3 lookup
# prepared from the supplied 2023 RUCC workbooks and ZIP-county crosswalk.
# Because KRD patient_zip contains only the first three ZIP digits, the lookup
# is joined by both patient_state and the three-digit patient_zip.
#
# After assigning both geography variables, this script excludes patient-years
# with a missing or Unknown Census region or rurality from the final comparison
# table. The upstream cohort, clinical-metrics, and utilization tables retain
# those patient-years. The pre-filter geography counts and excluded-row count
# are reported in aggregate QA. A matched reference row labeled Unknown is
# also excluded from the final comparison table.

config <- get_annual_healthcare_utilization_config()
if (!identical(config$analysis_year, 2022L)) {
  stop("This geography-enrichment script is configured for analysis_year = 2022 only.")
}

target_schema <- write_schema
target_table <- config$combined_table
target_identifier <- qualified_identifier(target_schema, target_table)
qa_path <- file.path(
  config$output_dir,
  paste0("7.1_census_region_rurality_qa_", config$analysis_year, ".csv")
)
rurality_lookup_path <- if (is.null(config$rurality_lookup_path)) {
  file.path(
    getwd(),
    "Documents",
    "Rurality Reference Tables",
    "rucc_2023_zip3_rurality_lookup.csv"
  )
} else {
  as.character(config$rurality_lookup_path)
}
if (
  length(rurality_lookup_path) != 1L ||
    is.na(rurality_lookup_path) ||
    !nzchar(rurality_lookup_path)
) {
  stop("rurality_lookup_path must be one nonempty file path.")
}

derived_column_types <- c(
  census_region = "VARCHAR(16)",
  rurality_primary_rucc = "INTEGER",
  rurality_group = "VARCHAR(16)",
  rurality_zip3_mixed_rucc_flag = "BOOLEAN",
  rurality_zip3_n_zip5 = "INTEGER",
  rurality_assignment_method = "VARCHAR(64)"
)

parse_nullable_integer <- function(values, column_name) {
  values <- trimws(as.character(values))
  values[is.na(values) | !nzchar(values)] <- NA_character_
  parsed <- suppressWarnings(as.integer(values))
  invalid <- !is.na(values) & is.na(parsed)
  if (any(invalid)) {
    stop(
      "Rurality lookup column '", column_name,
      "' contains invalid integer values."
    )
  }
  parsed
}

parse_logical_column <- function(values, column_name) {
  values <- tolower(trimws(as.character(values)))
  parsed <- rep(NA, length(values))
  parsed[values %in% c("true", "t", "1")] <- TRUE
  parsed[values %in% c("false", "f", "0")] <- FALSE
  invalid <- !is.na(values) & nzchar(values) & is.na(parsed)
  if (any(invalid)) {
    stop(
      "Rurality lookup column '", column_name,
      "' contains values other than TRUE/FALSE."
    )
  }
  parsed
}

read_rurality_reference_lookup <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Required rurality lookup was not found: ", path,
      ". Run Code/0_test/0.9_build_rucc_2023_zip3_rurality_lookup.R first."
    )
  }

  lookup <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character()
  )
  required_columns <- c(
    "state",
    "zip3",
    "rurality_group",
    "assignment_method",
    "modal_rucc_2023",
    "n_zip5",
    "rurality_mixed_flag"
  )
  missing_columns <- setdiff(required_columns, names(lookup))
  if (length(missing_columns) > 0L) {
    stop(
      "Rurality lookup is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  lookup$state <- toupper(trimws(lookup$state))
  lookup$zip3 <- trimws(lookup$zip3)
  if (any(is.na(lookup$state) | !grepl("^[A-Z]{2}$", lookup$state))) {
    stop("Rurality lookup contains an invalid state code.")
  }
  if (any(is.na(lookup$zip3) | !grepl("^[0-9]{3}$", lookup$zip3))) {
    stop("Rurality lookup contains an invalid three-digit ZIP code.")
  }
  allowed_groups <- c("Metro", "Urban", "Rural", "Unknown")
  if (any(is.na(lookup$rurality_group) | !lookup$rurality_group %in% allowed_groups)) {
    stop(
      "Rurality lookup contains values outside: ",
      paste(allowed_groups, collapse = ", ")
    )
  }
  if (any(is.na(lookup$assignment_method) | !nzchar(trimws(lookup$assignment_method)))) {
    stop("Rurality lookup contains a missing assignment_method.")
  }
  if (anyDuplicated(paste(lookup$state, lookup$zip3, sep = "|"))) {
    stop("Rurality lookup contains duplicate state-plus-ZIP3 keys.")
  }

  lookup$modal_rucc_2023 <- parse_nullable_integer(
    lookup$modal_rucc_2023,
    "modal_rucc_2023"
  )
  lookup$n_zip5 <- parse_nullable_integer(lookup$n_zip5, "n_zip5")
  if (any(is.na(lookup$n_zip5) | lookup$n_zip5 < 1L)) {
    stop("Rurality lookup n_zip5 must be positive for every row.")
  }
  lookup$rurality_mixed_flag <- parse_logical_column(
    lookup$rurality_mixed_flag,
    "rurality_mixed_flag"
  )
  if (anyNA(lookup$rurality_mixed_flag)) {
    stop("Rurality lookup contains a missing rurality_mixed_flag.")
  }

  lookup[, c(
    "state",
    "zip3",
    "modal_rucc_2023",
    "rurality_group",
    "rurality_mixed_flag",
    "n_zip5",
    "assignment_method"
  )]
}

sql_nullable_integer <- function(value) {
  if (is.na(value)) "NULL::INTEGER" else paste0(value, "::INTEGER")
}

build_lookup_select_sql <- function(lookup) {
  if (nrow(lookup) == 0L) {
    stop("The state-plus-ZIP3 rurality lookup is empty.")
  }
  rows <- vapply(seq_len(nrow(lookup)), function(index) {
    values <- paste0(
      sql_string(lookup$state[[index]]), ", ",
      sql_string(lookup$zip3[[index]]), ", ",
      sql_nullable_integer(lookup$modal_rucc_2023[[index]]), ", ",
      sql_string(lookup$rurality_group[[index]]), ", ",
      if (isTRUE(lookup$rurality_mixed_flag[[index]])) "TRUE" else "FALSE",
      ", ",
      lookup$n_zip5[[index]], ", ",
      sql_string(lookup$assignment_method[[index]])
    )
    if (index == 1L) {
      paste0(
        "SELECT ",
        sql_string(lookup$state[[index]]), "::VARCHAR(2) AS state, ",
        sql_string(lookup$zip3[[index]]), "::VARCHAR(3) AS zip3, ",
        sql_nullable_integer(lookup$modal_rucc_2023[[index]]),
        " AS rurality_primary_rucc, ",
        sql_string(lookup$rurality_group[[index]]), "::VARCHAR(16) AS rurality_group, ",
        if (isTRUE(lookup$rurality_mixed_flag[[index]])) "TRUE" else "FALSE",
        "::BOOLEAN AS rurality_zip3_mixed_rucc_flag, ",
        lookup$n_zip5[[index]], "::INTEGER AS rurality_zip3_n_zip5, ",
        sql_string(lookup$assignment_method[[index]]),
        "::VARCHAR(64) AS rurality_assignment_method"
      )
    } else {
      paste0("SELECT ", values)
    }
  }, character(1))
  paste(rows, collapse = "\n      UNION ALL\n      ")
}

state_region_sql <- paste0(
  "CASE\n",
  "  WHEN UPPER(TRIM(patient_state)) IN (",
  paste(sql_string(c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA")), collapse = ", "),
  ") THEN 'Northeast'\n",
  "  WHEN UPPER(TRIM(patient_state)) IN (",
  paste(sql_string(c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")), collapse = ", "),
  ") THEN 'Midwest'\n",
  "  WHEN UPPER(TRIM(patient_state)) IN (",
  paste(sql_string(c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX")), collapse = ", "),
  ") THEN 'South'\n",
  "  WHEN UPPER(TRIM(patient_state)) IN (",
  paste(sql_string(c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")), collapse = ", "),
  ") THEN 'West'\n",
  "  ELSE NULL\n",
  "END"
)

lookup <- read_rurality_reference_lookup(rurality_lookup_path)
lookup_select_sql <- build_lookup_select_sql(lookup)

con <- NULL
tryCatch(
  {
    con <- connect_komodo()

    if (!table_exists(con, target_schema, target_table)) {
      stop(
        "Required completed 6.6 table was not found: ",
        target_schema,
        ".",
        target_table,
        ". Run 6.6 first."
      )
    }

    table_has_columns(
      con,
      target_schema,
      target_table,
      c("patient_id", "analysis_year", "patient_state", "patient_zip")
    )

    existing_columns <- tolower(names(DBI::dbGetQuery(
      con,
      paste0("SELECT * FROM ", target_identifier, " LIMIT 0")
    )))

    legacy_column_renames <- c(
      rurality_primary_ruca = "rurality_primary_rucc",
      rurality_zip3_mixed_flag = "rurality_zip3_mixed_rucc_flag",
      rurality_zip3_n_zctas = "rurality_zip3_n_zip5"
    )
    for (legacy_column in names(legacy_column_renames)) {
      replacement_column <- unname(legacy_column_renames[[legacy_column]])
      legacy_present <- legacy_column %in% existing_columns
      replacement_present <- replacement_column %in% existing_columns
      if (legacy_present && replacement_present) {
        stop(
          "Cannot migrate legacy column '", legacy_column,
          "' because replacement column '", replacement_column,
          "' already exists in ", target_identifier, "."
        )
      }
      if (legacy_present) {
        utilization_run_sql_stage(
          con,
          paste0("rename ", legacy_column, " to ", replacement_column),
          paste0(
            "ALTER TABLE ", target_identifier,
            " RENAME COLUMN ", quote_identifier(legacy_column),
            " TO ", quote_identifier(replacement_column), ";"
          )
        )
        existing_columns[existing_columns == legacy_column] <- replacement_column
      }
    }

    missing_derived_columns <- setdiff(names(derived_column_types), existing_columns)
    if (length(missing_derived_columns) > 0L) {
      for (column in missing_derived_columns) {
        utilization_run_sql_stage(
          con,
          paste0("add ", column, " to ", target_table),
          paste0(
            "ALTER TABLE ", target_identifier,
            " ADD COLUMN ", quote_identifier(column),
            " ", derived_column_types[[column]], ";"
          )
        )
      }
    }

    before <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*)::BIGINT AS n_rows,\n",
        "       COUNT(*) - COUNT(DISTINCT patient_id || '|' || analysis_year::VARCHAR)\n",
        "         AS duplicate_patient_year_rows\n",
        "FROM ", target_identifier, "\n",
        "WHERE analysis_year = ", config$analysis_year
      )
    )
    if (before$duplicate_patient_year_rows[[1]] != 0) {
      stop("The 6.6 table already contains duplicate patient-year rows.")
    }

    update_sql <- paste0(
      "WITH zip3_lookup (\n",
      "  state, zip3, rurality_primary_rucc, rurality_group,\n",
      "  rurality_zip3_mixed_rucc_flag, rurality_zip3_n_zip5,\n",
      "  rurality_assignment_method\n",
      ") AS (\n",
      "      ", lookup_select_sql, "\n",
      "), source_rows AS (\n",
      "  SELECT\n",
      "    patient_id,\n",
      "    analysis_year,\n",
      "    ", state_region_sql, " AS census_region,\n",
      "    UPPER(NULLIF(TRIM(patient_state), '')) AS patient_state_key,\n",
      "    CASE\n",
      "      WHEN TRIM(patient_zip) ~ '^[0-9]{3}$' THEN TRIM(patient_zip)\n",
      "      ELSE NULL\n",
      "    END AS patient_zip3\n",
      "  FROM ", target_identifier, "\n",
      "  WHERE analysis_year = ", config$analysis_year, "\n",
      "), derived AS (\n",
      "  SELECT\n",
      "    source_rows.patient_id,\n",
      "    source_rows.analysis_year,\n",
      "    source_rows.census_region,\n",
      "    lookup.rurality_primary_rucc,\n",
      "    lookup.rurality_group,\n",
      "    lookup.rurality_zip3_mixed_rucc_flag,\n",
      "    lookup.rurality_zip3_n_zip5,\n",
      "    lookup.rurality_assignment_method\n",
      "  FROM source_rows\n",
      "  LEFT JOIN zip3_lookup lookup\n",
      "    ON source_rows.patient_state_key = lookup.state\n",
      "   AND source_rows.patient_zip3 = lookup.zip3\n",
      ")\n",
      "UPDATE ", target_identifier, "\n",
      "SET\n",
      "  census_region = derived.census_region,\n",
      "  rurality_primary_rucc = derived.rurality_primary_rucc,\n",
      "  rurality_group = derived.rurality_group,\n",
      "  rurality_zip3_mixed_rucc_flag = derived.rurality_zip3_mixed_rucc_flag,\n",
      "  rurality_zip3_n_zip5 = derived.rurality_zip3_n_zip5,\n",
      "  rurality_assignment_method = derived.rurality_assignment_method\n",
      "FROM derived\n",
      "WHERE ", quote_identifier(target_table), ".patient_id = derived.patient_id\n",
      "  AND ", quote_identifier(target_table), ".analysis_year = derived.analysis_year\n",
      "  AND ", quote_identifier(target_table), ".analysis_year = ", config$analysis_year, ";"
    )

    utilization_run_sql_stage(
      con,
      paste0("refresh Census region and ZIP3 rurality fields in ", target_table),
      update_sql
    )

    pre_filter <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT\n",
        "  analysis_year,\n",
        "  COUNT(*)::BIGINT AS n_rows,\n",
        "  COUNT(*) - COUNT(DISTINCT patient_id || '|' || analysis_year::VARCHAR)\n",
        "    AS duplicate_patient_year_rows,\n",
        "  SUM(CASE WHEN NULLIF(TRIM(patient_state), '') IS NULL THEN 1 ELSE 0 END)::BIGINT\n",
        "    AS n_missing_patient_state,\n",
        "  SUM(CASE WHEN NULLIF(TRIM(patient_zip), '') IS NULL THEN 1 ELSE 0 END)::BIGINT\n",
        "    AS n_missing_patient_zip,\n",
        "  SUM(CASE WHEN NULLIF(TRIM(patient_zip), '') IS NOT NULL\n",
        "             AND NOT (TRIM(patient_zip) ~ '^[0-9]{3}$')\n",
        "           THEN 1 ELSE 0 END)::BIGINT AS n_invalid_patient_zip3,\n",
        "  SUM(CASE WHEN census_region IS NULL THEN 1 ELSE 0 END)::BIGINT\n",
        "    AS n_missing_census_region,\n",
        "  SUM(CASE WHEN census_region IS NULL\n",
        "             OR NULLIF(TRIM(census_region), '') IS NULL\n",
        "             OR UPPER(TRIM(census_region)) = 'UNKNOWN'\n",
        "             OR rurality_group IS NULL\n",
        "             OR NULLIF(TRIM(rurality_group), '') IS NULL\n",
        "             OR UPPER(TRIM(rurality_group)) = 'UNKNOWN'\n",
        "           THEN 1 ELSE 0 END)::BIGINT\n",
        "    AS n_excluded_unknown_geography,\n",
        "  SUM(CASE WHEN NULLIF(TRIM(patient_zip), '') IS NOT NULL\n",
        "             AND TRIM(patient_zip) ~ '^[0-9]{3}$'\n",
        "             AND rurality_group IS NULL\n",
        "           THEN 1 ELSE 0 END)::BIGINT AS n_unmatched_rurality_zip3,\n",
        "  SUM(CASE WHEN NULLIF(TRIM(patient_zip), '') IS NOT NULL\n",
        "             AND TRIM(patient_zip) ~ '^[0-9]{3}$'\n",
        "             AND rurality_group = 'Unknown'\n",
        "           THEN 1 ELSE 0 END)::BIGINT AS n_unknown_rurality_zip3,\n",
        "  SUM(CASE WHEN rurality_group IS NULL THEN 1 ELSE 0 END)::BIGINT\n",
        "    AS n_missing_rurality_group,\n",
        "  SUM(CASE WHEN rurality_group IS NOT NULL\n",
        "             AND rurality_primary_rucc IS NULL THEN 1 ELSE 0 END)::BIGINT\n",
        "    AS n_classified_rows_without_modal_rucc,\n",
        "  SUM(CASE WHEN rurality_zip3_mixed_rucc_flag THEN 1 ELSE 0 END)::BIGINT\n",
        "    AS n_mixed_rucc_zip3_rows\n",
        "FROM ", target_identifier, "\n",
        "WHERE analysis_year = ", config$analysis_year, "\n",
        "GROUP BY analysis_year"
      )
    )

    if (nrow(pre_filter) != 1L) {
      stop("Geography QA did not return exactly one selected-year row.")
    }
    if (pre_filter$n_rows[[1]] != before$n_rows[[1]]) {
      stop("Geography enrichment changed the selected-year row count before filtering.")
    }
    if (pre_filter$duplicate_patient_year_rows[[1]] != 0) {
      stop("In-place geography update produced duplicate patient-year rows.")
    }

    exclusion_predicate <- paste0(
      "analysis_year = ", config$analysis_year, " AND (",
      "census_region IS NULL OR NULLIF(TRIM(census_region), '') IS NULL OR ",
      "UPPER(TRIM(census_region)) = 'UNKNOWN' OR ",
      "rurality_group IS NULL OR NULLIF(TRIM(rurality_group), '') IS NULL OR ",
      "UPPER(TRIM(rurality_group)) = 'UNKNOWN'",
      ")"
    )
    utilization_run_sql_stage(
      con,
      paste0("exclude final comparison rows with unknown Census region or rurality from ", target_table),
      paste0("DELETE FROM ", target_identifier, " WHERE ", exclusion_predicate, ";")
    )

    after <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*)::BIGINT AS n_rows,\n",
        "       COUNT(*) - COUNT(DISTINCT patient_id || '|' || analysis_year::VARCHAR)\n",
        "         AS duplicate_patient_year_rows\n",
        "FROM ", target_identifier, "\n",
        "WHERE analysis_year = ", config$analysis_year
      )
    )
    if (nrow(after) != 1L) {
      stop("Post-filter geography QA did not return exactly one selected-year row.")
    }
    if (after$duplicate_patient_year_rows[[1]] != 0) {
      stop("Final geography filter produced duplicate patient-year rows.")
    }
    expected_rows_after_filter <-
      pre_filter$n_rows[[1]] - pre_filter$n_excluded_unknown_geography[[1]]
    if (after$n_rows[[1]] != expected_rows_after_filter) {
      stop("Final geography filter removed an unexpected number of patient-year rows.")
    }

    metrics <- c(
      n_rows = after$n_rows[[1]],
      n_rows_before_update = before$n_rows[[1]],
      n_rows_after_update_before_filter = pre_filter$n_rows[[1]],
      n_rows_after_filter = after$n_rows[[1]],
      n_excluded_unknown_geography = pre_filter$n_excluded_unknown_geography[[1]],
      duplicate_patient_year_rows = after$duplicate_patient_year_rows[[1]],
      n_missing_patient_state = pre_filter$n_missing_patient_state[[1]],
      n_missing_patient_zip = pre_filter$n_missing_patient_zip[[1]],
      n_invalid_patient_zip3 = pre_filter$n_invalid_patient_zip3[[1]],
      n_missing_census_region = pre_filter$n_missing_census_region[[1]],
      n_unmatched_rurality_zip3 = pre_filter$n_unmatched_rurality_zip3[[1]],
      n_unknown_rurality_zip3 = pre_filter$n_unknown_rurality_zip3[[1]],
      n_missing_rurality_group = pre_filter$n_missing_rurality_group[[1]],
      n_classified_rows_without_modal_rucc = pre_filter$n_classified_rows_without_modal_rucc[[1]],
      n_mixed_rucc_zip3_rows = pre_filter$n_mixed_rucc_zip3_rows[[1]]
    )
    qa_output <- data.frame(
      analysis_year = config$analysis_year,
      qa_section = c(
        rep("row_counts", 6L),
        rep("geography_missingness", 9L)
      ),
      metric = names(metrics),
      n = as.numeric(metrics),
      stringsAsFactors = FALSE
    )
    qa_output$pct_of_selected_year_rows <- ifelse(
      qa_output$metric %in% c(
        "n_rows", "n_rows_before_update",
        "n_rows_after_update_before_filter", "n_rows_after_filter"
      ),
      100,
      100 * qa_output$n / as.numeric(after$n_rows[[1]])
    )
    qa_output$suppression_applied <- ifelse(
      qa_output$n >= 1 & qa_output$n < config$min_count,
      "yes",
      "no"
    )
    suppressed <- qa_output$suppression_applied == "yes"
    qa_output$n[suppressed] <- NA_real_
    qa_output$pct_of_selected_year_rows[suppressed] <- NA_real_
    utilization_write_csv(qa_output, qa_path)

    message(
      "Census region and ZIP3 rurality enrichment complete in ",
      target_schema,
      ".",
      target_table,
      ". QA written to ",
      qa_path,
      "."
    )
  },
  finally = {
    disconnect_komodo(con)
  }
)
