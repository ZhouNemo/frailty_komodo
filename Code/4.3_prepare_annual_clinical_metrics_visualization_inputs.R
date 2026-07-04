# Project: Frailty_Komoto annual clinical metrics descriptive visualization inputs
# Author: Nemo Zhou
# Date started: 2026-07-02
# Date last updated: 2026-07-03
#
# ---- Purpose ----
# Prepare CSV-only visualization inputs for
# Code/4.2_visualize_annual_clinical_metrics_descriptive_outputs.Rmd from
# aggregate outputs already written by Code/4.1_describe_annual_clinical_metrics.R.
# This script does not connect to Redshift. It reshapes existing categorical
# Table 1 count files into within-subgroup metric-level distributions for
# stacked percentage bar plots.

configured_output_dir <- getOption(
  "frailty.clinical_metrics_descriptive.output_dir",
  NULL
)

if (!is.null(configured_output_dir) && nzchar(configured_output_dir)) {
  output_dir <- configured_output_dir
} else {
  output_dir_candidates <- file.path(
    c("Outputs", "../Outputs"),
    "4.1_annual_clinical_metrics_descriptive"
  )
  output_dir <- output_dir_candidates[
    dir.exists(output_dir_candidates)
  ][1]
}

if (is.null(output_dir) || is.na(output_dir) || !dir.exists(output_dir)) {
  stop(
    "Could not find the clinical metrics descriptive output directory. ",
    "Run Code/4.1_describe_annual_clinical_metrics.R first."
  )
}

read_required_csv <- function(file_name) {
  path <- file.path(output_dir, file_name)
  if (!file.exists(path)) {
    stop("Missing required file: ", path)
  }

  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

write_output <- function(data, file_name) {
  path <- file.path(output_dir, file_name)
  utils::write.csv(data, path, row.names = FALSE, na = "")
  message("Wrote ", path)
  invisible(path)
}

build_level_distribution <- function(
  data,
  group_column,
  output_level_column,
  group_labels
) {
  required_columns <- c(
    "analysis_year",
    group_column,
    "variable",
    "category",
    "n_person_years"
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns for ",
      group_column,
      " distribution: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  distribution <- data[, required_columns, drop = FALSE]
  names(distribution)[names(distribution) == group_column] <- output_level_column
  distribution$n_person_years <- suppressWarnings(
    as.numeric(distribution$n_person_years)
  )
  distribution$metric_level <- group_labels[distribution[[output_level_column]]]
  distribution <- distribution[
    !is.na(distribution$n_person_years) &
      !is.na(distribution$metric_level),
    ,
    drop = FALSE
  ]

  key_columns <- c("analysis_year", "variable", "category")
  subgroup_keys <- unique(distribution[, key_columns, drop = FALSE])
  completed_rows <- list()
  for (key_index in seq_len(nrow(subgroup_keys))) {
    key_row <- subgroup_keys[key_index, , drop = FALSE]
    key_data <- distribution[
      distribution$analysis_year == key_row$analysis_year &
        distribution$variable == key_row$variable &
        distribution$category == key_row$category,
      ,
      drop = FALSE
    ]

    for (level_value in names(group_labels)) {
      level_data <- key_data[
        key_data[[output_level_column]] == level_value,
        ,
        drop = FALSE
      ]
      if (nrow(level_data) == 0L) {
        level_data <- data.frame(
          analysis_year = key_row$analysis_year,
          variable = key_row$variable,
          category = key_row$category,
          n_person_years = 0,
          metric_level = group_labels[[level_value]],
          stringsAsFactors = FALSE
        )
        level_data[[output_level_column]] <- level_value
        level_data <- level_data[
          ,
          c(
            "analysis_year",
            output_level_column,
            "variable",
            "category",
            "n_person_years",
            "metric_level"
          ),
          drop = FALSE
        ]
      }

      completed_rows[[length(completed_rows) + 1L]] <- level_data
    }
  }
  distribution <- do.call(rbind, completed_rows)

  key <- paste(
    distribution$analysis_year,
    distribution$variable,
    distribution$category,
    sep = "\r"
  )
  denominator <- ave(distribution$n_person_years, key, FUN = sum)
  distribution$subgroup_denominator <- denominator
  distribution$percent_within_subgroup <- ifelse(
    denominator > 0,
    distribution$n_person_years / denominator,
    NA_real_
  )

  distribution <- distribution[
    ,
    c(
      "analysis_year",
      "variable",
      "category",
      output_level_column,
      "metric_level",
      "n_person_years",
      "subgroup_denominator",
      "percent_within_subgroup"
    ),
    drop = FALSE
  ]
  names(distribution)[names(distribution) == "variable"] <- "stratification"
  names(distribution)[names(distribution) == "category"] <- "stratum_value"

  distribution
}

cfi_distribution <- build_level_distribution(
  read_required_csv("4.1_table_one_by_frailty_level_categorical.csv"),
  group_column = "frailty_level",
  output_level_column = "frailty_level",
  group_labels = c(
    "Robust (<0.15)" = "Non-frail",
    "Prefrail (0.15-<0.25)" = "Prefrail",
    "Frail (>=0.25)" = "Frail"
  )
)
write_output(
  cfi_distribution,
  "4.3_cfi_level_distribution_by_subgroup.csv"
)

gagne_distribution <- build_level_distribution(
  read_required_csv("4.1_table_one_by_gagne_level_categorical.csv"),
  group_column = "gagne_level",
  output_level_column = "gagne_level",
  group_labels = c(
    "Gagne <0" = "Gagne <0",
    "Gagne 0" = "Gagne 0",
    "Gagne 1-2" = "Gagne 1-2",
    "Gagne 3-5" = "Gagne 3-5",
    "Gagne 6+" = "Gagne 6+"
  )
)
write_output(
  gagne_distribution,
  "4.3_gagne_level_distribution_by_subgroup.csv"
)

hiv_distribution <- build_level_distribution(
  read_required_csv("4.1_table_one_by_hiv_status_categorical.csv"),
  group_column = "hiv_status_group",
  output_level_column = "hiv_status_group",
  group_labels = c(
    "HIV negative" = "HIV negative",
    "HIV positive" = "HIV positive"
  )
)
write_output(
  hiv_distribution,
  "4.3_hiv_status_distribution_by_subgroup.csv"
)
