# Project: Frailty_Komoto rurality reference lookup preparation
# Author: Nemo Zhou
# Date started: 2026-08-04
# Date last updated: 2026-08-04
#
# Purpose:
# Build a local, versioned rurality lookup from the supplied USDA Rural-Urban
# Continuum Code (RUCC) workbooks and the March 2026 ZIP-to-county crosswalk.
# The script combines the older workbook's 2023 RUCC field with the newer
# 2023-specific workbook by FIPS, preferring the newer value when both files
# contain a FIPS. It then joins ZIP5 county relationships to RUCC and creates
# a state-plus-ZIP3 lookup compatible with KRD's three-digit patient_zip field.
#
# Rurality definition:
#   RUCC 1, 2, 3 = Metro
#   RUCC 4, 6, 8 = Urban
#   RUCC 5, 7, 9 = Rural
#
# The ZIP3 assignment is an approximation. The ZIP-county file contains
# residence ratios for each ZIP5-to-county relationship, but it does not
# contain absolute resident counts for each ZIP5. Therefore, the lookup uses
# the modal Metro/Urban/Rural category after aggregating positive res_ratio
# values within patient_state plus ZIP3. Rows with no positive residence ratio
# or an exact modal tie are initially assigned Unknown. Those unresolved rows
# are then filled from the rurality package's ZIP5-to-ZIP3 modal RUCA lookup,
# with Metropolitan mapped to Metro, Micropolitan and Small town mapped to
# Urban, and Rural retained as Rural. Diagnostic columns retain both the
# res_ratio result and the package fallback used for review.
#
# Inputs (default locations):
#   ~/Downloads/Rural_Urban_Continuum_Codes.xlsx
#   ~/Downloads/Ruralurbancontinuumcodes2023.xlsx
#   ~/Downloads/ZIP-COUNTY_032026.xlsx
#
# Outputs:
#   Documents/Rurality Reference Tables/rucc_2023_fips_combined.csv
#   Documents/Rurality Reference Tables/rucc_2023_zip3_rurality_lookup.csv
#   Documents/Rurality Reference Tables/rucc_2023_zip3_rurality_qa.csv
#
# This is a local reference-table preparation script. It does not connect to
# Redshift, alter the existing rurality-package analysis, or create a
# persistent write-schema table.

if (!requireNamespace("rurality", quietly = TRUE)) {
  stop(
    "Package 'rurality' is required for unresolved ZIP3 fallback. " ,
    "Run this script from the project renv environment."
  )
}

python_candidates <- Sys.which(c("python", "python3"))
python_executable <- unname(
  python_candidates[nzchar(python_candidates)][1L]
)
if (length(python_executable) == 0L || is.na(python_executable)) {
  stop(
    "Python 3 is required to read the supplied .xlsx files. Add Python to PATH " ,
    "or set the 'python' executable before running this script."
  )
}

# ---- Configuration ---------------------------------------------------------

default_downloads_dir <- if (
  .Platform$OS.type == "windows" && nzchar(Sys.getenv("USERPROFILE"))
) {
  file.path(Sys.getenv("USERPROFILE"), "Downloads")
} else {
  file.path(path.expand("~"), "Downloads")
}

downloads_dir <- getOption(
  "frailty.rucc.downloads_dir",
  default_downloads_dir
)

repo_root <- normalizePath(
  if (nzchar(Sys.getenv("FRAILTY_KOMOTO_ROOT"))) {
    Sys.getenv("FRAILTY_KOMOTO_ROOT")
  } else {
    getwd()
  },
  winslash = "/",
  mustWork = TRUE
)

output_dir <- file.path(repo_root, "Documents", "Rurality Reference Tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

old_rucc_path <- file.path(downloads_dir, "Rural_Urban_Continuum_Codes.xlsx")
new_rucc_path <- file.path(downloads_dir, "Ruralurbancontinuumcodes2023.xlsx")
zip_county_path <- file.path(downloads_dir, "ZIP-COUNTY_032026.xlsx")

old_rucc_sheet <- "Rural.Urban.Continuum.Codes.197"
new_rucc_sheet <- "Rural-urban Continuum Code 2023"

fips_output_path <- file.path(output_dir, "rucc_2023_fips_combined.csv")
zip3_output_path <- file.path(
  output_dir,
  "rucc_2023_zip3_rurality_lookup.csv"
)
qa_output_path <- file.path(
  output_dir,
  "rucc_2023_zip3_rurality_qa.csv"
)

required_input_paths <- c(old_rucc_path, new_rucc_path, zip_county_path)
missing_input_paths <- required_input_paths[!file.exists(required_input_paths)]
if (length(missing_input_paths) > 0L) {
  stop(
    "Required input workbook(s) not found:\n",
    paste(missing_input_paths, collapse = "\n")
  )
}

# ---- Small XML/XLSX reader -------------------------------------------------

# readxl is not assumed to be installed in this renv. The small Python standard
# library reader below handles the supplied .xlsx files without installing a
# new R package and preserves ZIP/FIPS leading zeroes as character values.
column_number_from_reference <- function(cell_reference) {
  letters <- sub("[0-9]+$", "", cell_reference)
  letter_values <- utf8ToInt(letters) - utf8ToInt("A") + 1L
  sum(letter_values * 26L ^ rev(seq_along(letter_values) - 1L))
}

xml_text_from_cell <- function(cell_node, shared_strings) {
  cell_type <- xml2::xml_attr(cell_node, "t")

  if (identical(cell_type, "inlineStr")) {
    text_nodes <- xml2::xml_find_all(
      cell_node,
      ".//*[local-name()='t']"
    )
    return(paste(xml2::xml_text(text_nodes), collapse = ""))
  }

  value_node <- xml2::xml_find_first(
    cell_node,
    "./*[local-name()='v']"
  )
  value <- if (length(value_node) == 0L) "" else xml2::xml_text(value_node)

  if (identical(cell_type, "s") && nzchar(value)) {
    shared_index <- suppressWarnings(as.integer(value)) + 1L
    if (
      is.na(shared_index) ||
        shared_index < 1L ||
        shared_index > length(shared_strings)
    ) {
      stop("Invalid shared-string index in workbook: ", value)
    }
    return(shared_strings[[shared_index]])
  }

  value
}

read_xlsx_sheet <- function(path, sheet_name = NULL) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Package 'xml2' is required to read .xlsx files.")
  }

  extraction_dir <- tempfile("rucc_xlsx_")
  dir.create(extraction_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(
    unlink(extraction_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  utils::unzip(path, exdir = extraction_dir)

  workbook_path <- file.path(extraction_dir, "xl", "workbook.xml")
  relationships_path <- file.path(
    extraction_dir,
    "xl",
    "_rels",
    "workbook.xml.rels"
  )
  workbook_xml <- xml2::read_xml(workbook_path)
  relationships_xml <- xml2::read_xml(relationships_path)

  workbook_namespaces <- xml2::xml_ns(workbook_xml)
  workbook_namespace_name <- names(workbook_namespaces)[
    grepl("spreadsheetml", unname(workbook_namespaces))
  ][1L]
  if (is.na(workbook_namespace_name)) {
    stop("The workbook XML namespace could not be identified: ", path)
  }

  shared_strings_path <- file.path(
    extraction_dir,
    "xl",
    "sharedStrings.xml"
  )
  shared_strings <- character()
  if (file.exists(shared_strings_path)) {
    shared_xml <- xml2::read_xml(shared_strings_path)
    shared_namespaces <- xml2::xml_ns(shared_xml)
    shared_namespace_name <- names(shared_namespaces)[
      grepl("spreadsheetml", unname(shared_namespaces))
    ][1L]
    shared_nodes <- xml2::xml_find_all(
      shared_xml,
      paste0(".//", shared_namespace_name, ":si"),
      ns = shared_namespaces
    )
    shared_strings <- vapply(
      shared_nodes,
      function(node) {
        text_nodes <- xml2::xml_find_all(
          node,
          paste0(".//", shared_namespace_name, ":t"),
          ns = shared_namespaces
        )
        paste(xml2::xml_text(text_nodes), collapse = "")
      },
      character(1)
    )
  }

  sheet_nodes <- xml2::xml_find_all(
    workbook_xml,
    paste0(
      ".//",
      workbook_namespace_name,
      ":sheets/",
      workbook_namespace_name,
      ":sheet"
    ),
    ns = workbook_namespaces
  )
  if (length(sheet_nodes) == 0L) {
    stop("No worksheets were found in: ", path)
  }

  available_sheet_names <- xml2::xml_attr(sheet_nodes, "name")
  if (is.null(sheet_name)) {
    sheet_name <- available_sheet_names[[1L]]
  }
  if (!sheet_name %in% available_sheet_names) {
    stop(
      "Worksheet '", sheet_name, "' was not found in ", path,
      ". Available sheets: ", paste(available_sheet_names, collapse = ", ")
    )
  }

  selected_sheet <- sheet_nodes[which(available_sheet_names == sheet_name)[1L]]
  selected_sheet_attributes <- xml2::xml_attrs(selected_sheet)
  relationship_id_name <- names(selected_sheet_attributes)[
    grepl("(^|:)id$", names(selected_sheet_attributes), ignore.case = TRUE)
  ][1L]
  relationship_id <- unname(
    selected_sheet_attributes[[relationship_id_name]]
  )

  relationship_namespaces <- xml2::xml_ns(relationships_xml)
  relationship_namespace_name <- names(relationship_namespaces)[1L]
  relationship_nodes <- xml2::xml_find_all(
    relationships_xml,
    paste0(".//", relationship_namespace_name, ":Relationship"),
    ns = relationship_namespaces
  )
  relationship_ids <- xml2::xml_attr(relationship_nodes, "Id")
  relationship_targets <- xml2::xml_attr(relationship_nodes, "Target")
  target_index <- match(relationship_id, relationship_ids)
  if (is.na(target_index)) {
    stop("No worksheet relationship was found for: ", sheet_name)
  }

  relationship_target <- relationship_targets[[target_index]]
  relationship_target <- sub("^/", "", relationship_target)
  sheet_path <- if (startsWith(relationship_target, "xl/")) {
    file.path(extraction_dir, relationship_target)
  } else {
    file.path(extraction_dir, "xl", relationship_target)
  }
  # The large ZIP-county worksheet is a single compact XML line. Reading the
  # cell blocks with a streaming-style regular expression avoids constructing a
  # several-hundred-thousand-node XML DOM for this reference file.
  sheet_text <- readChar(
    sheet_path,
    file.info(sheet_path)[["size"]],
    useBytes = TRUE
  )
  cell_blocks <- regmatches(
    sheet_text,
    gregexpr("<c[^>]*>.*?</c>", sheet_text, perl = TRUE)
  )[[1L]]
  if (length(cell_blocks) == 0L) {
    stop("No worksheet rows were found in: ", path)
  }

  cell_references <- sub(
    '(?s).* r="([A-Z]+[0-9]+)".*',
    "\\1",
    cell_blocks,
    perl = TRUE
  )
  row_indices <- as.integer(sub("^[A-Z]+", "", cell_references))
  column_letters <- sub("[0-9]+$", "", cell_references)
  unique_column_letters <- unique(column_letters)
  unique_column_numbers <- vapply(
    unique_column_letters,
    function(value) column_number_from_reference(paste0(value, "1")),
    integer(1)
  )
  column_number_map <- setNames(
    unique_column_numbers,
    unique_column_letters
  )
  column_indices <- unname(column_number_map[column_letters])

  inline_cells <- grepl(' t="inlineStr"', cell_blocks, fixed = TRUE)
  shared_cells <- grepl(' t="s"', cell_blocks, fixed = TRUE)
  cell_values <- character(length(cell_blocks))
  if (any(inline_cells)) {
    cell_values[inline_cells] <- sub(
      "(?s).*<t[^>]*>(.*?)</t>.*",
      "\\1",
      cell_blocks[inline_cells],
      perl = TRUE
    )
  }
  non_inline_cells <- !inline_cells
  if (any(non_inline_cells)) {
    cell_values[non_inline_cells] <- sub(
      "(?s).*<v>(.*?)</v>.*",
      "\\1",
      cell_blocks[non_inline_cells],
      perl = TRUE
    )
  }
  shared_cells <- which(shared_cells & nzchar(cell_values))
  if (length(shared_cells) > 0L) {
    shared_indices <- suppressWarnings(
      as.integer(cell_values[shared_cells]) + 1L
    )
    if (
      anyNA(shared_indices) ||
        any(shared_indices < 1L) ||
        any(shared_indices > length(shared_strings))
    ) {
      stop("Invalid shared-string index in workbook: ", path)
    }
    cell_values[shared_cells] <- shared_strings[shared_indices]
  }

  max_column <- max(column_indices, na.rm = TRUE)
  max_row <- max(row_indices, na.rm = TRUE)

  if (
    !is.finite(max_column) ||
      max_column < 1L ||
      !is.finite(max_row) ||
      max_row < 1L
  ) {
    stop("Could not determine worksheet columns in: ", path)
  }

  workbook_matrix <- matrix(
    "",
    nrow = max_row,
    ncol = max_column
  )
  workbook_matrix[cbind(row_indices, column_indices)] <- cell_values

  headers <- trimws(workbook_matrix[1L, ])
  if (any(!nzchar(headers))) {
    stop("Blank header column found in worksheet: ", sheet_name)
  }
  if (anyDuplicated(headers)) {
    stop("Duplicate header found in worksheet: ", sheet_name)
  }

  data <- as.data.frame(
    workbook_matrix[-1L, , drop = FALSE],
    stringsAsFactors = FALSE
  )
  names(data) <- headers
  data
}

# The XML DOM implementation above is retained as a readable fallback for
# workbook structure inspection, but the March 2026 ZIP-county worksheet is
# large enough that its direct DOM traversal is unnecessarily slow. Use a
# temporary Python standard-library reader for the actual workbook extraction.
python_reader_code <- paste(
  c(
    "import csv",
    "import re",
    "import sys",
    "import zipfile",
    "import xml.etree.ElementTree as ET",
    "MAIN = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'",
    "REL = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships'",
    "PKG_REL = 'http://schemas.openxmlformats.org/package/2006/relationships'",
    "def column_number(reference):",
    "    letters = re.match(r'([A-Z]+)', reference).group(1)",
    "    value = 0",
    "    for letter in letters:",
    "        value = value * 26 + ord(letter) - ord('A') + 1",
    "    return value",
    "def text_value(node):",
    "    return ''.join((text.text or '') for text in node.iter('{%s}t' % MAIN))",
    "path, requested_sheet, output_path = sys.argv[1:4]",
    "with zipfile.ZipFile(path) as workbook:",
    "    workbook_xml = ET.fromstring(workbook.read('xl/workbook.xml'))",
    "    relationships_xml = ET.fromstring(workbook.read('xl/_rels/workbook.xml.rels'))",
    "    relationships = {node.attrib['Id']: node.attrib['Target'] for node in relationships_xml.findall('{%s}Relationship' % PKG_REL)}",
    "    shared_strings = []",
    "    if 'xl/sharedStrings.xml' in workbook.namelist():",
    "        shared_xml = ET.fromstring(workbook.read('xl/sharedStrings.xml'))",
    "        shared_strings = [text_value(node) for node in shared_xml.findall('{%s}si' % MAIN)]",
    "    sheets = workbook_xml.findall('{%s}sheets/{%s}sheet' % (MAIN, MAIN))",
    "    names = [node.attrib['name'] for node in sheets]",
    "    if requested_sheet:",
    "        if requested_sheet not in names:",
    "            raise ValueError('Worksheet not found: ' + requested_sheet)",
    "        sheet = sheets[names.index(requested_sheet)]",
    "    else:",
    "        sheet = sheets[0]",
    "    relationship_id = sheet.attrib['{%s}id' % REL]",
    "    target = relationships[relationship_id].lstrip('/')",
    "    if not target.startswith('xl/'):",
    "        target = 'xl/' + target",
    "    sheet_xml = ET.fromstring(workbook.read(target))",
    "    rows = []",
    "    max_column = 0",
    "    for row_node in sheet_xml.findall('.//{%s}sheetData/{%s}row' % (MAIN, MAIN)):",
    "        row_values = {}",
    "        for cell in row_node.findall('{%s}c' % MAIN):",
    "            reference = cell.attrib['r']",
    "            column = column_number(reference)",
    "            max_column = max(max_column, column)",
    "            cell_type = cell.attrib.get('t')",
    "            if cell_type == 'inlineStr':",
    "                value = text_value(cell)",
    "            else:",
    "                value_node = cell.find('{%s}v' % MAIN)",
    "                value = '' if value_node is None else (value_node.text or '')",
    "                if cell_type == 's' and value:",
    "                    value = shared_strings[int(value)]",
    "            row_values[column] = value",
    "        rows.append(row_values)",
    "    if not rows:",
    "        raise ValueError('No worksheet rows found')",
    "    headers = [rows[0].get(column, '') for column in range(1, max_column + 1)]",
    "    while headers and not headers[-1]:",
    "        headers.pop()",
    "    if any(not header for header in headers):",
    "        raise ValueError('Blank worksheet header found')",
    "    max_column = len(headers)",
    "    with open(output_path, 'w', newline='', encoding='utf-8') as output_file:",
    "        writer = csv.writer(output_file)",
    "        writer.writerow(headers)",
    "        for row_values in rows[1:]:",
    "            writer.writerow([row_values.get(column, '') for column in range(1, max_column + 1)])"
  ),
  collapse = "\n"
)

read_xlsx_sheet <- function(path, sheet_name = NULL) {
  python_script_path <- tempfile("rucc_xlsx_reader_", fileext = ".py")
  output_csv_path <- tempfile("rucc_xlsx_sheet_", fileext = ".csv")
  writeLines(python_reader_code, python_script_path, useBytes = TRUE)
  on.exit(
    unlink(c(python_script_path, output_csv_path), force = TRUE),
    add = TRUE
  )

  requested_sheet <- if (is.null(sheet_name)) "" else sheet_name
  python_args <- c(
    python_script_path,
    path,
    requested_sheet,
    output_csv_path
  )
  if (.Platform$OS.type == "windows") {
    python_args <- shQuote(python_args, type = "cmd")
  }
  python_output <- system2(
    python_executable,
    args = python_args,
    stdout = TRUE,
    stderr = TRUE
  )
  python_status <- attr(python_output, "status")
  if (!is.null(python_status) && python_status != 0L) {
    stop(
      "Python workbook reader failed for ", path, ":\n",
      paste(python_output, collapse = "\n")
    )
  }
  if (!file.exists(output_csv_path)) {
    stop("Python workbook reader did not create an output file: ", path)
  }

  read.csv(
    output_csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character()
  )
}

# ---- Normalization and RUCC definitions -----------------------------------

normalize_code <- function(x, width) {
  values <- trimws(as.character(x))
  values[is.na(values)] <- ""
  values <- sub("\\.0+$", "", values)
  numeric_values <- grepl(
    paste0("^[0-9]{1,", width, "}$"),
    values
  )
  values[numeric_values] <- vapply(
    values[numeric_values],
    function(value) {
      paste0(
        strrep("0", max(0L, width - nchar(value))),
        value
      )
    },
    character(1)
  )
  values[!numeric_values & nzchar(values)] <- NA_character_
  values[!nzchar(values)] <- NA_character_
  values
}

coalesce_character <- function(primary, secondary) {
  primary <- trimws(as.character(primary))
  secondary <- trimws(as.character(secondary))
  primary[is.na(primary)] <- ""
  secondary[is.na(secondary)] <- ""
  ifelse(nzchar(primary), primary, secondary)
}

as_numeric_safely <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}

rucc_to_group <- function(rucc) {
  result <- rep(NA_character_, length(rucc))
  result[rucc %in% c(1, 2, 3)] <- "Metro"
  result[rucc %in% c(4, 6, 8)] <- "Urban"
  result[rucc %in% c(5, 7, 9)] <- "Rural"
  result
}

require_columns <- function(data, columns, data_name) {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      data_name,
      " is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

build_rurality_package_zip3_lookup <- function() {
  ruca <- as.data.frame(rurality::ruca_codes, stringsAsFactors = FALSE)
  required_columns <- c("zip", "primary_ruca")
  missing_columns <- setdiff(required_columns, names(ruca))
  if (length(missing_columns) > 0L) {
    stop(
      "rurality::ruca_codes is missing required column(s): ",
      paste(missing_columns, collapse = ", ")
    )
  }

  zip_numeric <- suppressWarnings(as.integer(as.character(ruca$zip)))
  ruca$zip5 <- NA_character_
  valid_zip <- !is.na(zip_numeric)
  ruca$zip5[valid_zip] <- sprintf("%05d", zip_numeric[valid_zip])
  ruca$primary_ruca <- suppressWarnings(
    as.integer(as.character(ruca$primary_ruca))
  )
  ruca <- ruca[
    !is.na(ruca$zip5) &
      !is.na(ruca$primary_ruca) &
      ruca$primary_ruca >= 1L &
      ruca$primary_ruca <= 10L,
    ,
    drop = FALSE
  ]
  ruca$zip3 <- substr(ruca$zip5, 1L, 3L)
  ruca$rurality_group <- NA_character_
  ruca$rurality_group[ruca$primary_ruca %in% 1:3] <- "Metropolitan"
  ruca$rurality_group[ruca$primary_ruca %in% 4:6] <- "Micropolitan"
  ruca$rurality_group[ruca$primary_ruca %in% 7:9] <- "Small town"
  ruca$rurality_group[ruca$primary_ruca == 10L] <- "Rural"
  ruca <- unique(ruca[
    !is.na(ruca$rurality_group),
    c("zip5", "zip3", "primary_ruca", "rurality_group"),
    drop = FALSE
  ])
  if (nrow(ruca) == 0L) {
    stop("rurality::ruca_codes did not contain usable ZIP/RUCA rows.")
  }

  group_order <- c("Metropolitan", "Micropolitan", "Small town", "Rural")
  zip3_values <- sort(unique(ruca$zip3))
  lookup_rows <- lapply(zip3_values, function(zip3_value) {
    rows <- ruca[ruca$zip3 == zip3_value, , drop = FALSE]
    group_counts <- table(factor(rows$rurality_group, levels = group_order))
    modal_group <- group_order[
      order(-as.integer(group_counts), seq_along(group_order))
    ][[1L]]
    modal_rows <- rows[rows$rurality_group == modal_group, , drop = FALSE]
    code_counts <- table(modal_rows$primary_ruca)
    code_values <- as.integer(names(code_counts))
    modal_code <- code_values[
      order(-as.integer(code_counts), code_values)
    ][[1L]]

    data.frame(
      zip3 = zip3_value,
      package_rurality_group = modal_group,
      package_primary_ruca = modal_code,
      package_zip3_n_zctas = length(unique(rows$zip5)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, lookup_rows)
}

# ---- Read and combine the RUCC workbooks -------------------------------

message("Reading RUCC workbooks...")
old_rucc <- read_xlsx_sheet(old_rucc_path, old_rucc_sheet)
new_rucc <- read_xlsx_sheet(new_rucc_path, new_rucc_sheet)

require_columns(
  old_rucc,
  c("FIPS", "State", "County", "Rural-Urban Continuum Code 2023"),
  "Older RUCC workbook"
)
require_columns(
  new_rucc,
  c("FIPS", "State", "County_Name", "Population_2020", "RUCC_2023", "Description"),
  "Newer RUCC workbook"
)

old_fips <- data.frame(
  fips = normalize_code(old_rucc$FIPS, 5L),
  old_state = trimws(old_rucc$State),
  old_county_name = trimws(old_rucc$County),
  rucc_2023_old = trimws(old_rucc[["Rural-Urban Continuum Code 2023"]]),
  stringsAsFactors = FALSE
)
new_fips <- data.frame(
  fips = normalize_code(new_rucc$FIPS, 5L),
  new_state = trimws(new_rucc$State),
  new_county_name = trimws(new_rucc$County_Name),
  population_2020 = as_numeric_safely(new_rucc$Population_2020),
  rucc_2023_new = trimws(new_rucc$RUCC_2023),
  rucc_description = trimws(new_rucc$Description),
  stringsAsFactors = FALSE
)

if (anyNA(old_fips$fips) || anyNA(new_fips$fips)) {
  stop("At least one RUCC FIPS value could not be normalized to 5 digits.")
}
if (anyDuplicated(old_fips$fips) || anyDuplicated(new_fips$fips)) {
  stop("Duplicate FIPS values were found in a RUCC source workbook.")
}

combined_fips <- merge(old_fips, new_fips, by = "fips", all = TRUE, sort = TRUE)
combined_fips$rucc_2023_text <- coalesce_character(
  combined_fips$rucc_2023_new,
  combined_fips$rucc_2023_old
)
combined_fips$rucc_2023 <- as_numeric_safely(combined_fips$rucc_2023_text)
combined_fips$state <- coalesce_character(
  combined_fips$new_state,
  combined_fips$old_state
)
combined_fips$county_name <- coalesce_character(
  combined_fips$new_county_name,
  combined_fips$old_county_name
)
has_new_rucc_value <- !is.na(combined_fips$rucc_2023_new) &
  nzchar(combined_fips$rucc_2023_new)
has_old_rucc_value <- !is.na(combined_fips$rucc_2023_old) &
  nzchar(combined_fips$rucc_2023_old)
combined_fips$rucc_source <- ifelse(
  has_new_rucc_value,
  "newer_workbook",
  ifelse(
    has_old_rucc_value,
    "older_workbook",
    "no_value"
  )
)
combined_fips$rurality_group <- rucc_to_group(combined_fips$rucc_2023)

fips_output <- combined_fips[, c(
  "fips",
  "state",
  "county_name",
  "population_2020",
  "rucc_2023",
  "rucc_2023_text",
  "rucc_description",
  "rurality_group",
  "rucc_source"
)]
fips_output$rurality_group[is.na(fips_output$rurality_group)] <- "Unknown"
write.csv(
  fips_output,
  fips_output_path,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# ---- Join ZIP5 counties to RUCC -------------------------------------------

message("Reading ZIP-county crosswalk...")
zip_county <- read_xlsx_sheet(zip_county_path, "Sheet1")
require_columns(
  zip_county,
  c("zip", "geoid", "res_ratio", "tot_ratio", "city", "state"),
  "ZIP-county crosswalk"
)

zip_county_join <- data.frame(
  zip5 = normalize_code(zip_county$zip, 5L),
  geoid = normalize_code(zip_county$geoid, 5L),
  res_ratio = as_numeric_safely(zip_county$res_ratio),
  tot_ratio = as_numeric_safely(zip_county$tot_ratio),
  city = trimws(zip_county$city),
  state = toupper(trimws(zip_county$state)),
  stringsAsFactors = FALSE
)
zip_county_join$zip3 <- substr(zip_county_join$zip5, 1L, 3L)

if (anyNA(zip_county_join$zip5) || anyNA(zip_county_join$geoid)) {
  stop("ZIP-county crosswalk contains an invalid ZIP5 or county FIPS value.")
}
if (anyDuplicated(paste(zip_county_join$zip5, zip_county_join$geoid, sep = "|"))) {
  stop("ZIP-county crosswalk contains duplicate ZIP5-county relationships.")
}

fips_match <- match(zip_county_join$geoid, combined_fips$fips)
zip_county_join$rucc_2023 <- combined_fips$rucc_2023[fips_match]
zip_county_join$rucc_description <- combined_fips$rucc_description[fips_match]
zip_county_join$rurality_group <- combined_fips$rurality_group[fips_match]
zip_county_join$fips_in_combined_reference <- !is.na(fips_match)
zip_county_join$valid_rucc_match <- !is.na(zip_county_join$rurality_group)

# ---- Aggregate to state plus ZIP3 -----------------------------------------

message("Building unresolved ZIP3 fallback from the rurality package...")
package_zip3_lookup <- build_rurality_package_zip3_lookup()

group_key <- paste(zip_county_join$state, zip_county_join$zip3, sep = "|")
group_indices <- split(
  seq_len(nrow(zip_county_join)),
  group_key,
  drop = TRUE
)

group_labels <- c("Metro", "Urban", "Rural")
tie_tolerance <- 1e-10

lookup_rows <- lapply(group_indices, function(indices) {
  rows <- zip_county_join[indices, , drop = FALSE]
  scores <- setNames(numeric(length(group_labels)), group_labels)

  for (label in group_labels) {
    scores[[label]] <- sum(
      rows$res_ratio[
        rows$rurality_group == label &
          !is.na(rows$res_ratio) &
          rows$res_ratio > 0
      ],
      na.rm = TRUE
    )
  }

  positive_res_ratio_rows <- !is.na(rows$res_ratio) & rows$res_ratio > 0
  valid_positive_rows <- positive_res_ratio_rows & rows$valid_rucc_match
  n_positive_res_ratio_rows <- sum(positive_res_ratio_rows)
  n_valid_positive_rucc_rows <- sum(valid_positive_rows)
  max_score <- max(scores)
  winners <- names(scores)[
    scores >= max_score - tie_tolerance & scores > 0
  ]

  if (n_valid_positive_rucc_rows == 0L) {
    rurality_group <- "Unknown"
    assignment_method <- if (n_positive_res_ratio_rows == 0L) {
      "unknown_no_positive_res_ratio"
    } else {
      "unknown_no_valid_rucc"
    }
  } else if (length(winners) != 1L) {
    rurality_group <- "Unknown"
    assignment_method <- "unknown_res_ratio_tie"
  } else {
    rurality_group <- winners[[1L]]
    assignment_method <- "res_ratio_modal"
  }

  if (any(valid_positive_rows)) {
    rucc_scores <- tapply(
      rows$res_ratio[valid_positive_rows],
      rows$rucc_2023[valid_positive_rows],
      sum,
      na.rm = TRUE
    )
    rucc_scores <- rucc_scores[is.finite(rucc_scores)]
    rucc_winners <- names(rucc_scores)[
      rucc_scores >= max(rucc_scores) - tie_tolerance &
        rucc_scores > 0
    ]
    modal_rucc <- if (length(rucc_winners) == 1L) {
      as.integer(rucc_winners[[1L]])
    } else {
      NA_integer_
    }
  } else {
    modal_rucc <- NA_integer_
  }

  data.frame(
    state = rows$state[[1L]],
    zip3 = rows$zip3[[1L]],
    rurality_group = rurality_group,
    assignment_method = assignment_method,
    modal_rucc_2023 = modal_rucc,
    n_zip5 = length(unique(rows$zip5)),
    n_county_fips = length(unique(rows$geoid)),
    n_zip_county_rows = nrow(rows),
    n_unmatched_fips_rows = sum(!rows$fips_in_combined_reference),
    n_invalid_or_missing_rucc_rows = sum(
      rows$fips_in_combined_reference & !rows$valid_rucc_match
    ),
    n_positive_res_ratio_rows = n_positive_res_ratio_rows,
    n_valid_positive_rucc_rows = n_valid_positive_rucc_rows,
    res_ratio_sum_metro = unname(scores[["Metro"]]),
    res_ratio_sum_urban = unname(scores[["Urban"]]),
    res_ratio_sum_rural = unname(scores[["Rural"]]),
    rurality_mixed_flag = sum(scores > 0) > 1L,
    rurality_tie_flag = assignment_method == "unknown_res_ratio_tie",
    stringsAsFactors = FALSE
  )
})

zip3_lookup <- do.call(rbind, lookup_rows)
rownames(zip3_lookup) <- NULL

# Retain the original res_ratio result and package diagnostics, then use the
# package only for rows that remained Unknown after the RUCC/res_ratio step.
zip3_lookup$res_ratio_rurality_group <- zip3_lookup$rurality_group
package_match <- match(zip3_lookup$zip3, package_zip3_lookup$zip3)
zip3_lookup$package_rurality_group <- package_zip3_lookup$package_rurality_group[
  package_match
]
zip3_lookup$package_primary_ruca <- package_zip3_lookup$package_primary_ruca[
  package_match
]
zip3_lookup$package_zip3_n_zctas <- package_zip3_lookup$package_zip3_n_zctas[
  package_match
]

unknown_before_package <- zip3_lookup$rurality_group == "Unknown"
package_group_map <- c(
  Metropolitan = "Metro",
  Micropolitan = "Urban",
  `Small town` = "Urban",
  Rural = "Rural"
)
package_fallback_group <- unname(
  package_group_map[zip3_lookup$package_rurality_group]
)
package_fallback_rows <- unknown_before_package & !is.na(package_fallback_group)
zip3_lookup$rurality_group[package_fallback_rows] <- package_fallback_group[
  package_fallback_rows
]
zip3_lookup$assignment_method[package_fallback_rows] <-
  "rurality_package_fallback"
zip3_lookup$package_fallback_flag <- package_fallback_rows

allowed_groups <- c(group_labels, "Unknown")
if (!all(zip3_lookup$rurality_group %in% allowed_groups)) {
  stop("ZIP3 lookup contains an unexpected rurality category.")
}

write.csv(
  zip3_lookup,
  zip3_output_path,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# ---- Aggregate QA ----------------------------------------------------------

qa_metrics <- data.frame(
  qa_section = c(
    rep("rucc_sources", 7L),
    rep("zip_county_crosswalk", 9L),
    rep("zip3_lookup", 11L)
  ),
  metric = c(
    "old_rucc_rows",
    "new_rucc_rows",
    "combined_fips",
    "combined_valid_rucc_1_to_9",
    "combined_invalid_or_missing_rucc",
    "overlap_fips",
    "overlap_fips_with_conflicting_2023_codes",
    "zip_county_rows",
    "unique_zip5",
    "unique_zip3",
    "unique_county_fips",
    "zip_county_rows_with_fips_in_combined_reference",
    "zip_county_rows_with_valid_rucc_match",
    "zip_county_rows_with_unmatched_fips",
    "unique_zip5_with_all_zero_res_ratio",
    "zip3_state_rows_with_no_positive_res_ratio",
    "zip3_state_rows",
    "zip3_state_rows_metro",
    "zip3_state_rows_urban",
    "zip3_state_rows_rural",
    "zip3_state_rows_unknown",
    "zip3_state_rows_unknown_before_package_fallback",
    "zip3_state_rows_package_fallback",
    "zip3_state_rows_mixed_category",
    "zip3_state_rows_tied_modal_category",
    "zip3_state_rows_with_unmatched_fips",
    "zip3_state_rows_with_invalid_or_missing_rucc"
  ),
  n = c(
    nrow(old_rucc),
    nrow(new_rucc),
    nrow(combined_fips),
    sum(!is.na(combined_fips$rurality_group)),
    sum(is.na(combined_fips$rurality_group)),
    sum(combined_fips$fips %in% old_fips$fips & combined_fips$fips %in% new_fips$fips),
    0L,
    nrow(zip_county_join),
    length(unique(zip_county_join$zip5)),
    length(unique(zip_county_join$zip3)),
    length(unique(zip_county_join$geoid)),
    sum(zip_county_join$fips_in_combined_reference),
    sum(zip_county_join$valid_rucc_match),
    sum(!zip_county_join$fips_in_combined_reference),
    sum(tapply(
      zip_county_join$res_ratio,
      zip_county_join$zip5,
      function(x) all(!is.na(x)) & all(x == 0)
    )),
    sum(zip3_lookup$n_positive_res_ratio_rows == 0L),
    nrow(zip3_lookup),
    sum(zip3_lookup$rurality_group == "Metro"),
    sum(zip3_lookup$rurality_group == "Urban"),
    sum(zip3_lookup$rurality_group == "Rural"),
    sum(zip3_lookup$rurality_group == "Unknown"),
    sum(unknown_before_package),
    sum(zip3_lookup$package_fallback_flag),
    sum(zip3_lookup$rurality_mixed_flag),
    sum(zip3_lookup$rurality_tie_flag),
    sum(zip3_lookup$n_unmatched_fips_rows > 0L),
    sum(zip3_lookup$n_invalid_or_missing_rucc_rows > 0L)
  ),
  stringsAsFactors = FALSE
)

# The two source workbooks agree wherever both contain a FIPS. Compute this
# explicitly for QA rather than hard-coding the expected zero in the output.
overlap_fips <- intersect(old_fips$fips, new_fips$fips)
old_overlap <- old_fips[match(overlap_fips, old_fips$fips), ]
new_overlap <- new_fips[match(overlap_fips, new_fips$fips), ]
conflicting_overlap <- sum(
  nzchar(old_overlap$rucc_2023_old) &
    nzchar(new_overlap$rucc_2023_new) &
    old_overlap$rucc_2023_old != new_overlap$rucc_2023_new
)
qa_metrics$n[qa_metrics$metric == "overlap_fips_with_conflicting_2023_codes"] <-
  conflicting_overlap

write.csv(
  qa_metrics,
  qa_output_path,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

message("RUCC FIPS table written to: ", fips_output_path)
message("State-plus-ZIP3 rurality lookup written to: ", zip3_output_path)
message("QA summary written to: ", qa_output_path)
message(
  "Lookup rows: ", nrow(zip3_lookup),
  "; Metro = ", sum(zip3_lookup$rurality_group == "Metro"),
  "; Urban = ", sum(zip3_lookup$rurality_group == "Urban"),
  "; Rural = ", sum(zip3_lookup$rurality_group == "Rural"),
  "; Unknown = ", sum(zip3_lookup$rurality_group == "Unknown")
)
