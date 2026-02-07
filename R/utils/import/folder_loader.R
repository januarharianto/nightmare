# Folder-based data loading for NIGHTMARE
# Scans a folder for CSV/Excel files, auto-detects types, and imports them

#' Scan data directory for unit subfolders containing data files
#'
#' @param data_dir Path to the root data directory
#' @return Character vector of subfolder names, or empty character if none found
scan_data_folders <- function(data_dir) {
  if (!dir.exists(data_dir)) return(character(0))

  dirs <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
  # Keep only folders that contain at least one CSV or Excel file
  has_data <- vapply(dirs, function(d) {
    files <- list.files(file.path(data_dir, d), pattern = "\\.(csv|xlsx|xls)$",
                        ignore.case = TRUE)
    length(files) > 0
  }, logical(1))

  sort(dirs[has_data])
}

#' Load all data files from a unit folder using auto-detection
#'
#' @param folder_path Full path to the unit subfolder
#' @param unit_filter Unit code for filtering (derived from folder name)
#' @return Named list with canvas, consids, plans (each NULL if not found)
load_folder <- function(folder_path, unit_filter = NULL) {
  files <- list.files(folder_path, pattern = "\\.(csv|xlsx|xls)$",
                      ignore.case = TRUE, full.names = TRUE)

  result <- list(canvas = NULL, consids = NULL, plans = NULL)

  for (f in files) {
    file_type <- detect_file_type(f)

    if (file_type == "canvas" && is.null(result$canvas)) {
      result$canvas <- import_canvas_grades(f)
    } else if (file_type == "special_consids" && is.null(result$consids)) {
      year_filter <- NULL
      if (!is.null(result$canvas)) {
        year_filter <- detect_year_from_canvas(result$canvas)
      }
      result$consids <- import_special_considerations(f, unit_filter = unit_filter,
                                                       year_filter = year_filter)
    } else if (file_type == "plans" && is.null(result$plans)) {
      year_filter <- NULL
      if (!is.null(result$canvas)) {
        year_filter <- detect_year_from_canvas(result$canvas)
      }
      result$plans <- import_disability_plans(f, unit_filter = unit_filter,
                                               year_filter = year_filter)
    }
  }

  result
}

#' Read the last loaded unit from disk
#'
#' @param data_dir Path to the root data directory
#' @return Character string of last unit name, or NULL
read_last_unit <- function(data_dir) {
  path <- file.path(data_dir, ".last_unit")
  if (file.exists(path)) trimws(readLines(path, n = 1, warn = FALSE))
  else NULL
}

#' Save the last loaded unit to disk
#'
#' @param data_dir Path to the root data directory
#' @param unit Character string of unit name
save_last_unit <- function(data_dir, unit) {
  writeLines(unit, file.path(data_dir, ".last_unit"))
}
