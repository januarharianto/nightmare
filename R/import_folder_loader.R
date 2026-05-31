# -- folder_loader.R -----------------------------------------------
# Scan unit folders for CSV/Excel files, auto-detect types, and import.

#' Scan data directory for unit subfolders containing data files
#' @keywords internal
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

folder_cache_path <- function(folder_path) {
  file.path(folder_path, ".nightmare", "import_cache.rds")
}

build_folder_cache_key <- function(folder_path, unit_filter = NULL) {
  files <- list.files(folder_path, pattern = "\\.(csv|xlsx|xls)$",
                      ignore.case = TRUE, full.names = TRUE)
  files <- sort(normalizePath(files, winslash = "/", mustWork = FALSE))
  info <- file.info(files)

  list(
    version = 1L,
    unit_filter = if (is.null(unit_filter)) "" else as.character(unit_filter),
    files = data.frame(
      path = files,
      size = as.numeric(info$size),
      mtime = as.numeric(info$mtime),
      stringsAsFactors = FALSE
    )
  )
}

read_folder_cache <- function(folder_path, cache_key) {
  path <- folder_cache_path(folder_path)
  if (!file.exists(path)) return(NULL)

  payload <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(payload) || !identical(payload$key, cache_key)) return(NULL)

  payload$imported
}

save_folder_cache <- function(folder_path, cache_key, imported) {
  nightmare_dir <- file.path(folder_path, ".nightmare")
  if (!dir.exists(nightmare_dir)) dir.create(nightmare_dir, recursive = TRUE)

  saveRDS(
    list(key = cache_key, imported = imported, saved_at = Sys.time()),
    folder_cache_path(folder_path)
  )
  invisible(folder_cache_path(folder_path))
}

#' Load all data files from a unit folder using auto-detection
#' @keywords internal
#'
#' @param folder_path Full path to the unit subfolder
#' @param unit_filter Unit code for filtering (derived from folder name)
#' @return Named list with canvas, consids, plans (each NULL if not found)
load_folder <- function(folder_path, unit_filter = NULL) {
  cache_key <- build_folder_cache_key(folder_path, unit_filter)
  cached <- read_folder_cache(folder_path, cache_key)
  if (!is.null(cached)) return(cached)

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

  save_folder_cache(folder_path, cache_key, result)
  result
}

#' Read the last loaded unit from disk
#' @keywords internal
#'
#' @param data_dir Path to the root data directory
#' @return Character string of last unit name, or NULL
read_last_unit <- function(data_dir) {
  path <- file.path(data_dir, ".last_unit")
  if (file.exists(path)) trimws(readLines(path, n = 1, warn = FALSE))
  else NULL
}

#' Save the last loaded unit to disk
#' @keywords internal
#'
#' @param data_dir Path to the root data directory
#' @param unit Character string of unit name
save_last_unit <- function(data_dir, unit) {
  writeLines(unit, file.path(data_dir, ".last_unit"))
}
