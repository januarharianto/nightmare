# -- folder_loader.R -----------------------------------------------
# Scan unit folders for CSV/Excel files, auto-detect types, and import.

#' Check whether a path points to a supported source data file
#' @keywords internal
#'
#' @param path File path to check
#' @return TRUE when the path has a supported data-file extension
supported_data_file <- function(path) {
  supported <- !is.na(path) & nzchar(path) & !dir.exists(path) &
    grepl("\\.(csv|xlsx|xls)$", basename(path), ignore.case = TRUE)
  supported[is.na(supported)] <- FALSE
  supported
}

#' Check whether a folder directly contains supported source data
#' @keywords internal
#'
#' @param folder_path Folder path to inspect
#' @return TRUE when the folder directly contains CSV or Excel files
folder_has_supported_data <- function(folder_path) {
  if (!dir.exists(folder_path)) return(FALSE)

  files <- list.files(folder_path, full.names = TRUE, recursive = FALSE)
  any(vapply(files, supported_data_file, logical(1)))
}

#' Parse a nested offering folder name
#' @keywords internal
#'
#' @param name Folder basename, usually YYYY-SEMESTER
#' @return List with year, semester, and display label
parse_offering_name <- function(name) {
  match <- regexec("^([0-9]{4})-(.+)$", name)
  parts <- regmatches(name, match)[[1]]

  if (length(parts) == 3L) {
    year <- as.integer(parts[[2]])
    semester <- parts[[3]]
    return(list(
      year = year,
      semester = semester,
      label = paste(year, semester)
    ))
  }

  list(
    year = NA_integer_,
    semester = NA_character_,
    label = name
  )
}

#' Build the ordering for offering rows
#' @keywords internal
#'
#' @param offerings Offering data frame from scan_data_offerings()
#' @return Integer row order: unit ascending, year descending, label ascending
offering_sort_key <- function(offerings) {
  if (is.null(offerings) || nrow(offerings) == 0L) return(integer(0))

  year_sort <- ifelse(is.na(offerings$year), -Inf, offerings$year)
  order(offerings$unit, -year_sort, offerings$label, offerings$offering_id)
}

newest_offering_for_unit <- function(offerings, unit) {
  if (is.null(offerings) || nrow(offerings) == 0L || is.null(unit)) return(NULL)
  unit_offerings <- offerings[offerings$unit == unit, , drop = FALSE]
  if (nrow(unit_offerings) == 0L) return(NULL)
  unit_offerings <- unit_offerings[offering_sort_key(unit_offerings), , drop = FALSE]
  unit_offerings[1L, , drop = FALSE]
}

offering_canvas_config <- function(offering) {
  if (is.null(offering) || nrow(offering) == 0L) {
    return(list(base_url = "", course_id = "", configured = FALSE))
  }
  canvas_course_config(
    offering$offering_id[[1]],
    legacy_unit = if (isTRUE(offering$legacy[[1]])) offering$unit[[1]] else NULL
  )
}

empty_offerings <- function() {
  data.frame(
    offering_id = character(0),
    unit = character(0),
    year = integer(0),
    semester = character(0),
    path = character(0),
    label = character(0),
    legacy = logical(0),
    stringsAsFactors = FALSE
  )
}

#' Scan data directory for unit offerings containing data files
#' @keywords internal
#'
#' @param data_dir Path to the root data directory
#' @return Data frame of discovered offerings
scan_data_offerings <- function(data_dir) {
  if (!dir.exists(data_dir)) return(empty_offerings())

  unit_paths <- list.dirs(data_dir, full.names = TRUE, recursive = FALSE)
  unit_names <- basename(unit_paths)
  visible_units <- !startsWith(unit_names, ".")
  unit_paths <- unit_paths[visible_units]
  unit_names <- unit_names[visible_units]

  rows <- list()

  for (i in seq_along(unit_paths)) {
    unit <- unit_names[[i]]
    unit_path <- unit_paths[[i]]

    if (folder_has_supported_data(unit_path)) {
      rows[[length(rows) + 1L]] <- data.frame(
        offering_id = unit,
        unit = unit,
        year = NA_integer_,
        semester = NA_character_,
        path = unit_path,
        label = paste0("Legacy folder: ", unit, "/"),
        legacy = TRUE,
        stringsAsFactors = FALSE
      )
    }

    child_paths <- list.dirs(unit_path, full.names = TRUE, recursive = FALSE)
    child_names <- basename(child_paths)
    visible_children <- !startsWith(child_names, ".")
    child_paths <- child_paths[visible_children]
    child_names <- child_names[visible_children]

    for (j in seq_along(child_paths)) {
      child_path <- child_paths[[j]]
      child_name <- child_names[[j]]
      if (!folder_has_supported_data(child_path)) next

      parsed <- parse_offering_name(child_name)
      rows[[length(rows) + 1L]] <- data.frame(
        offering_id = paste(unit, child_name, sep = "/"),
        unit = unit,
        year = parsed$year,
        semester = parsed$semester,
        path = child_path,
        label = parsed$label,
        legacy = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) return(empty_offerings())

  offerings <- do.call(rbind, rows)
  offerings <- offerings[offering_sort_key(offerings), , drop = FALSE]
  row.names(offerings) <- NULL
  offerings
}

#' Scan data directory for unit subfolders containing data files
#' @keywords internal
#'
#' @param data_dir Path to the root data directory
#' @return Character vector of subfolder names, or empty character if none found
scan_data_folders <- function(data_dir) {
  if (!dir.exists(data_dir)) return(character(0))

  unit_paths <- list.dirs(data_dir, full.names = TRUE, recursive = FALSE)
  unit_names <- basename(unit_paths)
  visible_units <- !startsWith(unit_names, ".")
  unit_paths <- unit_paths[visible_units]
  unit_names <- unit_names[visible_units]

  has_data <- vapply(unit_paths, folder_has_supported_data, logical(1))
  sort(unit_names[has_data])
}

folder_cache_path <- function(folder_path) {
  file.path(folder_path, ".nightmare", "import_cache.rds")
}

build_folder_cache_key <- function(folder_path, unit_filter = NULL, year_filter = NULL) {
  files <- list.files(folder_path, pattern = "\\.(csv|xlsx|xls)$",
                      ignore.case = TRUE, full.names = TRUE)
  snapshot_path <- canvas_snapshot_path(folder_path)
  if (file.exists(snapshot_path)) files <- c(files, snapshot_path)
  files <- sort(normalizePath(files, winslash = "/", mustWork = FALSE))
  info <- file.info(files)

  list(
    version = 1L,
    unit_filter = if (is.null(unit_filter)) "" else as.character(unit_filter),
    year_filter = if (is.null(year_filter)) "" else as.character(year_filter),
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
#' @param year_filter Optional offering year for filtering nested source files
#' @return Named list with canvas, consids, plans (each NULL if not found)
load_folder <- function(folder_path, unit_filter = NULL, year_filter = NULL) {
  cache_key <- build_folder_cache_key(folder_path, unit_filter, year_filter)
  cached <- read_folder_cache(folder_path, cache_key)
  if (!is.null(cached)) return(cached)

  files <- list.files(folder_path, pattern = "\\.(csv|xlsx|xls)$",
                      ignore.case = TRUE, full.names = TRUE)
  file_types <- vapply(files, detect_file_type, character(1))

  result <- list(canvas = NULL, consids = NULL, plans = NULL)
  snapshot <- load_canvas_api_snapshot(folder_path)
  if (!is.null(snapshot) && !is.null(snapshot$canvas)) {
    result$canvas <- snapshot$canvas
  }

  if (is.null(result$canvas)) {
    canvas_files <- files[file_types == "canvas"]
    if (length(canvas_files) > 0L) {
      result$canvas <- import_canvas_grades(canvas_files[[1]])
    }
  }

  needs_year_filter <- any(file_types %in% c("special_consids", "plans"))
  effective_year <- year_filter
  if (is.null(effective_year) && needs_year_filter && !is.null(result$canvas)) {
    canvas_year <- attr(result$canvas, "academic_year")
    effective_year <- if (!is.null(canvas_year) && !is.na(canvas_year)) {
      canvas_year
    } else {
      detect_year_from_canvas(result$canvas)
    }
  }

  for (i in seq_along(files)) {
    f <- files[[i]]
    file_type <- file_types[[i]]

    if (file_type == "special_consids" && is.null(result$consids)) {
      result$consids <- import_special_considerations(f, unit_filter = unit_filter,
                                                       year_filter = effective_year)
    } else if (file_type == "plans" && is.null(result$plans)) {
      result$plans <- import_disability_plans(f, unit_filter = unit_filter,
                                                year_filter = effective_year)
    }
  }

  if (!is.null(snapshot) && !is.null(snapshot$canvas)) {
    result$canvas <- snapshot$canvas
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

#' Read the last loaded offering from disk
#' @keywords internal
#'
#' @param data_dir Path to the root data directory
#' @return Character string of last offering id, or NULL
read_last_offering <- function(data_dir) {
  path <- file.path(data_dir, ".last_offering")
  if (file.exists(path)) trimws(readLines(path, n = 1, warn = FALSE))
  else NULL
}

#' Save the last loaded offering to disk
#' @keywords internal
#'
#' @param data_dir Path to the root data directory
#' @param offering_id Character string of offering id
save_last_offering <- function(data_dir, offering_id) {
  writeLines(offering_id, file.path(data_dir, ".last_offering"))
}

#' Resolve a saved offering selection from available offerings
#' @keywords internal
#'
#' @param data_dir Path to the root data directory
#' @param offerings Offering data frame from scan_data_offerings()
#' @return One-row offering data frame, or NULL
resolve_saved_offering <- function(data_dir, offerings) {
  if (is.null(offerings) || nrow(offerings) == 0L) return(NULL)

  last_offering <- read_last_offering(data_dir)
  if (!is.null(last_offering) && last_offering %in% offerings$offering_id) {
    return(offerings[match(last_offering, offerings$offering_id), , drop = FALSE])
  }

  last_unit <- read_last_unit(data_dir)
  if (!is.null(last_unit) && last_unit %in% offerings$unit) {
    unit_offerings <- offerings[offerings$unit == last_unit, , drop = FALSE]

    nested <- unit_offerings[!unit_offerings$legacy, , drop = FALSE]
    if (nrow(nested) > 0L) {
      nested <- nested[offering_sort_key(nested), , drop = FALSE]
      return(nested[1L, , drop = FALSE])
    }

    legacy <- unit_offerings[unit_offerings$legacy, , drop = FALSE]
    if (nrow(legacy) > 0L) {
      return(legacy[1L, , drop = FALSE])
    }
  }

  if (nrow(offerings) == 1L) return(offerings[1L, , drop = FALSE])

  NULL
}
