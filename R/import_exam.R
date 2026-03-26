# -- exam_import.R -------------------------------------------------
# Parse Gradescope and manual exam score exports for upload wizard.

#' Read tabular file (CSV or Excel)
#' @param path Path to file
#' @param ... Additional arguments passed to reader
#' @return data.frame
#' @keywords internal
#' @importFrom tools file_ext
read_tabular <- function(path, ...) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) readxl::read_excel(path, ...) else readr::read_csv(path, show_col_types = FALSE, ...)
}

# Detect exam source type by reading headers.
# Returns list(type = "gradescope"|"manual", headers = character(), preview = data.frame())
detect_exam_source <- function(file_path) {
  raw <- read_tabular(file_path, n_max = 5)

  headers <- names(raw)
  headers_lower <- tolower(headers)

  # Gradescope signature: has SID and Total Score columns
  has_sid <- any(headers_lower %in% c("sid", "student id"))
  has_total <- any(grepl("total\\s*score", headers_lower))

  source_type <- if (has_sid && has_total) "gradescope" else "manual"

  list(type = source_type, headers = headers, preview = raw)
}

# Parse a Gradescope export with user-chosen score column.
# SID column is auto-detected. Returns a named list (SID -> score).
parse_gradescope_export <- function(file_path, score_col) {
  raw <- read_tabular(file_path)

  headers_lower <- tolower(names(raw))

  # Find SID column (auto-detected)
  sid_idx <- which(headers_lower %in% c("sid", "student id"))
  if (length(sid_idx) == 0) stop("No SID column found in Gradescope export")
  sid_col <- names(raw)[sid_idx[1]]

  if (!score_col %in% names(raw)) stop(paste("Column not found:", score_col))

  sids <- as.character(raw[[sid_col]])
  scores <- suppressWarnings(as.numeric(raw[[score_col]]))

  # Drop rows with missing SID or score
  valid <- !is.na(sids) & sids != "" & !is.na(scores)
  sids <- sids[valid]
  scores <- scores[valid]

  setNames(as.list(scores), sids)
}

# Extract max marks from a Gradescope column (first non-NA numeric value).
extract_max_marks <- function(file_path, max_col) {
  raw <- read_tabular(file_path, n_max = 5)

  if (!max_col %in% names(raw)) stop(paste("Column not found:", max_col))

  vals <- suppressWarnings(as.numeric(raw[[max_col]]))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) stop("No numeric values found in max marks column")
  vals[1]
}

# Parse manual columns: extract specified id and score columns.
# Returns a named list (SID -> score).
parse_manual_columns <- function(file_path, id_col, score_col) {
  raw <- read_tabular(file_path)

  if (!id_col %in% names(raw)) stop(paste("Column not found:", id_col))
  if (!score_col %in% names(raw)) stop(paste("Column not found:", score_col))

  sids <- as.character(raw[[id_col]])
  scores <- suppressWarnings(as.numeric(raw[[score_col]]))

  valid <- !is.na(sids) & sids != "" & !is.na(scores)
  sids <- sids[valid]
  scores <- scores[valid]

  setNames(as.list(scores), sids)
}

# Match parsed SIDs against Canvas student data.
# Returns list(matched, unmatched_sids, missing_from_upload)
# matched: data.frame(sid, name, score)
# unmatched_sids: character vector of SIDs in upload but not in Canvas
# missing_from_upload: character vector of SIDs in Canvas but not in upload
match_exam_students <- function(scores, student_data) {
  empty_matched <- data.frame(
    sid = character(), name = character(), score = numeric(),
    stringsAsFactors = FALSE
  )

  upload_sids <- names(scores)

  if (is.null(student_data) || nrow(student_data) == 0) {
    return(list(
      matched = empty_matched,
      unmatched_sids = upload_sids,
      missing_from_upload = character()
    ))
  }

  canvas_sids <- as.character(student_data$student_id)

  matched_sids <- intersect(upload_sids, canvas_sids)
  unmatched_sids <- setdiff(upload_sids, canvas_sids)
  missing_from_upload <- setdiff(canvas_sids, upload_sids)

  if (length(matched_sids) == 0) {
    return(list(
      matched = empty_matched,
      unmatched_sids = unmatched_sids,
      missing_from_upload = missing_from_upload
    ))
  }

  matched <- data.frame(
    sid = matched_sids,
    name = vapply(matched_sids, function(sid) {
      idx <- which(canvas_sids == sid)
      if (length(idx) > 0) as.character(student_data$name[idx[1]]) else ""
    }, character(1)),
    score = vapply(matched_sids, function(sid) scores[[sid]], numeric(1)),
    stringsAsFactors = FALSE
  )

  list(
    matched = matched,
    unmatched_sids = unmatched_sids,
    missing_from_upload = missing_from_upload
  )
}
