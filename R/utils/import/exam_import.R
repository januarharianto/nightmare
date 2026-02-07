# Exam import utilities for NIGHTMARE
# File parsing for Gradescope and manual exam score exports.

# Detect exam source type by reading headers.
# Returns list(type = "gradescope"|"manual", headers = character(), preview = data.frame())
detect_exam_source <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))

  raw <- if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(file_path, n_max = 5)
  } else {
    readr::read_csv(file_path, n_max = 5, show_col_types = FALSE)
  }

  headers <- names(raw)
  headers_lower <- tolower(headers)

  # Gradescope signature: has SID and Total Score columns
  has_sid <- any(headers_lower %in% c("sid", "student id"))
  has_total <- any(grepl("total\\s*score", headers_lower))

  source_type <- if (has_sid && has_total) "gradescope" else "manual"

  list(type = source_type, headers = headers, preview = raw)
}

# Parse a Gradescope export: auto-map SID + Total Score columns.
# Returns a named list (SID -> score).
parse_gradescope_export <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))

  raw <- if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(file_path)
  } else {
    readr::read_csv(file_path, show_col_types = FALSE)
  }

  headers_lower <- tolower(names(raw))

  # Find SID column
  sid_idx <- which(headers_lower %in% c("sid", "student id"))
  if (length(sid_idx) == 0) stop("No SID column found in Gradescope export")
  sid_col <- names(raw)[sid_idx[1]]

  # Find Total Score column
  total_idx <- which(grepl("total\\s*score", headers_lower))
  if (length(total_idx) == 0) stop("No Total Score column found in Gradescope export")
  score_col <- names(raw)[total_idx[1]]

  sids <- as.character(raw[[sid_col]])
  scores <- suppressWarnings(as.numeric(raw[[score_col]]))

  # Drop rows with missing SID or score
  valid <- !is.na(sids) & sids != "" & !is.na(scores)
  sids <- sids[valid]
  scores <- scores[valid]

  setNames(as.list(scores), sids)
}

# Parse manual columns: extract specified id and score columns.
# Returns a named list (SID -> score).
parse_manual_columns <- function(file_path, id_col, score_col) {
  ext <- tolower(tools::file_ext(file_path))

  raw <- if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(file_path)
  } else {
    readr::read_csv(file_path, show_col_types = FALSE)
  }

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
