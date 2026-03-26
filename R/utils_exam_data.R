# -- exam_data.R ---------------------------------------------------
# Exam sittings lifecycle, conflict resolution, summary queries.

# Load exam data from .nightmare/exams.json.
# Returns a list with version, saved_at, assessments.
load_exam_data <- function(data_dir, unit) {
  default <- list(version = 1L, saved_at = NULL, assessments = list())
  payload <- load_json(data_dir, unit, "exams.json", default)
  if (is.null(payload$assessments)) default else payload
}

# Save exam data to .nightmare/exams.json.
save_exam_data <- function(data_dir, unit, exam_data) {
  save_nightmare_json(data_dir, unit, "exams.json", exam_data)
}

# Look up a student's score in a specific sitting. Returns NULL if not found.
get_sitting_score <- function(sittings, sitting_id, sid) {
  for (s in sittings) {
    if (s$sitting_id == sitting_id && !is.null(s$scores[[sid]])) return(s$scores[[sid]])
  }
  NULL
}

# Add a sitting to an assessment. Creates the assessment if it doesn't exist.
# sitting: list(sitting_id, upload_date, source_type, source_file, scores)
# scores: named list (SID -> score)
# Sets active_sitting for new students only (existing students keep current).
add_exam_sitting <- function(exam_data, name, max_points, sitting) {
  if (is.null(exam_data$assessments)) exam_data$assessments <- list()

  assessment <- exam_data$assessments[[name]]

  if (is.null(assessment)) {
    # New assessment
    sitting$sitting_id <- 1L
    exam_data$assessments[[name]] <- list(
      max_points = max_points,
      sittings = list(sitting),
      active_sitting = setNames(
        as.list(rep(1L, length(sitting$scores))),
        names(sitting$scores)
      )
    )
  } else {
    # Existing assessment -- append sitting
    existing_ids <- vapply(assessment$sittings, function(s) s$sitting_id, integer(1))
    sitting$sitting_id <- max(existing_ids) + 1L

    assessment$sittings <- c(assessment$sittings, list(sitting))
    assessment$max_points <- max_points

    # Set active_sitting for new students only
    if (is.null(assessment$active_sitting)) assessment$active_sitting <- list()
    for (sid in names(sitting$scores)) {
      if (is.null(assessment$active_sitting[[sid]])) {
        assessment$active_sitting[[sid]] <- sitting$sitting_id
      }
    }

    exam_data$assessments[[name]] <- assessment
  }

  exam_data
}

# Detect students with existing scores in the assessment.
# Returns a data.frame: SID, prev_score, new_score, prev_sitting_id
detect_conflicts <- function(exam_data, name, new_scores) {
  empty <- data.frame(
    sid = character(), prev_score = numeric(), new_score = numeric(),
    prev_sitting_id = integer(), stringsAsFactors = FALSE
  )

  assessment <- exam_data$assessments[[name]]
  if (is.null(assessment)) return(empty)

  rows <- lapply(names(new_scores), function(sid) {
    active_id <- assessment$active_sitting[[sid]]
    if (is.null(active_id)) return(NULL)

    # Find the active sitting's score for this student
    prev_score <- get_sitting_score(assessment$sittings, active_id, sid)
    if (is.null(prev_score)) return(NULL)

    data.frame(
      sid = sid, prev_score = prev_score, new_score = new_scores[[sid]],
      prev_sitting_id = as.integer(active_id), stringsAsFactors = FALSE
    )
  })

  rbind_or_empty(rows, empty)
}

# Update active_sitting per the user's choices.
# resolutions: named list (SID -> sitting_id)
resolve_conflicts <- function(exam_data, name, resolutions) {
  assessment <- exam_data$assessments[[name]]
  if (is.null(assessment)) return(exam_data)

  for (sid in names(resolutions)) {
    assessment$active_sitting[[sid]] <- as.integer(resolutions[[sid]])
  }

  exam_data$assessments[[name]] <- assessment
  exam_data
}

# Get active score per assessment for one student.
# Returns a data.frame: assessment, score, max_points, sitting_id
get_student_exam_scores <- function(exam_data, student_id) {
  empty <- data.frame(
    assessment = character(), score = numeric(), max_points = numeric(),
    sitting_id = integer(), stringsAsFactors = FALSE
  )

  if (is.null(exam_data$assessments) || length(exam_data$assessments) == 0) return(empty)

  sid <- as.character(student_id)

  rows <- lapply(names(exam_data$assessments), function(aname) {
    a <- exam_data$assessments[[aname]]
    active_id <- a$active_sitting[[sid]]
    if (is.null(active_id)) return(NULL)

    score <- get_sitting_score(a$sittings, active_id, sid)
    if (is.null(score)) return(NULL)

    data.frame(
      assessment = aname, score = score, max_points = a$max_points,
      sitting_id = as.integer(active_id), stringsAsFactors = FALSE
    )
  })

  rbind_or_empty(rows, empty)
}

# Get all sittings for one student on one assessment.
# Returns a data.frame: sitting_id, upload_date, source_type, score, is_active
get_student_sittings <- function(exam_data, name, student_id) {
  empty <- data.frame(
    sitting_id = integer(), upload_date = character(), source_type = character(),
    score = numeric(), is_active = logical(), stringsAsFactors = FALSE
  )

  assessment <- exam_data$assessments[[name]]
  if (is.null(assessment)) return(empty)

  sid <- as.character(student_id)
  active_id <- assessment$active_sitting[[sid]]

  rows <- lapply(assessment$sittings, function(s) {
    sc <- s$scores[[sid]]
    if (is.null(sc)) return(NULL)
    data.frame(
      sitting_id = s$sitting_id,
      upload_date = s$upload_date %||% "",
      source_type = s$source_type %||% "",
      score = sc,
      is_active = identical(as.integer(s$sitting_id), as.integer(active_id)),
      stringsAsFactors = FALSE
    )
  })

  rbind_or_empty(rows, empty)
}

# Extract class-level percentage scores per assessment (for Analytics view).
# Returns a named list matching the format of extract_class_scores():
# list(name, max_points, scores = numeric vector of percentages)
extract_exam_class_scores <- function(exam_data) {
  if (is.null(exam_data$assessments) || length(exam_data$assessments) == 0) return(list())

  result <- list()
  for (aname in names(exam_data$assessments)) {
    a <- exam_data$assessments[[aname]]
    mp <- a$max_points
    if (is.null(mp) || mp <= 0) next

    # Collect active score for each student as a percentage
    pcts <- vapply(names(a$active_sitting), function(sid) {
      active_id <- a$active_sitting[[sid]]
      score <- get_sitting_score(a$sittings, active_id, sid)
      if (is.null(score)) return(NA_real_)
      (score / mp) * 100
    }, numeric(1))

    pcts <- pcts[!is.na(pcts)]
    if (length(pcts) == 0) next

    result[[aname]] <- list(
      name = aname,
      max_points = mp,
      scores = pcts
    )
  }

  result
}

# Upload activity log: flat reverse-chronological list of all uploads.
# Returns data.frame: assessment, sitting_id, upload_time, upload_date, source_type, source_file, num_scores, num_replaced
get_upload_log <- function(exam_data) {
  empty <- data.frame(
    assessment = character(), sitting_id = integer(), upload_time = character(),
    upload_date = character(), source_type = character(), source_file = character(),
    num_scores = integer(), num_replaced = integer(), stringsAsFactors = FALSE
  )

  if (is.null(exam_data$assessments) || length(exam_data$assessments) == 0) return(empty)

  rows <- list()
  for (aname in names(exam_data$assessments)) {
    a <- exam_data$assessments[[aname]]
    for (s in a$sittings) {
      rows[[length(rows) + 1]] <- data.frame(
        assessment = aname,
        sitting_id = as.integer(s$sitting_id),
        upload_time = s$upload_time %||% "",
        upload_date = s$upload_date %||% "",
        source_type = s$source_type %||% "manual",
        source_file = s$source_file %||% "",
        num_scores = length(s$scores),
        num_replaced = as.integer(s$num_replaced %||% 0L),
        stringsAsFactors = FALSE
      )
    }
  }

  result <- do.call(rbind, rows)
  if (is.null(result) || nrow(result) == 0) return(empty)

  # Sort newest-first: prefer upload_time, fall back to upload_date
  sort_key <- ifelse(result$upload_time != "", result$upload_time, result$upload_date)
  result <- result[order(sort_key, decreasing = TRUE), ]
  rownames(result) <- NULL
  result
}

# Summary table: assessment, max_points, sittings_count, students_count, last_upload, source_type
get_exam_summary <- function(exam_data) {
  empty <- data.frame(
    assessment = character(), max_points = numeric(), sittings_count = integer(),
    students_count = integer(), last_upload = character(), source_type = character(),
    stringsAsFactors = FALSE
  )

  if (is.null(exam_data$assessments) || length(exam_data$assessments) == 0) return(empty)

  rows <- lapply(names(exam_data$assessments), function(aname) {
    a <- exam_data$assessments[[aname]]
    n_sittings <- length(a$sittings)
    n_students <- length(a$active_sitting)
    last_upload <- if (n_sittings > 0) {
      dates <- vapply(a$sittings, function(s) s$upload_date %||% "", character(1))
      dates <- dates[dates != ""]
      if (length(dates) > 0) max(dates) else ""
    } else ""

    # Collect unique source types across sittings
    src_types <- if (n_sittings > 0) {
      unique(vapply(a$sittings, function(s) s$source_type %||% "manual", character(1)))
    } else "manual"
    source_type <- paste(src_types, collapse = ",")

    data.frame(
      assessment = aname, max_points = a$max_points, sittings_count = n_sittings,
      students_count = n_students, last_upload = last_upload, source_type = source_type,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
