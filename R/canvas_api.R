# -- canvas_api.R --------------------------------------------------
# Read-only Canvas API gradebook sync.

CANVAS_KEYRING_SERVICE <- "nightmare.canvas"

canvas_value <- function(x, name, default = NULL) {
  if (is.null(x) || is.null(x[[name]])) default else x[[name]]
}

canvas_first_value <- function(..., default = NA_character_) {
  vals <- list(...)
  for (val in vals) {
    if (!is.null(val) && length(val) > 0 && !is.na(val[[1]]) && nzchar(as.character(val[[1]]))) {
      return(as.character(val[[1]]))
    }
  }
  default
}

normalize_canvas_base_url <- function(base_url) {
  if (is.null(base_url) || !nzchar(trimws(base_url))) return("")
  sub("/+$", "", trimws(base_url))
}

canvas_course_id_value <- function(course) {
  if (is.null(course)) return("")
  if (is.list(course) && !is.null(course$course_id)) {
    return(canvas_first_value(course$course_id, default = ""))
  }
  canvas_first_value(course, default = "")
}

canvas_course_config <- function(offering_id, settings = read_settings(), legacy_unit = NULL) {
  canvas <- settings$canvas
  if (is.null(canvas)) canvas <- list()
  offerings <- canvas$offerings
  if (is.null(offerings)) offerings <- list()
  courses <- canvas$courses
  if (is.null(courses)) courses <- list()

  base_url <- normalize_canvas_base_url(canvas$base_url)
  course_id <- canvas_course_id_value(offerings[[offering_id]])

  if (!nzchar(course_id) && !is.null(legacy_unit) && identical(offering_id, legacy_unit)) {
    course_id <- canvas_course_id_value(courses[[legacy_unit]])
  }

  list(
    base_url = base_url,
    course_id = as.character(course_id),
    configured = nzchar(base_url) && nzchar(course_id)
  )
}

save_canvas_course_config <- function(offering_id, base_url, course_id) {
  settings <- read_settings()
  if (is.null(settings$canvas)) settings$canvas <- list()
  settings$canvas$base_url <- normalize_canvas_base_url(base_url)
  settings$canvas$token <- NULL
  if (is.null(settings$canvas$offerings)) settings$canvas$offerings <- list()
  settings$canvas$offerings[[offering_id]] <- list(course_id = as.character(course_id))
  path <- settings_path()
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  save_json(path, settings)
}

canvas_keyring_available <- function() {
  requireNamespace("keyring", quietly = TRUE)
}

canvas_store_token <- function(base_url, token) {
  if (!canvas_keyring_available()) {
    stop("Install the keyring package to store Canvas API tokens securely.", call. = FALSE)
  }
  keyring::key_set_with_value(
    service = CANVAS_KEYRING_SERVICE,
    username = normalize_canvas_base_url(base_url),
    password = token
  )
}

canvas_get_token <- function(base_url) {
  if (!canvas_keyring_available()) {
    stop("Install the keyring package to use Canvas API tokens securely.", call. = FALSE)
  }
  keyring::key_get(
    service = CANVAS_KEYRING_SERVICE,
    username = normalize_canvas_base_url(base_url)
  )
}

canvas_api_url <- function(base_url, path) {
  paste0(normalize_canvas_base_url(base_url), "/api/v1", path)
}

canvas_api_get_paginated <- function(base_url, path, token, query = list()) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Install the httr2 package to refresh Canvas data.", call. = FALSE)
  }

  next_url <- canvas_api_url(base_url, path)
  first_page <- TRUE
  results <- list()

  repeat {
    req <- httr2::request(next_url) |>
      httr2::req_headers_redacted(Authorization = paste("Bearer", token)) |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_retry(max_tries = 3)

    if (first_page && length(query) > 0) {
      req <- do.call(httr2::req_url_query, c(list(req), query))
    }

    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    if (length(body) > 0) results <- c(results, body)

    next_url <- httr2::resp_link_url(resp, "next")
    if (is.null(next_url) || !nzchar(next_url)) break
    first_page <- FALSE
  }

  results
}

canvas_parse_time <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value) || !nzchar(as.character(value))) {
    return(as.POSIXct(NA))
  }
  parsed <- as.POSIXct(strptime(as.character(value), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))
  if (is.na(parsed)) as.POSIXct(NA) else parsed
}

canvas_valid_assignments <- function(assignments) {
  Filter(function(a) {
    pts <- suppressWarnings(as.numeric(canvas_value(a, "points_possible", NA_real_)))
    !is.na(pts) && pts > 0 && !isTRUE(canvas_value(a, "hide_in_gradebook", FALSE))
  }, assignments)
}

canvas_submission_maps <- function(submissions) {
  maps <- list()
  for (row in submissions) {
    user_id <- canvas_first_value(canvas_value(row, "user_id"), default = "")
    if (!nzchar(user_id)) next

    submission_map <- list()
    for (sub in canvas_value(row, "submissions", list())) {
      assignment_id <- canvas_first_value(canvas_value(sub, "assignment_id"), default = "")
      if (nzchar(assignment_id)) submission_map[[assignment_id]] <- sub
    }
    maps[[user_id]] <- submission_map
  }
  maps
}

canvas_api_to_gradebook <- function(assignments, enrollments, submissions, unit = NULL) {
  assignments <- canvas_valid_assignments(assignments)
  submission_maps <- canvas_submission_maps(submissions)

  empty_assignments <- data.frame(
    name = character(), score = numeric(), max_points = numeric(),
    percentage = numeric(), is_ongoing = logical(), assignment_id = character(),
    stringsAsFactors = FALSE
  )

  if (length(enrollments) == 0) {
    result <- data.frame(
      student_id = character(), name = character(), canvas_id = character(),
      sis_login_id = character(), section = character(), unit_of_study = character(),
      email = character(), final_grade = numeric(), stringsAsFactors = FALSE
    )
    result$assignments <- list()
    attr(result, "due_dates") <- list()
    attr(result, "canvas_source") <- "api"
    return(result)
  }

  rows <- lapply(enrollments, function(enrollment) {
    user <- canvas_value(enrollment, "user", list())
    user_id <- canvas_first_value(canvas_value(enrollment, "user_id"), canvas_value(user, "id"), default = "")
    submission_map <- submission_maps[[user_id]]
    if (is.null(submission_map)) submission_map <- list()

    assignment_rows <- lapply(assignments, function(assignment) {
      assignment_id <- canvas_first_value(canvas_value(assignment, "id"), default = "")
      max_points <- suppressWarnings(as.numeric(canvas_value(assignment, "points_possible", NA_real_)))
      sub <- submission_map[[assignment_id]]
      score <- if (is.null(sub)) NA_real_ else suppressWarnings(as.numeric(canvas_value(sub, "score", NA_real_)))
      pct <- if (!is.na(score) && !is.na(max_points) && max_points > 0) score / max_points * 100 else NA_real_
      due_at <- canvas_parse_time(canvas_value(assignment, "due_at"))

      data.frame(
        name = as.character(canvas_value(assignment, "name", "")),
        score = score,
        max_points = max_points,
        percentage = pct,
        is_ongoing = !is.na(due_at) && due_at > Sys.time() && is.na(score),
        assignment_id = assignment_id,
        stringsAsFactors = FALSE
      )
    })

    assignments_df <- rbind_or_empty(assignment_rows, empty_assignments)
    grades <- canvas_value(enrollment, "grades", list())

    row <- data.frame(
      student_id = canvas_first_value(
        canvas_value(user, "sis_user_id"),
        canvas_value(enrollment, "sis_user_id"),
        canvas_value(user, "id"),
        default = user_id
      ),
      name = canvas_first_value(canvas_value(user, "name"), canvas_value(enrollment, "user_name"), default = ""),
      canvas_id = canvas_first_value(canvas_value(user, "id"), canvas_value(enrollment, "user_id"), default = user_id),
      sis_login_id = canvas_first_value(canvas_value(user, "login_id"), canvas_value(enrollment, "sis_login_id"), default = ""),
      section = canvas_first_value(canvas_value(enrollment, "section"), default = as.character(unit)),
      unit_of_study = as.character(unit),
      email = canvas_first_value(canvas_value(user, "email"), default = ""),
      final_grade = suppressWarnings(as.numeric(canvas_value(grades, "current_score", NA_real_))),
      stringsAsFactors = FALSE
    )
    row$assignments <- list(assignments_df)
    row
  })

  result <- do.call(rbind, rows)
  result$student_id <- as.character(result$student_id)

  due_dates <- list()
  for (assignment in assignments) {
    due_at <- canvas_parse_time(canvas_value(assignment, "due_at"))
    if (!is.na(due_at)) due_dates[[as.character(canvas_value(assignment, "name", ""))]] <- due_at
  }
  attr(result, "due_dates") <- due_dates
  attr(result, "canvas_source") <- "api"
  result
}

fetch_canvas_gradebook <- function(base_url, course_id, token, unit = NULL,
                                   requester = canvas_api_get_paginated) {
  assignments <- requester(
    base_url, paste0("/courses/", course_id, "/assignments"), token,
    query = list(per_page = 100)
  )
  enrollments <- requester(
    base_url, paste0("/courses/", course_id, "/enrollments"), token,
    query = list(per_page = 100, "type[]" = "StudentEnrollment", "include[]" = "current_points")
  )
  submissions <- requester(
    base_url, paste0("/courses/", course_id, "/students/submissions"), token,
    query = list(per_page = 100, "student_ids[]" = "all", grouped = "true")
  )

  canvas <- canvas_api_to_gradebook(assignments, enrollments, submissions, unit = unit)
  metadata <- list(
    base_url = normalize_canvas_base_url(base_url),
    course_id = as.character(course_id),
    fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    student_count = nrow(canvas),
    assignment_count = if (nrow(canvas) == 0) 0L else nrow(canvas$assignments[[1]])
  )

  list(canvas = canvas, metadata = metadata)
}

canvas_snapshot_path <- function(folder_path) {
  file.path(folder_path, ".nightmare", "canvas_api_snapshot.rds")
}

canvas_apply_snapshot_metadata <- function(canvas, metadata = list()) {
  if (is.null(canvas)) return(canvas)

  academic_year <- canvas_value(metadata, "academic_year", default = NULL)
  if (!is.null(academic_year) && (is.null(attr(canvas, "academic_year")) || is.na(attr(canvas, "academic_year")))) {
    attr(canvas, "academic_year") <- academic_year
  }

  semester <- canvas_value(metadata, "semester", default = NULL)
  if (!is.null(semester) && (is.null(attr(canvas, "semester")) || is.na(attr(canvas, "semester")))) {
    attr(canvas, "semester") <- semester
  }

  canvas
}

save_canvas_api_snapshot <- function(folder_path, canvas, metadata = list()) {
  nightmare_dir <- file.path(folder_path, ".nightmare")
  if (!dir.exists(nightmare_dir)) dir.create(nightmare_dir, recursive = TRUE)

  metadata$token <- NULL
  metadata$saved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  canvas <- canvas_apply_snapshot_metadata(canvas, metadata)
  saveRDS(list(canvas = canvas, metadata = metadata), canvas_snapshot_path(folder_path))
  invisible(canvas_snapshot_path(folder_path))
}

load_canvas_api_snapshot <- function(folder_path) {
  path <- canvas_snapshot_path(folder_path)
  if (!file.exists(path)) return(NULL)
  snapshot <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(snapshot)) return(NULL)
  snapshot$canvas <- canvas_apply_snapshot_metadata(snapshot$canvas, snapshot$metadata)
  snapshot
}

canvas_refresh_status <- function(folder_path) {
  snapshot <- load_canvas_api_snapshot(folder_path)
  if (is.null(snapshot)) return(NULL)
  snapshot$metadata
}
