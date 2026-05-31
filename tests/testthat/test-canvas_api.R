local_temp_settings_dir <- function() {
  temp_home <- tempfile("nightmare-settings-")
  dir.create(temp_home)
  withr::local_envvar(
    c(R_USER_DATA_DIR = temp_home, R_USER_CONFIG_DIR = temp_home),
    .local_envir = parent.frame()
  )
  temp_home
}

test_that("Canvas API responses convert to the existing gradebook shape", {
  assignments <- list(
    list(id = 11, name = "Quiz 1", points_possible = 10, due_at = "2026-03-01T23:59:00Z"),
    list(id = 12, name = "Hidden", points_possible = 5, hide_in_gradebook = TRUE),
    list(id = 13, name = "Ungraded", points_possible = NULL)
  )
  enrollments <- list(
    list(
      user_id = 501,
      grades = list(current_score = 82.5),
      user = list(
        id = 501, name = "Ada Lovelace", sis_user_id = "123456789",
        login_id = "alove123", sortable_name = "Lovelace, Ada"
      )
    )
  )
  submissions <- list(
    list(
      user_id = 501,
      submissions = list(
        list(assignment_id = 11, score = 8)
      )
    )
  )

  canvas <- canvas_api_to_gradebook(assignments, enrollments, submissions, unit = "TEST1001")

  expect_equal(nrow(canvas), 1)
  expect_equal(canvas$student_id, "123456789")
  expect_equal(canvas$sis_login_id, "alove123")
  expect_equal(canvas$final_grade, 82.5)
  expect_equal(canvas$unit_of_study, "TEST1001")

  first_assignments <- canvas$assignments[[1]]
  expect_equal(nrow(first_assignments), 1)
  expect_equal(first_assignments$name, "Quiz 1")
  expect_equal(first_assignments$score, 8)
  expect_equal(first_assignments$max_points, 10)
  expect_equal(first_assignments$percentage, 80)
  expect_equal(first_assignments$assignment_id, "11")
  expect_equal(names(attr(canvas, "due_dates")), "Quiz 1")
})

test_that("Canvas API fetcher uses read-only endpoints and injected requester", {
  calls <- list()
  requester <- function(base_url, path, token, query = list()) {
    calls[[length(calls) + 1]] <<- list(
      base_url = base_url, path = path, token = token, query = query
    )
    if (grepl("/assignments$", path)) return(list(
      list(id = 11, name = "Quiz 1", points_possible = 10)
    ))
    if (grepl("/enrollments$", path)) return(list(
      list(user_id = 501, grades = list(current_score = 80),
           user = list(id = 501, name = "Ada", sis_user_id = "123", login_id = "ada"))
    ))
    if (grepl("/students/submissions$", path)) return(list(
      list(user_id = 501, submissions = list(list(assignment_id = 11, score = 7)))
    ))
    stop("Unexpected path: ", path)
  }

  result <- fetch_canvas_gradebook(
    base_url = "https://canvas.example.edu/",
    course_id = "42",
    token = "secret",
    unit = "TEST1001",
    requester = requester
  )

  expect_equal(nrow(result$canvas), 1)
  expect_equal(vapply(calls, `[[`, character(1), "path"), c(
    "/courses/42/assignments",
    "/courses/42/enrollments",
    "/courses/42/students/submissions"
  ))
  expect_true(all(vapply(calls, function(x) identical(x$token, "secret"), logical(1))))
  expect_equal(calls[[3]]$query[["student_ids[]"]], "all")
  expect_true(isTRUE(result$metadata$student_count == 1))
})

test_that("Canvas snapshot round-trips without storing credentials", {
  folder <- tempfile("canvas-snapshot-")
  dir.create(folder)
  canvas <- data.frame(
    student_id = "123", name = "Ada", canvas_id = "501",
    sis_login_id = "ada", section = "TEST1001", unit_of_study = "TEST1001",
    email = "ada@example.edu", final_grade = 80,
    stringsAsFactors = FALSE
  )
  canvas$assignments <- list(data.frame(
    name = "Quiz 1", score = 7, max_points = 10, percentage = 70,
    is_ongoing = FALSE, assignment_id = "11", stringsAsFactors = FALSE
  ))
  metadata <- list(
    base_url = "https://canvas.example.edu", course_id = "42",
    academic_year = 2026L, semester = "S1C"
  )

  save_canvas_api_snapshot(folder, canvas, metadata)
  snapshot <- load_canvas_api_snapshot(folder)

  expect_equal(snapshot$canvas$student_id, "123")
  expect_equal(snapshot$metadata$course_id, "42")
  expect_null(snapshot$metadata$token)
  expect_equal(attr(snapshot$canvas, "academic_year"), 2026L)
  expect_equal(attr(snapshot$canvas, "semester"), "S1C")
})

test_that("Canvas course settings persist without API tokens", {
  local_temp_settings_dir()

  save_canvas_course_config(
    offering_id = "TEST1001/2026-S1C",
    base_url = "https://canvas.example.edu/",
    course_id = "42"
  )
  settings <- read_settings()
  settings_text <- paste(readLines(settings_path(), warn = FALSE), collapse = "\n")

  expect_equal(settings$canvas$base_url, "https://canvas.example.edu")
  expect_equal(settings$canvas$offerings[["TEST1001/2026-S1C"]]$course_id, "42")
  expect_null(settings$canvas$courses)
  expect_false(grepl("secret-token", settings_text, fixed = TRUE))
  expect_false("token" %in% names(settings$canvas))
})

test_that("Canvas course settings scrub existing plaintext API tokens", {
  local_temp_settings_dir()
  save_settings(list(
    data_dir = "/tmp/nightmare-data",
    canvas = list(
      base_url = "https://old-canvas.example.edu",
      token = "secret-token",
      courses = list(TEST1001 = list(course_id = "old-course")),
      offerings = list("TEST1001/2025-S1C" = list(course_id = "old-offering"))
    )
  ))

  save_canvas_course_config(
    offering_id = "TEST1001/2026-S1C",
    base_url = "https://canvas.example.edu/",
    course_id = "42"
  )
  settings <- read_settings()
  settings_text <- paste(readLines(settings_path(), warn = FALSE), collapse = "\n")

  expect_equal(settings$data_dir, "/tmp/nightmare-data")
  expect_equal(settings$canvas$base_url, "https://canvas.example.edu")
  expect_equal(settings$canvas$courses$TEST1001$course_id, "old-course")
  expect_equal(settings$canvas$offerings[["TEST1001/2025-S1C"]]$course_id, "old-offering")
  expect_equal(settings$canvas$offerings[["TEST1001/2026-S1C"]]$course_id, "42")
  expect_null(settings$canvas$token)
  expect_false(grepl("secret-token", settings_text, fixed = TRUE))
})

test_that("Canvas course config is keyed by offering id", {
  local_temp_settings_dir()

  save_canvas_course_config(
    offering_id = "ENVX2001/2025-S1C",
    base_url = "https://canvas.example.edu",
    course_id = "111"
  )
  save_canvas_course_config(
    offering_id = "ENVX2001/2026-S1C",
    base_url = "https://canvas.example.edu",
    course_id = "222"
  )

  expect_equal(canvas_course_config("ENVX2001/2025-S1C")$course_id, "111")
  expect_equal(canvas_course_config("ENVX2001/2026-S1C")$course_id, "222")
})

test_that("legacy unit-keyed Canvas config is only used when requested", {
  settings <- list(canvas = list(
    base_url = "https://canvas.example.edu",
    courses = list(ENVX2001 = list(course_id = "111"))
  ))

  legacy <- canvas_course_config("ENVX2001", settings = settings, legacy_unit = "ENVX2001")
  nested <- canvas_course_config("ENVX2001/2026-S1C", settings = settings, legacy_unit = NULL)

  expect_true(legacy$configured)
  expect_false(nested$configured)
})

test_that("offering_canvas_config maps legacy and nested offerings to Canvas settings", {
  local_temp_settings_dir()
  save_settings(list(canvas = list(
    base_url = "https://canvas.example.edu/",
    courses = list(ENVX2001 = list(course_id = "legacy-course")),
    offerings = list("ENVX2001/2026-S1C" = list(course_id = "nested-course"))
  )))

  legacy_offering <- data.frame(
    offering_id = "ENVX2001",
    unit = "ENVX2001",
    year = NA_integer_,
    semester = NA_character_,
    path = "/data/ENVX2001",
    label = "Legacy folder: ENVX2001/",
    legacy = TRUE,
    stringsAsFactors = FALSE
  )
  nested_offering <- data.frame(
    offering_id = "ENVX2001/2026-S1C",
    unit = "ENVX2001",
    year = 2026L,
    semester = "S1C",
    path = "/data/ENVX2001/2026-S1C",
    label = "2026 S1C",
    legacy = FALSE,
    stringsAsFactors = FALSE
  )

  expect_equal(offering_canvas_config(legacy_offering)$course_id, "legacy-course")
  expect_equal(offering_canvas_config(nested_offering)$course_id, "nested-course")
})

test_that("folder loader reuses Canvas API snapshot as the Canvas source", {
  folder <- tempfile("canvas-folder-")
  dir.create(folder)
  canvas <- data.frame(
    student_id = "123", name = "Ada", canvas_id = "501",
    sis_login_id = "ada", section = "TEST1001", unit_of_study = "TEST1001",
    email = "ada@example.edu", final_grade = 80,
    stringsAsFactors = FALSE
  )
  canvas$assignments <- list(data.frame(
    name = "Quiz 1", score = 7, max_points = 10, percentage = 70,
    is_ongoing = FALSE, assignment_id = "11", stringsAsFactors = FALSE
  ))

  save_canvas_api_snapshot(folder, canvas, list(
    course_id = "42", academic_year = 2026L, semester = "S1C"
  ))

  loaded <- load_folder(folder, unit_filter = "TEST1001")

  expect_equal(loaded$canvas$student_id, "123")
  expect_equal(attr(loaded$canvas, "academic_year"), 2026L)
  expect_equal(attr(loaded$canvas, "semester"), "S1C")
  expect_null(loaded$consids)
  expect_null(loaded$plans)
})
