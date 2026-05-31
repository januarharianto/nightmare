test_that("folder import cache invalidates when source files change", {
  folder <- tempfile("unit-folder-")
  dir.create(folder)
  writeLines("initial", file.path(folder, "grades.csv"))

  key <- build_folder_cache_key(folder, unit_filter = "TEST1001")
  cached <- list(canvas = "cached", consids = NULL, plans = NULL)
  save_folder_cache(folder, key, cached)

  expect_identical(read_folder_cache(folder, key), cached)

  writeLines("changed", file.path(folder, "grades.csv"))
  changed_key <- build_folder_cache_key(folder, unit_filter = "TEST1001")

  expect_null(read_folder_cache(folder, changed_key))
})

test_that("student search index normalises searchable fields once", {
  students <- data.frame(
    name = c("Ada Lovelace", "Grace Hopper"),
    student_id = c("123456789", "987654321"),
    sis_login_id = c("alove123", "ghopp456"),
    stringsAsFactors = FALSE
  )

  indexed <- prepare_student_search_index(students)

  expect_true(all(c(".search_name", ".search_student_id", ".search_login") %in% names(indexed)))
  expect_equal(filter_student_search_index(indexed, " LOVE ")$student_id, "123456789")
  expect_equal(filter_student_search_index(indexed, "GHOPP")$student_id, "987654321")
  expect_equal(nrow(filter_student_search_index(indexed, "")), 0)
})

test_that("Canvas assignment metadata is parsed once per assignment column", {
  data <- data.frame(
    check.names = FALSE,
    `Quiz 1 [10%] (12345)` = c(8, NA),
    `Survey (99999)` = c(NA, NA)
  )
  points_possible <- data.frame(
    check.names = FALSE,
    `Quiz 1 [10%] (12345)` = 10,
    `Survey (99999)` = 0
  )

  meta <- prepare_canvas_assignment_metadata(
    data,
    names(data),
    points_possible,
    has_points_possible = TRUE
  )

  expect_length(meta, 1)
  expect_equal(meta[[1]]$name, "Quiz 1")
  expect_equal(meta[[1]]$assignment_id, "12345")
  expect_equal(meta[[1]]$max_points, 10)
})
