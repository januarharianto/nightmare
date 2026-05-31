source("../../R/canvas_api.R")
source("../../R/import_file_detection.R")
source("../../R/import_canvas.R")
source("../../R/import_consolidate.R")
source("../../R/utils_extensions_data.R")
source("../../R/utils_storage.R")
source("../../R/utils_notes_data.R")
source("../../R/utils_exam_data.R")
source("../../R/utils_weights_data.R")
source("../../R/import_special_consids.R")
source("../../R/import_folder_loader.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(rlang)
  library(stringr)
})

touch_file <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines("placeholder", path)
}

write_canvas_year_file <- function(path, unit = "ENVX2001", year = "2026") {
  writeLines(
    c(
      "Student,ID,SIS User ID,SIS Login ID,Section",
      "Points Possible,,,,",
      sprintf("\"Student, Test\",1,1,test,%s-%s-S1C", year, unit)
    ),
    path
  )
}

write_special_consids_year_file <- function(path, unit = "ENVX2001") {
  writeLines(
    c(
      "number,state,availability,student_id,u_outcome_type,assessment",
      sprintf("SC-2025,Approved,%s 2025 S1C,202500001,Extension of time,Assessment 1", unit),
      sprintf("SC-2026,Approved,%s 2026 S1C,202600001,Extension of time,Assessment 1", unit)
    ),
    path
  )
}

test_that("scan_data_offerings discovers legacy and nested offerings", {
  root <- tempfile("data-root-")
  dir.create(root)

  touch_file(file.path(root, "ENVX2001", "canvas.csv"))
  touch_file(file.path(root, "BIOL2022", "2026-S2C", "canvas.csv"))

  offerings <- scan_data_offerings(root)

  expect_setequal(offerings$offering_id, c("ENVX2001", "BIOL2022/2026-S2C"))

  legacy <- offerings[offerings$offering_id == "ENVX2001", ]
  expect_equal(legacy$unit, "ENVX2001")
  expect_true(legacy$legacy)
  expect_equal(legacy$label, "Legacy folder: ENVX2001/")

  nested <- offerings[offerings$offering_id == "BIOL2022/2026-S2C", ]
  expect_equal(nested$unit, "BIOL2022")
  expect_equal(nested$year, 2026L)
  expect_equal(nested$semester, "S2C")
  expect_false(nested$legacy)
})

test_that("scan_data_offerings ignores folders without supported source files", {
  root <- tempfile("data-root-")
  dir.create(root)

  touch_file(file.path(root, "ENVX2001", ".nightmare", "import_cache.rds"))
  dir.create(file.path(root, "ENVX2001", "2026-S1C"), recursive = TRUE)

  offerings <- scan_data_offerings(root)

  expect_equal(nrow(offerings), 0L)
})

test_that("scan_data_folders only returns legacy folders with direct data", {
  root <- tempfile("data-root-")
  dir.create(root)

  touch_file(file.path(root, "ENVX2001", "canvas.csv"))
  touch_file(file.path(root, "BIOL2022", "2026-S2C", "canvas.csv"))

  expect_equal(scan_data_folders(root), "ENVX2001")
})

test_that("last offering is read and written separately from last unit", {
  root <- tempfile("data-root-")
  dir.create(root)

  save_last_offering(root, "ENVX2001/2026-S1C")

  expect_equal(read_last_offering(root), "ENVX2001/2026-S1C")
  expect_null(read_last_unit(root))
})

test_that("resolve_saved_offering prefers exact last offering", {
  root <- tempfile("data-root-")
  dir.create(root)

  touch_file(file.path(root, "ENVX2001", "2025-S1C", "canvas.csv"))
  touch_file(file.path(root, "ENVX2001", "2026-S1C", "canvas.csv"))
  offerings <- scan_data_offerings(root)
  save_last_offering(root, "ENVX2001/2025-S1C")

  selected <- resolve_saved_offering(root, offerings)

  expect_equal(selected$offering_id, "ENVX2001/2025-S1C")
})

test_that("resolve_saved_offering falls back from last unit to newest dated offering", {
  root <- tempfile("data-root-")
  dir.create(root)

  touch_file(file.path(root, "ENVX2001", "2025-S1C", "canvas.csv"))
  touch_file(file.path(root, "ENVX2001", "2026-S1C", "canvas.csv"))
  offerings <- scan_data_offerings(root)
  save_last_unit(root, "ENVX2001")

  selected <- resolve_saved_offering(root, offerings)

  expect_equal(selected$offering_id, "ENVX2001/2026-S1C")
})

test_that("folder cache key includes explicit year filter", {
  root <- withr::local_tempdir()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  writeLines("x", file.path(root, "canvas.csv"))

  key_2025 <- build_folder_cache_key(root, unit_filter = "ENVX2001", year_filter = "2025")
  key_2026 <- build_folder_cache_key(root, unit_filter = "ENVX2001", year_filter = "2026")

  expect_false(identical(key_2025, key_2026))
})

test_that("explicit year filter wins for special considerations", {
  root <- withr::local_tempdir()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  write_special_consids_year_file(file.path(root, "a-special-consids.csv"))
  write_canvas_year_file(file.path(root, "z-canvas gradebook.csv"), year = "2025")

  loaded <- suppressMessages(load_folder(root, unit_filter = "ENVX2001", year_filter = "2026"))

  expect_equal(loaded$consids$student_id, "202600001")
})

test_that("legacy folder derives special consideration year from Canvas regardless of file order", {
  root <- withr::local_tempdir()
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  write_special_consids_year_file(file.path(root, "a-special-consids.csv"))
  write_canvas_year_file(file.path(root, "z-canvas gradebook.csv"), year = "2026")

  loaded <- suppressMessages(load_folder(root, unit_filter = "ENVX2001"))

  expect_equal(loaded$consids$student_id, "202600001")
})

test_that("offering storage directories are isolated by path", {
  root <- withr::local_tempdir()
  offering_2025 <- file.path(root, "ENVX2001", "2025-S1C")
  offering_2026 <- file.path(root, "ENVX2001", "2026-S1C")

  dir.create(offering_2025, recursive = TRUE)
  dir.create(offering_2026, recursive = TRUE)

  save_nightmare_json_for_path(offering_2025, "marker.json", list(year = "2025"))
  save_nightmare_json_for_path(offering_2026, "marker.json", list(year = "2026"))

  expect_equal(load_json_for_path(offering_2025, "marker.json", list())$year, "2025")
  expect_equal(load_json_for_path(offering_2026, "marker.json", list())$year, "2026")
})

test_that("path-saved notes can be read by unit-based loader", {
  root <- withr::local_tempdir()
  offering_path <- file.path(root, "ENVX2001", "2026-S1C")
  dir.create(offering_path, recursive = TRUE)

  notes <- list("123456789" = list(list(id = "note-1", text = "Path note")))

  save_notes_data_for_path(offering_path, notes)

  loaded <- load_student_notes(root, "ENVX2001/2026-S1C")
  expect_equal(loaded[["123456789"]][[1]]$text, "Path note")
})

test_that("path-saved exams can be read by unit-based loader", {
  root <- withr::local_tempdir()
  offering_path <- file.path(root, "ENVX2001", "2026-S1C")
  dir.create(offering_path, recursive = TRUE)

  exam_data <- list(assessments = list(Final = list(max_points = 60)))

  save_exam_data_for_path(offering_path, exam_data)

  loaded <- load_exam_data(root, "ENVX2001/2026-S1C")
  expect_equal(loaded$assessments$Final$max_points, 60)
})

test_that("path-saved weights can be read by unit-based loader", {
  root <- withr::local_tempdir()
  offering_path <- file.path(root, "ENVX2001", "2026-S1C")
  dir.create(offering_path, recursive = TRUE)

  weights_data <- list(weights = list(Quiz = 15), due_dates = list(Quiz = "2026-04-01"))

  save_weights_data_for_path(offering_path, weights_data)

  loaded <- load_weights_data(root, "ENVX2001/2026-S1C")
  expect_equal(loaded$weights$Quiz, 15)
  expect_equal(loaded$due_dates$Quiz, "2026-04-01")
})

test_that("unit-saved persistence can be read by path-based loaders", {
  root <- withr::local_tempdir()
  offering <- "ENVX2001/2026-S1C"
  offering_path <- file.path(root, "ENVX2001", "2026-S1C")

  save_student_notes(root, offering, list("123456789" = list(list(id = "note-1", text = "Unit note"))))
  save_exam_data(root, offering, list(assessments = list(Final = list(max_points = 70))))
  save_weights_data(root, offering, list(weights = list(Quiz = 20), due_dates = list()))

  expect_equal(load_notes_data_for_path(offering_path)[["123456789"]][[1]]$text, "Unit note")
  expect_equal(load_exam_data_for_path(offering_path)$assessments$Final$max_points, 70)
  expect_equal(load_weights_data_for_path(offering_path)$weights$Quiz, 20)
})

test_that("resolve_saved_offering selects newest offering by real unit code", {
  root <- withr::local_tempdir()
  offerings <- data.frame(
    offering_id = c("ENVX2001/2025-S1C", "ENVX2001/2026-S1C", "BIOL1001/2026-S2C"),
    unit = c("ENVX2001", "ENVX2001", "BIOL1001"),
    year = c(2025L, 2026L, 2026L),
    semester = c("S1C", "S1C", "S2C"),
    path = c("/data/ENVX2001/2025-S1C", "/data/ENVX2001/2026-S1C", "/data/BIOL1001/2026-S2C"),
    label = c("2025 S1C", "2026 S1C", "2026 S2C"),
    legacy = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  save_last_unit(root, "ENVX2001")

  selected <- resolve_saved_offering(root, offerings)

  expect_equal(selected$offering_id, "ENVX2001/2026-S1C")
  expect_equal(selected$unit, "ENVX2001")
})

test_that("newest_offering_for_unit selects newest nested offering for a unit", {
  offerings <- data.frame(
    offering_id = c("ENVX2001", "ENVX2001/2025-S1C", "ENVX2001/2026-S1C", "BIOL1001/2026-S2C"),
    unit = c("ENVX2001", "ENVX2001", "ENVX2001", "BIOL1001"),
    year = c(NA_integer_, 2025L, 2026L, 2026L),
    semester = c(NA_character_, "S1C", "S1C", "S2C"),
    path = c(
      "/data/ENVX2001",
      "/data/ENVX2001/2025-S1C",
      "/data/ENVX2001/2026-S1C",
      "/data/BIOL1001/2026-S2C"
    ),
    label = c("Legacy folder: ENVX2001/", "2025 S1C", "2026 S1C", "2026 S2C"),
    legacy = c(TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  selected <- newest_offering_for_unit(offerings, "ENVX2001")

  expect_equal(selected$offering_id, "ENVX2001/2026-S1C")
  expect_equal(selected$unit, "ENVX2001")
})
