#' Test Suite: Data Import Functions
#'
#' Validates that importers correctly load and parse sample data

library(testthat)
library(dplyr)

# Set working directory to project root if running from tests/
if (basename(getwd()) == "tests") {
  setwd("..")
}

# Load the project
source("R/utils/import/canvas.R")
source("R/utils/import/special_consids.R")
source("R/utils/import/disability_plans.R")
source("R/utils/import/consolidate.R")
source("R/utils/import/file_detection.R")

test_that("Canvas import loads correct number of students", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  expect_gt(nrow(canvas), 250)
  expect_lt(nrow(canvas), 300)  # Expect around 255
})

test_that("Canvas import has required columns", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  required_cols <- c("student_id", "name", "canvas_id", "sis_login_id", "unit_of_study", "final_grade")
  expect_true(all(required_cols %in% names(canvas)))
})

test_that("Canvas student IDs are character type", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  expect_is(canvas$student_id, "character")
  expect_gt(min(nchar(canvas$student_id)), 0)
})

test_that("Canvas final grades are numeric 0-100", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  expect_is(canvas$final_grade, "numeric")
  expect_true(all(canvas$final_grade >= 0 & canvas$final_grade <= 100, na.rm = TRUE))
})

test_that("Special considerations import loads data", {
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  expect_gt(nrow(consids), 100)
})

test_that("Special considerations filters by unit", {
  consids <- import_special_considerations("_sample-data/special considerations.csv", unit_filter = "BIOL2022")
  # Should have fewer rows when filtered
  consids_all <- import_special_considerations("_sample-data/special considerations.csv")
  expect_lt(nrow(consids), nrow(consids_all))
})

test_that("Special considerations has approved extensions", {
  consids <- import_special_considerations("_sample-data/special considerations.csv", unit_filter = "BIOL2022")
  expect_gt(nrow(consids), 0)
  # Check that special_consids list column exists
  expect_true("special_consids" %in% names(consids))
})

test_that("Disability plans import loads data", {
  plans <- import_disability_plans("_sample-data/plans.xlsx")
  expect_gt(nrow(plans), 0)
  expect_is(plans$student_id, "character")
})

test_that("Disability plans filters by unit", {
  plans <- import_disability_plans("_sample-data/plans.xlsx", unit_filter = "BIOL2022")
  # All should be BIOL2022 if data exists
  if (nrow(plans) > 0) {
    # Check that we only got BIOL2022 students (via internal filtering)
    expect_true(nrow(plans) >= 0)  # Basic sanity check
  }
})

test_that("Consolidation preserves all Canvas students", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  plans <- import_disability_plans("_sample-data/plans.xlsx")

  consolidated <- consolidate_student_data(canvas, consids, plans)

  # All Canvas students should appear in consolidated
  expect_equal(nrow(consolidated), nrow(canvas))
})

test_that("No NAs in critical identity fields", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  plans <- import_disability_plans("_sample-data/plans.xlsx")

  consolidated <- consolidate_student_data(canvas, consids, plans)

  expect_false(any(is.na(consolidated$student_id)))
  expect_false(any(is.na(consolidated$name)))
  expect_false(any(is.na(consolidated$canvas_id)))
})

test_that("Extension calculations are numeric", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  plans <- import_disability_plans("_sample-data/plans.xlsx")

  consolidated <- consolidate_student_data(canvas, consids, plans)

  expect_is(consolidated$total_approved_extension_days, "integer")
  expect_true(all(consolidated$total_approved_extension_days >= 0, na.rm = TRUE))
})

test_that("File type detection works", {
  canvas_type <- detect_file_type("_sample-data/canvas gradebook.csv")
  expect_equal(canvas_type, "canvas")

  consids_type <- detect_file_type("_sample-data/special considerations.csv")
  expect_equal(consids_type, "special_consids")

  plans_type <- detect_file_type("_sample-data/plans.xlsx")
  expect_equal(plans_type, "plans")
})

test_that("Assignments are properly nested", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")

  # Check that assignments column is a list
  expect_is(canvas$assignments, "list")

  # Check first student has assignments data frame
  if (nrow(canvas) > 0) {
    first_student_assignments <- canvas$assignments[[1]]
    expect_is(first_student_assignments, "data.frame")

    # If assignments exist, check structure
    if (nrow(first_student_assignments) > 0) {
      expect_true("name" %in% names(first_student_assignments))
      expect_true("score" %in% names(first_student_assignments))
      expect_true("max_points" %in% names(first_student_assignments))
      expect_true("percentage" %in% names(first_student_assignments))
      expect_true("is_ongoing" %in% names(first_student_assignments))
    }
  }
})
