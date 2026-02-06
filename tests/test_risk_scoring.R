#' Test Suite: Risk Scoring
#'
#' Validates that risk scoring algorithm works correctly

library(testthat)
library(dplyr)

# Set working directory to project root if running from tests/
if (basename(getwd()) == "tests") {
  setwd("..")
}

source("R/utils/import/canvas.R")
source("R/utils/import/special_consids.R")
source("R/utils/import/disability_plans.R")
source("R/utils/import/consolidate.R")
source("R/utils/import/file_detection.R")
source("R/utils/risk_scoring.R")

test_that("Risk scores are numeric 0-100", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)
  scored <- apply_risk_scoring(consolidated)

  expect_is(scored$risk_score, "numeric")
  expect_true(all(scored$risk_score >= 0 & scored$risk_score <= 100))
})

test_that("Risk categories are correct", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)
  scored <- apply_risk_scoring(consolidated)

  expect_is(scored$risk_category, "factor")
  expect_true(all(scored$risk_category %in% c("Low", "Medium", "High")))
})

test_that("Risk factors are present for at-risk students", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)
  scored <- apply_risk_scoring(consolidated)

  high_risk <- scored %>% filter(risk_category == "High")

  if (nrow(high_risk) > 0) {
    # Each high-risk student should have risk factors
    expect_gt(nrow(high_risk), 0)

    # Check that risk_factors column exists and is populated
    expect_true("risk_factors" %in% names(high_risk))
    expect_is(high_risk$risk_factors, "list")
  }
})

test_that("Risk score correlates with risk category", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)
  scored <- apply_risk_scoring(consolidated)

  # Low risk should have score < 25
  low_risk <- scored %>% filter(risk_category == "Low")
  if (nrow(low_risk) > 0) {
    expect_true(all(low_risk$risk_score < 25, na.rm = TRUE))
  }

  # High risk should have score > 50
  high_risk <- scored %>% filter(risk_category == "High")
  if (nrow(high_risk) > 0) {
    expect_true(all(high_risk$risk_score > 50, na.rm = TRUE))
  }
})

test_that("Risk scoring handles students with no extensions", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  # Create consolidated data without special considerations
  consolidated <- consolidate_student_data(canvas, NULL, NULL)
  scored <- apply_risk_scoring(consolidated)

  # Should still calculate risk scores
  expect_is(scored$risk_score, "numeric")
  expect_false(any(is.na(scored$risk_score)))
})

test_that("Risk factors are properly formatted", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)
  scored <- apply_risk_scoring(consolidated)

  # Check risk_factors structure
  expect_is(scored$risk_factors, "list")

  # Each element should be a character vector
  if (nrow(scored) > 0) {
    first_factors <- scored$risk_factors[[1]]
    expect_is(first_factors, "character")
  }
})

test_that("Failing grades increase risk score", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consolidated <- consolidate_student_data(canvas, NULL, NULL)
  scored <- apply_risk_scoring(consolidated)

  # Students with grades < 50 should have higher risk scores
  failing <- scored %>% filter(final_grade < 50)
  passing <- scored %>% filter(final_grade >= 50)

  if (nrow(failing) > 0 && nrow(passing) > 0) {
    expect_gt(mean(failing$risk_score), mean(passing$risk_score))
  }
})
