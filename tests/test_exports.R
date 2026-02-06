#' Test Suite: Export Functions
#'
#' Validates that all export formats produce valid output

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
source("R/utils/export.R")

test_that("Canvas export has required columns", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)

  export <- export_for_canvas(consolidated)

  expect_true("SIS Login ID" %in% names(export))
  expect_true("Student Name" %in% names(export))
  expect_true("Final Grade" %in% names(export))
})

test_that("Canvas export row count matches input", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  export <- export_for_canvas(canvas)

  expect_equal(nrow(export), nrow(canvas))
})

test_that("Extensions report has data", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)

  export <- export_extensions_report(consolidated)

  # Should have data if extensions exist in sample data
  # Or should return empty data.frame with correct structure
  expect_is(export, "data.frame")
  expect_true(all(c("Student ID", "Name", "Assessment", "Extension Days", "Reason") %in% names(export)))
})

test_that("At-risk report filters correctly", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)
  scored <- apply_risk_scoring(consolidated)

  export <- export_at_risk_report(scored)

  # Should only contain High/Medium risk students
  if (nrow(export) > 0) {
    expect_true(all(export$`Risk Category` %in% c("High", "Medium")))
  }
})

test_that("At-risk report is sorted by risk score", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)
  scored <- apply_risk_scoring(consolidated)

  export <- export_at_risk_report(scored)

  if (nrow(export) > 1) {
    # Risk scores should be in descending order
    scores <- export$`Risk Score`
    expect_true(all(diff(scores) <= 0))
  }
})

test_that("CSV export is valid", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  export <- export_for_canvas(canvas)

  temp_file <- tempfile(fileext = ".csv")
  write.csv(export, temp_file, row.names = FALSE)

  # Should be readable
  reloaded <- read.csv(temp_file)
  expect_equal(nrow(export), nrow(reloaded))

  unlink(temp_file)
})

test_that("Format risk factors helper works", {
  # Test with character vector
  factors1 <- c("Multiple extensions", "Failing grade")
  result1 <- format_risk_factors(factors1)
  expect_is(result1, "character")
  expect_true(grepl(";", result1))

  # Test with NULL
  result2 <- format_risk_factors(NULL)
  expect_equal(result2, "None")

  # Test with empty vector
  result3 <- format_risk_factors(character(0))
  expect_equal(result3, "None")
})

test_that("Comprehensive export creates multiple sheets", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)
  scored <- apply_risk_scoring(consolidated)

  # Export to sheets (in-memory)
  sheets <- export_comprehensive_report(scored, output_file = NULL)

  expect_is(sheets, "list")
  expect_true("Summary" %in% names(sheets))
  expect_true("All Students" %in% names(sheets))
  expect_true("Extensions" %in% names(sheets))
  expect_true("At-Risk Students" %in% names(sheets))
  expect_true("Disability Plans" %in% names(sheets))

  # Check Summary sheet structure
  expect_is(sheets$Summary, "data.frame")
  expect_true("Metric" %in% names(sheets$Summary))
  expect_true("Value" %in% names(sheets$Summary))
})

test_that("Export handles students with no risk factors", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consolidated <- consolidate_student_data(canvas, NULL, NULL)
  scored <- apply_risk_scoring(consolidated)

  # Should still export successfully
  export <- export_for_canvas(scored)
  expect_is(export, "data.frame")
  expect_equal(nrow(export), nrow(scored))
})
