#' Test Suite: Performance Benchmarks
#'
#' Validates that operations complete within target times

library(testthat)

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

test_that("Data import completes in <5 seconds", {
  time <- system.time({
    canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  })
  expect_lt(time["elapsed"], 5)
  message(sprintf("Canvas import time: %.2f seconds", time["elapsed"]))
})

test_that("Special considerations import completes in <5 seconds", {
  time <- system.time({
    consids <- import_special_considerations("_sample-data/special considerations.csv")
  })
  expect_lt(time["elapsed"], 5)
  message(sprintf("Special considerations import time: %.2f seconds", time["elapsed"]))
})

test_that("Disability plans import completes in <5 seconds", {
  time <- system.time({
    plans <- import_disability_plans("_sample-data/plans.xlsx")
  })
  expect_lt(time["elapsed"], 5)
  message(sprintf("Disability plans import time: %.2f seconds", time["elapsed"]))
})

test_that("Consolidation completes in <2 seconds", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  plans <- import_disability_plans("_sample-data/plans.xlsx")

  time <- system.time({
    consolidated <- consolidate_student_data(canvas, consids, plans)
  })
  expect_lt(time["elapsed"], 2)
  message(sprintf("Consolidation time: %.2f seconds", time["elapsed"]))
})

test_that("Risk scoring completes in <2 seconds", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)

  time <- system.time({
    scored <- apply_risk_scoring(consolidated)
  })
  expect_lt(time["elapsed"], 2)
  message(sprintf("Risk scoring time: %.2f seconds", time["elapsed"]))
})

test_that("Export functions complete in <5 seconds", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)
  scored <- apply_risk_scoring(consolidated)

  time <- system.time({
    export1 <- export_for_canvas(scored)
    export2 <- export_extensions_report(scored)
    export3 <- export_at_risk_report(scored)
  })
  expect_lt(time["elapsed"], 5)
  message(sprintf("All exports time: %.2f seconds", time["elapsed"]))
})

test_that("Full pipeline (import to export) completes in <15 seconds", {
  time <- system.time({
    # Import
    canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
    consids <- import_special_considerations("_sample-data/special considerations.csv")
    plans <- import_disability_plans("_sample-data/plans.xlsx")

    # Consolidate
    consolidated <- consolidate_student_data(canvas, consids, plans)

    # Score
    scored <- apply_risk_scoring(consolidated)

    # Export
    export1 <- export_for_canvas(scored)
    export2 <- export_extensions_report(scored)
    export3 <- export_at_risk_report(scored)
  })
  expect_lt(time["elapsed"], 15)
  message(sprintf("Full pipeline time: %.2f seconds", time["elapsed"]))
})

test_that("File type detection is fast (<0.5 seconds per file)", {
  time1 <- system.time({
    type1 <- detect_file_type("_sample-data/canvas gradebook.csv")
  })
  expect_lt(time1["elapsed"], 0.5)

  time2 <- system.time({
    type2 <- detect_file_type("_sample-data/special considerations.csv")
  })
  expect_lt(time2["elapsed"], 0.5)

  time3 <- system.time({
    type3 <- detect_file_type("_sample-data/plans.xlsx")
  })
  expect_lt(time3["elapsed"], 0.5)

  message(sprintf("File detection times: %.3f, %.3f, %.3f seconds",
                  time1["elapsed"], time2["elapsed"], time3["elapsed"]))
})

test_that("RDS save/load is performant (<1 second)", {
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)

  temp_file <- tempfile(fileext = ".rds")

  # Test save performance
  time_save <- system.time({
    saveRDS(consolidated, temp_file)
  })
  expect_lt(time_save["elapsed"], 1)

  # Test load performance
  time_load <- system.time({
    loaded <- readRDS(temp_file)
  })
  expect_lt(time_load["elapsed"], 1)

  message(sprintf("RDS save: %.3f seconds, load: %.3f seconds",
                  time_save["elapsed"], time_load["elapsed"]))

  unlink(temp_file)
})
