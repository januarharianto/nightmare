# Test Suite: match_assessments

test_that("exact case-insensitive match auto-accepts", {
  canvas <- c("Final Exam", "Lab Report")
  result <- match_assessments("final exam", canvas)
  expect_equal(nrow(result$matched), 1)
  expect_equal(result$matched$canvas_name, "Final Exam")
  expect_length(result$unmatched, 0)
  expect_length(result$ambiguous, 0)
})

test_that("unique substring match auto-accepts", {
  canvas <- c("Lab Report 2026", "Final Exam")
  result <- match_assessments("Lab Report", canvas)
  expect_equal(nrow(result$matched), 1)
  expect_equal(result$matched$canvas_name, "Lab Report 2026")
})

test_that("ambiguous substring match goes to ambiguous (not matched)", {
  canvas <- c("Pre-prac Quiz Week 1", "Pre-prac Quiz Week 2")
  result <- match_assessments("Pre-prac Quiz", canvas)
  expect_equal(nrow(result$matched), 0)
  expect_true("Pre-prac Quiz" %in% names(result$ambiguous))
  expect_length(result$ambiguous[["Pre-prac Quiz"]], 2)
})

test_that("single fuzzy hit is treated as ambiguous, never auto-matched", {
  # Regression: "Scientific Report" used to auto-match to
  # "2026 Scientific Paper_Commons et al  Quiz" because agrep returned 1 hit.
  canvas <- c(
    "Biodiversity Report_2026",
    "2026 Scientific Paper_Commons et al  Quiz",
    "Writing Task 2026"
  )
  result <- match_assessments("Scientific Report", canvas)
  expect_equal(nrow(result$matched), 0)
  expect_true("Scientific Report" %in% names(result$ambiguous))
  expect_length(result$unmatched, 0)
})

test_that("no fuzzy hits at all goes to unmatched", {
  canvas <- c("Biodiversity Report_2026", "Writing Task 2026")
  result <- match_assessments("Lab Notebook check-in", canvas)
  expect_equal(nrow(result$matched), 0)
  expect_length(result$ambiguous, 0)
  expect_true("Lab Notebook check-in" %in% result$unmatched)
})
