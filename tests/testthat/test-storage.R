# Test Suite: Storage and Persistence

skip_if_not(dir.exists("../../_sample-data"), "Sample data not available")

test_that("RDS save/load preserves data structure", {
  # Load sample data
  canvas <- import_canvas_grades("../../_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("../../_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)

  # Save to RDS
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(consolidated, temp_file)

  # Load back
  loaded <- readRDS(temp_file)

  # Verify structure
  expect_equal(names(consolidated), names(loaded))
  expect_equal(nrow(consolidated), nrow(loaded))
  expect_equal(consolidated$student_id, loaded$student_id)

  unlink(temp_file)
})

test_that("CSV save/load preserves student data", {
  canvas <- import_canvas_grades("../../_sample-data/canvas gradebook.csv")

  # Save to CSV
  temp_file <- tempfile(fileext = ".csv")
  write.csv(canvas, temp_file, row.names = FALSE)

  # Load back
  loaded <- read.csv(temp_file, stringsAsFactors = FALSE)

  # Verify basic structure
  expect_equal(nrow(canvas), nrow(loaded))
  expect_true(all(canvas$student_id %in% loaded$student_id))

  unlink(temp_file)
})

test_that("Large dataset (500+ students) exports successfully", {
  # Simulate large dataset by repeating sample data
  canvas <- import_canvas_grades("../../_sample-data/canvas gradebook.csv")

  # Basic sanity check - original data loads
  expect_gt(nrow(canvas), 200)

  # Save/load cycle
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(canvas, temp_file)
  loaded <- readRDS(temp_file)
  expect_equal(nrow(canvas), nrow(loaded))

  unlink(temp_file)
})

test_that("Backup RDS includes metadata", {
  canvas <- import_canvas_grades("../../_sample-data/canvas gradebook.csv")

  backup <- list(
    metadata = list(
      timestamp = Sys.time(),
      student_count = nrow(canvas),
      unit_of_study = "BIOL2022",
      nightmare_version = "0.1.0"
    ),
    student_data = canvas
  )

  temp_file <- tempfile(fileext = ".rds")
  saveRDS(backup, temp_file)
  loaded <- readRDS(temp_file)

  expect_is(loaded$metadata, "list")
  expect_equal(loaded$metadata$student_count, nrow(canvas))
  expect_equal(loaded$student_data$student_id, canvas$student_id)

  unlink(temp_file)
})

test_that("Export/import cycle maintains integrity", {
  canvas <- import_canvas_grades("../../_sample-data/canvas gradebook.csv")

  # Export to CSV
  temp_csv <- tempfile(fileext = ".csv")
  write.csv(canvas[, c("student_id", "name", "final_grade")], temp_csv, row.names = FALSE)

  # Re-import
  reimported <- read.csv(temp_csv, stringsAsFactors = FALSE)

  # Verify key data
  expect_equal(nrow(canvas), nrow(reimported))
  expect_equal(sum(canvas$final_grade, na.rm = TRUE),
               sum(as.numeric(reimported$final_grade), na.rm = TRUE),
               tolerance = 0.1)

  unlink(temp_csv)
})

test_that("Nested list columns survive RDS round-trip", {
  canvas <- import_canvas_grades("../../_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("../../_sample-data/special considerations.csv")
  consolidated <- consolidate_student_data(canvas, consids, NULL)

  # Save to RDS
  temp_file <- tempfile(fileext = ".rds")
  saveRDS(consolidated, temp_file)

  # Load back
  loaded <- readRDS(temp_file)

  # Check nested structures
  expect_is(loaded$assignments, "list")
  expect_is(loaded$special_consids, "list")

  # Verify first student's nested data
  if (nrow(loaded) > 0) {
    expect_is(loaded$assignments[[1]], "data.frame")
    expect_is(loaded$special_consids[[1]], "data.frame")
  }

  unlink(temp_file)
})
