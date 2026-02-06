#' Extension Calculation Functions for NIGHTMARE
#'
#' Functions to calculate total extensions and detect conflicts
#' across multiple data sources (Special Considerations + Disability Plans)

library(dplyr)
library(stringr)

#' Calculate total extension days for a student
#'
#' Implements additive stacking: sum all extension days from both
#' special considerations and disability plans.
#'
#' @param student_consids data.frame from special_consids column (one student)
#' @param plan_adjustments data.frame from plan_adjustments column
#' @param due_date Optional reference date for calculating extensions (default: today)
#' @return Integer total extension days
calculate_total_extensions <- function(student_consids, plan_adjustments, due_date = Sys.Date()) {
  total_days <- 0L

  # === 1. Sum calendar days from special considerations ===
  if (!is.null(student_consids) && nrow(student_consids) > 0) {
    # Filter to approved extensions with valid dates
    valid_extensions <- student_consids %>%
      filter(approved == TRUE) %>%
      filter(!is.na(extension_date) & extension_date != "")

    if (nrow(valid_extensions) > 0) {
      # Parse each extension date and calculate days
      extension_days <- sapply(valid_extensions$extension_date, function(date_str) {
        tryCatch({
          ext_date <- as.Date(date_str, format = "%d-%m-%Y")
          if (is.na(ext_date)) return(0L)

          # Calculate days from due date to extension date
          days <- as.integer(ext_date - due_date)
          return(max(0L, days))  # Only positive extensions
        }, error = function(e) {
          return(0L)
        })
      })

      total_days <- total_days + sum(extension_days, na.rm = TRUE)
    }
  }

  # === 2. Extract days from disability plan adjustments ===
  if (!is.null(plan_adjustments) && nrow(plan_adjustments) > 0) {
    for (i in 1:nrow(plan_adjustments)) {
      adjustment_desc <- plan_adjustments$description[i]

      # Parse common extension patterns from adjustment descriptions
      # Examples:
      # - "up to 1 week" → 7 days
      # - "up to 2 weeks" → 14 days
      # - "5 days extension" → 5 days
      # - "10% extra time" → 0 days (time, not date extension)

      if (is.na(adjustment_desc) || adjustment_desc == "") {
        next
      }

      # Extract numeric days patterns
      days_match <- str_extract(adjustment_desc, "(?i)(\\d+)\\s*(day|days)")
      if (!is.na(days_match)) {
        days <- as.integer(str_extract(days_match, "\\d+"))
        if (!is.na(days)) {
          total_days <- total_days + days
          next
        }
      }

      # Extract week patterns (convert to days)
      weeks_match <- str_extract(adjustment_desc, "(?i)(\\d+)\\s*(week|weeks)")
      if (!is.na(weeks_match)) {
        weeks <- as.integer(str_extract(weeks_match, "\\d+"))
        if (!is.na(weeks)) {
          total_days <- total_days + (weeks * 7L)
          next
        }
      }

      # Check for common adjustment types that imply extensions
      # (These would need to be mapped to actual days based on policy)
      if (grepl("extension", adjustment_desc, ignore.case = TRUE)) {
        # Default extension assumption (could be parameterized)
        # Only if no numeric value was found above
        if (is.na(days_match) && is.na(weeks_match)) {
          # Look for "short" vs "long" patterns
          if (grepl("short", adjustment_desc, ignore.case = TRUE)) {
            total_days <- total_days + 3L
          } else if (grepl("long", adjustment_desc, ignore.case = TRUE)) {
            total_days <- total_days + 7L
          }
        }
      }
    }
  }

  return(as.integer(total_days))
}

#' Detect extension conflicts for a student
#'
#' Checks for logical inconsistencies in extension data:
#' - Multiple extensions for same assessment with different deadlines
#' - Extensions in the past
#' - Zero-day or negative extensions
#' - Overlapping or redundant extensions
#'
#' @param student_record Single row data.frame with full student record
#' @return Character vector of conflict descriptions (empty if none)
detect_extension_conflicts <- function(student_record) {
  conflicts <- character()

  # Extract special considerations for this student
  consids <- student_record$special_consids[[1]]

  if (is.null(consids) || nrow(consids) == 0) {
    return(conflicts)
  }

  # === 1. Check for multiple extensions on same assessment ===
  if ("assessment_name" %in% names(consids)) {
    assessment_counts <- consids %>%
      filter(!is.na(assessment_name) & assessment_name != "") %>%
      group_by(assessment_name) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(count > 1)

    if (nrow(assessment_counts) > 0) {
      for (i in 1:nrow(assessment_counts)) {
        assessment <- assessment_counts$assessment_name[i]
        count <- assessment_counts$count[i]

        # Check if dates differ
        dates <- consids %>%
          filter(assessment_name == assessment) %>%
          pull(extension_date) %>%
          unique()

        if (length(dates) > 1) {
          conflicts <- c(conflicts, sprintf(
            "Multiple extensions for '%s' with different deadlines: %s",
            assessment,
            paste(dates, collapse = ", ")
          ))
        }
      }
    }
  }

  # === 2. Check for invalid extension dates ===
  if ("extension_date" %in% names(consids)) {
    for (i in 1:nrow(consids)) {
      date_str <- consids$extension_date[i]

      if (is.na(date_str) || date_str == "") {
        next
      }

      tryCatch({
        ext_date <- as.Date(date_str, format = "%d-%m-%Y")

        if (is.na(ext_date)) {
          conflicts <- c(conflicts, sprintf(
            "Invalid date format: '%s'", date_str
          ))
          next
        }

        # Check if extension is in the past (more than 1 year old)
        days_diff <- as.integer(Sys.Date() - ext_date)
        if (days_diff > 365) {
          conflicts <- c(conflicts, sprintf(
            "Extension date suspiciously old: %s (%d days ago)",
            format(ext_date, "%d-%m-%Y"), days_diff
          ))
        }

        # Check if extension is far in the future (more than 2 years)
        if (days_diff < -730) {
          conflicts <- c(conflicts, sprintf(
            "Extension date suspiciously far in future: %s (%d days from now)",
            format(ext_date, "%d-%m-%Y"), abs(days_diff)
          ))
        }
      }, error = function(e) {
        conflicts <- c(conflicts, sprintf(
          "Error parsing extension date '%s': %s", date_str, e$message
        ))
      })
    }
  }

  # === 3. Check for conflicting outcome types ===
  if ("outcome_type" %in% names(consids)) {
    # Check if student has both "Discontinuation" and active extensions
    has_discontinuation <- any(grepl("discontinuation", consids$outcome_type, ignore.case = TRUE))
    has_extensions <- any(grepl("extension", consids$outcome_type, ignore.case = TRUE))

    if (has_discontinuation && has_extensions) {
      conflicts <- c(conflicts,
        "Student has both discontinuation and active extensions - verify status"
      )
    }

    # Check for replacement exam + extensions on same assessment
    replacement_assessments <- consids %>%
      filter(grepl("replacement", outcome_type, ignore.case = TRUE)) %>%
      pull(assessment_name) %>%
      unique()

    if (length(replacement_assessments) > 0) {
      for (assessment in replacement_assessments) {
        also_has_extension <- consids %>%
          filter(assessment_name == assessment) %>%
          filter(grepl("extension", outcome_type, ignore.case = TRUE)) %>%
          nrow() > 0

        if (also_has_extension) {
          conflicts <- c(conflicts, sprintf(
            "Assessment '%s' has both replacement exam and extension - verify intent",
            assessment
          ))
        }
      }
    }
  }

  return(conflicts)
}

#' Generate extension summary for a student
#'
#' @param student_record Single row data.frame with full student record
#' @return List with summary statistics
summarize_student_extensions <- function(student_record) {
  consids <- student_record$special_consids[[1]]
  plans <- student_record$plan_adjustments[[1]]

  summary <- list(
    total_extension_days = student_record$total_approved_extension_days,
    num_special_considerations = ifelse(is.null(consids), 0, nrow(consids)),
    num_disability_adjustments = ifelse(is.null(plans), 0, nrow(plans)),
    has_replacement_exam = student_record$has_replacement_exam,
    has_mark_adjustment = student_record$has_mark_adjustment,
    conflicts = detect_extension_conflicts(student_record)
  )

  # Add breakdown by type
  if (!is.null(consids) && nrow(consids) > 0 && "outcome_type" %in% names(consids)) {
    summary$outcome_types <- table(consids$outcome_type)
  } else {
    summary$outcome_types <- table(character())
  }

  if (!is.null(plans) && nrow(plans) > 0 && "adjustment_type" %in% names(plans)) {
    summary$adjustment_types <- table(plans$adjustment_type)
  } else {
    summary$adjustment_types <- table(character())
  }

  return(summary)
}

#' Apply extension calculations to consolidated data
#'
#' Updates total_approved_extension_days field for all students
#'
#' @param consolidated_data data.frame from consolidate_student_data()
#' @return data.frame with updated extension days
apply_extension_calculations <- function(consolidated_data) {
  message("Calculating total extension days for all students...")

  consolidated_data$total_approved_extension_days <- sapply(1:nrow(consolidated_data), function(i) {
    student <- consolidated_data[i, ]
    consids <- student$special_consids[[1]]
    plans <- student$plan_adjustments[[1]]

    calculate_total_extensions(consids, plans)
  })

  # Log summary statistics
  total_students_with_extensions <- sum(consolidated_data$total_approved_extension_days > 0)
  max_extension <- max(consolidated_data$total_approved_extension_days, na.rm = TRUE)
  mean_extension <- mean(consolidated_data$total_approved_extension_days[
    consolidated_data$total_approved_extension_days > 0
  ], na.rm = TRUE)

  message(sprintf("  Students with extensions: %d", total_students_with_extensions))
  message(sprintf("  Max extension days: %d", max_extension))
  message(sprintf("  Mean extension days (for students with extensions): %.1f", mean_extension))

  return(consolidated_data)
}

# === VERIFICATION (run manually) ===
if (FALSE) {
  # Load required data
  source("R/utils/import/canvas.R")
source("R/utils/import/special_consids.R")
source("R/utils/import/disability_plans.R")
source("R/utils/import/consolidate.R")
source("R/utils/import/file_detection.R")

  # Import and consolidate
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv", unit_filter = "BIOL2022")
  plans <- import_disability_plans("_sample-data/plans.xlsx", unit_filter = "BIOL2022")
  consolidated <- consolidate_student_data(canvas, consids, plans)

  # Apply extension calculations
  consolidated <- apply_extension_calculations(consolidated)

  # Test individual student
  cat("\n=== EXTENSION CALCULATION TEST ===\n")
  sample_student <- consolidated %>%
    filter(total_approved_extension_days > 0) %>%
    slice(1)

  if (nrow(sample_student) > 0) {
    cat(sprintf("\nStudent: %s (ID: %s)\n", sample_student$name, sample_student$student_id))
    cat(sprintf("Total extension days: %d\n", sample_student$total_approved_extension_days))

    summary <- summarize_student_extensions(sample_student)
    cat("\nExtension Summary:\n")
    print(summary)

    conflicts <- detect_extension_conflicts(sample_student)
    if (length(conflicts) > 0) {
      cat("\nConflicts detected:\n")
      for (conflict in conflicts) {
        cat(sprintf("  - %s\n", conflict))
      }
    } else {
      cat("\nNo conflicts detected.\n")
    }
  } else {
    cat("No students with extensions found in sample data.\n")
  }

  # Overall statistics
  cat("\n=== OVERALL EXTENSION STATISTICS ===\n")
  cat(sprintf("Students with extensions: %d\n",
              sum(consolidated$total_approved_extension_days > 0)))
  cat(sprintf("Total extension days across all students: %d\n",
              sum(consolidated$total_approved_extension_days, na.rm = TRUE)))
  cat(sprintf("Max extension: %d days\n",
              max(consolidated$total_approved_extension_days, na.rm = TRUE)))
  cat(sprintf("Mean extension (non-zero): %.1f days\n",
              mean(consolidated$total_approved_extension_days[
                consolidated$total_approved_extension_days > 0
              ], na.rm = TRUE)))
}
