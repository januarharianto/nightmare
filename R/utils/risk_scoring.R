#' Risk Scoring Functions for NIGHTMARE
#'
#' Identifies and scores students at risk based on multiple factors:
#' - Multiple extensions (>2 assessments with extensions)
#' - Failing assessments (score = 0 or < 30%)
#' - Missing work (past due date, no extension granted)
#' - Near 10-day policy limit (>5 days overdue after extensions)

library(dplyr)
library(purrr)

#' Calculate At-Risk Status for Students
#'
#' @param student_data Single row data.frame with consolidated student data
#' @return Numeric score 0-100
calculate_risk_score <- function(student_data) {
  score <- 0

  # Extract nested data
  special_consids <- student_data$special_consids[[1]]
  assignments <- student_data$assignments[[1]]

  # FACTOR 1: Multiple Extensions (>2 assessments with extensions)
  has_multiple_extensions <- FALSE
  if (nrow(special_consids) > 0) {
    # Count unique assessments with approved extensions
    unique_assessments <- special_consids %>%
      filter(approved == TRUE, !is.na(extension_date), extension_date != "") %>%
      pull(assessment_name) %>%
      unique() %>%
      length()

    if (unique_assessments > 2) {
      has_multiple_extensions <- TRUE
      score <- score + 25
    }
  }

  # FACTOR 2: Failing Assessments
  # Only flag if final grade < 50 OR if they have failing weighted assessments (>5% weight, score <40)
  has_failing_assessment <- FALSE

  # Check final grade first (most reliable indicator)
  if (!is.na(student_data$final_grade) && student_data$final_grade < 50) {
    has_failing_assessment <- TRUE
    score <- score + 35
  } else if (nrow(assignments) > 0) {
    # Only check significant weighted assignments (>5% weight)
    failing_weighted <- assignments %>%
      filter(weight > 5, score < 40, !is.na(score))

    # Flag if 2+ significant assignments are failing
    if (nrow(failing_weighted) >= 2) {
      has_failing_assessment <- TRUE
      score <- score + 35
    }
  }

  # FACTOR 3: Missing Work (assignments with score = 0 and no extension, and weight > 5%)
  has_missing_work <- FALSE
  if (nrow(assignments) > 0) {
    # Get assessment names with extensions
    extended_assessments <- character(0)
    if (nrow(special_consids) > 0) {
      extended_assessments <- special_consids %>%
        filter(approved == TRUE) %>%
        pull(assessment_name) %>%
        unique()
    }

    # Check for zero-score weighted assignments without extensions
    missing_count <- assignments %>%
      filter(score == 0, weight > 5, !(name %in% extended_assessments)) %>%
      nrow()

    if (missing_count > 0) {
      has_missing_work <- TRUE
      score <- score + 40
    }
  }

  # FACTOR 4: Near Policy Limit (extension dates >7 days after due date)
  # Parse extension_date field (format: dd-mm-yyyy) and calculate days from due_date
  near_policy_limit <- FALSE
  if (nrow(special_consids) > 0) {
    # Check if extension dates are significantly far from typical due dates
    # Since we have extension_date in format "dd-mm-yyyy", we can count how many extensions exist
    # More than 3 extensions could indicate approaching policy limit
    extension_count <- special_consids %>%
      filter(!is.na(extension_date), extension_date != "", approved == TRUE) %>%
      nrow()

    if (extension_count >= 4) {
      near_policy_limit <- TRUE
      score <- score + 30
    }
  }

  # Cap at 100
  score <- min(score, 100)

  return(score)
}

#' Identify Risk Category from Score
#'
#' @param risk_score Numeric 0-100
#' @return Factor with levels c("Low", "Medium", "High")
identify_risk_category <- function(risk_score) {
  category <- case_when(
    risk_score < 25 ~ "Low",
    risk_score <= 50 ~ "Medium",
    TRUE ~ "High"
  )

  return(factor(category, levels = c("Low", "Medium", "High")))
}

#' Get Human-Readable Risk Factors for a Student
#'
#' @param student_record Single row data.frame with consolidated student data
#' @return Character vector of risk factor descriptions
get_risk_factors_for_student <- function(student_record) {
  factors <- character()

  # Extract nested data
  special_consids <- student_record$special_consids[[1]]
  assignments <- student_record$assignments[[1]]

  # FACTOR 1: Multiple Extensions
  if (nrow(special_consids) > 0) {
    unique_assessments <- special_consids %>%
      filter(approved == TRUE, !is.na(extension_date), extension_date != "") %>%
      pull(assessment_name) %>%
      unique()

    if (length(unique_assessments) > 2) {
      assessment_list <- paste(head(unique_assessments, 3), collapse = ", ")
      if (length(unique_assessments) > 3) {
        assessment_list <- paste0(assessment_list, ", ...")
      }
      factors <- c(factors, sprintf(
        "%d assessments with extensions (%s)",
        length(unique_assessments),
        assessment_list
      ))
    }
  }

  # FACTOR 2: Failing Assessments
  if (!is.na(student_record$final_grade) && student_record$final_grade < 50) {
    factors <- c(factors, sprintf("Final grade: %.1f%% (failing)", student_record$final_grade))
  } else if (nrow(assignments) > 0) {
    failing_weighted <- assignments %>%
      filter(weight > 5, score < 40, !is.na(score))

    if (nrow(failing_weighted) >= 2) {
      factors <- c(factors, sprintf(
        "%d weighted assessments (>5%%) with scores < 40%%",
        nrow(failing_weighted)
      ))
      # Show first 2 examples
      for (i in 1:min(2, nrow(failing_weighted))) {
        factors <- c(factors, sprintf(
          "  • %s (%.0f%% weight): score = %.1f",
          failing_weighted$name[i],
          failing_weighted$weight[i],
          failing_weighted$score[i]
        ))
      }
    }
  }

  # FACTOR 3: Missing Work (weighted assignments only)
  if (nrow(assignments) > 0) {
    extended_assessments <- character(0)
    if (nrow(special_consids) > 0) {
      extended_assessments <- special_consids %>%
        filter(approved == TRUE) %>%
        pull(assessment_name) %>%
        unique()
    }

    missing <- assignments %>%
      filter(score == 0, weight > 5, !(name %in% extended_assessments))

    if (nrow(missing) > 0) {
      for (i in 1:min(2, nrow(missing))) {
        factors <- c(factors, sprintf(
          "Missing: %s (%.0f%% weight, no extension)",
          missing$name[i],
          missing$weight[i]
        ))
      }
    }
  }

  # FACTOR 4: Near Policy Limit (>= 4 extensions total)
  if (nrow(special_consids) > 0) {
    extension_count <- special_consids %>%
      filter(!is.na(extension_date), extension_date != "", approved == TRUE) %>%
      nrow()

    if (extension_count >= 4) {
      factors <- c(factors, sprintf(
        "%d total extensions granted (approaching policy limit)",
        extension_count
      ))
    }
  }

  if (length(factors) == 0) {
    factors <- "No risk factors identified"
  }

  return(factors)
}

#' Apply Risk Scoring to All Students
#'
#' @param consolidated_data data.frame with consolidated student records
#' @return Same data.frame with risk columns added
apply_risk_scoring <- function(consolidated_data) {
  message("Calculating risk scores for ", nrow(consolidated_data), " students...")

  # Calculate risk scores using lapply (more reliable with nested data)
  risk_scores <- lapply(1:nrow(consolidated_data), function(i) {
    student_row <- consolidated_data[i, ]
    list(
      risk_score = calculate_risk_score(student_row),
      risk_category = identify_risk_category(calculate_risk_score(student_row)),
      risk_factors = list(get_risk_factors_for_student(student_row))
    )
  })

  # Add results to data frame
  consolidated_data$risk_score <- sapply(risk_scores, function(x) x$risk_score)
  consolidated_data$risk_category <- sapply(risk_scores, function(x) as.character(x$risk_category))
  consolidated_data$risk_factors <- lapply(risk_scores, function(x) x$risk_factors[[1]])

  # Convert risk_category to factor
  consolidated_data$risk_category <- factor(
    consolidated_data$risk_category,
    levels = c("Low", "Medium", "High")
  )

  # Summary statistics
  high_risk <- sum(consolidated_data$risk_category == "High")
  medium_risk <- sum(consolidated_data$risk_category == "Medium")
  low_risk <- sum(consolidated_data$risk_category == "Low")

  message(sprintf(
    "Risk assessment complete: %d High, %d Medium, %d Low",
    high_risk, medium_risk, low_risk
  ))

  return(consolidated_data)
}
