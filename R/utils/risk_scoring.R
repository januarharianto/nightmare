#' Risk Scoring Functions for NIGHTMARE
#'
#' Identifies and scores students at risk based on 5 factors:
#' - Factor 1: Average performance on completed assessments (0-40 pts)
#' - Factor 2: Missing submissions with no extension (0-30 pts)
#' - Factor 3: Multiple special considerations (>2 assessments) (0-25 pts)
#' - Factor 4: Near policy limit (>=4 consids) + replacement exams (0-30 pts)
#' - Factor 5: Zero scores on completed assessments (0-20 pts)

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

  # Filter to completed assessments only (exclude ongoing)
  completed <- if (nrow(assignments) > 0) {
    assignments %>% filter(is_ongoing == FALSE)
  } else {
    assignments[0, ]
  }

  # Get assessment names with approved extensions
  extended_assessments <- character(0)
  if (nrow(special_consids) > 0) {
    extended_assessments <- special_consids %>%
      filter(approved == TRUE) %>%
      pull(assessment_name) %>%
      unique()
  }

  # FACTOR 1: Average Performance (0-40 points)
  scored <- completed %>%
    filter(!is.na(score), max_points > 0)

  if (nrow(scored) > 0) {
    avg_pct <- mean(scored$percentage, na.rm = TRUE)

    if (avg_pct < 40) {
      score <- score + 40
    } else if (avg_pct < 50) {
      score <- score + 20
    } else if (avg_pct < 60) {
      score <- score + 10
    }
  }

  # FACTOR 2: Missing Submissions (0-30 points)
  # Completed assessments where student has NA score and no approved extension
  missing <- completed %>%
    filter(is.na(score), !(name %in% extended_assessments))

  if (nrow(missing) >= 2) {
    score <- score + 30
  } else if (nrow(missing) == 1) {
    score <- score + 15
  }

  # FACTOR 3: Multiple Special Considerations (0-25 points)
  if (nrow(special_consids) > 0) {
    unique_consid_assessments <- special_consids %>%
      filter(approved == TRUE) %>%
      pull(assessment_name) %>%
      unique()

    if (length(unique_consid_assessments) > 2) {
      score <- score + 25
    }
  }

  # FACTOR 4: Near Policy Limit / Replacement Exams (0-30 points)
  if (nrow(special_consids) > 0) {
    consid_count <- special_consids %>%
      filter(approved == TRUE) %>%
      nrow()

    has_replacement <- any(grepl("replacement.*exam", special_consids$outcome_type,
                                  ignore.case = TRUE), na.rm = TRUE)

    if (consid_count >= 4) {
      score <- score + 20
    }
    if (has_replacement) {
      score <- score + 10
    }
  }

  # FACTOR 5: Zero Scores (0-20 points)
  zeros <- completed %>%
    filter(score == 0, max_points > 0)

  if (nrow(zeros) > 0) {
    score <- score + 20
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

#' Get Structured Risk Factors for a Student
#'
#' @param student_record Single row data.frame with consolidated student data
#' @return data.frame with columns: factor, detail, points
get_risk_factors_for_student <- function(student_record) {
  rows <- list()

  # Extract nested data
  special_consids <- student_record$special_consids[[1]]
  assignments <- student_record$assignments[[1]]

  # Filter to completed assessments only
  completed <- if (nrow(assignments) > 0) {
    assignments %>% filter(is_ongoing == FALSE)
  } else {
    assignments[0, ]
  }

  # Get assessment names with approved extensions
  extended_assessments <- character(0)
  if (nrow(special_consids) > 0) {
    extended_assessments <- special_consids %>%
      filter(approved == TRUE) %>%
      pull(assessment_name) %>%
      unique()
  }

  # FACTOR 1: Average Performance
  scored <- completed %>%
    filter(!is.na(score), max_points > 0)

  if (nrow(scored) > 0) {
    avg_pct <- mean(scored$percentage, na.rm = TRUE)

    if (avg_pct < 40) {
      rows <- c(rows, list(list(
        factor = "Low average", detail = sprintf("%.0f%% across %d assessment%s",
          avg_pct, nrow(scored), if (nrow(scored) == 1) "" else "s"), points = 40)))
    } else if (avg_pct < 50) {
      rows <- c(rows, list(list(
        factor = "Low average", detail = sprintf("%.0f%% across %d assessment%s",
          avg_pct, nrow(scored), if (nrow(scored) == 1) "" else "s"), points = 20)))
    } else if (avg_pct < 60) {
      rows <- c(rows, list(list(
        factor = "Below average", detail = sprintf("%.0f%% across %d assessment%s",
          avg_pct, nrow(scored), if (nrow(scored) == 1) "" else "s"), points = 10)))
    }
  }

  # FACTOR 2: Missing Submissions
  missing <- completed %>%
    filter(is.na(score), !(name %in% extended_assessments))

  if (nrow(missing) > 0) {
    pts <- if (nrow(missing) >= 2) 30 else 15
    rows <- c(rows, list(list(
      factor = "Missing submissions",
      detail = paste(missing$name, collapse = ", "),
      points = pts)))
  }

  # FACTOR 3: Multiple Special Considerations
  if (nrow(special_consids) > 0) {
    unique_consid_assessments <- special_consids %>%
      filter(approved == TRUE) %>%
      pull(assessment_name) %>%
      unique()

    if (length(unique_consid_assessments) > 2) {
      rows <- c(rows, list(list(
        factor = "Multiple consids",
        detail = sprintf("%d assessments affected", length(unique_consid_assessments)),
        points = 25)))
    }
  }

  # FACTOR 4: Near Policy Limit / Replacement Exams
  if (nrow(special_consids) > 0) {
    consid_count <- special_consids %>%
      filter(approved == TRUE) %>%
      nrow()

    has_replacement <- any(grepl("replacement.*exam", special_consids$outcome_type,
                                  ignore.case = TRUE), na.rm = TRUE)

    if (consid_count >= 4) {
      rows <- c(rows, list(list(
        factor = "Policy limit",
        detail = sprintf("%d total considerations", consid_count),
        points = 20)))
    }
    if (has_replacement) {
      rows <- c(rows, list(list(
        factor = "Replacement exam",
        detail = "Approved",
        points = 10)))
    }
  }

  # FACTOR 5: Zero Scores
  zeros <- completed %>%
    filter(score == 0, max_points > 0)

  if (nrow(zeros) > 0) {
    rows <- c(rows, list(list(
      factor = "Zero scores",
      detail = paste(zeros$name, collapse = ", "),
      points = 20)))
  }

  if (length(rows) == 0) {
    return(data.frame(factor = character(), detail = character(),
                      points = integer(), stringsAsFactors = FALSE))
  }

  data.frame(
    factor = sapply(rows, `[[`, "factor"),
    detail = sapply(rows, `[[`, "detail"),
    points = sapply(rows, `[[`, "points"),
    stringsAsFactors = FALSE
  )
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
