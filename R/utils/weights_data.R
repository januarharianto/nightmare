# Assessment weight utilities for NIGHTMARE
# Pure data-processing functions — no Shiny reactives or side effects.

# Load weights data from .nightmare/weights.json.
# Returns a list with version, saved_at, weights.
load_weights_data <- function(data_dir, unit) {
  path <- file.path(data_dir, unit, ".nightmare", "weights.json")
  default <- list(version = 1L, saved_at = NULL, weights = list())

  if (!file.exists(path)) return(default)

  tryCatch({
    payload <- fromJSON(path, simplifyVector = FALSE)
    if (is.null(payload$weights)) return(default)
    payload
  }, error = function(e) {
    default
  })
}

# Save weights data to .nightmare/weights.json.
save_weights_data <- function(data_dir, unit, weights_data) {
  nightmare_dir <- ensure_nightmare_dir(data_dir, unit)

  weights_data$version <- 1L
  weights_data$saved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  path <- file.path(nightmare_dir, "weights.json")
  writeLines(toJSON(weights_data, auto_unbox = TRUE, null = "null", pretty = TRUE), path)
  invisible(path)
}

# Calculate projected grade for a student given assessment weights.
# weights: named list (assessment_name -> weight percentage)
# canvas_assignments: data.frame of student's Canvas assignments
# exam_data: exam data list (from load_exam_data)
# student_id: student identifier
calculate_projected_grade <- function(weights, canvas_assignments, exam_data, student_id) {
  completed_points <- 0
  completed_weight <- 0
  total_weight <- 0

  if (length(weights) == 0) {
    return(list(
      completed_points = 0,
      completed_weight = 0,
      total_weight = 0,
      remaining_weight = 0,
      current_average = NA_real_,
      max_possible = 0,
      projected_pct = NA_real_
    ))
  }

  # Get exam scores for this student
  exam_scores <- get_student_exam_scores(exam_data, student_id)

  for (aname in names(weights)) {
    w <- as.numeric(weights[[aname]])
    if (is.na(w) || w <= 0) next
    total_weight <- total_weight + w

    # Try Canvas assignments first
    score_pct <- NA_real_
    if (!is.null(canvas_assignments) && nrow(canvas_assignments) > 0) {
      match_idx <- which(canvas_assignments$name == aname)
      if (length(match_idx) > 0) {
        row <- canvas_assignments[match_idx[1], ]
        if (!is.na(row$score) && !isTRUE(row$is_ongoing)) {
          score_pct <- row$percentage
        }
      }
    }

    # Try exam scores if not found in Canvas
    if (is.na(score_pct) && nrow(exam_scores) > 0) {
      exam_idx <- which(exam_scores$assessment == aname)
      if (length(exam_idx) > 0) {
        erow <- exam_scores[exam_idx[1], ]
        if (erow$max_points > 0) {
          score_pct <- erow$score / erow$max_points * 100
        }
      }
    }

    if (!is.na(score_pct)) {
      completed_points <- completed_points + (score_pct * w / 100)
      completed_weight <- completed_weight + w
    }
  }

  remaining_weight <- total_weight - completed_weight
  current_average <- if (completed_weight > 0) completed_points / completed_weight * 100 else NA_real_
  max_possible <- completed_points + remaining_weight
  projected_pct <- current_average

  list(
    completed_points = completed_points,
    completed_weight = completed_weight,
    total_weight = total_weight,
    remaining_weight = remaining_weight,
    current_average = current_average,
    max_possible = max_possible,
    projected_pct = projected_pct
  )
}

# Calculate risk level from projection results.
calculate_risk_level <- function(projection) {
  if (projection$completed_weight == 0 || projection$total_weight == 0) {
    return(list(level = "none", label = "--"))
  }

  if (projection$remaining_weight == 0) {
    if (projection$current_average >= 50) {
      return(list(level = "low", label = "Low"))
    } else {
      return(list(level = "critical", label = "Critical"))
    }
  }

  required_average <- (50 * projection$total_weight / 100 - projection$completed_points) /
    projection$remaining_weight * 100

  if (projection$max_possible < 50) {
    return(list(level = "critical", label = "Critical"))
  }

  if (required_average > 80) {
    return(list(level = "high", label = "High"))
  }

  if (required_average > 60) {
    return(list(level = "moderate", label = "Moderate"))
  }

  list(level = "low", label = "Low")
}
