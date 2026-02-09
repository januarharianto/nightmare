# -- weights_data.R ------------------------------------------------
# Grade projection + risk calculation (Arnold & Pistilli model).

# Load weights data from .nightmare/weights.json.
# Returns a list with version, saved_at, weights.
load_weights_data <- function(data_dir, unit) {
  path <- file.path(data_dir, unit, ".nightmare", "weights.json")
  default <- list(version = 1L, saved_at = NULL, weights = list(), due_dates = list())

  if (!file.exists(path)) return(default)

  tryCatch({
    payload <- fromJSON(path, simplifyVector = FALSE)
    if (is.null(payload$weights)) return(default)
    if (is.null(payload$due_dates)) payload$due_dates <- list()
    payload
  }, error = function(e) {
    default
  })
}

# Save weights data to .nightmare/weights.json.
save_weights_data <- function(data_dir, unit, weights_data) {
  nightmare_dir <- ensure_nightmare_dir(data_dir, unit)

  weights_data$version <- 2L
  weights_data$saved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")

  path <- file.path(nightmare_dir, "weights.json")
  save_json(path, weights_data)
}

# Compute assessment status from due date and score presence.
# Returns "ongoing", "completed", or "missing".
compute_assessment_status <- function(due_date, has_score) {
  if (is.null(due_date) || is.na(due_date) || due_date == "") {
    return(if (isTRUE(has_score)) "completed" else "missing")
  }
  parsed <- tryCatch(as.Date(due_date), error = function(e) NA)
  if (is.na(parsed)) {
    return(if (isTRUE(has_score)) "completed" else "missing")
  }
  if (parsed >= Sys.Date()) return("ongoing")
  if (isTRUE(has_score)) "completed" else "missing"
}

# Calculate projected grade for a student given assessment weights.
# weights: named list (assessment_name -> weight percentage)
# canvas_assignments: data.frame of student's Canvas assignments
# exam_data: exam data list (from load_exam_data)
# student_id: student identifier
calculate_projected_grade <- function(weights, canvas_assignments, exam_data, student_id, due_dates = list()) {
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
        if (!is.na(row$score) && compute_assessment_status(due_dates[[aname]], !is.na(row$score)) == "completed") {
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

  if (projection$remaining_weight <= 0) {
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
