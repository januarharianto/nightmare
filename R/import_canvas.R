# -- canvas.R -----------------------------------------------------
# Import and process Canvas gradebook CSV exports.

#' Import Canvas Gradebook
#' @keywords internal
#'
#' @param file_path Path to Canvas gradebook CSV file
#' @return data.frame with student records and assignment data
import_canvas_grades <- function(file_path) {
  message("Importing Canvas gradebook from: ", file_path)

  # Read CSV (Canvas uses standard headers)
  raw_data <- read_csv(file_path, show_col_types = FALSE)

  # Extract Points Possible row BEFORE filtering it out
  points_possible_row <- raw_data %>%
    filter(grepl("Points Possible", Student, ignore.case = TRUE)) %>%
    slice(1)

  has_points_possible <- nrow(points_possible_row) > 0
  if (!has_points_possible) {
    warning("No 'Points Possible' row found in Canvas export. max_points will be NA.")
  }

  # Filter out metadata rows (where Student is NA or contains non-student text)
  data <- raw_data %>%
    filter(!is.na(Student)) %>%
    filter(!grepl("Points Possible|Test Student", Student, ignore.case = TRUE))

  # Extract core student fields
  students <- data %>%
    select(
      student_id = `SIS User ID`,
      name = Student,
      canvas_id = ID,
      sis_login_id = `SIS Login ID`,
      section = Section
    ) %>%
    mutate(
      # Ensure student_id is character for joining
      student_id = as.character(student_id),
      # Extract unit code from Section (e.g. BIOL2022, ENVX2001)
      unit_of_study = str_extract(section, "[A-Z]+\\d{4}"),
      # Handle email if present, otherwise construct from login
      email = if ("Email" %in% names(data)) {
        data$Email
      } else {
        paste0(sis_login_id, "@uni.sydney.edu.au")
      }
    )

  # Identify assignment columns: match columns ending with (id)
  assignment_cols <- names(data)[grepl("\\(\\d+\\)$", names(data))]

  # Pre-compute per-column: max_points, is_ongoing, and filter out non-graded
  col_meta <- lapply(assignment_cols, function(col) {
    max_pts <- if (has_points_possible) as.numeric(points_possible_row[[col]]) else NA_real_
    scores_raw <- as.numeric(data[[col]])
    na_ratio <- sum(is.na(scores_raw)) / nrow(data)
    list(col = col, max_points = max_pts, is_ongoing = na_ratio > 0.60)
  })

  # Filter out assignments where max_points is 0 or NA (attendance, surveys, etc.)
  col_meta <- Filter(function(m) !is.na(m$max_points) && m$max_points > 0, col_meta)

  # Extract final grade (look for "Current Grade" or similar)
  final_grade_col <- names(data)[grepl("^Current (Score|Grade|Points)", names(data), ignore.case = TRUE)]
  if (length(final_grade_col) > 0) {
    students$final_grade <- as.numeric(data[[final_grade_col[1]]])
  } else {
    students$final_grade <- NA_real_
  }

  # Process assignments into nested structure
  students$assignments <- lapply(1:nrow(students), function(i) {
    if (length(col_meta) == 0) {
      return(data.frame(
        name = character(),
        score = numeric(),
        max_points = numeric(),
        percentage = numeric(),
        is_ongoing = logical(),
        assignment_id = character(),
        stringsAsFactors = FALSE
      ))
    }

    assignments_list <- lapply(col_meta, function(m) {
      col <- m$col

      # Parse assignment name and ID from column header
      name_part <- str_extract(col, "^[^\\[\\(]+")
      # Strip trailing weight bracket if present e.g. " [10%] "
      name_part <- str_replace(name_part, "\\s*$", "")
      id_part <- str_extract(col, "\\((\\d+)\\)")
      assignment_id <- str_extract(id_part, "\\d+")

      # Get score for this student - keep NA as NA
      score <- as.numeric(data[[col]][i])

      max_pts <- m$max_points
      pct <- if (!is.na(score) && !is.na(max_pts) && max_pts > 0) {
        score / max_pts * 100
      } else {
        NA_real_
      }

      data.frame(
        name = str_trim(name_part),
        score = score,
        max_points = max_pts,
        percentage = pct,
        is_ongoing = m$is_ongoing,
        assignment_id = ifelse(is.na(assignment_id), "", assignment_id),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, assignments_list)
  })

  # Extract academic year from test student row (before filtering)
  test_row <- raw_data %>%
    filter(grepl("Student, Test", Student, fixed = TRUE)) %>%
    slice(1)

  if (nrow(test_row) > 0) {
    section <- as.character(test_row$Section[1])
    year_match <- str_extract(section, "\\d{4}")
    if (!is.na(year_match)) {
      attr(students, "academic_year") <- year_match
      message(sprintf("Academic year from Canvas: %s", year_match))
    }

    # Extract semester (e.g., S1, S2, S1C, S2C)
    semester_match <- str_extract(section, "S[1-2][A-Z]?")
    if (!is.na(semester_match)) {
      attr(students, "semester") <- semester_match
      message(sprintf("Semester from Canvas: %s", semester_match))
    }
  }

  message(sprintf("Imported %d students from Canvas", nrow(students)))
  return(students)
}
