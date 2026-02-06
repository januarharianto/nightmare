#' Canvas Gradebook Import for NIGHTMARE
#'
#' Import and process Canvas gradebook CSV exports

library(dplyr)
library(readr)
library(stringr)

#' Import Canvas Gradebook
#'
#' @param file_path Path to Canvas gradebook CSV file
#' @return data.frame with student records and assignment data
import_canvas_grades <- function(file_path) {
  message("Importing Canvas gradebook from: ", file_path)

  # Read CSV (Canvas uses standard headers)
  raw_data <- read_csv(file_path, show_col_types = FALSE)

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
      # Extract unit code from Section (first token before space)
      unit_of_study = str_extract(section, "^[A-Z]+[0-9]+"),
      # Handle email if present, otherwise construct from login
      email = if ("Email" %in% names(data)) {
        data$Email
      } else {
        paste0(sis_login_id, "@uni.sydney.edu.au")
      }
    )

  # Identify assignment columns (contain weight % and ID in brackets)
  # Pattern: "Assignment Name [weight%] (id)"
  assignment_cols <- names(data)[grepl("\\[.*%\\].*\\(\\d+\\)", names(data))]

  # Extract final grade (look for "Current Grade" or similar)
  final_grade_col <- names(data)[grepl("^Current (Score|Grade|Points)", names(data), ignore.case = TRUE)]
  if (length(final_grade_col) > 0) {
    students$final_grade <- as.numeric(data[[final_grade_col[1]]])
  } else {
    students$final_grade <- NA_real_
  }

  # Process assignments into nested structure
  students$assignments <- lapply(1:nrow(students), function(i) {
    if (length(assignment_cols) == 0) {
      return(data.frame(
        name = character(),
        score = numeric(),
        max_points = numeric(),
        weight = numeric(),
        assignment_id = character(),
        stringsAsFactors = FALSE
      ))
    }

    assignments_list <- lapply(assignment_cols, function(col) {
      # Parse assignment name, weight, and ID
      # Pattern: "Name [weight%] (id)"
      name_part <- str_extract(col, "^[^\\[]+")
      weight_part <- str_extract(col, "\\[(.+?)%\\]")
      id_part <- str_extract(col, "\\((\\d+)\\)")

      weight <- as.numeric(str_extract(weight_part, "\\d+\\.?\\d*"))
      assignment_id <- str_extract(id_part, "\\d+")

      # Get score for this student
      score <- as.numeric(data[[col]][i])

      data.frame(
        name = str_trim(name_part),
        score = ifelse(is.na(score), 0, score),
        max_points = NA_real_,  # Canvas doesn't always include max points in export
        weight = ifelse(is.na(weight), 0, weight),
        assignment_id = ifelse(is.na(assignment_id), "", assignment_id),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, assignments_list)
  })

  # Category scores (placeholder - would need additional parsing if available)
  students$category_scores <- lapply(1:nrow(students), function(i) {
    data.frame(
      category_name = character(),
      current_score = numeric(),
      final_score = numeric(),
      stringsAsFactors = FALSE
    )
  })

  message(sprintf("Imported %d students from Canvas", nrow(students)))
  return(students)
}
