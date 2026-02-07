#' Special Considerations Import for NIGHTMARE
#'
#' Import and process special considerations CSV files

library(dplyr)
library(readr)

#' Import Special Considerations
#'
#' @param file_path Path to special considerations CSV file
#' @param unit_filter Optional unit code to filter by (e.g., "BIOL2022")
#' @param year_filter Optional year to filter by (e.g., "2025")
#' @return data.frame with student_id and special considerations data
import_special_considerations <- function(file_path, unit_filter = NULL, year_filter = NULL) {
  message("Importing special considerations from: ", file_path)

  # Read CSV
  raw_data <- read_csv(file_path, show_col_types = FALSE)

  # Keep Approved and Pending (Pending shows in-progress considerations)
  # Exclude Withdrawn and Not Approved
  data <- raw_data %>%
    filter(state %in% c("Approved", "Pending"))

  # Apply unit filter if provided
  if (!is.null(unit_filter)) {
    if ("availability" %in% names(data)) {
      pattern <- unit_filter
      if (!is.null(year_filter)) {
        pattern <- paste0(unit_filter, ".*", year_filter)
      }
      data <- data %>%
        filter(grepl(pattern, availability, ignore.case = TRUE))
    } else {
      warning("No 'availability' column found for filtering by unit")
    }
  }

  # Extract student ID column (handle different possible names)
  student_id_col <- names(data)[grepl("student.*id", names(data), ignore.case = TRUE)][1]
  if (is.na(student_id_col)) {
    stop("Could not find student ID column in special considerations data")
  }

  # Helper: safely extract a column, returning NA if it doesn't exist
  safe_col <- function(df, col_name, default = NA_character_) {
    if (col_name %in% names(df)) df[[col_name]] else rep(default, nrow(df))
  }

  # Process each consideration record — extract all relevant fields
  consids_processed <- data %>%
    mutate(
      student_id = as.character(.data[[student_id_col]]),
      ticket_id = as.character(number),
      assessment_name = as.character(safe_col(., "assessment", NA_character_)),
      assessment_title = as.character(safe_col(., "assessment_title", NA_character_)),
      assessment_category = as.character(safe_col(., "assessment_category", NA_character_)),
      assessment_type = as.character(safe_col(., "assessment_type", NA_character_)),
      outcome_type = as.character(safe_col(., "u_outcome_type", NA_character_)),
      classification = as.character(safe_col(., "classification", NA_character_)),
      state = as.character(state),
      approved = (state == "Approved"),
      # Parse extension_in_calendar_days as Date (DD-MM-YYYY format)
      extension_date = if ("extension_in_calendar_days" %in% names(.)) {
        as.Date(extension_in_calendar_days, format = "%d-%m-%Y")
      } else {
        as.Date(NA)
      },
      # Parse due_date as POSIXct datetime
      due_date = if ("due_date" %in% names(.)) {
        as.POSIXct(due_date, format = "%d-%m-%Y %H:%M:%S")
      } else {
        as.POSIXct(NA)
      },
      # Parse assessment.u_closing_date as POSIXct datetime
      closing_date = if ("assessment.u_closing_date" %in% names(.)) {
        as.POSIXct(assessment.u_closing_date, format = "%d-%m-%Y %H:%M:%S")
      } else {
        as.POSIXct(NA)
      }
    ) %>%
    select(student_id, ticket_id, assessment_name, assessment_title,
           assessment_category, assessment_type, outcome_type, classification,
           state, approved, extension_date, due_date, closing_date)

  # No dedup — each ticket is a unique consideration event.
  # A student can have multiple replacement exams for the same assessment
  # (each sitting generates a new ticket with its own due date).
  # Sort by due_date so records appear in chronological order.
  consids_processed <- consids_processed %>%
    arrange(student_id, assessment_name, due_date, ticket_id)

  # Group by student and create nested structure
  consids_by_student <- consids_processed %>%
    group_by(student_id) %>%
    summarise(
      special_consids = list(data.frame(
        ticket_id = ticket_id,
        assessment_name = assessment_name,
        assessment_title = assessment_title,
        assessment_category = assessment_category,
        assessment_type = assessment_type,
        outcome_type = outcome_type,
        classification = classification,
        state = state,
        approved = approved,
        extension_date = extension_date,
        due_date = due_date,
        closing_date = closing_date,
        stringsAsFactors = FALSE
      )),
      total_extensions = sum(
        approved & outcome_type %in% c("Simple Extension", "Extension of time"),
        na.rm = TRUE
      ),
      has_replacement_exam = any(
        approved & grepl("replacement.*exam", outcome_type, ignore.case = TRUE),
        na.rm = TRUE
      ),
      has_mark_adjustment = any(
        approved & grepl("mark.*adjustment", outcome_type, ignore.case = TRUE),
        na.rm = TRUE
      ),
      total_consids = n(),
      has_pending = any(state == "Pending", na.rm = TRUE),
      .groups = "drop"
    )

  message(sprintf("Imported %d special considerations for %d unique students",
                  nrow(consids_processed), nrow(consids_by_student)))

  return(consids_by_student)
}
