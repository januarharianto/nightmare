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

  # Filter to approved only
  data <- raw_data %>%
    filter(state == "Approved")

  # Apply unit filter if provided
  if (!is.null(unit_filter)) {
    if ("availability" %in% names(data)) {
      # Build pattern: match unit code and optionally year in the availability string
      # Availability format: "BIOL2022-S2C-2025-ND-CC"
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

  # Process each consideration record
  consids_processed <- data %>%
    mutate(
      student_id = as.character(.data[[student_id_col]]),
      ticket_id = as.character(number),
      assessment_name = ifelse("assessment_title" %in% names(.), assessment_title, NA_character_),
      outcome_type = ifelse("u_outcome_type" %in% names(.), u_outcome_type, NA_character_),
      extension_date_raw = ifelse("extension_in_calendar_days" %in% names(.),
                                   extension_in_calendar_days,
                                   NA_character_),
      state = state,
      approved = TRUE  # Already filtered to approved
    ) %>%
    select(student_id, ticket_id, assessment_name, outcome_type,
           extension_date_raw, state, approved)

  # Deduplicate: keep only the most recent ticket per student per assessment
  # Sort by ticket_id descending (higher = more recent) then take first per group
  consids_processed <- consids_processed %>%
    arrange(student_id, assessment_name, desc(ticket_id)) %>%
    group_by(student_id, assessment_name) %>%
    slice(1) %>%
    ungroup()

  # Group by student and create nested structure
  consids_by_student <- consids_processed %>%
    group_by(student_id) %>%
    summarise(
      special_consids = list(data.frame(
        ticket_id = ticket_id,
        assessment_name = assessment_name,
        outcome_type = outcome_type,
        extension_date = extension_date_raw,
        state = state,
        approved = approved,
        stringsAsFactors = FALSE
      )),
      total_extensions = sum(!is.na(extension_date_raw) & extension_date_raw != ""),
      has_replacement_exam = any(grepl("replacement.*exam", outcome_type, ignore.case = TRUE), na.rm = TRUE),
      has_mark_adjustment = any(grepl("mark.*adjustment", outcome_type, ignore.case = TRUE), na.rm = TRUE),
      .groups = "drop"
    )

  message(sprintf("Imported %d special considerations for %d unique students",
                  nrow(consids_processed), nrow(consids_by_student)))

  return(consids_by_student)
}
