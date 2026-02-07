#' Data Consolidation for NIGHTMARE
#'
#' Merge student data from multiple sources

library(dplyr)

#' Detect unit code from Canvas data
#'
#' @param canvas_data data.frame from import_canvas_grades
#' @return Character unit code (e.g., "BIOL2022")
detect_unit_from_canvas <- function(canvas_data) {
  if (!"unit_of_study" %in% names(canvas_data)) {
    stop("Canvas data does not contain unit_of_study column")
  }

  # Get most common unit code
  unit_counts <- table(canvas_data$unit_of_study)
  most_common <- names(unit_counts)[which.max(unit_counts)]

  message(sprintf("Detected unit code: %s", most_common))
  return(most_common)
}

#' Detect year from Canvas data
#'
#' @param canvas_data data.frame from import_canvas_grades
#' @return Character year code (e.g., "2025")
detect_year_from_canvas <- function(canvas_data) {
  year <- attr(canvas_data, "academic_year")
  if (!is.null(year) && !is.na(year)) {
    message(sprintf("Using academic year from Canvas data: %s", year))
    return(year)
  }
  warning("Academic year not found in Canvas data, using current year")
  return(format(Sys.Date(), "%Y"))
}

#' Detect semester from Canvas data
#'
#' @param canvas_data data.frame from import_canvas_grades with semester attribute
#' @return Character semester code (e.g., "S1", "S2", "S2C")
detect_semester_from_canvas <- function(canvas_data) {
  semester <- attr(canvas_data, "semester")

  if (!is.null(semester) && !is.na(semester)) {
    return(semester)
  }

  # Fallback: try to extract from section column directly
  if ("section" %in% names(canvas_data)) {
    section_sample <- canvas_data$section[!is.na(canvas_data$section)][1]
    if (!is.na(section_sample)) {
      semester_match <- str_extract(section_sample, "S[1-2][A-Z]?")
      if (!is.na(semester_match)) {
        return(semester_match)
      }
    }
  }

  return("Unknown")
}

#' Consolidate Student Data
#'
#' @param canvas data.frame from import_canvas_grades
#' @param consids data.frame from import_special_considerations
#' @param plans data.frame from import_disability_plans
#' @param unit_filter Optional unit code to filter final results
#' @param year_filter Optional year to filter plans data (defaults to auto-detect from plans)
#' @return data.frame with fully consolidated student records
consolidate_student_data <- function(canvas, consids, plans, unit_filter = NULL, year_filter = NULL) {
  message("Consolidating student data from all sources...")

  # If unit_filter not provided, detect from Canvas
  if (is.null(unit_filter)) {
    unit_filter <- detect_unit_from_canvas(canvas)
  }

  # If year_filter not provided, detect from Canvas data
  if (is.null(year_filter)) {
    year_filter <- detect_year_from_canvas(canvas)
  } else {
    message(sprintf("Using provided year filter: %s", year_filter))
  }

  # Start with Canvas as base (all students in unit)
  consolidated <- canvas

  # Left join special considerations (if provided)
  if (!is.null(consids)) {
    consolidated <- consolidated %>%
      left_join(consids, by = "student_id", suffix = c("", "_consid"))
  }

  # Left join disability plans (if provided)
  if (!is.null(plans)) {
    consolidated <- consolidated %>%
      left_join(
        plans %>% select(student_id, has_disability_plan, plan_adjustments),
        by = "student_id",
        suffix = c("", "_plan")
      )
  }

  # Fill NAs with sensible defaults
  # Handle special considerations columns
  if (!"special_consids" %in% names(consolidated)) {
    consolidated$special_consids <- lapply(1:nrow(consolidated), function(i) {
      data.frame(
        ticket_id = character(),
        assessment_name = character(),
        outcome_type = character(),
        extension_date = character(),
        state = character(),
        approved = logical(),
        stringsAsFactors = FALSE
      )
    })
  }
  if (!"total_extensions" %in% names(consolidated)) {
    consolidated$total_extensions <- 0L
  }
  if (!"has_replacement_exam" %in% names(consolidated)) {
    consolidated$has_replacement_exam <- FALSE
  }
  if (!"has_mark_adjustment" %in% names(consolidated)) {
    consolidated$has_mark_adjustment <- FALSE
  }

  # Handle disability plan columns
  if (!"has_disability_plan" %in% names(consolidated)) {
    consolidated$has_disability_plan <- FALSE
  }
  if (!"plan_adjustments" %in% names(consolidated)) {
    consolidated$plan_adjustments <- lapply(1:nrow(consolidated), function(i) {
      data.frame(
        category = character(),
        arrangement_type = character(),
        value = character(),
        stringsAsFactors = FALSE
      )
    })
  }

  # Now fill NAs in existing columns
  consolidated <- consolidated %>%
    mutate(
      # Special considerations defaults
      special_consids = ifelse(
        sapply(special_consids, is.null) | is.na(special_consids),
        list(data.frame(
          ticket_id = character(),
          assessment_name = character(),
          outcome_type = character(),
          extension_date = character(),
          state = character(),
          approved = logical(),
          stringsAsFactors = FALSE
        )),
        special_consids
      ),
      total_extensions = ifelse(is.na(total_extensions), 0L, as.integer(total_extensions)),
      has_replacement_exam = ifelse(is.na(has_replacement_exam), FALSE, has_replacement_exam),
      has_mark_adjustment = ifelse(is.na(has_mark_adjustment), FALSE, has_mark_adjustment),

      # Disability plan defaults
      has_disability_plan = ifelse(is.na(has_disability_plan), FALSE, has_disability_plan),
      plan_adjustments = ifelse(
        sapply(plan_adjustments, is.null) | is.na(plan_adjustments),
        list(data.frame(
          category = character(),
          arrangement_type = character(),
          value = character(),
          stringsAsFactors = FALSE
        )),
        plan_adjustments
      ),

      # Metadata
      total_approved_extension_days = 0L,  # Will be calculated by extensions.R
      last_updated = Sys.time(),
      data_sources = list(c("canvas", "special_consids", "plans"))
    )

  message(sprintf("Consolidated data for %d students", nrow(consolidated)))

  return(consolidated)
}
