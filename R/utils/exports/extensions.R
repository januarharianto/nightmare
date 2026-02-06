# Extensions Report Export for NIGHTMARE

library(dplyr)
library(tidyr)

#' Export Extensions Report
#'
#' @param student_data Consolidated student data.frame
#' @return data.frame with one row per extension (unnested)
#' @export
export_extensions_report <- function(student_data) {
  # Export detailed extensions report
  # One row per extension (students may have multiple rows)

  # Process special considerations extensions
  consids_extensions <- student_data %>%
    filter(!sapply(special_consids, is.null)) %>%
    select(student_id, name, special_consids) %>%
    unnest(special_consids) %>%
    filter(!is.na(extension_date) & extension_date != "") %>%
    mutate(
      assessment = assessment_name,
      extension_days = extension_date,  # Already in days from import
      reason = "Special Consideration",
      source = "Special Consids"
    ) %>%
    select(student_id, name, assessment, extension_days, reason)

  # Process disability plan extensions
  plan_extensions <- student_data %>%
    filter(!sapply(plan_adjustments, is.null), has_disability_plan) %>%
    select(student_id, name, plan_adjustments) %>%
    unnest(plan_adjustments) %>%
    filter(grepl("extension|time", adjustment_type, ignore.case = TRUE)) %>%
    mutate(
      assessment = adjustment_type,
      extension_days = description,
      reason = "Disability Plan",
      source = "Plans"
    ) %>%
    select(student_id, name, assessment, extension_days, reason)

  # Combine both sources
  all_extensions <- bind_rows(consids_extensions, plan_extensions)

  # If no extensions, return empty data.frame with correct structure
  if (nrow(all_extensions) == 0) {
    return(data.frame(
      `Student ID` = character(),
      Name = character(),
      Assessment = character(),
      `Extension Days` = character(),
      Reason = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  # Format for export
  extensions_report <- all_extensions %>%
    rename(
      `Student ID` = student_id,
      Name = name,
      Assessment = assessment,
      `Extension Days` = extension_days,
      Reason = reason
    ) %>%
    arrange(`Student ID`, Assessment)

  return(extensions_report)
}
