#' Export Functions for NIGHTMARE
#'
#' Generate various export formats for different stakeholders

library(dplyr)
library(tidyr)
library(readr)

#' Export for Canvas Bulk Grade Upload
#'
#' @param student_data Consolidated student data.frame
#' @return data.frame ready for CSV export to Canvas
#' @export
export_for_canvas <- function(student_data) {
  # Export for Canvas bulk grade upload
  # COLUMNS: SIS Login ID, Student Name, Final Grade
  # PURPOSE: Upload to Canvas for grade recording
  # NOTES: Extensions are NOT imported to Canvas grades

  canvas_export <- student_data %>%
    select(
      `SIS Login ID` = sis_login_id,
      `Student Name` = name,
      `Final Grade` = final_grade
    ) %>%
    arrange(`SIS Login ID`)

  return(canvas_export)
}

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

#' Export At-Risk Students Report
#'
#' @param student_data Consolidated student data.frame
#' @return data.frame with at-risk students and recommended actions
#' @export
export_at_risk_report <- function(student_data) {
  # Export at-risk students report
  # FILTER: Only High and Medium risk students

  at_risk_report <- student_data %>%
    filter(risk_category %in% c("High", "Medium")) %>%
    mutate(
      risk_factors_text = sapply(risk_factors, format_risk_factors),
      has_disability_plan_text = ifelse(has_disability_plan, "Yes", "No"),
      recommended_action = case_when(
        risk_category == "High" & has_disability_plan ~ "Urgent: Contact for support (disability plan active)",
        risk_category == "High" ~ "Urgent: Contact for support",
        risk_category == "Medium" & total_approved_extension_days > 14 ~ "Monitor closely - multiple extensions",
        risk_category == "Medium" ~ "Monitor closely",
        TRUE ~ "Review"
      )
    ) %>%
    select(
      `Student ID` = student_id,
      Name = name,
      `Final Grade` = final_grade,
      `Risk Category` = risk_category,
      `Risk Score` = risk_score,
      `Risk Factors` = risk_factors_text,
      `Total Extensions` = total_approved_extension_days,
      `Has Disability Plan` = has_disability_plan_text,
      `Recommended Action` = recommended_action
    ) %>%
    arrange(desc(`Risk Score`))

  return(at_risk_report)
}

#' Export Comprehensive Report (Multi-Sheet Excel)
#'
#' @param student_data Consolidated student data.frame
#' @param output_file Path to output XLSX file
#' @return Path to created file (invisibly)
#' @export
export_comprehensive_report <- function(student_data, output_file = NULL) {
  # Check if writexl is available (lighter weight than openxlsx)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required for Excel export. Install with: install.packages('writexl')")
  }

  # SHEET 1: Summary Statistics
  summary_stats <- data.frame(
    Metric = c(
      "Total Students",
      "At-Risk (High)",
      "At-Risk (High) %",
      "At-Risk (Medium)",
      "At-Risk (Medium) %",
      "Average Final Grade",
      "Average Extension Days",
      "Students with Disability Plans",
      "Students with Disability Plans %",
      "Students with Extensions",
      "Students with Extensions %"
    ),
    Value = c(
      nrow(student_data),
      sum(student_data$risk_category == "High", na.rm = TRUE),
      sprintf("%.1f%%", 100 * mean(student_data$risk_category == "High", na.rm = TRUE)),
      sum(student_data$risk_category == "Medium", na.rm = TRUE),
      sprintf("%.1f%%", 100 * mean(student_data$risk_category == "Medium", na.rm = TRUE)),
      sprintf("%.2f", mean(student_data$final_grade, na.rm = TRUE)),
      sprintf("%.1f", mean(student_data$total_approved_extension_days, na.rm = TRUE)),
      sum(student_data$has_disability_plan, na.rm = TRUE),
      sprintf("%.1f%%", 100 * mean(student_data$has_disability_plan, na.rm = TRUE)),
      sum(student_data$total_approved_extension_days > 0, na.rm = TRUE),
      sprintf("%.1f%%", 100 * mean(student_data$total_approved_extension_days > 0, na.rm = TRUE))
    ),
    stringsAsFactors = FALSE
  )

  # SHEET 2: Student List (Flat Structure)
  student_list <- student_data %>%
    mutate(
      risk_factors_text = sapply(risk_factors, format_risk_factors)
    ) %>%
    select(
      `Student ID` = student_id,
      Name = name,
      Email = email,
      `Unit of Study` = unit_of_study,
      Section = section,
      `Final Grade` = final_grade,
      `Risk Score` = risk_score,
      `Risk Category` = risk_category,
      `Risk Factors` = risk_factors_text,
      `Total Extensions` = total_approved_extension_days,
      `Has Disability Plan` = has_disability_plan,
      `Has Replacement Exam` = has_replacement_exam,
      `Has Mark Adjustment` = has_mark_adjustment
    ) %>%
    arrange(desc(`Risk Score`))

  # SHEET 3: Extensions Breakdown
  extensions_breakdown <- export_extensions_report(student_data)

  # SHEET 4: At-Risk Students with Details
  at_risk_details <- export_at_risk_report(student_data)

  # SHEET 5: Disability Plan Summary
  disability_summary <- student_data %>%
    filter(has_disability_plan) %>%
    mutate(
      adjustment_types = sapply(plan_adjustments, function(adj) {
        if (is.null(adj) || nrow(adj) == 0) {
          return("None specified")
        }
        paste(adj$adjustment_type, collapse = "; ")
      })
    ) %>%
    select(
      `Student ID` = student_id,
      Name = name,
      `Has Plan` = has_disability_plan,
      `Adjustment Types` = adjustment_types
    ) %>%
    arrange(`Student ID`)

  # Handle empty disability summary
  if (nrow(disability_summary) == 0) {
    disability_summary <- data.frame(
      `Student ID` = character(),
      Name = character(),
      `Has Plan` = logical(),
      `Adjustment Types` = character(),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # Create sheets list
  sheets <- list(
    "Summary" = summary_stats,
    "All Students" = student_list,
    "Extensions" = extensions_breakdown,
    "At-Risk Students" = at_risk_details,
    "Disability Plans" = disability_summary
  )

  # Write to file if output_file provided, otherwise return as bytes
  if (!is.null(output_file)) {
    writexl::write_xlsx(sheets, output_file)
    return(invisible(output_file))
  } else {
    # For in-memory use (e.g., Shiny downloadHandler)
    return(sheets)
  }
}

#' Helper: Format risk factors for display
#'
#' @param risk_factors_list Character vector or list of risk factors
#' @return Character string with semicolon-separated factors
#' @export
format_risk_factors <- function(risk_factors_list) {
  # INPUT: character vector of risk factors
  # OUTPUT: semicolon-separated string suitable for CSV

  if (is.null(risk_factors_list) || length(risk_factors_list) == 0) {
    return("None")
  }

  # Handle nested lists (from list columns)
  if (is.list(risk_factors_list) && !is.data.frame(risk_factors_list)) {
    risk_factors_list <- unlist(risk_factors_list)
  }

  # Remove NAs and empty strings
  risk_factors_list <- risk_factors_list[!is.na(risk_factors_list) & risk_factors_list != ""]

  if (length(risk_factors_list) == 0) {
    return("None")
  }

  # Join with semicolon
  paste(risk_factors_list, collapse = "; ")
}

# === VERIFICATION (run manually) ===
if (FALSE) {
  # Load sample data
  setwd("/Users/jhar8696/Sydney Uni Dropbox/Januar Harianto/projects/automation/nightmare")
  source("R/utils/import/canvas.R")
source("R/utils/import/special_consids.R")
source("R/utils/import/disability_plans.R")
source("R/utils/import/consolidate.R")
source("R/utils/import/file_detection.R")
  source("R/utils/risk_scoring.R")

  # Import and consolidate
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  consids <- import_special_considerations("_sample-data/special considerations.csv", unit_filter = "BIOL2022")
  plans <- import_disability_plans("_sample-data/plans.xlsx", unit_filter = "BIOL2022")
  consolidated <- consolidate_student_data(canvas, consids, plans)
  consolidated <- apply_risk_scoring(consolidated)

  # Test exports
  cat("\n=== TESTING EXPORTS ===\n")

  # Test Canvas export
  canvas_export <- export_for_canvas(consolidated)
  cat(sprintf("Canvas export: %d rows\n", nrow(canvas_export)))
  print(head(canvas_export, 3))

  # Test Extensions report
  ext_report <- export_extensions_report(consolidated)
  cat(sprintf("\nExtensions report: %d rows\n", nrow(ext_report)))
  print(head(ext_report, 3))

  # Test At-Risk report
  risk_report <- export_at_risk_report(consolidated)
  cat(sprintf("\nAt-risk report: %d rows\n", nrow(risk_report)))
  print(head(risk_report, 3))

  # Test Comprehensive report
  sheets <- export_comprehensive_report(consolidated)
  cat(sprintf("\nComprehensive report: %d sheets\n", length(sheets)))
  cat("Sheet names:", paste(names(sheets), collapse = ", "), "\n")
  cat("Summary stats:\n")
  print(sheets$Summary)
}
