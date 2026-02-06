# Comprehensive Multi-Sheet Excel Export for NIGHTMARE

library(dplyr)
source("R/utils/exports/helpers.R")
source("R/utils/exports/extensions.R")
source("R/utils/exports/at_risk.R")

#' Export Comprehensive Report (Multi-Sheet Excel)
#'
#' @param student_data Consolidated student data.frame
#' @param output_file Path to output XLSX file
#' @return Path to created file (invisibly) or list of sheets
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
