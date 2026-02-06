# At-Risk Report Export for NIGHTMARE

library(dplyr)
source("R/utils/exports/helpers.R")

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
