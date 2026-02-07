# UI helper functions for NIGHTMARE
# Extracted from server.R lines 164-384

build_student_detail_view <- function(student) {
  tagList(
    # Header
    tags$div(
      class = "student-detail-header",
      tags$div(class = "name", student$name),
      tags$div(
        class = "meta-inline",
        tags$span(class = "meta", paste("ID:", student$student_id)),
        tags$span(class = "meta", paste("Email:", student$email)),
        tags$span(class = "meta", paste("Unikey:", student$sis_login_id))
      )
    ),

    # Cards grid
    tags$div(
      class = "detail-cards-grid",

      # Risk Assessment Section (first — most actionable)
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Risk Assessment"),
        tags$div(
          class = "detail-section-content",
          tags$div(
            class = "risk-score-display",
            tags$div(
              tags$span("Risk Score: "),
              tags$span(class = "score", sprintf("%.0f / 100", student$risk_score))
            ),
            tags$span(
              class = paste0("risk-badge ", tolower(student$risk_category)),
              student$risk_category
            )
          ),

          # Risk factors
          if (length(student$risk_factors[[1]]) > 0) {
            tagList(
              tags$h5(style = "margin-bottom: 8px;", "Risk Factors"),
              tags$ul(
                style = "margin-left: 20px;",
                lapply(student$risk_factors[[1]], function(factor) {
                  tags$li(factor)
                })
              )
            )
          }
        )
      ),

      # Academic Performance Section
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Academic Performance"),
        tags$div(
          class = "detail-section-content",
          tags$div(
            class = "detail-row",
            tags$div(class = "detail-label", "Final Grade"),
            tags$div(class = "detail-value", sprintf("%.1f%%", student$final_grade))
          ),
          tags$div(
            class = "detail-row",
            tags$div(class = "detail-label", "Unit of Study"),
            tags$div(class = "detail-value", student$unit_of_study)
          ),
          tags$div(
            class = "detail-row",
            tags$div(class = "detail-label", "Section"),
            tags$div(class = "detail-value", student$section)
          ),

          # Assignments table
          if (nrow(student$assignments[[1]]) > 0) {
            assignments <- student$assignments[[1]] %>%
              mutate(
                Score = sprintf("%.1f", score),
                Weight = sprintf("%.1f%%", weight)
              ) %>%
              select(Assessment = name, Score, Weight)

            tagList(
              tags$h5(style = "margin-top: 12px; margin-bottom: 8px;", "Assessment Breakdown"),
              tags$table(
                class = "detail-table",
                tags$thead(
                  tags$tr(
                    tags$th("Assessment"),
                    tags$th("Score"),
                    tags$th("Weight")
                  )
                ),
                tags$tbody(
                  lapply(1:nrow(assignments), function(i) {
                    tags$tr(
                      tags$td(assignments$Assessment[i]),
                      tags$td(assignments$Score[i]),
                      tags$td(assignments$Weight[i])
                    )
                  })
                )
              )
            )
          }
        )
      ),

      # Extensions Section
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Extensions & Special Considerations"),
        tags$div(
          class = "detail-section-content",
          tags$div(
            class = "detail-row",
            tags$div(class = "detail-label", "Total Extension Days"),
            tags$div(class = "detail-value", student$total_approved_extension_days)
          ),
          tags$div(
            class = "detail-row",
            tags$div(class = "detail-label", "Replacement Exam"),
            tags$div(class = "detail-value", if (student$has_replacement_exam) "Yes" else "No")
          ),
          tags$div(
            class = "detail-row",
            tags$div(class = "detail-label", "Mark Adjustment"),
            tags$div(class = "detail-value", if (student$has_mark_adjustment) "Yes" else "No")
          ),

          # Extensions table
          if (nrow(student$special_consids[[1]]) > 0) {
            extensions <- student$special_consids[[1]] %>%
              filter(approved == TRUE) %>%
              mutate(
                Extension = ifelse(!is.na(extension_date) & extension_date != "",
                                 extension_date, "N/A"),
                Type = ifelse(!is.na(outcome_type), outcome_type, "Extension")
              ) %>%
              select(Assessment = assessment_name, Extension, Type, Ticket = ticket_id)

            if (nrow(extensions) > 0) {
              tagList(
                tags$h5(style = "margin-top: 12px; margin-bottom: 8px;", "Extension Details"),
                tags$table(
                  class = "detail-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Assessment"),
                      tags$th("Extension Date"),
                      tags$th("Type"),
                      tags$th("Ticket")
                    )
                  ),
                  tags$tbody(
                    lapply(1:nrow(extensions), function(i) {
                      tags$tr(
                        tags$td(extensions$Assessment[i]),
                        tags$td(extensions$Extension[i]),
                        tags$td(extensions$Type[i]),
                        tags$td(extensions$Ticket[i])
                      )
                    })
                  )
                )
              )
            }
          }
        )
      ),

      # Support Plans Section
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Support Plans"),
        tags$div(
          class = "detail-section-content",
          tags$div(
            class = "detail-row",
            tags$div(class = "detail-label", "Disability Plan"),
            tags$div(class = "detail-value", if (student$has_disability_plan) "Yes" else "No")
          ),

          # Plan adjustments
          if (student$has_disability_plan && nrow(student$plan_adjustments[[1]]) > 0) {
            adjustments <- student$plan_adjustments[[1]]

            tagList(
              tags$h5(style = "margin-top: 12px; margin-bottom: 8px;", "Plan Adjustments"),
              lapply(1:nrow(adjustments), function(i) {
                adj <- adjustments[i, ]
                tags$div(
                  class = "support-item",
                  tags$div(class = "support-item-title", adj$adjustment_type),
                  tags$div(class = "support-item-desc", adj$description)
                )
              })
            )
          }
        )
      )
    )
  )
}
