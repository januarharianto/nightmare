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

      # Assessments Section
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Assessments"),
        tags$div(
          class = "detail-section-content",
          {
            assignments <- student$assignments[[1]]

            if (nrow(assignments) == 0) {
              tags$div(
                class = "empty-state",
                tags$p("No assessment data available")
              )
            } else {
              # Compute summary stats
              completed <- assignments[!assignments$is_ongoing, ]
              n_total <- nrow(assignments)
              has_score <- completed[!is.na(completed$score), ]
              n_completed <- nrow(has_score)
              n_ongoing <- sum(assignments$is_ongoing)
              avg_pct <- if (n_completed > 0) round(mean(has_score$percentage, na.rm = TRUE), 0) else NA
              n_at_risk <- sum(!is.na(completed$score) & completed$percentage < 50) +
                sum(is.na(completed$score))

              all_ongoing <- n_total == n_ongoing

              tagList(
                # Summary statistics
                tags$div(
                  class = "assessment-summary",
                  tags$div(
                    tags$div(class = "stat-label", "Completed"),
                    tags$div(class = "stat-value",
                      if (all_ongoing) "--" else sprintf("%d of %d", n_completed, n_total)
                    )
                  ),
                  tags$div(
                    tags$div(class = "stat-label", "Average"),
                    tags$div(class = "stat-value",
                      if (all_ongoing || is.na(avg_pct)) "--" else sprintf("%d%%", avg_pct)
                    )
                  ),
                  tags$div(
                    tags$div(class = "stat-label", "At Risk"),
                    tags$div(class = "stat-value",
                      if (all_ongoing) "--" else as.character(n_at_risk)
                    )
                  )
                ),

                if (all_ongoing) {
                  tags$div(
                    style = "padding: 8px 0; color: #AAAAAA; font-size: 12px;",
                    "All assessments pending"
                  )
                },

                # Assessment table
                tags$table(
                  class = "detail-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Assessment"),
                      tags$th("Score"),
                      tags$th("Percentage"),
                      tags$th("Status")
                    )
                  ),
                  tags$tbody(
                    lapply(1:nrow(assignments), function(i) {
                      a <- assignments[i, ]
                      has_sc <- !is.na(a$score)
                      ongoing <- isTRUE(a$is_ongoing)
                      missing <- !has_sc && !ongoing

                      # Score column
                      score_display <- if (has_sc) {
                        sprintf("%g / %g", a$score, a$max_points)
                      } else if (ongoing) {
                        "--"
                      } else {
                        sprintf("-- / %g", a$max_points)
                      }

                      # Percentage column
                      pct_display <- if (has_sc) {
                        sprintf("%.0f%%", a$percentage)
                      } else if (ongoing) {
                        "--"
                      } else {
                        "Missing"
                      }

                      # Status column and row class
                      if (ongoing) {
                        row_class <- "assessment-pending"
                        status_html <- tags$span(class = "assessment-status status-pending", "Pending")
                      } else if (missing) {
                        row_class <- "assessment-missing"
                        status_html <- tags$span(class = "assessment-status status-missing", "Missing")
                      } else if (a$percentage < 50) {
                        row_class <- "assessment-failing"
                        status_html <- tags$span(class = "assessment-status status-failing", "Failing")
                      } else {
                        row_class <- ""
                        status_html <- ""
                      }

                      tags$tr(
                        class = row_class,
                        tags$td(a$name),
                        tags$td(score_display),
                        tags$td(class = "assessment-pct", pct_display),
                        tags$td(status_html)
                      )
                    })
                  )
                )
              )
            }
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
