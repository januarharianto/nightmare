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

      # Special Considerations Section
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Special Considerations"),
        tags$div(
          class = "detail-section-content",
          {
            consids <- student$special_consids[[1]]
            n_consids <- nrow(consids)

            if (n_consids == 0) {
              tags$div(class = "empty-state",
                tags$p("No special considerations"))
            } else {
              # Split by outcome type groups
              extensions <- consids[consids$outcome_type %in%
                c("Simple Extension", "Extension of time"), ]
              replacements <- consids[grepl("replacement.*exam", consids$outcome_type,
                                            ignore.case = TRUE), ]
              mark_adj <- consids[grepl("mark.*adjustment", consids$outcome_type,
                                        ignore.case = TRUE), ]
              other_types <- c("Simple Extension", "Extension of time")
              other <- consids[!consids$outcome_type %in% other_types &
                !grepl("replacement.*exam", consids$outcome_type, ignore.case = TRUE) &
                !grepl("mark.*adjustment", consids$outcome_type, ignore.case = TRUE), ]

              tagList(
                # Summary bar
                tags$div(
                  class = "consids-summary",
                  tags$div(
                    tags$div(class = "stat-label", "Total"),
                    tags$div(class = "stat-value", as.character(n_consids))
                  ),
                  tags$div(
                    tags$div(class = "stat-label", "Extensions"),
                    tags$div(class = "stat-value", as.character(nrow(extensions)))
                  ),
                  if (nrow(replacements) > 0) {
                    tags$div(
                      tags$div(class = "stat-label", "Replacement Exams"),
                      tags$div(class = "stat-value consids-alert",
                               as.character(nrow(replacements)))
                    )
                  },
                  if (nrow(mark_adj) > 0) {
                    tags$div(
                      tags$div(class = "stat-label", "Mark Adj."),
                      tags$div(class = "stat-value", as.character(nrow(mark_adj)))
                    )
                  }
                ),

                # Replacement Exams group (shown first — most actionable)
                if (nrow(replacements) > 0) {
                  tagList(
                    tags$div(class = "consids-group-header consids-alert-header",
                             "Replacement Exams"),
                    tags$table(
                      class = "detail-table",
                      tags$thead(tags$tr(
                        tags$th("Assessment"),
                        tags$th("Type"),
                        tags$th("Ticket")
                      )),
                      tags$tbody(
                        lapply(1:nrow(replacements), function(i) {
                          r <- replacements[i, ]
                          tags$tr(
                            tags$td(r$assessment_name),
                            tags$td(r$assessment_type),
                            tags$td(r$ticket_id)
                          )
                        })
                      )
                    )
                  )
                },

                # Extensions group
                if (nrow(extensions) > 0) {
                  tagList(
                    tags$div(class = "consids-group-header", "Extensions"),
                    tags$table(
                      class = "detail-table",
                      tags$thead(tags$tr(
                        tags$th("Assessment"),
                        tags$th("Extended To"),
                        tags$th("Due Date"),
                        tags$th("")
                      )),
                      tags$tbody(
                        lapply(1:nrow(extensions), function(i) {
                          e <- extensions[i, ]
                          ext_str <- if (!is.na(e$extension_date)) {
                            format(e$extension_date, "%d %b %Y")
                          } else {
                            "--"
                          }
                          due_str <- if (!is.na(e$due_date)) {
                            format(e$due_date, "%d %b %Y")
                          } else {
                            "--"
                          }
                          # Flag if extension goes past closing date
                          past_closing <- !is.na(e$extension_date) &&
                            !is.na(e$closing_date) &&
                            as.Date(e$extension_date) > as.Date(e$closing_date)
                          warning_tag <- if (past_closing) {
                            tags$span(class = "consids-past-closing", "Past closing")
                          } else {
                            ""
                          }

                          tags$tr(
                            tags$td(e$assessment_name),
                            tags$td(ext_str),
                            tags$td(due_str),
                            tags$td(warning_tag)
                          )
                        })
                      )
                    )
                  )
                },

                # Mark Adjustments group
                if (nrow(mark_adj) > 0) {
                  tagList(
                    tags$div(class = "consids-group-header", "Mark Adjustments"),
                    tags$table(
                      class = "detail-table",
                      tags$thead(tags$tr(
                        tags$th("Assessment"),
                        tags$th("Type"),
                        tags$th("Ticket")
                      )),
                      tags$tbody(
                        lapply(1:nrow(mark_adj), function(i) {
                          m <- mark_adj[i, ]
                          tags$tr(
                            tags$td(m$assessment_name),
                            tags$td(m$assessment_type),
                            tags$td(m$ticket_id)
                          )
                        })
                      )
                    )
                  )
                },

                # Other outcomes
                if (nrow(other) > 0) {
                  tagList(
                    tags$div(class = "consids-group-header", "Other"),
                    tags$table(
                      class = "detail-table",
                      tags$thead(tags$tr(
                        tags$th("Assessment"),
                        tags$th("Outcome"),
                        tags$th("Ticket")
                      )),
                      tags$tbody(
                        lapply(1:nrow(other), function(i) {
                          o <- other[i, ]
                          tags$tr(
                            tags$td(o$assessment_name),
                            tags$td(o$outcome_type),
                            tags$td(o$ticket_id)
                          )
                        })
                      )
                    )
                  )
                }
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
          {
            has_plan <- isTRUE(student$has_disability_plan)
            adjustments <- if (has_plan) student$plan_adjustments[[1]] else data.frame(
              category = character(), arrangement_type = character(),
              value = character(), stringsAsFactors = FALSE
            )
            n_adjustments <- nrow(adjustments)

            # Summary row: Disability Plan status + count
            summary_tags <- tagList(
              tags$div(
                class = "detail-row",
                tags$div(class = "detail-label", "Disability Plan"),
                tags$div(class = "detail-value",
                  if (has_plan) {
                    if (n_adjustments > 0) {
                      paste0("Yes (", n_adjustments, " active adjustment",
                             if (n_adjustments != 1) "s" else "", ")")
                    } else {
                      "Yes"
                    }
                  } else {
                    "No"
                  }
                )
              )
            )

            if (!has_plan || n_adjustments == 0) {
              summary_tags
            } else {
              # Helper: clean arrangement name for display
              clean_name <- function(name) {
                name <- gsub("\\s*\\(Pro Rata\\)\\s*\\(Minutes/hour\\)", "", name)
                name <- gsub("\\s*\\(Pro Rata\\)", "", name)
                name <- gsub("\\s*\\(Minutes/hour\\)", "", name)
                name <- gsub("^Online - ", "", name)
                name <- trimws(name)
                name
              }

              # Helper: determine if value is a simple yes/flag or has specific data
              is_yes_only <- function(val) {
                tolower(trimws(val)) %in% c("yes", "y", "true", "x")
              }

              # Helper: format value for display
              format_value <- function(val, arrangement) {
                v <- trimws(val)
                # If arrangement mentions minutes/hour, add suffix
                if (grepl("Minutes/hour", arrangement, ignore.case = TRUE)) {
                  return(paste0(v, " min/hour"))
                }
                v
              }

              # Build groups from adjustments
              # 1. Extensions
              ext_mask <- adjustments$category == "Assessment Adjustment" &
                grepl("Extension|Take Home", adjustments$arrangement_type, ignore.case = TRUE)
              ext_df <- adjustments[ext_mask, ]

              # 2. Exam Accommodations
              exam_mask <- grepl("Exam Adjustment", adjustments$category, ignore.case = TRUE)
              exam_df <- adjustments[exam_mask, ]

              # Merge in-person and online exam adjustments
              if (nrow(exam_df) > 0) {
                exam_df$clean_name <- sapply(exam_df$arrangement_type, clean_name)
                # Deduplicate: if same clean_name + same value exists, keep once
                exam_df <- exam_df[!duplicated(paste(exam_df$clean_name, exam_df$value)), ]
                exam_df$arrangement_type <- exam_df$clean_name
                exam_df$clean_name <- NULL
              }

              # 3. Presentation
              pres_mask <- grepl("Content Not|Present To|Group Presentations",
                                adjustments$arrangement_type, ignore.case = TRUE)
              pres_df <- adjustments[pres_mask, ]

              # 4. Classroom Support
              class_mask <- adjustments$category == "In-Class Support & Management"
              class_df <- adjustments[class_mask, ]

              # 5. Placement
              place_mask <- grepl("Placement", adjustments$category, ignore.case = TRUE)
              place_df <- adjustments[place_mask, ]

              # Collect already-assigned rows
              assigned <- ext_mask | exam_mask | pres_mask | class_mask | place_mask

              groups <- list(
                list(label = "Extensions", df = ext_df),
                list(label = "Exam Accommodations", df = exam_df),
                list(label = "Presentation", df = pres_df),
                list(label = "Classroom Support", df = class_df),
                list(label = "Placement", df = place_df)
              )

              # Add uncategorised group for anything not matched
              other_df <- adjustments[!assigned, ]
              if (nrow(other_df) > 0) {
                groups <- c(groups, list(list(label = "Other", df = other_df)))
              }

              # Build UI for each group
              group_tags <- lapply(groups, function(g) {
                df <- g$df
                if (nrow(df) == 0) return(NULL)

                # Split into value rows vs yes-only tags
                yes_mask <- sapply(df$value, is_yes_only)
                value_rows <- df[!yes_mask, ]
                tag_rows <- df[yes_mask, ]

                tagList(
                  tags$div(
                    class = "plan-group",
                    tags$div(class = "plan-group-header", g$label),

                    # Value rows (label-value flex layout)
                    if (nrow(value_rows) > 0) {
                      lapply(1:nrow(value_rows), function(j) {
                        row <- value_rows[j, ]
                        tags$div(
                          class = "plan-adjustment-row",
                          tags$span(class = "plan-adjustment-label",
                                    clean_name(row$arrangement_type)),
                          tags$span(class = "plan-adjustment-value",
                                    format_value(row$value, row$arrangement_type))
                        )
                      })
                    },

                    # Yes-only tags (inline)
                    if (nrow(tag_rows) > 0) {
                      tags$div(
                        class = "plan-tags",
                        lapply(1:nrow(tag_rows), function(j) {
                          tags$span(class = "plan-tag",
                                    clean_name(tag_rows[j, "arrangement_type"]))
                        })
                      )
                    }
                  )
                )
              })

              tagList(summary_tags, Filter(Negate(is.null), group_tags))
            }
          }
        )
      )
    )
  )
}
