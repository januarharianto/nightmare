# -- ui_detail_consids.R -------------------------------------------
# Special considerations card for student detail view.

build_consids_card <- function(student) {
  # Helper: format date or return "--"
  fmt_date <- function(d) {
    if (!is.na(d)) format(as.Date(d), "%d %b %Y") else "--"
  }

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
          # Split by outcome type groups (handle NA outcome_type)
          has_outcome <- !is.na(consids$outcome_type)
          extensions <- consids[has_outcome & consids$outcome_type %in%
            c("Simple Extension", "Extension of time"), ]
          replacements <- consids[has_outcome &
            grepl("replacement.*exam", consids$outcome_type, ignore.case = TRUE), ]
          mark_adj <- consids[has_outcome &
            grepl("mark.*adjustment", consids$outcome_type, ignore.case = TRUE), ]
          ext_types <- c("Simple Extension", "Extension of time")
          other <- consids[has_outcome &
            !(consids$outcome_type %in% ext_types) &
            !grepl("replacement.*exam", consids$outcome_type, ignore.case = TRUE) &
            !grepl("mark.*adjustment", consids$outcome_type, ignore.case = TRUE), ]
          # Pending with no outcome type
          pending_no_outcome <- consids[!has_outcome & consids$state == "Pending", ]

          n_approved <- sum(consids$approved, na.rm = TRUE)
          n_pending <- sum(consids$state == "Pending", na.rm = TRUE)

          tagList(
            # Summary bar
            tags$div(
              class = "consids-summary summary-bar",
              tags$div(
                class = "consids-summary-primary",
                tags$div(
                  tags$div(class = "stat-label", "Approved"),
                  tags$div(class = "stat-value", as.character(n_approved))
                ),
                if (n_pending > 0) {
                  tags$div(
                    tags$div(class = "stat-label", "Pending"),
                    tags$div(class = "stat-value consids-pending-value",
                             as.character(n_pending))
                  )
                }
              ),
              tags$div(
                class = "consids-summary-breakdown",
                tags$div(
                  tags$div(class = "stat-label", "Extensions"),
                  tags$div(class = "stat-value", as.character(nrow(extensions)))
                ),
                if (nrow(replacements) > 0 || nrow(pending_no_outcome) > 0) {
                  tags$div(
                    tags$div(class = "stat-label", "Repl. Exams"),
                    tags$div(class = "stat-value",
                             as.character(nrow(replacements) + nrow(pending_no_outcome)))
                  )
                },
                if (nrow(mark_adj) > 0) {
                  tags$div(
                    tags$div(class = "stat-label", "Mark Adj."),
                    tags$div(class = "stat-value", as.character(nrow(mark_adj)))
                  )
                }
              )
            ),

            # Replacement Exams group (shown first — most actionable)
            if (nrow(replacements) > 0 || nrow(pending_no_outcome) > 0) {
              exam_rows <- rbind(
                if (nrow(replacements) > 0) replacements else NULL,
                if (nrow(pending_no_outcome) > 0) pending_no_outcome else NULL
              )

              tagList(
                tags$div(class = "consids-group-header consids-alert-header",
                         "Exam Considerations"),
                tags$table(
                  class = "detail-table",
                  tags$thead(tags$tr(
                    tags$th("Assessment"),
                    tags$th("Due Date"),
                    tags$th("Outcome"),
                    tags$th("Status")
                  )),
                  tags$tbody(
                    lapply(1:nrow(exam_rows), function(i) {
                      r <- exam_rows[i, ]
                      due_str <- fmt_date(r$due_date)
                      outcome_str <- if (!is.na(r$outcome_type)) {
                        r$outcome_type
                      } else {
                        "--"
                      }
                      state_class <- if (r$state == "Pending") {
                        "consids-state-pending"
                      } else {
                        "consids-state-approved"
                      }

                      tags$tr(
                        tags$td(r$assessment_name),
                        tags$td(due_str),
                        tags$td(outcome_str),
                        tags$td(tags$span(class = state_class, r$state))
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
                    tags$th("Due Date")
                  )),
                  tags$tbody(
                    lapply(1:nrow(extensions), function(i) {
                      e <- extensions[i, ]
                      ext_str <- fmt_date(e$extension_date)
                      due_str <- fmt_date(e$due_date)
                      tags$tr(
                        tags$td(e$assessment_name),
                        tags$td(ext_str),
                        tags$td(due_str)
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
                    tags$th("Status")
                  )),
                  tags$tbody(
                    lapply(1:nrow(other), function(i) {
                      o <- other[i, ]
                      state_class <- if (o$state == "Pending") {
                        "consids-state-pending"
                      } else {
                        "consids-state-approved"
                      }
                      tags$tr(
                        tags$td(o$assessment_name),
                        tags$td(o$outcome_type),
                        tags$td(tags$span(class = state_class, o$state))
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
  )
}
