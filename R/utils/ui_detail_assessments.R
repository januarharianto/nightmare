# -- ui_detail_assessments.R ---------------------------------------
# Assessments card for student detail view: weights, projections, score table.

# Shared JS for live weight-total calculation.
weight_total_script <- function() {
  tags$script(HTML("
    document.querySelectorAll('.weight-input').forEach(function(el) {
      el.addEventListener('input', function() {
        var total = 0;
        document.querySelectorAll('.weight-input').forEach(function(inp) {
          var v = parseFloat(inp.value);
          if (!isNaN(v)) total += v;
        });
        var display = document.getElementById('weight-total-display');
        if (display) {
          display.textContent = total.toFixed(0) + ' / 100%';
          if (total > 100) display.classList.add('weight-total-over');
          else display.classList.remove('weight-total-over');
        }
      });
    });
  "))
}

build_assessments_card <- function(student, all_students = NULL, exam_data = NULL, weights_data = NULL, editing_weights = FALSE) {
  weights <- if (!is.null(weights_data)) weights_data$weights else list()
  due_dates <- if (!is.null(weights_data)) weights_data$due_dates else list()

  # Section header depends on edit mode
  assess_header <- if (editing_weights) {
    tags$div(class = "detail-section-header editing-weights",
      "Assessments \u2014 Editing Weights",
      tags$button(class = "weights-done-btn btn-primary",
        onclick = "var inputs=document.querySelectorAll('.weight-input');var w={};inputs.forEach(function(el){var v=parseFloat(el.value);if(!isNaN(v)&&v>0)w[el.dataset.assessment]=v;});Shiny.setInputValue('save_weights',JSON.stringify(w),{priority:'event'});",
        "Done"
      )
    )
  } else {
    tags$div(class = "detail-section-header",
      "Assessments",
      tags$button(class = "edit-weights-link",
        onclick = "Shiny.setInputValue('toggle_edit_weights', true, {priority: 'event'})",
        "Edit Weights"
      )
    )
  }

  # Pre-compute exam scores once for this student (used in projection and table rendering)
  student_exam_scores <- get_student_exam_scores(exam_data, as.character(student$student_id))

  # Grade projection strip (only when weights configured and at least one scored)
  projection_strip <- NULL
  if (!is.null(weights_data) && length(weights_data$weights) > 0) {
    projection <- calculate_projected_grade(
      weights, student$assignments[[1]], exam_data, as.character(student$student_id), due_dates,
      exam_scores = student_exam_scores
    )
    if (projection$completed_weight > 0) {
      risk <- calculate_risk_level(projection)
      pct_completed <- projection$completed_weight / projection$total_weight * 100
      current_pct <- projection$completed_points / projection$total_weight * 100
      max_pct <- projection$max_possible / projection$total_weight * 100
      projection_strip <- tags$div(class = "grade-projection-strip",
        tags$div(class = "grade-projection-top",
          tags$div(class = "grade-projection-left",
            tags$div(class = "grade-projection-pair",
              tags$span(class = "grade-projection-label meta-label", "Current"),
              tags$span(class = "grade-projection-value", sprintf("%.1f%%", current_pct))
            ),
            tags$div(class = "grade-projection-pair",
              tags$span(class = "grade-projection-label meta-label", "Range"),
              tags$span(class = "grade-projection-value grade-projection-range",
                sprintf("%.1f%% \u2013 %.1f%%", current_pct, max_pct))
            )
          ),
          tags$div(class = "risk-info-wrapper",
            tags$span(class = paste("risk-badge", paste0("risk-", risk$level)), risk$label),
            tags$button(class = "risk-info-btn", onclick = "this.nextElementSibling.classList.toggle('open')", "i"),
            tags$div(class = "risk-info-popover",
              tags$h4("Risk Assessment Model"),
              tags$p("Based on projected performance analysis (Arnold & Pistilli, 2012). Calculates the minimum average score needed on remaining assessments to achieve a passing grade (50%)."),
              tags$table(class = "risk-threshold-table",
                tags$tr(tags$td(tags$span(class = "risk-badge risk-critical", "Critical")), tags$td("Cannot pass \u2014 maximum possible grade is below 50%")),
                tags$tr(tags$td(tags$span(class = "risk-badge risk-high", "High")), tags$td("Needs >80% average on all remaining assessments")),
                tags$tr(tags$td(tags$span(class = "risk-badge risk-moderate", "Moderate")), tags$td("Needs >60% average on all remaining assessments")),
                tags$tr(tags$td(tags$span(class = "risk-badge risk-low", "Low")), tags$td("On track \u2014 needs \u226460% average on remaining"))
              ),
              tags$p(style = "margin-top: 8px; font-size: 10px; color: #AAA;", "Ref: Arnold & Pistilli (2012), Course Signals at Purdue")
            )
          )
        ),
        tags$div(class = "grade-progress-bar",
          tags$div(class = "grade-progress-fill",
            style = sprintf("width: %.1f%%", pct_completed)
          )
        ),
        tags$div(class = "grade-progress-label meta-label",
          sprintf("%.0f%% of grade assessed", pct_completed)
        )
      )
    }
  }

  tags$div(
    class = "detail-section",
    assess_header,
    projection_strip,
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
          # Compute summary stats using due-date-based status
          n_total <- nrow(assignments)
          statuses <- vapply(seq_len(n_total), function(i) {
            compute_assessment_status(due_dates[[assignments$name[i]]], !is.na(assignments$score[i]))
          }, character(1))
          n_completed <- sum(statuses == "completed")
          n_ongoing <- sum(statuses == "ongoing")
          n_failing <- sum(statuses == "completed" & assignments$percentage < 50) +
            sum(statuses == "missing")
          n_spec_cons <- nrow(student$special_consids[[1]])

          all_ongoing <- n_total == n_ongoing

          tagList(
            # Summary statistics
            tags$div(
              class = "assessment-summary summary-bar",
              tags$div(
                tags$div(class = "stat-label", "Completed"),
                tags$div(class = "stat-value",
                  if (all_ongoing) "--" else sprintf("%d of %d", n_completed, n_total)
                )
              ),
              tags$div(
                tags$div(class = "stat-label", "Failing"),
                tags$div(class = "stat-value",
                  if (all_ongoing) "--" else as.character(n_failing)
                )
              ),
              tags$div(
                tags$div(class = "stat-label", "Spec Cons"),
                tags$div(class = "stat-value", as.character(n_spec_cons))
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
                  tags$th("Weight")
                )
              ),
              tags$tbody(
                # Canvas assignment rows
                lapply(1:nrow(assignments), function(i) {
                  a <- assignments[i, ]
                  has_sc <- !is.na(a$score)
                  status <- compute_assessment_status(due_dates[[a$name]], has_sc)
                  ongoing <- status == "ongoing"
                  missing <- status == "missing"

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

                  row_class <- if (ongoing) {
                    "assessment-pending"
                  } else if (missing) {
                    "assessment-missing"
                  } else if (a$percentage < 50) {
                    "assessment-failing"
                  } else {
                    ""
                  }

                  w_val <- weights[[a$name]]
                  tags$tr(
                    class = row_class,
                    tags$td(a$name),
                    tags$td(score_display),
                    tags$td(class = "assessment-pct", pct_display),
                    if (editing_weights) {
                      tags$td(
                        tags$input(type = "text", class = "weight-input", inputmode = "numeric", pattern = "[0-9]*",
                          `data-assessment` = a$name,
                          value = if (!is.null(w_val)) w_val else "",
                          placeholder = "\u2014"
                        )
                      )
                    } else {
                      tags$td(class = "assessment-weight",
                        if (!is.null(w_val)) paste0(w_val, "%") else "\u2014"
                      )
                    }
                  )
                }),

                # Exam data rows (all uploaded assessments, missing if no score)
                if (!is.null(exam_data) && length(exam_data$assessments) > 0) {
                  scores_df <- student_exam_scores
                  lapply(names(exam_data$assessments), function(aname) {
                    a <- exam_data$assessments[[aname]]
                    mp <- a$max_points
                    row <- if (nrow(scores_df) > 0) scores_df[scores_df$assessment == aname, , drop = FALSE] else scores_df
                    has_score <- nrow(row) > 0
                    exam_status <- compute_assessment_status(due_dates[[aname]], has_score)

                    # Source type tag
                    src_types <- unique(vapply(a$sittings, function(s) s$source_type %||% "manual", character(1)))
                    src_label <- if (any(src_types == "gradescope")) "Gradescope" else "Manual"
                    src_tag <- tags$span(class = "exam-source-tag", src_label)

                    ew_val <- weights[[aname]]
                    weight_td <- if (editing_weights) {
                      tags$td(
                        tags$input(type = "text", class = "weight-input", inputmode = "numeric", pattern = "[0-9]*",
                          `data-assessment` = aname,
                          value = if (!is.null(ew_val)) ew_val else "",
                          placeholder = "\u2014"
                        )
                      )
                    } else {
                      tags$td(class = "assessment-weight",
                        if (!is.null(ew_val)) paste0(ew_val, "%") else "\u2014"
                      )
                    }

                    if (has_score) {
                      pct <- (row$score[1] / mp) * 100
                      row_class <- if (exam_status == "ongoing") "assessment-pending" else if (pct < 50) "assessment-failing" else ""
                      tags$tr(
                        class = row_class,
                        tags$td(aname, src_tag),
                        tags$td(sprintf("%g / %g", row$score[1], mp)),
                        tags$td(class = "assessment-pct", sprintf("%.0f%%", pct)),
                        weight_td
                      )
                    } else {
                      no_score_class <- if (exam_status == "ongoing") "assessment-pending" else "assessment-missing"
                      no_score_label <- if (exam_status == "ongoing") "--" else "Missing"
                      tags$tr(
                        class = no_score_class,
                        tags$td(aname, src_tag),
                        tags$td(if (exam_status == "ongoing") "--" else sprintf("-- / %g", mp)),
                        tags$td(class = "assessment-pct", no_score_label),
                        weight_td
                      )
                    }
                  })
                }
              ),
              # Weight total footer (edit mode only)
              if (editing_weights) {
                total_weight <- sum(unlist(weights_data$weights))
                tagList(
                  tags$tfoot(
                    tags$tr(class = "weight-total-row",
                      tags$td(style = "text-align: right;", "Total"),
                      tags$td(""),
                      tags$td(""),
                      tags$td(id = "weight-total-display", sprintf("%.0f / 100%%", total_weight))
                    )
                  ),
                  weight_total_script()
                )
              }
            )
          )
        }
      }
    ),
    # Close risk popover on outside click
    tags$script(HTML("
      document.addEventListener('click', function(e) {
        if (!e.target.closest('.risk-info-wrapper')) {
          document.querySelectorAll('.risk-info-popover.open').forEach(function(p) {
            p.classList.remove('open');
          });
        }
      });
    "))
  )
}
