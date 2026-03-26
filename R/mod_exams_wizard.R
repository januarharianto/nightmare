#' @keywords internal
# -- mod_exams_wizard.R ---------------------------------------------
# Wizard step renderers for exam upload.

# Standalone helpers (no closure deps)

#' Get wizard step definitions
#' @return list with nums and labels
get_steps <- function() {
  list(
    nums = c(1L, 2L, 3L, 4L),
    labels = c("Upload", "Columns", "Name", "Review")
  )
}

#' Build the wizard card wrapper with step indicator and footer
#' @param current_step integer step number
#' @param content tag list for card body
#' @param footer tag list for card footer
#' @return shiny tags
render_wizard_card <- function(current_step, content, footer) {
  steps <- get_steps()
  step_nums <- steps$nums
  step_labels <- steps$labels
  total <- length(step_nums)
  current_idx <- which(step_nums == current_step)
  if (length(current_idx) == 0) current_idx <- 1

  # Build step indicator dots with connectors
  indicator_items <- list()
  for (i in seq_along(step_nums)) {
    dot_class <- "exams-step-dot"
    if (i < current_idx) {
      dot_class <- paste(dot_class, "completed")
    } else if (i == current_idx) {
      dot_class <- paste(dot_class, "active")
    }

    indicator_items <- c(indicator_items, list(
      tags$div(class = "exams-step-item",
        tags$div(class = dot_class, as.character(i)),
        tags$div(class = "exams-step-label meta-label", step_labels[i])
      )
    ))

    # Add connector between dots (not after last)
    if (i < total) {
      conn_class <- "exams-step-connector"
      if (i < current_idx) conn_class <- paste(conn_class, "completed")
      indicator_items <- c(indicator_items, list(
        tags$div(class = conn_class)
      ))
    }
  }

  tags$div(class = "exams-wizard-card",
    tags$div(class = "exams-step-indicator",
      indicator_items
    ),
    tags$div(class = "exams-wizard-card-body",
      content
    ),
    tags$div(class = "exams-wizard-card-footer",
      footer
    )
  )
}

# Wizard renderer -- called from examsModuleServer with closure access.
render_exam_wizard <- function(ns, input, output, wizardOpen, wizardStep,
                                parsedScores, detectedSource, matchPreview,
                                conflicts, assessmentName, maxPoints,
                                examData, studentData, dataSources) {

  cancel_btn <- function() {
    tags$button(
      class = "exams-wizard-btn-secondary",
      onclick = sprintf(
        "Shiny.setInputValue('%s', true, {priority: 'event'})",
        ns("wizard_cancel")
      ),
      "Cancel"
    )
  }

  back_btn <- function(target_step) {
    tags$button(
      class = "exams-wizard-btn-secondary",
      onclick = sprintf(
        "Shiny.setInputValue('%s', %d, {priority: 'event'})",
        ns("wizard_back"), target_step
      ),
      "Back"
    )
  }

  # --- Wizard overlay rendering ---

  output$wizard_overlay <- renderUI({
    if (!wizardOpen()) return(NULL)

    step <- wizardStep()
    wizard_content <- if (step == 1L) render_upload_step()
      else if (step == 2L) render_column_mapping()
      else if (step == 3L) render_assessment_naming()
      else if (step == 4L) render_review_step()
      else NULL

    tags$div(class = "exams-wizard-overlay",
      tags$div(class = "exams-wizard-backdrop",
        onclick = sprintf(
          "Shiny.setInputValue('%s', true, {priority: 'event'})",
          ns("wizard_cancel")
        )
      ),
      wizard_content
    )
  })

  # --- Step 1: Upload ---

  render_upload_step <- function() {
    detected <- detectedSource()
    detected_badge <- NULL
    if (!is.null(detected)) {
      badge_text <- if (detected$type == "gradescope") "Gradescope detected" else "Manual CSV"
      detected_badge <- tags$div(class = "exams-detected-badge", badge_text)
    }

    content <- tags$div(class = "exams-upload-zone",
      tags$div(class = "exams-upload-prompt",
        "Upload exam scores to begin"
      ),
      fileInput(ns("exam_file"), label = NULL, accept = c(".csv", ".xlsx", ".xls"),
                width = "100%"),
      tags$div(class = "exams-upload-formats",
        "Accepted: .csv, .xlsx, .xls"
      ),
      detected_badge
    )

    # No cancel on upload step, just empty left side
    footer <- tags$div(style = "display: contents;",
      tags$div(),
      tags$div()
    )

    render_wizard_card(1L, content, footer)
  }

  # --- Step 2: Column mapping ---

  render_column_mapping <- function() {
    detected <- detectedSource()
    if (is.null(detected)) return(NULL)

    headers <- detected$headers
    is_gradescope <- detected$type == "gradescope"
    header_choices <- c("Select..." = "", headers)

    # Build column pickers based on source type
    if (is_gradescope) {
      pickers <- tags$div(
        tags$div(class = "exams-detected-badge", style = "margin-bottom: 12px;",
          "Gradescope detected"),
        tags$div(
          style = "display: flex; gap: 16px; margin-bottom: 12px;",
          tags$div(
            style = "flex: 1;",
            tags$span(class = "exams-label meta-label", "Score Column"),
            selectInput(ns("score_col"), label = NULL, choices = header_choices, width = "100%")
          ),
          tags$div(
            style = "flex: 1;",
            tags$span(class = "exams-label meta-label", "Max Marks Column"),
            selectInput(ns("max_col"), label = NULL, choices = header_choices, width = "100%")
          )
        )
      )
    } else {
      pickers <- tags$div(
        tags$div(
          style = "display: flex; gap: 16px; margin-bottom: 12px;",
          tags$div(
            style = "flex: 1;",
            tags$span(class = "exams-label meta-label", "Student ID Column"),
            selectInput(ns("id_col"), label = NULL, choices = header_choices, width = "100%")
          ),
          tags$div(
            style = "flex: 1;",
            tags$span(class = "exams-label meta-label", "Score Column"),
            selectInput(ns("score_col"), label = NULL, choices = header_choices, width = "100%")
          )
        ),
        tags$div(
          style = "margin-bottom: 12px;",
          tags$span(class = "exams-label meta-label", "Max Marks"),
          numericInput(ns("manual_max_points"), label = NULL, value = NA, min = 1,
                       width = "100%")
        )
      )
    }

    content <- pickers

    footer <- tags$div(style = "display: contents;",
      cancel_btn(),
      tags$div(style = "display: flex; gap: 8px;",
        back_btn(1L),
        tags$button(
          class = "exams-wizard-btn btn-primary",
          onclick = sprintf(
            "Shiny.setInputValue('%s', true, {priority: 'event'})",
            ns("confirm_columns")
          ),
          "Next"
        )
      )
    )

    render_wizard_card(2L, content, footer)
  }

  # --- Step 3: Assessment naming ---

  render_assessment_naming <- function() {
    exam <- examData()
    existing_names <- names(exam$assessments)

    name_choices <- c("New assessment" = "__new__")
    if (length(existing_names) > 0) {
      name_choices <- c(setNames(existing_names, existing_names), name_choices)
    }

    content <- tags$div(
      tags$div(
        style = "margin-bottom: 12px;",
        tags$span(class = "exams-label meta-label", "Assessment"),
        selectInput(ns("assessment_choice"), label = NULL,
                    choices = name_choices, width = "100%")
      ),
      uiOutput(ns("new_name_input"))
    )

    footer <- tags$div(style = "display: contents;",
      cancel_btn(),
      tags$div(style = "display: flex; gap: 8px;",
        back_btn(2L),
        tags$button(
          class = "exams-wizard-btn btn-primary",
          onclick = sprintf(
            "Shiny.setInputValue('%s', true, {priority: 'event'})",
            ns("confirm_naming")
          ),
          "Next"
        )
      )
    )

    render_wizard_card(3L, content, footer)
  }

  output$new_name_input <- renderUI({
    if (is.null(input$assessment_choice) || input$assessment_choice != "__new__") {
      return(NULL)
    }
    tags$div(
      style = "margin-bottom: 12px;",
      tags$span(class = "exams-label meta-label", "Assessment Name"),
      textInput(ns("new_assessment_name"), label = NULL, placeholder = "e.g. Final Exam",
                width = "100%")
    )
  })

  # --- Step 4: Review (match preview + conflict resolution) ---

  render_review_step <- function() {
    match_result <- matchPreview()
    if (is.null(match_result)) return(NULL)

    n_matched <- nrow(match_result$matched)
    n_unmatched <- length(match_result$unmatched_sids)
    n_missing <- length(match_result$missing_from_upload)
    conf <- conflicts()
    n_conflicts <- if (!is.null(conf)) nrow(conf) else 0L

    # Stats row
    stats <- tags$div(class = "exams-match-stats summary-bar",
      tags$div(
        tags$div(class = "stat-label", "Matched"),
        tags$div(class = "stat-value", as.character(n_matched))
      ),
      tags$div(
        tags$div(class = "stat-label", "Unmatched"),
        tags$div(class = "stat-value", as.character(n_unmatched))
      ),
      tags$div(
        tags$div(class = "stat-label", "Missing"),
        tags$div(class = "stat-value", as.character(n_missing))
      ),
      if (n_conflicts > 0) {
        tags$div(
          tags$div(class = "stat-label", "Conflicts"),
          tags$div(class = "stat-value", as.character(n_conflicts))
        )
      }
    )

    # Unmatched warning
    unmatched_warning <- NULL
    if (n_unmatched > 0) {
      unmatched_warning <- tags$div(
        style = "margin-top: 8px; padding: 8px; background: #F5F5F5; border: 1px solid #CCCCCC; font-size: 12px;",
        tags$span(style = "font-weight: 700; text-transform: uppercase; font-size: 10px; color: #AAAAAA;",
          "Unmatched SIDs (not in Canvas): "),
        paste(match_result$unmatched_sids, collapse = ", ")
      )
    }

    # Conflict table (scrollable)
    conflict_section <- NULL
    if (n_conflicts > 0) {
      data <- studentData()
      sources <- dataSources()

      conflict_section <- tags$div(
        tags$div(class = "exams-label meta-label", style = "margin-top: 12px;", "Conflicts"),
        tags$div(class = "exams-conflict-scroll",
          tags$table(class = "detail-table",
            tags$thead(tags$tr(
              tags$th("Student"),
              tags$th("SID"),
              tags$th("Previous"),
              tags$th("New"),
              tags$th("Special Con"),
              tags$th("Action")
            )),
            tags$tbody(
              lapply(seq_len(nrow(conf)), function(i) {
                row <- conf[i, ]
                sid <- row$sid

                student_name <- ""
                has_replacement <- FALSE
                if (!is.null(data) && nrow(data) > 0) {
                  idx <- which(as.character(data$student_id) == sid)
                  if (length(idx) > 0) {
                    student_name <- as.character(data$name[idx[1]])
                    if (isTRUE(sources$consids)) {
                      consids <- data$special_consids[[idx[1]]]
                      if (!is.null(consids) && nrow(consids) > 0) {
                        has_replacement <- any(
                          grepl("replacement.*exam", consids$outcome_type, ignore.case = TRUE) &
                          consids$approved
                        )
                      }
                    }
                  }
                }

                default_action <- if (has_replacement) "new" else "old"
                btn_id_new <- ns(paste0("conflict_", sid, "_new"))
                btn_id_old <- ns(paste0("conflict_", sid, "_old"))

                speccon_badge <- if (has_replacement) {
                  tags$span(class = "exam-speccon-badge", "Replacement Approved")
                } else {
                  ""
                }

                tags$tr(
                  tags$td(student_name),
                  tags$td(sid),
                  tags$td(as.character(row$prev_score)),
                  tags$td(as.character(row$new_score)),
                  tags$td(speccon_badge),
                  tags$td(
                    tags$div(class = "plot-type-toggle toggle-group",
                      tags$button(
                        id = btn_id_new,
                        class = paste("conflict-btn toggle-btn", if (default_action == "new") "active" else ""),
                        `data-sid` = sid,
                        `data-action` = "new",
                        onclick = sprintf(
                          "this.classList.add('active');document.getElementById('%s').classList.remove('active');",
                          btn_id_old
                        ),
                        "Use New"
                      ),
                      tags$button(
                        id = btn_id_old,
                        class = paste("conflict-btn toggle-btn", if (default_action == "old") "active" else ""),
                        `data-sid` = sid,
                        `data-action` = "old",
                        onclick = sprintf(
                          "this.classList.add('active');document.getElementById('%s').classList.remove('active');",
                          btn_id_new
                        ),
                        "Keep Old"
                      )
                    )
                  )
                )
              })
            )
          )
        )
      )
    }

    content <- tags$div(
      tags$div(class = "exams-label meta-label",
        paste0("Review \u2014 ", assessmentName())
      ),
      stats,
      unmatched_warning,
      conflict_section
    )

    # Confirm button: uses conflict JS if conflicts exist
    confirm_btn <- if (n_conflicts > 0) {
      tags$button(
        class = "exams-wizard-btn btn-primary",
        onclick = sprintf(
          "var btns=document.querySelectorAll('.conflict-btn.active');var res={};btns.forEach(function(b){res[b.dataset.sid]=b.dataset.action;});Shiny.setInputValue('%s',res,{priority:'event'});",
          ns("confirm_conflicts")
        ),
        "Confirm"
      )
    } else {
      tags$button(
        class = "exams-wizard-btn btn-primary",
        onclick = sprintf(
          "Shiny.setInputValue('%s', true, {priority: 'event'})",
          ns("confirm_upload")
        ),
        "Confirm"
      )
    }

    footer <- tags$div(style = "display: contents;",
      cancel_btn(),
      tags$div(style = "display: flex; gap: 8px;",
        back_btn(3L),
        confirm_btn
      )
    )

    render_wizard_card(4L, content, footer)
  }
}
