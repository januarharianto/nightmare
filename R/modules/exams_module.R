# Exams Module for NIGHTMARE
# Upload and manage exam scores with multi-sitting conflict resolution.

examsModuleUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "exams-view",
    uiOutput(ns("wizard_overlay")),
    tags$div(class = "exams-summary-container",
      uiOutput(ns("exam_summary"))
    )
  )
}

examsModuleServer <- function(id, studentData, examData, currentUnit, dataSources, weightsData = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Edit mode toggle for assessment config
    editingConfig <- reactiveVal(FALSE)

    # Wizard visibility
    wizardOpen <- reactiveVal(FALSE)

    # Wizard state (steps: 1=Upload, 2=Columns, 3=Name, 4=Review)
    wizardStep <- reactiveVal(1L)
    parsedScores <- reactiveVal(NULL)      # named list (SID -> score)
    detectedSource <- reactiveVal(NULL)    # list(type, headers, preview)
    matchPreview <- reactiveVal(NULL)      # list(matched, unmatched_sids, missing_from_upload)
    conflicts <- reactiveVal(NULL)         # data.frame or NULL
    assessmentName <- reactiveVal(NULL)
    maxPoints <- reactiveVal(NULL)
    newSittingId <- reactiveVal(NULL)

    # All flows use 4 steps
    get_steps <- function() {
      list(
        nums = c(1L, 2L, 3L, 4L),
        labels = c("Upload", "Columns", "Name", "Review")
      )
    }

    # Build the wizard card wrapper with step indicator and footer
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
            tags$div(class = "exams-step-label", step_labels[i])
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

    # Cancel button resets to step 1
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

    # Back button goes to previous step
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

    # --- Open wizard ---

    observeEvent(input$open_wizard, {
      wizardOpen(TRUE)
    })

    # --- File upload triggers wizard ---

    observeEvent(input$exam_file, {
      req(input$exam_file)
      file_path <- input$exam_file$datapath

      tryCatch({
        detected <- detect_exam_source(file_path)
        detectedSource(detected)
        wizardStep(2L)  # Both types go to column mapping
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
      })
    })

    # --- Back ---

    observeEvent(input$wizard_back, {
      wizardStep(as.integer(input$wizard_back))
    })

    # --- Cancel ---

    observeEvent(input$wizard_cancel, {
      wizardOpen(FALSE)
      wizardStep(1L)
      parsedScores(NULL)
      detectedSource(NULL)
      matchPreview(NULL)
      conflicts(NULL)
      assessmentName(NULL)
      maxPoints(NULL)
    })

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
              tags$span(class = "exams-label", "Score Column"),
              selectInput(ns("score_col"), label = NULL, choices = header_choices, width = "100%")
            ),
            tags$div(
              style = "flex: 1;",
              tags$span(class = "exams-label", "Max Marks Column"),
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
              tags$span(class = "exams-label", "Student ID Column"),
              selectInput(ns("id_col"), label = NULL, choices = header_choices, width = "100%")
            ),
            tags$div(
              style = "flex: 1;",
              tags$span(class = "exams-label", "Score Column"),
              selectInput(ns("score_col"), label = NULL, choices = header_choices, width = "100%")
            )
          ),
          tags$div(
            style = "margin-bottom: 12px;",
            tags$span(class = "exams-label", "Max Marks"),
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
            class = "exams-wizard-btn",
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

    observeEvent(input$confirm_columns, {
      req(input$exam_file)
      detected <- detectedSource()
      is_gradescope <- !is.null(detected) && detected$type == "gradescope"

      score_col <- input$score_col
      if (is.null(score_col) || score_col == "") {
        showNotification("Please select a score column", type = "warning")
        return()
      }

      tryCatch({
        if (is_gradescope) {
          max_col <- input$max_col
          if (is.null(max_col) || max_col == "") {
            showNotification("Please select a max marks column", type = "warning")
            return()
          }
          scores <- parse_gradescope_export(
            input$exam_file$datapath, score_col
          )
          mp <- extract_max_marks(input$exam_file$datapath, max_col)
          maxPoints(mp)
        } else {
          id_col <- input$id_col
          if (is.null(id_col) || id_col == "") {
            showNotification("Please select a student ID column", type = "warning")
            return()
          }
          mp <- input$manual_max_points
          if (is.null(mp) || is.na(mp) || mp < 1) {
            showNotification("Please enter max marks", type = "warning")
            return()
          }
          scores <- parse_manual_columns(
            input$exam_file$datapath, id_col, score_col
          )
          maxPoints(mp)
        }
        parsedScores(scores)
        wizardStep(3L)
      }, error = function(e) {
        showNotification(paste("Error parsing columns:", e$message), type = "error")
      })
    })

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
          tags$span(class = "exams-label", "Assessment"),
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
            class = "exams-wizard-btn",
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
        tags$span(class = "exams-label", "Assessment Name"),
        textInput(ns("new_assessment_name"), label = NULL, placeholder = "e.g. Final Exam",
                  width = "100%")
      )
    })

    observeEvent(input$confirm_naming, {
      choice <- input$assessment_choice
      if (is.null(choice)) return()

      aname <- if (choice == "__new__") {
        input$new_assessment_name
      } else {
        choice
      }

      if (is.null(aname) || trimws(aname) == "") {
        showNotification("Please enter an assessment name", type = "warning")
        return()
      }

      assessmentName(trimws(aname))

      # Use max_points from existing assessment if selecting existing
      if (choice != "__new__") {
        exam <- examData()
        existing <- exam$assessments[[choice]]
        if (!is.null(existing)) {
          maxPoints(existing$max_points)
        }
      }

      # Match students
      scores <- parsedScores()
      data <- studentData()
      match_result <- match_exam_students(scores, data)
      matchPreview(match_result)

      # Detect conflicts
      exam <- examData()
      conf <- detect_conflicts(exam, trimws(aname), scores)
      if (nrow(conf) > 0) {
        conflicts(conf)
      } else {
        conflicts(NULL)
      }

      wizardStep(4L)
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
      stats <- tags$div(class = "exams-match-stats",
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
          tags$div(class = "exams-label", style = "margin-top: 12px;", "Conflicts"),
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
                      tags$div(class = "plot-type-toggle",
                        tags$button(
                          id = btn_id_new,
                          class = paste("conflict-btn", if (default_action == "new") "active" else ""),
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
                          class = paste("conflict-btn", if (default_action == "old") "active" else ""),
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
        tags$div(class = "exams-label",
          paste0("Review — ", assessmentName())
        ),
        stats,
        unmatched_warning,
        conflict_section
      )

      # Confirm button: uses conflict JS if conflicts exist
      confirm_btn <- if (n_conflicts > 0) {
        tags$button(
          class = "exams-wizard-btn",
          onclick = sprintf(
            "var btns=document.querySelectorAll('.conflict-btn.active');var res={};btns.forEach(function(b){res[b.dataset.sid]=b.dataset.action;});Shiny.setInputValue('%s',res,{priority:'event'});",
            ns("confirm_conflicts")
          ),
          "Confirm"
        )
      } else {
        tags$button(
          class = "exams-wizard-btn",
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

    # --- Confirm upload (no conflicts) ---

    observeEvent(input$confirm_upload, {
      do_save(list())
    })

    # --- Confirm conflicts ---

    observeEvent(input$confirm_conflicts, {
      resolutions_raw <- input$confirm_conflicts
      do_save(resolutions_raw)
    })

    # --- Save logic ---

    do_save <- function(resolutions_raw) {
      scores <- parsedScores()
      aname <- assessmentName()
      mp <- maxPoints()
      if (is.null(scores) || is.null(aname)) return()

      detected <- detectedSource()
      source_type <- if (!is.null(detected)) detected$type else "manual"
      source_file <- if (!is.null(input$exam_file)) input$exam_file$name else ""

      sitting <- list(
        upload_date = format(Sys.Date(), "%Y-%m-%d"),
        upload_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
        source_type = source_type,
        source_file = source_file,
        scores = scores
      )

      exam <- examData()
      exam <- add_exam_sitting(exam, aname, mp, sitting)

      # Get the new sitting_id
      new_sitting <- tail(exam$assessments[[aname]]$sittings, 1)[[1]]
      new_id <- new_sitting$sitting_id

      # Apply conflict resolutions: "new" -> set active to new sitting, "old" -> keep current
      num_replaced <- 0L
      if (length(resolutions_raw) > 0) {
        resolution_map <- list()
        for (sid in names(resolutions_raw)) {
          action <- resolutions_raw[[sid]]
          if (action == "new") {
            resolution_map[[sid]] <- new_id
          }
          # "old" means keep current active_sitting — no change needed
        }
        num_replaced <- length(resolution_map)
        if (num_replaced > 0) {
          exam <- resolve_conflicts(exam, aname, resolution_map)
        }
      }

      # Store replacement count on the sitting
      n_sittings <- length(exam$assessments[[aname]]$sittings)
      exam$assessments[[aname]]$sittings[[n_sittings]]$num_replaced <- num_replaced

      # Save
      unit <- currentUnit()
      if (!is.null(unit)) {
        save_exam_data(NIGHTMARE_CONFIG$data$data_dir, unit, exam)
      }
      examData(exam)

      # Reset wizard and close overlay
      wizardOpen(FALSE)
      wizardStep(1L)
      parsedScores(NULL)
      detectedSource(NULL)
      matchPreview(NULL)
      conflicts(NULL)
      assessmentName(NULL)
      maxPoints(NULL)

      showNotification(
        paste0("Saved ", length(scores), " scores for '", aname, "'"),
        type = "message"
      )
    }

    # --- Delete assessment ---

    observeEvent(input$delete_assessment, {
      aname <- input$delete_assessment
      exam <- examData()
      if (!is.null(exam$assessments[[aname]])) {
        exam$assessments[[aname]] <- NULL
        examData(exam)
        unit <- currentUnit()
        if (!is.null(unit)) {
          save_exam_data(NIGHTMARE_CONFIG$data$data_dir, unit, exam)
        }
        showNotification(paste0("Deleted '", aname, "'"), type = "message")
      }
    })

    # --- Summary table ---

    # Toggle config editing mode
    observeEvent(input$toggle_edit_config, {
      editingConfig(!editingConfig())
    })

    output$exam_summary <- renderUI({
      exam <- examData()
      summary_df <- get_exam_summary(exam)
      data <- studentData()
      editing <- editingConfig()
      wd <- if (!is.null(weightsData)) weightsData() else list(weights = list(), due_dates = list())
      weights <- wd$weights
      due_dates <- wd$due_dates
      n_students <- if (!is.null(data) && nrow(data) > 0) nrow(data) else 0L

      # --- Build unified row list ---
      all_rows <- list()

      # Canvas assessments
      if (n_students > 0 && "assignments" %in% names(data)) {
        ref <- data$assignments[[1]]
        if (!is.null(ref) && nrow(ref) > 0) {
          for (i in seq_len(nrow(ref))) {
            aname <- ref$name[i]
            n_scored <- sum(vapply(data$assignments, function(a) {
              row <- a[a$name == aname, , drop = FALSE]
              nrow(row) > 0 && !is.na(row$score[1])
            }, logical(1)))

            all_rows[[length(all_rows) + 1]] <- list(
              name = aname,
              source = "Canvas",
              max_points = ref$max_points[i],
              scored = sprintf("%d / %d", n_scored, n_students),
              any_scored = n_scored > 0,
              deletable = FALSE
            )
          }
        }
      }

      # Uploaded assessments
      if (nrow(summary_df) > 0) {
        for (i in seq_len(nrow(summary_df))) {
          row <- summary_df[i, ]
          src <- if (grepl("gradescope", row$source_type, ignore.case = TRUE)) "Gradescope" else "Manual"
          all_rows[[length(all_rows) + 1]] <- list(
            name = row$assessment,
            source = src,
            max_points = row$max_points,
            scored = as.character(row$students_count),
            any_scored = row$students_count > 0,
            deletable = TRUE
          )
        }
      }

      # --- Toolbar buttons ---
      toolbar <- if (editing) {
        tags$div(class = "exams-toolbar-actions",
          tags$button(class = "exams-action-btn exams-action-btn-primary",
            onclick = sprintf(
              "var ws=document.querySelectorAll('.weight-input');var ds=document.querySelectorAll('.due-date-input');var w={};var d={};ws.forEach(function(el){var v=parseFloat(el.value);if(!isNaN(v)&&v>0)w[el.dataset.assessment]=v;});ds.forEach(function(el){if(el.value)d[el.dataset.assessment]=el.value;});Shiny.setInputValue('save_assessment_config',JSON.stringify({weights:w,due_dates:d}),{priority:'event'});Shiny.setInputValue('%s',true,{priority:'event'});",
              ns("toggle_edit_config")
            ),
            "Save"
          ),
          tags$button(class = "exams-action-btn",
            onclick = sprintf(
              "Shiny.setInputValue('%s', true, {priority: 'event'})",
              ns("toggle_edit_config")
            ),
            "Cancel"
          )
        )
      } else {
        tags$div(class = "exams-toolbar-actions",
          tags$button(class = "exams-action-btn",
            onclick = sprintf(
              "Shiny.setInputValue('%s', true, {priority: 'event'})",
              ns("open_wizard")
            ),
            "Upload Scores"
          ),
          tags$button(class = "exams-action-btn",
            onclick = sprintf(
              "Shiny.setInputValue('%s', true, {priority: 'event'})",
              ns("toggle_edit_config")
            ),
            "Edit Weights & Dates"
          )
        )
      }

      if (length(all_rows) == 0) {
        return(tags$div(
          toolbar,
          tags$div(class = "empty-state",
            tags$p("No assessments detected")
          )
        ))
      }

      # --- Render table rows ---
      table_rows <- lapply(all_rows, function(item) {
        aname <- item$name
        status <- compute_assessment_status(due_dates[[aname]], item$any_scored)
        status_label <- tags$span(class = "exam-source-tag",
          if (status == "ongoing") "Ongoing" else if (status == "completed") "Completed" else "Missing"
        )
        row_class <- if (status == "ongoing") "assessment-pending" else ""
        src_tag <- tags$span(class = "exam-source-tag", item$source)

        if (editing) {
          tags$tr(class = row_class,
            tags$td(aname, src_tag),
            tags$td(as.character(item$max_points)),
            tags$td(item$scored),
            tags$td(
              tags$input(type = "text", class = "weight-input", inputmode = "numeric", pattern = "[0-9]*",
                `data-assessment` = aname,
                value = if (!is.null(weights[[aname]])) weights[[aname]] else "",
                placeholder = "\u2014"
              )
            ),
            tags$td(
              tags$input(type = "date", class = "due-date-input",
                `data-assessment` = aname,
                value = if (!is.null(due_dates[[aname]])) due_dates[[aname]] else ""
              )
            ),
            tags$td(status_label),
            tags$td(
              if (item$deletable) {
                tags$button(class = "note-action-btn note-delete-btn",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                    ns("delete_assessment"), aname
                  ),
                  "Delete"
                )
              }
            )
          )
        } else {
          weight_display <- if (!is.null(weights[[aname]])) paste0(weights[[aname]], "%") else "\u2014"
          due_display <- if (!is.null(due_dates[[aname]]) && due_dates[[aname]] != "") {
            tryCatch(format(as.Date(due_dates[[aname]]), "%d %b %Y"), error = function(e) "\u2014")
          } else {
            "\u2014"
          }

          tags$tr(class = row_class,
            tags$td(aname, src_tag),
            tags$td(as.character(item$max_points)),
            tags$td(item$scored),
            tags$td(weight_display),
            tags$td(due_display),
            tags$td(status_label),
            tags$td(
              if (item$deletable) {
                tags$button(class = "note-action-btn note-delete-btn",
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                    ns("delete_assessment"), aname
                  ),
                  "Delete"
                )
              }
            )
          )
        }
      })

      # Weight total footer (edit mode only)
      table_footer <- NULL
      if (editing) {
        table_footer <- tagList(
          tags$tfoot(
            tags$tr(class = "weight-total-row",
              tags$td(style = "text-align: right;", "Total"),
              tags$td(""),
              tags$td(""),
              tags$td(id = "weight-total-display", sprintf("%.0f / 100%%", sum(unlist(weights)))),
              tags$td(colspan = "3", "")
            )
          ),
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
        )
      }

      # --- Upload log ---
      upload_log <- get_upload_log(exam)
      log_section <- NULL
      if (nrow(upload_log) > 0) {
        log_items <- lapply(seq_len(nrow(upload_log)), function(i) {
          row <- upload_log[i, ]
          src_label <- if (grepl("gradescope", row$source_type, ignore.case = TRUE)) "Gradescope" else "Manual"

          # Format timestamp for display
          display_time <- if (row$upload_time != "") {
            tryCatch({
              t <- as.POSIXct(row$upload_time, format = "%Y-%m-%dT%H:%M:%S")
              format(t, "%d %b %Y %H:%M")
            }, error = function(e) row$upload_date)
          } else if (row$upload_date != "") {
            tryCatch(format(as.Date(row$upload_date), "%d %b %Y"), error = function(e) row$upload_date)
          } else ""

          replaced_badge <- if (row$num_replaced > 0) {
            tags$span(class = "upload-log-replaced", paste0("Replaced ", row$num_replaced))
          }

          tags$div(class = "upload-log-item",
            tags$div(class = "upload-log-meta",
              tags$span(class = "upload-log-assessment", row$assessment),
              tags$span(class = "exam-source-tag", src_label),
              replaced_badge,
              tags$span(class = "upload-log-scores", paste0(row$num_scores, " scores"))
            ),
            tags$div(class = "upload-log-meta",
              tags$span(class = "upload-log-file", row$source_file),
              tags$span(class = "upload-log-timestamp", display_time)
            )
          )
        })

        log_section <- tags$div(class = "upload-log-section",
          tags$div(class = "exams-label", "Upload History"),
          log_items
        )
      }

      tags$div(
        toolbar,
        tags$table(class = "detail-table",
          style = "margin: 0 12px; width: calc(100% - 24px);",
          tags$thead(tags$tr(
            tags$th("Assessment"),
            tags$th("Max Points"),
            tags$th("Scored"),
            tags$th("Weight"),
            tags$th("Due Date"),
            tags$th("Status"),
            tags$th("")
          )),
          tags$tbody(table_rows),
          table_footer
        ),
        log_section
      )
    })
  })
}
