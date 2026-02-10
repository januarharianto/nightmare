# -- exams_summary.R -----------------------------------------------
# Summary table and config editing for exams. Sourced inside examsModuleServer()
# with local = TRUE — has access to: ns, input, output, session,
# examData(), studentData(), dataSources(), currentUnit(), weightsData(),
# editingConfig()

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
      tags$button(class = "exams-action-btn btn-secondary",
        onclick = sprintf(
          "Shiny.setInputValue('%s', true, {priority: 'event'})",
          ns("toggle_edit_config")
        ),
        "Cancel"
      )
    )
  } else {
    tags$div(class = "exams-toolbar-actions",
      tags$button(class = "exams-action-btn btn-secondary",
        onclick = sprintf(
          "Shiny.setInputValue('%s', true, {priority: 'event'})",
          ns("open_wizard")
        ),
        "Upload Scores"
      ),
      tags$button(class = "exams-action-btn btn-secondary",
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
      tags$div(class = "exams-label meta-label", "Upload History"),
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
