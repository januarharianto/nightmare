# Extensions Module for NIGHTMARE
# Assessment-level view of special consideration extensions

extensionsModuleUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "extensions-view",
    # Toolbar: assessment picker + export button
    tags$div(class = "extensions-toolbar",
      tags$div(class = "extensions-picker",
        tags$span(class = "extensions-label", "Assessment"),
        selectInput(ns("assessment_select"), label = NULL,
                    choices = c("Select an assessment..." = ""), width = "100%")
      ),
      downloadButton(ns("export_seams2"), "Export SEAMS2", class = "btn-export-seams2")
    ),

    # Summary stats bar
    uiOutput(ns("extension_stats")),

    # Unmatched assessments banner
    uiOutput(ns("unmatched_banner")),

    # Extensions table (scrollable)
    tags$div(class = "extensions-table-container",
      uiOutput(ns("extensions_table"))
    ),

    # Match disambiguation modal (hidden by default, rendered by server)
    uiOutput(ns("match_modal"))
  )
}

extensionsModuleServer <- function(id, studentData, dataSources) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactives ---

    extensionsFlat <- reactive({
      data <- studentData()
      if (is.null(data) || nrow(data) == 0) return(data.frame())
      flatten_extensions(data)
    })

    canvasNames <- reactive({
      data <- studentData()
      if (is.null(data) || nrow(data) == 0) return(character())
      get_canvas_assignment_names(data)
    })

    matchResult <- reactiveVal(NULL)
    matchOverrides <- reactiveVal(list())

    selectedAssessment <- reactive({
      val <- input$assessment_select
      if (is.null(val) || val == "") return(NULL)
      val
    })

    # --- Compute initial match when data loads ---

    observe({
      ext <- extensionsFlat()
      canvas <- canvasNames()
      if (is.null(ext) || nrow(ext) == 0 || length(canvas) == 0) {
        matchResult(NULL)
        return()
      }
      sc_names <- unique(ext$assessment_name)
      result <- match_assessments(sc_names, canvas)
      overrides <- matchOverrides()
      if (length(overrides) > 0) {
        result <- apply_match_overrides(result, overrides)
      }
      matchResult(result)
    })

    # --- Update assessment picker ---

    observe({
      mr <- matchResult()
      canvas <- canvasNames()
      ext <- extensionsFlat()
      if (is.null(mr) || length(canvas) == 0) {
        updateSelectInput(session, "assessment_select",
                          choices = c("Select an assessment..." = ""))
        return()
      }

      # Count extensions per canvas assignment
      matched_canvas <- unique(mr$matched$canvas_name)

      # Build choices safely
      choices <- c("Select an assessment..." = "")

      if (length(matched_canvas) > 0 && nrow(ext) > 0) {
        counts <- vapply(matched_canvas, function(cn) {
          sc_names <- mr$matched$spec_cons_name[mr$matched$canvas_name == cn]
          sum(ext$assessment_name %in% sc_names)
        }, integer(1))
        matched_choices <- matched_canvas
        names(matched_choices) <- paste0(matched_canvas, " (", counts, ")")
        choices <- c(choices, matched_choices)
      } else if (length(matched_canvas) > 0) {
        matched_choices <- matched_canvas
        names(matched_choices) <- paste0(matched_canvas, " (0)")
        choices <- c(choices, matched_choices)
      }

      unmatched_canvas <- setdiff(canvas, matched_canvas)
      if (length(unmatched_canvas) > 0) {
        choices <- c(choices, setNames(unmatched_canvas, unmatched_canvas))
      }

      updateSelectInput(session, "assessment_select", choices = choices)
    })

    # --- Render summary stats ---

    output$extension_stats <- renderUI({
      assess <- selectedAssessment()
      if (is.null(assess)) return(NULL)

      mr <- matchResult()
      if (is.null(mr)) return(NULL)

      ext <- extensionsFlat()
      stats <- compute_extension_stats(ext, assess, mr)

      avg_display <- if (is.na(stats$avg_days)) "—" else sprintf("%.1f", stats$avg_days)

      tags$div(class = "extensions-summary",
        tags$div(
          tags$span(class = "stat-label", "TOTAL"),
          tags$span(class = "stat-value", stats$total)
        ),
        tags$div(
          tags$span(class = "stat-label", "APPROVED"),
          tags$span(class = "stat-value", stats$approved)
        ),
        tags$div(
          tags$span(class = "stat-label", "PENDING"),
          tags$span(class = "stat-value", stats$pending)
        ),
        tags$div(
          tags$span(class = "stat-label", "WITH PLAN"),
          tags$span(class = "stat-value", stats$with_plan)
        ),
        tags$div(
          tags$span(class = "stat-label", "AVG DAYS"),
          tags$span(class = "stat-value", avg_display)
        )
      )
    })

    # --- Render unmatched banner ---

    output$unmatched_banner <- renderUI({
      mr <- matchResult()
      if (is.null(mr)) return(NULL)

      n_unresolved <- length(mr$unmatched) + length(mr$ambiguous)
      if (n_unresolved == 0) return(NULL)

      tags$div(class = "extensions-unmatched-banner",
        tags$span(
          paste0(n_unresolved, " assessment(s) could not be matched")
        ),
        actionLink(ns("show_match_modal"), "Resolve")
      )
    })

    # --- Render extensions table ---

    output$extensions_table <- renderUI({
      sources <- dataSources()
      if (!sources$consids) {
        return(tags$div(class = "empty-state",
          tags$p("No special considerations data available")
        ))
      }

      assess <- selectedAssessment()
      if (is.null(assess)) {
        return(tags$div(class = "empty-state",
          tags$p("Select an assessment to view extensions")
        ))
      }

      mr <- matchResult()
      if (is.null(mr)) {
        return(tags$div(class = "empty-state",
          tags$p("No special considerations data available")
        ))
      }

      ext <- extensionsFlat()
      tbl <- build_extensions_table(ext, assess, mr)

      if (nrow(tbl) == 0) {
        return(tags$div(class = "empty-state",
          tags$p("No extensions found for this assessment")
        ))
      }

      # Build HTML table
      header <- tags$thead(
        tags$tr(
          tags$th("Student"),
          tags$th("UniKey"),
          tags$th("Due Date"),
          tags$th("Extended To"),
          tags$th("+Days"),
          tags$th("Outcome"),
          tags$th("Status"),
          tags$th("Plan")
        )
      )

      rows <- lapply(seq_len(nrow(tbl)), function(i) {
        row_class <- if (tbl$Status[i] == "Pending") "ext-row-pending" else ""

        status_class <- if (tbl$Status[i] == "Pending") {
          "consids-state-pending"
        } else {
          "consids-state-approved"
        }

        plan_cell <- if (tbl$Plan[i] == "Yes") {
          tags$span(class = "source-tag active", "PLAN")
        } else {
          ""
        }

        tags$tr(class = row_class,
          tags$td(tbl$Student[i]),
          tags$td(tbl$UniKey[i]),
          tags$td(tbl$`Due Date`[i]),
          tags$td(tbl$`Extended To`[i]),
          tags$td(tbl$`+Days`[i]),
          tags$td(tbl$Outcome[i]),
          tags$td(tags$span(class = status_class, tbl$Status[i])),
          tags$td(plan_cell)
        )
      })

      body <- tags$tbody(rows)

      tags$table(class = "detail-table", header, body)
    })

    # --- Match disambiguation modal ---

    observeEvent(input$show_match_modal, {
      mr <- matchResult()
      if (is.null(mr)) return()

      ambiguous_items <- mr$ambiguous
      unmatched_items <- mr$unmatched
      canvas <- canvasNames()

      modal_content <- tagList()

      # Ambiguous items: show radio buttons with candidates
      if (length(ambiguous_items) > 0) {
        for (sc_name in names(ambiguous_items)) {
          candidates <- ambiguous_items[[sc_name]]
          input_id <- ns(paste0("match_", gsub("[^a-zA-Z0-9]", "_", sc_name)))
          choices <- c(setNames(candidates, candidates), "Skip (no match)" = "__skip__")

          modal_content <- tagList(modal_content,
            tags$div(class = "match-item",
              tags$div(class = "stat-label", sc_name),
              radioButtons(input_id, label = NULL, choices = choices, selected = "__skip__")
            )
          )
        }
      }

      # Unmatched items: show radio buttons with all canvas names
      if (length(unmatched_items) > 0) {
        for (sc_name in unmatched_items) {
          input_id <- ns(paste0("match_", gsub("[^a-zA-Z0-9]", "_", sc_name)))
          choices <- c(setNames(canvas, canvas), "Skip (no match)" = "__skip__")

          modal_content <- tagList(modal_content,
            tags$div(class = "match-item",
              tags$div(class = "stat-label", sc_name),
              radioButtons(input_id, label = NULL, choices = choices, selected = "__skip__")
            )
          )
        }
      }

      showModal(modalDialog(
        title = "Match Assessments",
        modal_content,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_matches"), "Confirm", class = "btn-dark")
        ),
        easyClose = TRUE
      ))
    })

    # --- Handle confirm matches ---

    observeEvent(input$confirm_matches, {
      mr <- matchResult()
      if (is.null(mr)) return()

      all_names <- c(names(mr$ambiguous), mr$unmatched)
      overrides <- list()

      for (sc_name in all_names) {
        input_id <- paste0("match_", gsub("[^a-zA-Z0-9]", "_", sc_name))
        chosen <- input[[input_id]]
        if (!is.null(chosen)) {
          overrides[[sc_name]] <- if (chosen == "__skip__") NA else chosen
        }
      }

      # Merge with existing overrides
      existing <- matchOverrides()
      for (nm in names(overrides)) {
        existing[[nm]] <- overrides[[nm]]
      }
      matchOverrides(existing)

      # Recompute match result
      ext <- extensionsFlat()
      canvas <- canvasNames()
      sc_names <- unique(ext$assessment_name)
      result <- match_assessments(sc_names, canvas)
      result <- apply_match_overrides(result, matchOverrides())
      matchResult(result)

      removeModal()
    })

    # --- SEAMS2 download handler ---

    output$export_seams2 <- downloadHandler(
      filename = function() {
        assess <- selectedAssessment()
        clean_name <- gsub("[^a-zA-Z0-9]+", "_", assess)
        paste0("SEAMS2_", clean_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        assess <- selectedAssessment()
        mr <- matchResult()
        ext <- extensionsFlat()
        display_tbl <- build_extensions_table(ext, assess, mr)
        export <- build_seams2_export(display_tbl, assess)
        write.csv(export, file, row.names = FALSE, na = "")
      }
    )

    # --- Toggle export button ---

    observe({
      assess <- selectedAssessment()
      mr <- matchResult()
      ext <- extensionsFlat()

      has_approved <- FALSE
      if (!is.null(assess) && !is.null(mr) && nrow(ext) > 0) {
        tbl <- build_extensions_table(ext, assess, mr)
        has_approved <- any(tbl$.approved, na.rm = TRUE)
      }

      shinyjs::toggleState("export_seams2", condition = has_approved)
    })
  })
}
