# -- extensions_module.R -------------------------------------------
# Special considerations view + SEAMS2 export.

extensionsModuleUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "extensions-view",
    # Toolbar: assessment picker + export button
    tags$div(class = "extensions-toolbar",
      tags$div(class = "extensions-picker",
        tags$span(class = "extensions-label", "Assessment"),
        uiOutput(ns("assessment_picker"))
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

extensionsModuleServer <- function(id, studentData, dataSources, currentUnit) {
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
    overridesLoadedFromDisk <- reactiveVal(FALSE)

    selectedAssessment <- reactive({
      val <- input$assessment_select
      if (is.null(val) || val == "") return(NULL)
      val
    })

    # --- Load overrides from disk when data loads ---

    observe({
      unit <- currentUnit()
      ext <- extensionsFlat()
      canvas <- canvasNames()
      if (is.null(unit) || is.null(ext) || nrow(ext) == 0 || length(canvas) == 0) {
        overridesLoadedFromDisk(FALSE)
        return()
      }

      data_dir <- NIGHTMARE_CONFIG$data$data_dir
      saved <- load_match_overrides(data_dir, unit)
      if (length(saved) == 0) {
        overridesLoadedFromDisk(FALSE)
        matchOverrides(list())
        return()
      }

      sc_names <- unique(ext$assessment_name)
      validated <- validate_match_overrides(saved, sc_names, canvas)
      if (length(validated) > 0) {
        matchOverrides(validated)
        overridesLoadedFromDisk(TRUE)
      } else {
        matchOverrides(list())
        overridesLoadedFromDisk(FALSE)
      }
    })

    # --- Compute match when data or overrides change ---

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

    # --- Render assessment picker (renderUI so it evaluates lazily when visible) ---

    output$assessment_picker <- renderUI({
      mr <- matchResult()
      canvas <- canvasNames()
      ext <- extensionsFlat()

      choices <- c("Select an assessment..." = "")

      if (!is.null(mr) && length(canvas) > 0) {
        matched_canvas <- unique(mr$matched$canvas_name)

        if (length(matched_canvas) > 0 && !is.null(ext) && nrow(ext) > 0) {
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
      }

      # Preserve current selection across re-renders
      current <- isolate(input$assessment_select)
      selected <- if (!is.null(current) && current %in% choices) current else ""

      selectInput(ns("assessment_select"), label = NULL,
                  choices = choices, selected = selected, width = "100%")
    })

    # --- Render summary stats ---

    output$extension_stats <- renderUI({
      assess <- selectedAssessment()
      if (is.null(assess)) return(NULL)

      mr <- matchResult()
      if (is.null(mr)) return(NULL)

      ext <- extensionsFlat()
      stats <- compute_extension_stats(ext, assess, mr)

      avg_display <- if (is.na(stats$avg_days)) "\u2014" else sprintf("%.1f", stats$avg_days)

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

    # --- Render unmatched/override banner ---

    output$unmatched_banner <- renderUI({
      mr <- matchResult()
      if (is.null(mr)) return(NULL)

      overrides <- matchOverrides()
      n_overrides <- length(overrides)
      n_unresolved <- length(mr$unmatched) + length(mr$ambiguous)
      from_disk <- overridesLoadedFromDisk()

      # Nothing to show
      if (n_unresolved == 0 && n_overrides == 0) return(NULL)

      parts <- c()
      if (n_unresolved > 0) {
        parts <- c(parts, paste0(n_unresolved, " assessment(s) could not be matched"))
      }
      if (n_overrides > 0) {
        suffix <- if (from_disk) " (loaded from disk)" else ""
        parts <- c(parts, paste0(n_overrides, " manual match(es) active", suffix))
      }

      # Action label
      action_label <- if (n_unresolved > 0) "Resolve" else "Edit"

      tags$div(class = "extensions-unmatched-banner",
        tags$span(paste(parts, collapse = " | ")),
        actionLink(ns("show_match_modal"), action_label)
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
          tags$th("Ref"),
          tags$th("Student"),
          tags$th("UniKey"),
          tags$th("Due Date"),
          tags$th("Extended To"),
          tags$th("+Days"),
          tags$th("Plan"),
          tags$th("Outcome"),
          tags$th("Status")
        )
      )

      rows <- lapply(seq_len(nrow(tbl)), function(i) {
        row_class <- if (tbl$Status[i] == "Pending") "ext-row-pending" else ""

        status_class <- if (tbl$Status[i] == "Pending") {
          "consids-state-pending"
        } else {
          "consids-state-approved"
        }

        plan_val <- tbl$Plan[i]
        plan_cell <- if (plan_val == "") {
          ""
        } else if (plan_val == "Yes") {
          tags$span(class = "source-tag active", "PLAN")
        } else {
          tags$span(class = "source-tag active", plan_val)
        }

        sid <- tbl$.student_id[i]
        nav_js <- sprintf(
          "Shiny.setInputValue('navigate_to_student','%s',{priority:'event'});Shiny.setInputValue('active_view','student',{priority:'event'});",
          sid
        )

        tags$tr(class = row_class,
          tags$td(tbl$Ref[i]),
          tags$td(tags$span(class = "ext-student-link", onclick = nav_js, tbl$Student[i])),
          tags$td(tags$span(class = "ext-student-link", onclick = nav_js, tbl$UniKey[i])),
          tags$td(tbl$`Due Date`[i]),
          tags$td(tbl$`Extended To`[i]),
          tags$td(tbl$`+Days`[i]),
          tags$td(plan_cell),
          tags$td(tbl$Outcome[i]),
          tags$td(tags$span(class = status_class, tbl$Status[i]))
        )
      })

      body <- tags$tbody(rows)

      tags$table(class = "detail-table", header, body)
    })

    # --- Match disambiguation modal ---

    observeEvent(input$show_match_modal, {
      mr <- matchResult()
      if (is.null(mr)) return()

      overrides <- matchOverrides()
      ambiguous_items <- mr$ambiguous
      unmatched_items <- mr$unmatched
      canvas <- canvasNames()

      modal_content <- tagList()

      # Section 1: Currently matched (manual) — editable existing overrides
      if (length(overrides) > 0) {
        section_items <- tagList()
        for (sc_name in names(overrides)) {
          input_id <- ns(paste0("match_", gsub("[^a-zA-Z0-9]", "_", sc_name)))
          current_val <- overrides[[sc_name]]
          selected <- if (is.na(current_val)) "__skip__" else current_val
          choices <- c(setNames(canvas, canvas), "Skip (no match)" = "__skip__")

          section_items <- tagList(section_items,
            tags$div(class = "match-item",
              tags$div(class = "stat-label",
                sc_name,
                tags$span(class = "source-tag active match-saved-badge", "SAVED")
              ),
              radioButtons(input_id, label = NULL, choices = choices, selected = selected)
            )
          )
        }
        modal_content <- tagList(modal_content,
          tags$div(class = "match-section-header", "CURRENTLY MATCHED (MANUAL)"),
          section_items
        )
      }

      # Section 2: Ambiguous matches (excluding already-overridden)
      new_ambiguous <- ambiguous_items[!names(ambiguous_items) %in% names(overrides)]
      if (length(new_ambiguous) > 0) {
        section_items <- tagList()
        for (sc_name in names(new_ambiguous)) {
          candidates <- new_ambiguous[[sc_name]]
          input_id <- ns(paste0("match_", gsub("[^a-zA-Z0-9]", "_", sc_name)))
          choices <- c(setNames(candidates, candidates), "Skip (no match)" = "__skip__")

          section_items <- tagList(section_items,
            tags$div(class = "match-item",
              tags$div(class = "stat-label", sc_name),
              radioButtons(input_id, label = NULL, choices = choices, selected = "__skip__")
            )
          )
        }
        modal_content <- tagList(modal_content,
          tags$div(class = "match-section-header", "AMBIGUOUS MATCHES"),
          section_items
        )
      }

      # Section 3: Unmatched assessments (excluding already-overridden)
      new_unmatched <- unmatched_items[!unmatched_items %in% names(overrides)]
      if (length(new_unmatched) > 0) {
        section_items <- tagList()
        for (sc_name in new_unmatched) {
          input_id <- ns(paste0("match_", gsub("[^a-zA-Z0-9]", "_", sc_name)))
          choices <- c(setNames(canvas, canvas), "Skip (no match)" = "__skip__")

          section_items <- tagList(section_items,
            tags$div(class = "match-item",
              tags$div(class = "stat-label", sc_name),
              radioButtons(input_id, label = NULL, choices = choices, selected = "__skip__")
            )
          )
        }
        modal_content <- tagList(modal_content,
          tags$div(class = "match-section-header", "UNMATCHED ASSESSMENTS"),
          section_items
        )
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

      # Collect from all sections: existing overrides + new ambiguous + new unmatched
      overrides <- matchOverrides()
      all_names <- unique(c(
        names(overrides),
        names(mr$ambiguous),
        mr$unmatched
      ))

      new_overrides <- list()
      for (sc_name in all_names) {
        input_id <- paste0("match_", gsub("[^a-zA-Z0-9]", "_", sc_name))
        chosen <- input[[input_id]]
        if (!is.null(chosen)) {
          new_overrides[[sc_name]] <- if (chosen == "__skip__") NA else chosen
        }
      }

      # Replace overrides wholesale
      matchOverrides(new_overrides)
      overridesLoadedFromDisk(FALSE)

      # Recompute match result
      ext <- extensionsFlat()
      canvas <- canvasNames()
      sc_names <- unique(ext$assessment_name)
      result <- match_assessments(sc_names, canvas)
      result <- apply_match_overrides(result, new_overrides)
      matchResult(result)

      # Save to disk
      unit <- currentUnit()
      if (!is.null(unit) && length(new_overrides) > 0) {
        data_dir <- NIGHTMARE_CONFIG$data$data_dir
        save_match_overrides(data_dir, unit, new_overrides)
      }

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
