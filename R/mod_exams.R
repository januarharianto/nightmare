#' @keywords internal
# -- exams_module.R ------------------------------------------------
# Exam upload module: UI, reactive state, event handlers, save logic.

examsModuleUI <- function(id) {
  ns <- NS(id)
  tags$div(class = "exams-view",
    uiOutput(ns("wizard_overlay")),
    tags$div(class = "exams-summary-container scroll-container",
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

    # Wizard renderers and summary table
    render_exam_wizard(ns, input, output, wizardOpen, wizardStep,
                        parsedScores, detectedSource, matchPreview,
                        conflicts, assessmentName, maxPoints,
                        examData, studentData, dataSources)

    render_exam_summary(ns, input, output, editingConfig,
                         examData, studentData, dataSources,
                         currentUnit, weightsData)

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

    # --- Column confirmation ---

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

    # --- Naming confirmation ---

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
          # "old" means keep current active_sitting -- no change needed
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
        unit <- currentUnit()
        if (!is.null(unit)) {
          save_exam_data(NIGHTMARE_CONFIG$data$data_dir, unit, exam)
        }
        examData(exam)
        showNotification(paste0("Deleted '", aname, "'"), type = "message")
      }
    })
  })
}
