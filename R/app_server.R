#' NIGHTMARE Server
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @export
#'
# -- server.R -----------------------------------------------------
# Main server: reactive state, module orchestration, event handling.

offering_by_id <- function(offerings, offering_id) {
  if (is.null(offerings) || nrow(offerings) == 0L || is.null(offering_id)) return(NULL)
  idx <- match(offering_id, offerings$offering_id)
  if (is.na(idx)) return(NULL)
  offerings[idx, , drop = FALSE]
}

offering_year_filter <- function(offering) {
  if (is.null(offering) || nrow(offering) == 0L || is.na(offering$year[[1]])) return(NULL)
  as.character(offering$year[[1]])
}

app_server <- function(input, output, session) {

  # Reactive values
  studentData <- reactiveVal(data.frame())
  isLoaded <- reactiveVal(FALSE)
  activeView <- reactiveVal("student")
  currentUnit <- reactiveVal(NULL)
  currentOffering <- reactiveVal(NULL)
  selectedStudentId <- reactiveVal(NULL)
  dataSources <- reactiveVal(list(canvas = FALSE, consids = FALSE, plans = FALSE))
  studentNotes <- reactiveVal(list())
  examData <- reactiveVal(list(version = 1L, saved_at = NULL, assessments = list()))
  weightsData <- reactiveVal(list(version = 1L, saved_at = NULL, weights = list()))
  canvasRefreshStatus <- reactiveVal(list(state = "idle", message = NULL, metadata = NULL))
  editingWeights <- reactiveVal(FALSE)
  availableFolders <- reactiveVal(character(0))
  availableOfferings <- reactiveVal(empty_offerings())
  dataDir <- reactiveVal({
    saved <- read_settings()$data_dir
    if (!is.null(saved) && dir.exists(saved)) saved else NULL
  })
  currentOfferingPath <- reactive({
    offering <- currentOffering()
    if (is.null(offering) || nrow(offering) == 0L) return(NULL)
    offering$path[[1]]
  })

  validate_and_save_weights <- function(new_weights, new_due_dates = NULL,
                                         toggle_editing = FALSE) {
    total <- sum(unlist(new_weights), na.rm = TRUE)
    if (total > 100) {
      showNotification(sprintf("Weights total %.0f%% exceeds 100%%. Please adjust.", total),
                       type = "warning")
      return(FALSE)
    }

    current <- weightsData()
    current$weights <- new_weights
    if (!is.null(new_due_dates)) current$due_dates <- new_due_dates
    weightsData(current)
    if (toggle_editing) editingWeights(FALSE)

    offering_path <- currentOfferingPath()
    if (!is.null(offering_path)) {
      save_weights_data_for_path(offering_path, current)
    }
    TRUE
  }

  # Open native OS folder picker (Finder on macOS, native on Windows, zenity on Linux).
  choose_directory <- function() {
    if (Sys.info()["sysname"] == "Darwin") {
      path <- system('osascript -e "POSIX path of (choose folder)"', intern = TRUE)
    } else if (.Platform$OS.type == "windows") {
      path <- utils::choose.dir()
    } else {
      path <- system("zenity --file-selection --directory 2>/dev/null", intern = TRUE)
    }
    if (length(path) > 0 && !is.na(path) && nchar(trimws(path)) > 0) trimws(path) else NULL
  }

  observeEvent(input$browse_data_dir, {
    path <- choose_directory()
    if (!is.null(path)) {
      abs_path <- normalizePath(path)
      save_settings(list(data_dir = abs_path))
      removeModal()
      dataDir(abs_path)
    }
  })

  show_data_dir_modal <- function(current_dir = NULL, allow_cancel = FALSE) {
    footer_btns <- if (allow_cancel) {
      tagList(modalButton("Cancel"), actionButton("browse_data_dir", "Browse\u2026", class = "btn-dark"))
    } else {
      actionButton("browse_data_dir", "Browse\u2026", class = "btn-dark")
    }
    showModal(modalDialog(
      title = "Select Data Directory",
      tags$p("Choose the folder containing your unit data subfolders."),
      if (!is.null(current_dir)) {
        tags$p(class = "metadata-value", style = "padding: 8px; background: #F5F5F5;",
               tags$span(class = "metadata-label", "Current: "), current_dir)
      },
      footer = footer_btns,
      easyClose = allow_cancel
    ))
  }

  observeEvent(input$change_data_dir, {
    show_data_dir_modal(dataDir(), allow_cancel = TRUE)
  })

  show_canvas_config_modal <- function(offering) {
    cfg <- offering_canvas_config(offering)
    showModal(modalDialog(
      title = "Canvas Sync",
      tags$p("Store the Canvas API token in your system keychain and map this unit to a Canvas course."),
      textInput("canvas_base_url", "Canvas URL", value = cfg$base_url,
                placeholder = "https://canvas.sydney.edu.au"),
      textInput("canvas_course_id", "Canvas course ID", value = cfg$course_id),
      passwordInput("canvas_token", "API token (stored in Keychain)", value = ""),
      tags$p(class = "canvas-config-note",
        "Leave the token blank to keep the existing keychain token."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_canvas_config", "Save", class = "btn-dark")
      ),
      easyClose = TRUE
    ))
  }

  weights_with_canvas_due_dates <- function(current, canvas_data) {
    due_dates <- attr(canvas_data, "due_dates")
    if (is.null(current)) current <- list(version = 1L, saved_at = NULL, weights = list(), due_dates = list())
    if (is.null(due_dates) || length(due_dates) == 0) {
      return(list(weights = current, changed = FALSE))
    }

    existing <- current$due_dates
    if (is.null(existing)) existing <- list()
    current$due_dates <- modifyList(existing, due_dates)
    list(weights = current, changed = TRUE)
  }

  apply_canvas_due_dates <- function(canvas_data, unit) {
    offering_path <- isolate(currentOfferingPath())
    merged <- weights_with_canvas_due_dates(isolate(weightsData()), canvas_data)
    if (!isTRUE(merged$changed)) return(invisible(FALSE))

    current <- merged$weights
    weightsData(current)
    if (!is.null(offering_path)) save_weights_data_for_path(offering_path, current)
    invisible(TRUE)
  }

  update_loaded_data <- function(imported, unit) {
    if (is.null(imported$canvas)) {
      showNotification("No Canvas gradebook found in folder", type = "error")
      return(FALSE)
    }

    consolidated <- suppressMessages(consolidate_student_data(
      imported$canvas, imported$consids, imported$plans
    ))

    dataSources(list(
      canvas = !is.null(imported$canvas),
      consids = !is.null(imported$consids),
      plans = !is.null(imported$plans)
    ))

    studentData(consolidated)
    isLoaded(TRUE)
    currentUnit(unit)

    if (nrow(consolidated) > 0) {
      sorted <- consolidated[order(consolidated$name), ]
      selectedStudentId(as.character(sorted$student_id[1]))
    }

    TRUE
  }

  # Helper: load offering data (reusable from startup, modal confirm, and switchers)
  load_offering_data <- function(offering) {
    data_dir <- dataDir()
    if (is.null(data_dir) || is.null(offering) || nrow(offering) == 0L) return(FALSE)
    folder_path <- offering$path[[1]]
    unit <- offering$unit[[1]]
    year_filter <- offering_year_filter(offering)

    tryCatch({
      imported <- suppressMessages(load_folder(folder_path, unit_filter = unit, year_filter = year_filter))
      loaded_notes <- load_notes_data_for_path(folder_path)
      loaded_exams <- load_exam_data_for_path(folder_path)
      loaded_weights <- load_weights_data_for_path(folder_path)
      merged_weights <- weights_with_canvas_due_dates(loaded_weights, imported$canvas)

      if (!update_loaded_data(imported, unit)) {
        return(FALSE)
      }

      studentNotes(loaded_notes)
      examData(loaded_exams)
      weightsData(merged_weights$weights)
      if (isTRUE(merged_weights$changed)) {
        save_weights_data_for_path(folder_path, merged_weights$weights)
      }
      currentOffering(offering)
      save_last_offering(data_dir, offering$offering_id[[1]])
      save_last_unit(data_dir, unit)

      snapshot_meta <- canvas_refresh_status(folder_path)
      canvasRefreshStatus(list(
        state = "idle",
        message = NULL,
        metadata = snapshot_meta
      ))

      return(TRUE)
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error"
      )
      return(FALSE)
    })
  }

  # Reset app state on every session start
  observe({
    shinyjs::runjs("localStorage.clear(); sessionStorage.clear();")
  }) |> bindEvent(TRUE, once = TRUE)

  # Handle view navigation
  observeEvent(input$active_view, {
    activeView(input$active_view)
    # Update sidebar active state
    shinyjs::runjs(sprintf("
      document.querySelectorAll('.sidebar-item').forEach(item => {
        item.classList.remove('active');
      });
      document.querySelector('[data-view=\"%s\"]').classList.add('active');
    ", input$active_view))
  })

  # Dataset metadata reactive
  datasetMetadata <- reactive({
    data <- studentData()

    # Return placeholder if no data
    if (is.null(data) || nrow(data) == 0) {
      return(list(
        unit = "\u2014",
        year = "\u2014",
        semester = "\u2014",
        student_count = "\u2014",
        sources = list(canvas = FALSE, consids = FALSE, plans = FALSE)
      ))
    }

    # Extract unit code
    unit <- if ("unit_of_study" %in% names(data) && nrow(data) > 0) {
      unique(data$unit_of_study)[1]
    } else {
      "\u2014"
    }

    # Extract year from attribute
    year <- attr(data, "academic_year")
    if (is.null(year) || is.na(year)) {
      offering <- currentOffering()
      if (!is.null(offering) && !is.na(offering$year[[1]])) {
        year <- offering$year[[1]]
      } else {
        year <- "\u2014"
      }
    }

    # Extract semester from attribute (set by canvas import)
    semester <- attr(data, "semester")
    if (is.null(semester) || is.na(semester)) {
      # Fallback: try detection function
      semester <- tryCatch(
        detect_semester_from_canvas(data),
        error = function(e) "\u2014"
      )
      if ((is.null(semester) || is.na(semester) || identical(semester, "\u2014")) &&
          !is.null(currentOffering()) && !is.na(currentOffering()$semester[[1]])) {
        semester <- currentOffering()$semester[[1]]
      }
    }

    list(
      unit = as.character(unit),
      year = as.character(year),
      semester = as.character(semester),
      student_count = nrow(data),
      sources = dataSources()
    )
  })

  canvasStatusLabel <- reactive({
    offering <- currentOffering()
    if (is.null(offering)) return("Not configured")
    cfg <- offering_canvas_config(offering)
    status <- canvasRefreshStatus()

    if (identical(status$state, "syncing")) return("Refreshing...")
    if (identical(status$state, "error") && !is.null(status$message)) return(status$message)
    if (is.null(cfg$base_url) || !nzchar(cfg$base_url) ||
        is.null(cfg$course_id) || !nzchar(cfg$course_id)) {
      return("Not configured")
    }

    meta <- status$metadata
    if (!is.null(meta) && !is.null(meta$fetched_at)) {
      return(paste("Updated", meta$fetched_at))
    }

    "Configured"
  })

  # Render main content (switches between views)
  output$main_content_output <- renderUI({
    if (!isLoaded()) {
      return(NULL)
    }

    if (activeView() == "student") {
      # Student view with search and detail panels
      tags$div(
        class = "main-container",
        searchModuleUI("search"),
        tags$div(class = "student-content", uiOutput("student_detail_panel"))
      )
    } else if (activeView() == "extensions") {
      tags$div(
        class = "main-container",
        extensionsModuleUI("extensions")
      )
    } else if (activeView() == "assessments") {
      tags$div(
        class = "main-container",
        assessmentsModuleUI("assessments")
      )
    } else if (activeView() == "notes") {
      tags$div(
        class = "main-container",
        notesModuleUI("notes")
      )
    } else if (activeView() == "plans") {
      tags$div(
        class = "main-container",
        plansModuleUI("plans")
      )
    } else if (activeView() == "exams") {
      tags$div(
        class = "main-container",
        examsModuleUI("exams")
      )
    }
  })

  # Render dataset metadata panel
  output$dataset_metadata_panel <- renderUI({
    # Only render metadata panel after data is loaded
    if (!isLoaded()) {
      return(NULL)
    }

    meta <- datasetMetadata()
    offerings <- availableOfferings()
    active_offering <- currentOffering()
    active_unit <- currentUnit()
    cfg <- offering_canvas_config(active_offering)
    configured <- nzchar(cfg$base_url) && nzchar(cfg$course_id)
    syncing <- identical(canvasRefreshStatus()$state, "syncing")
    refresh_button <- actionButton(
      "refresh_canvas",
      if (syncing) "Refreshing" else "Refresh",
      class = "canvas-refresh-btn"
    )
    if (!configured || syncing) {
      refresh_button <- shinyjs::disabled(refresh_button)
    }

    units <- sort(unique(offerings$unit))
    unit_control <- if (length(units) > 1L) {
      tags$div(
        class = "metadata-value metadata-value-clickable metadata-value-static",
        onclick = "event.stopPropagation(); document.getElementById('unit-dropdown').classList.toggle('open')",
        tags$span(active_unit %||% meta$unit),
        tags$span(class = "unit-dropdown-indicator", HTML("&#9660;")),
        tags$div(
          id = "unit-dropdown",
          class = "metadata-dropdown unit-dropdown",
          lapply(units, function(unit) {
            is_active <- identical(unit, active_unit)
            tags$div(
              class = paste("metadata-dropdown-item unit-dropdown-item", if (is_active) "active" else ""),
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('unit_dropdown_select', %s, {priority: 'event'}); document.getElementById('unit-dropdown').classList.remove('open');",
                jsonlite::toJSON(unit, auto_unbox = TRUE)
              ),
              unit
            )
          })
        )
      )
    } else {
      tags$span(class = "metadata-value metadata-value-static", active_unit %||% meta$unit)
    }

    unit_offerings <- offerings[offerings$unit == active_unit, , drop = FALSE]
    offering_label <- if (!is.null(active_offering) && nrow(active_offering) > 0L) {
      active_offering$label[[1]]
    } else {
      "\u2014"
    }
    offering_control <- if (nrow(unit_offerings) > 1L) {
      tags$div(
        class = "metadata-value metadata-value-clickable metadata-value-offering",
        onclick = "event.stopPropagation(); document.getElementById('offering-dropdown').classList.toggle('open')",
        tags$span(offering_label),
        tags$span(class = "unit-dropdown-indicator", HTML("&#9660;")),
        tags$div(
          id = "offering-dropdown",
          class = "metadata-dropdown unit-dropdown",
          lapply(seq_len(nrow(unit_offerings)), function(i) {
            offering <- unit_offerings[i, , drop = FALSE]
            is_active <- !is.null(active_offering) &&
              identical(offering$offering_id[[1]], active_offering$offering_id[[1]])
            tags$div(
              class = paste("metadata-dropdown-item unit-dropdown-item", if (is_active) "active" else ""),
              onclick = sprintf(
                "event.stopPropagation(); Shiny.setInputValue('offering_dropdown_select', %s, {priority: 'event'}); document.getElementById('offering-dropdown').classList.remove('open');",
                jsonlite::toJSON(offering$offering_id[[1]], auto_unbox = TRUE)
              ),
              offering$label[[1]]
            )
          })
        )
      )
    } else {
      tags$span(class = "metadata-value metadata-value-static metadata-value-offering", offering_label)
    }

    tags$div(
      class = "metadata-panel",
      tags$div(
        class = "metadata-grid",
        # Row 1
        tags$div(
          class = "metadata-item unit-selector",
          tags$span(class = "metadata-label", "Unit:"),
          unit_control
        ),
        tags$div(
          class = "metadata-item offering-selector",
          tags$span(class = "metadata-label", "Offering:"),
          offering_control
        ),
        tags$div(
          class = "metadata-item",
          tags$span(class = "metadata-label", "Year:"),
          tags$span(class = "metadata-value", meta$year)
        ),
        # Row 2
        tags$div(
          class = "metadata-item",
          tags$span(class = "metadata-label", "Students:"),
          tags$span(
            class = "metadata-value",
            if (meta$student_count == "\u2014") "\u2014" else as.character(meta$student_count)
          )
        ),
        tags$div(
          class = "metadata-item",
          tags$span(class = "metadata-label", "Semester:"),
          tags$span(class = "metadata-value", meta$semester)
        ),
        tags$div(
          class = "metadata-item canvas-sync-item",
          tags$span(class = "metadata-label", "Canvas:"),
          tags$span(
            class = paste(
              "canvas-sync-status",
              if (configured) "configured" else "missing",
              if (identical(canvasRefreshStatus()$state, "error")) "error" else ""
            ),
            canvasStatusLabel()
          ),
          refresh_button,
          actionButton("configure_canvas", "Configure", class = "canvas-configure-btn")
        )
      ),
      # Close dropdown on outside click (idempotent listener)
      tags$script(HTML("
        if (!window._unitDropdownListener) {
          window._unitDropdownListener = true;
          document.addEventListener('click', function(e) {
            if (!e.target.closest('.unit-selector') && !e.target.closest('.offering-selector')) {
              ['unit-dropdown', 'offering-dropdown'].forEach(function(id) {
                var dd = document.getElementById(id);
                if (dd) dd.classList.remove('open');
              });
            }
          });
        }
      "))
    )
  })

  # Sidebar data source indicators
  output$sidebar_sources <- renderUI({
    if (!isLoaded()) return(NULL)
    sources <- dataSources()
    tags$div(
      class = "sidebar-sources",
      tags$div(class = "sidebar-sources-label meta-label", "Sources"),
      tags$div(
        class = paste("source-tag", if (sources$canvas) "active" else "inactive"),
        "Canvas"
      ),
      tags$div(
        class = paste("source-tag", if (sources$consids) "active" else "inactive"),
        "Spec Cons"
      ),
      tags$div(
        class = paste("source-tag", if (sources$plans) "active" else "inactive"),
        "Plans"
      )
    )
  })

  # Startup: load data or prompt for directory
  observe({
    dir <- dataDir()

    # Reset state when directory changes (prevents stale data)
    studentData(data.frame())
    isLoaded(FALSE)
    currentUnit(NULL)
    currentOffering(NULL)
    studentNotes(list())
    examData(list(version = 1L, saved_at = NULL, assessments = list()))
    weightsData(list(version = 1L, saved_at = NULL, weights = list(), due_dates = list()))
    availableFolders(character(0))
    availableOfferings(empty_offerings())

    # Branch 1: No saved directory — show directory picker
    if (is.null(dir)) {
      show_data_dir_modal()
      return()
    }

    # Branch 2 & 3: Directory exists — scan for offerings
    offerings <- scan_data_offerings(dir)
    availableOfferings(offerings)
    availableFolders(sort(unique(offerings$unit)))

    if (nrow(offerings) == 0) {
      # Branch 2: Directory exists but has no unit folders
      showModal(modalDialog(
        title = "No Data Found",
        tags$p("No data folders found in ", tags$code(dir), "."),
        tags$p("Each subfolder should contain Canvas gradebook CSV, special considerations CSV,
               and/or disability plans Excel files."),
        footer = tagList(
          actionButton("change_dir_from_empty", "Change Directory", class = "btn-dark"),
          modalButton("OK")
        ),
        easyClose = TRUE
      ))
      return()
    }

    # Branch 3: Valid directory with offerings
    selected <- resolve_saved_offering(dir, offerings)
    if (!is.null(selected)) {
      isolate(load_offering_data(selected))
      return()
    }

    choices <- stats::setNames(
      offerings$offering_id,
      paste(offerings$unit, offerings$label, sep = " - ")
    )
    showModal(modalDialog(
      title = "Select Offering",
      selectInput("folder_select", "Unit + Offering", choices = choices, selected = offerings$offering_id[[1]]),
      footer = actionButton("folder_confirm", "Load", class = "btn-dark"),
      easyClose = FALSE
    ))
  })

  observeEvent(input$change_dir_from_empty, {
    removeModal()
    show_data_dir_modal(dataDir())
  })

  # Handle folder selection (startup modal)
  observeEvent(input$folder_confirm, {
    removeModal()
    offering <- offering_by_id(availableOfferings(), input$folder_select)
    load_offering_data(offering)
  })

  # Handle unit selection from inline dropdown
  observeEvent(input$unit_dropdown_select, {
    new_unit <- input$unit_dropdown_select
    if (!is.null(new_unit) && new_unit != currentUnit()) {
      studentData(data.frame())
      isLoaded(FALSE)
      offering <- newest_offering_for_unit(availableOfferings(), new_unit)
      load_offering_data(offering)
    }
  })

  observeEvent(input$offering_dropdown_select, {
    offering_id <- input$offering_dropdown_select
    active_offering <- currentOffering()
    if (!is.null(offering_id) &&
        (is.null(active_offering) || !identical(offering_id, active_offering$offering_id[[1]]))) {
      studentData(data.frame())
      isLoaded(FALSE)
      offering <- offering_by_id(availableOfferings(), offering_id)
      load_offering_data(offering)
    }
  })

  observeEvent(input$configure_canvas, {
    offering <- currentOffering()
    if (is.null(offering)) return()
    show_canvas_config_modal(offering)
  })

  observeEvent(input$save_canvas_config, {
    offering <- currentOffering()
    if (is.null(offering)) return()

    base_url <- normalize_canvas_base_url(input$canvas_base_url)
    course_id <- trimws(input$canvas_course_id)
    token <- input$canvas_token

    if (!nzchar(base_url) || !nzchar(course_id)) {
      showNotification("Canvas URL and course ID are required.", type = "warning")
      return()
    }

    save_canvas_course_config(
      offering_id = offering$offering_id[[1]],
      base_url = base_url,
      course_id = course_id
    )

    if (!is.null(token) && nzchar(token)) {
      tryCatch({
        canvas_store_token(base_url, token)
        showNotification("Canvas settings saved. Token stored in Keychain.", type = "message")
      }, error = function(e) {
        showNotification(e$message, type = "error")
      })
    } else {
      showNotification("Canvas settings saved. Existing token unchanged.", type = "message")
    }

    current_status <- canvasRefreshStatus()
    canvasRefreshStatus(list(state = "idle", message = NULL, metadata = current_status$metadata))
    removeModal()
  })

  observeEvent(input$refresh_canvas, {
    offering <- currentOffering()
    if (is.null(offering)) return()
    unit <- offering$unit[[1]]
    folder_path <- currentOfferingPath()
    if (is.null(folder_path)) return()

    cfg <- offering_canvas_config(offering)
    if (!nzchar(cfg$base_url) || !nzchar(cfg$course_id)) {
      show_canvas_config_modal(offering)
      return()
    }

    canvasRefreshStatus(list(state = "syncing", message = "Refreshing...", metadata = canvasRefreshStatus()$metadata))

    tryCatch({
      token <- canvas_get_token(cfg$base_url)
      fetched <- fetch_canvas_gradebook(
        base_url = cfg$base_url,
        course_id = cfg$course_id,
        token = token,
        unit = unit
      )
      if (!is.na(offering$year[[1]])) {
        fetched$metadata$academic_year <- offering$year[[1]]
      }
      if (!is.na(offering$semester[[1]])) {
        fetched$metadata$semester <- offering$semester[[1]]
      }
      save_canvas_api_snapshot(folder_path, fetched$canvas, fetched$metadata)

      imported <- suppressMessages(load_folder(
        folder_path,
        unit_filter = unit,
        year_filter = offering_year_filter(offering)
      ))
      if (!update_loaded_data(imported, unit)) {
        stop("Canvas refresh completed, but the refreshed gradebook could not be loaded.", call. = FALSE)
      }
      apply_canvas_due_dates(imported$canvas, unit)

      canvasRefreshStatus(list(
        state = "ok",
        message = NULL,
        metadata = fetched$metadata
      ))
      showNotification(
        sprintf("Canvas refreshed: %d students, %d assignments.",
                fetched$metadata$student_count, fetched$metadata$assignment_count),
        type = "message"
      )
    }, error = function(e) {
      canvasRefreshStatus(list(
        state = "error",
        message = "Refresh failed",
        metadata = canvasRefreshStatus()$metadata
      ))
      showNotification(paste("Canvas refresh failed:", e$message), type = "error")
    })
  })

  # Search module
  searchModuleServer("search", studentData, selectedStudentId)

  selectedStudent <- reactive({
    sid <- selectedStudentId()
    if (is.null(sid)) return(NULL)
    data <- studentData()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    student <- data[data$student_id == sid, ]
    if (nrow(student) == 0) return(NULL)
    student[1, ]
  })

  # Extensions module
  extensionsModuleServer("extensions", studentData, dataSources, currentUnit, dataDir = dataDir, weightsData = weightsData)

  # Assessments module
  assessmentsModuleServer("assessments", studentData, examData, weightsData)

  # Notes module
  notesModuleServer("notes", studentData, studentNotes, currentUnit)

  # Plans module
  plansModuleServer("plans", studentData, dataSources)

  # Exams module
  examsModuleServer(
    "exams", studentData, examData, currentUnit, dataSources,
    weightsData, dataDir = dataDir, currentOfferingPath = currentOfferingPath
  )

  # Navigate to student from notes feed
  observeEvent(input$navigate_to_student, {
    selectedStudentId(input$navigate_to_student)
  })

  # Save a new note
  observeEvent(input$save_note, {
    req(input$save_note)
    info <- input$save_note
    offering_path <- currentOfferingPath()
    if (is.null(offering_path)) return()

    updated <- add_note(studentNotes(), info$student_id, info$category, info$text)
    studentNotes(updated)
    save_notes_data_for_path(offering_path, updated)
  })

  # Delete a note
  observeEvent(input$delete_note, {
    req(input$delete_note)
    info <- input$delete_note
    offering_path <- currentOfferingPath()
    if (is.null(offering_path)) return()

    updated <- delete_note(studentNotes(), info$student_id, info$note_id)
    studentNotes(updated)
    save_notes_data_for_path(offering_path, updated)
  })

  # Edit a note -- show modal with pre-filled values
  observeEvent(input$edit_note, {
    req(input$edit_note)
    info <- input$edit_note
    notes <- studentNotes()
    sid <- as.character(info$student_id)
    note <- NULL
    if (!is.null(notes[[sid]])) {
      for (n in notes[[sid]]) {
        if (identical(n$id, info$note_id)) { note <- n; break }
      }
    }
    if (is.null(note)) return()

    # Build tag buttons for modal
    modal_tag_buttons <- lapply(names(NOTE_TAGS), function(tag_key) {
      tag_info <- NOTE_TAGS[[tag_key]]
      sel_class <- if (identical(tag_key, note$category)) " selected" else ""
      tags$button(
        class = paste0("notes-tag-btn", sel_class),
        `data-tag` = tag_key,
        `data-description` = tag_info$description,
        type = "button",
        onclick = sprintf(
          "document.querySelectorAll('#edit-note-modal .notes-tag-btn').forEach(function(b){b.classList.remove('selected')});this.classList.add('selected');document.querySelector('#edit-note-modal .notes-tag-description').textContent=this.dataset.description;"
        ),
        tag_info$label
      )
    })

    showModal(modalDialog(
      id = "edit-note-modal",
      title = "Edit Note",
      tags$div(
        id = "edit-note-modal",
        tags$div(class = "notes-tag-selector", modal_tag_buttons),
        tags$div(class = "notes-tag-description",
          if (!is.null(NOTE_TAGS[[note$category]])) NOTE_TAGS[[note$category]]$description else ""
        ),
        tags$textarea(
          id = "edit_note_text",
          class = "notes-textarea",
          rows = "4",
          style = "width:100%; margin-top:8px;",
          note$text
        ),
        tags$input(type = "hidden", id = "edit_note_sid", value = sid),
        tags$input(type = "hidden", id = "edit_note_id", value = note$id)
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_note", "Save", class = "btn-dark")
      ),
      easyClose = TRUE
    ))
  })

  # Confirm edit note
  observeEvent(input$confirm_edit_note, {
    # Read values from modal via JS
    shinyjs::runjs("
      var sel = document.querySelector('#edit-note-modal .notes-tag-btn.selected');
      var cat = sel ? sel.dataset.tag : 'general';
      var text = document.getElementById('edit_note_text').value;
      var sid = document.getElementById('edit_note_sid').value;
      var nid = document.getElementById('edit_note_id').value;
      Shiny.setInputValue('confirm_edit_note_data', {student_id: sid, note_id: nid, category: cat, text: text}, {priority: 'event'});
    ")
  })

  # Handle exam sitting change from student detail view
  observeEvent(input$exam_sitting_change, {
    req(input$exam_sitting_change)
    info <- input$exam_sitting_change
    offering_path <- currentOfferingPath()
    if (is.null(offering_path)) return()

    exam <- examData()
    resolutions <- list()
    resolutions[[info$student_id]] <- as.integer(info$sitting_id)
    exam <- resolve_conflicts(exam, info$assessment, resolutions)
    examData(exam)
    save_exam_data_for_path(offering_path, exam)
  })

  # Toggle weight editing mode
  observeEvent(input$toggle_edit_weights, {
    editingWeights(!editingWeights())
  })

  # Save weights from client-side JSON (student detail view)
  observeEvent(input$save_weights, {
    req(input$save_weights)
    weights_list <- fromJSON(input$save_weights, simplifyVector = FALSE)
    weights_list <- lapply(weights_list, as.numeric)
    validate_and_save_weights(weights_list, toggle_editing = TRUE)
  })

  # Save assessment config from Assessments tab (weights + due dates)
  observeEvent(input$save_assessment_config, {
    req(input$save_assessment_config)
    config <- fromJSON(input$save_assessment_config, simplifyVector = FALSE)
    new_due_dates <- config$due_dates
    if (!is.null(config$weights)) {
      new_weights <- lapply(config$weights, as.numeric)
      validate_and_save_weights(new_weights, new_due_dates)
    } else if (!is.null(new_due_dates)) {
      current <- weightsData()
      current$due_dates <- new_due_dates
      weightsData(current)
      offering_path <- currentOfferingPath()
      if (!is.null(offering_path)) save_weights_data_for_path(offering_path, current)
    }
  })

  observeEvent(input$confirm_edit_note_data, {
    req(input$confirm_edit_note_data)
    info <- input$confirm_edit_note_data
    offering_path <- currentOfferingPath()
    if (is.null(offering_path)) return()

    updated <- edit_note(studentNotes(), info$student_id, info$note_id, info$category, info$text)
    studentNotes(updated)
    save_notes_data_for_path(offering_path, updated)
    removeModal()
  })

  # Student detail: orchestrator (banner + card placeholders)
  output$student_detail_panel <- renderUI({
    if (!isLoaded()) return(NULL)
    student <- selectedStudent()
    if (is.null(student)) {
      return(div(class = "empty-state",
        tags$p("Select a student from the list to view details")))
    }
    build_student_detail_banner(student, studentData())
  })

  # Individual card renderUI outputs
  output$card_assessments <- renderUI({
    student <- selectedStudent()
    if (is.null(student)) return(NULL)
    build_assessments_card(student, studentData(), examData(), weightsData(), editingWeights())
  })

  output$card_consids <- renderUI({
    student <- selectedStudent()
    if (is.null(student)) return(NULL)
    build_consids_card(student)
  })

  output$card_plans <- renderUI({
    student <- selectedStudent()
    if (is.null(student)) return(NULL)
    build_plans_card(student)
  })

  output$card_notes <- renderUI({
    student <- selectedStudent()
    if (is.null(student)) return(NULL)
    notes_for_student <- studentNotes()[[as.character(student$student_id)]] %||% list()
    build_notes_card(student, notes_for_student)
  })

}
