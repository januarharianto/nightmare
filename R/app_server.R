#' NIGHTMARE Server
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @export
#'
# -- server.R -----------------------------------------------------
# Main server: reactive state, module orchestration, event handling.

app_server <- function(input, output, session) {

  # Reactive values
  studentData <- reactiveVal(data.frame())
  isLoaded <- reactiveVal(FALSE)
  activeView <- reactiveVal("student")
  currentUnit <- reactiveVal(NULL)
  dataSources <- reactiveVal(list(canvas = FALSE, consids = FALSE, plans = FALSE))
  studentNotes <- reactiveVal(list())
  examData <- reactiveVal(list(version = 1L, saved_at = NULL, assessments = list()))
  weightsData <- reactiveVal(list(version = 1L, saved_at = NULL, weights = list()))
  editingWeights <- reactiveVal(FALSE)
  availableFolders <- reactiveVal(character(0))
  dataDir <- reactiveVal({
    saved <- read_settings()$data_dir
    if (!is.null(saved) && dir.exists(saved)) saved else NULL
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

    unit <- currentUnit()
    if (!is.null(unit)) {
      save_weights_data(dataDir(), unit, current)
    }
    TRUE
  }

  # Helper: load unit data (reusable from startup, modal confirm, and unit switcher)
  load_unit_data <- function(unit) {
    data_dir <- dataDir()
    folder_path <- file.path(data_dir, unit)

    tryCatch({
      imported <- load_folder(folder_path, unit_filter = unit)

      dataSources(list(
        canvas = !is.null(imported$canvas),
        consids = !is.null(imported$consids),
        plans = !is.null(imported$plans)
      ))

      if (is.null(imported$canvas)) {
        showNotification("No Canvas gradebook found in folder", type = "error")
        return(FALSE)
      }

      consolidated <- consolidate_student_data(
        imported$canvas, imported$consids, imported$plans
      )

      studentData(consolidated)
      isLoaded(TRUE)
      currentUnit(unit)
      studentNotes(load_student_notes(data_dir, unit))
      examData(load_exam_data(data_dir, unit))
      weightsData(load_weights_data(data_dir, unit))
      save_last_unit(data_dir, unit)

      # Default to first student alphabetically
      if (nrow(consolidated) > 0) {
        sorted <- consolidated[order(consolidated$name), ]
        selectedStudentId(as.character(sorted$student_id[1]))
      }

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
      year <- "\u2014"
    }

    # Extract semester from attribute (set by canvas import)
    semester <- attr(data, "semester")
    if (is.null(semester) || is.na(semester)) {
      # Fallback: try detection function
      semester <- tryCatch(
        detect_semester_from_canvas(data),
        error = function(e) "\u2014"
      )
    }

    list(
      unit = as.character(unit),
      year = as.character(year),
      semester = as.character(semester),
      student_count = nrow(data),
      sources = dataSources()
    )
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
    folders <- availableFolders()
    active <- currentUnit()

    tags$div(
      class = "metadata-panel",
      tags$div(
        class = "metadata-grid",
        # Row 1
        tags$div(
          class = "metadata-item unit-selector",
          tags$span(class = "metadata-label", "Unit:"),
          tags$div(
            class = "metadata-value metadata-value-clickable",
            onclick = "document.getElementById('unit-dropdown').classList.toggle('open')",
            tags$span(meta$unit),
            tags$span(class = "unit-dropdown-indicator", HTML("&#9660;")),
            # Inline dropdown
            tags$div(
              id = "unit-dropdown",
              class = "unit-dropdown",
              lapply(sort(folders), function(f) {
                is_active <- identical(f, active)
                tags$div(
                  class = paste("unit-dropdown-item", if (is_active) "active" else ""),
                  onclick = sprintf(
                    "Shiny.setInputValue('unit_dropdown_select', '%s', {priority: 'event'}); document.getElementById('unit-dropdown').classList.remove('open');",
                    f
                  ),
                  f
                )
              })
            )
          )
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
      ),
      # Close dropdown on outside click (idempotent listener)
      tags$script(HTML("
        if (!window._unitDropdownListener) {
          window._unitDropdownListener = true;
          document.addEventListener('click', function(e) {
            if (!e.target.closest('.unit-selector')) {
              var dd = document.getElementById('unit-dropdown');
              if (dd) dd.classList.remove('open');
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

  # Show folder picker on startup (auto-load last unit if available)
  observe({
    data_dir <- dataDir()
    folders <- scan_data_folders(data_dir)
    availableFolders(folders)
    last_unit <- read_last_unit(data_dir)

    if (length(folders) == 0) {
      showModal(modalDialog(
        title = "No Data Found",
        tags$p("No data folders found. Place your files in a subfolder under ",
               tags$code("data/"), " (e.g. ", tags$code("data/BIOL2022/"), ")."),
        tags$p("Each subfolder should contain Canvas gradebook CSV, special considerations CSV,
               and/or disability plans Excel files."),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      return()
    }

    # Auto-load last unit if valid
    if (!is.null(last_unit) && last_unit %in% folders) {
      load_unit_data(last_unit)
      return()
    }

    # Fallback: show modal to select unit
    showModal(modalDialog(
      title = "Select Unit",
      selectInput("folder_select", "Unit of Study", choices = folders, selected = folders[1]),
      footer = actionButton("folder_confirm", "Load", class = "btn-dark"),
      easyClose = FALSE
    ))
  })

  # Handle folder selection (startup modal)
  observeEvent(input$folder_confirm, {
    removeModal()
    load_unit_data(input$folder_select)
  })

  # Handle unit selection from inline dropdown
  observeEvent(input$unit_dropdown_select, {
    new_unit <- input$unit_dropdown_select
    if (!is.null(new_unit) && new_unit != currentUnit()) {
      studentData(data.frame())
      isLoaded(FALSE)
      load_unit_data(new_unit)
      availableFolders(scan_data_folders(dataDir()))
    }
  })

  # Search module
  selectedStudentId <- searchModuleServer("search", studentData)

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
  extensionsModuleServer("extensions", studentData, dataSources, currentUnit)

  # Assessments module
  assessmentsModuleServer("assessments", studentData, examData, weightsData)

  # Notes module
  notesModuleServer("notes", studentData, studentNotes, currentUnit)

  # Plans module
  plansModuleServer("plans", studentData, dataSources)

  # Exams module
  examsModuleServer("exams", studentData, examData, currentUnit, dataSources, weightsData)

  # Navigate to student from notes feed
  observeEvent(input$navigate_to_student, {
    selectedStudentId(input$navigate_to_student)
  })

  # Save a new note
  observeEvent(input$save_note, {
    req(input$save_note)
    info <- input$save_note
    unit <- currentUnit()
    if (is.null(unit)) return()

    updated <- add_note(studentNotes(), info$student_id, info$category, info$text)
    studentNotes(updated)
    save_student_notes(dataDir(), unit, updated)
  })

  # Delete a note
  observeEvent(input$delete_note, {
    req(input$delete_note)
    info <- input$delete_note
    unit <- currentUnit()
    if (is.null(unit)) return()

    updated <- delete_note(studentNotes(), info$student_id, info$note_id)
    studentNotes(updated)
    save_student_notes(dataDir(), unit, updated)
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
    unit <- currentUnit()
    if (is.null(unit)) return()

    exam <- examData()
    resolutions <- list()
    resolutions[[info$student_id]] <- as.integer(info$sitting_id)
    exam <- resolve_conflicts(exam, info$assessment, resolutions)
    examData(exam)
    save_exam_data(dataDir(), unit, exam)
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
      unit <- currentUnit()
      if (!is.null(unit)) save_weights_data(dataDir(), unit, current)
    }
  })

  observeEvent(input$confirm_edit_note_data, {
    req(input$confirm_edit_note_data)
    info <- input$confirm_edit_note_data
    unit <- currentUnit()
    if (is.null(unit)) return()

    updated <- edit_note(studentNotes(), info$student_id, info$note_id, info$category, info$text)
    studentNotes(updated)
    save_student_notes(dataDir(), unit, updated)
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
