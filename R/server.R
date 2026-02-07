# Server Logic for NIGHTMARE

source("R/utils/import/canvas.R")
source("R/utils/import/special_consids.R")
source("R/utils/import/disability_plans.R")
source("R/utils/import/consolidate.R")
source("R/utils/import/file_detection.R")
source("R/utils/import/folder_loader.R")
source("R/utils/ui_helpers.R")
source("R/utils/extensions_data.R")
source("R/utils/assessments_data.R")
source("R/utils/notes_data.R")
source("R/modules/search_module.R")
source("R/modules/extensions_module.R")
source("R/modules/assessments_module.R")
source("R/modules/notes_module.R")

server <- function(input, output, session) {

  # Reactive values
  studentData <- reactiveVal(data.frame())
  isLoaded <- reactiveVal(FALSE)
  activeView <- reactiveVal("student")
  currentUnit <- reactiveVal(NULL)
  dataSources <- reactiveVal(list(canvas = FALSE, consids = FALSE, plans = FALSE))
  studentNotes <- reactiveVal(list())

  # Helper: load unit data (reusable from startup, modal confirm, and unit switcher)
  load_unit_data <- function(unit) {
    data_dir <- NIGHTMARE_CONFIG$data$data_dir
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
      save_last_unit(data_dir, unit)

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
  session$onFlush(function() {
    # Clear browser storage to prevent state persistence
    shinyjs::runjs("
      localStorage.clear();
      sessionStorage.clear();
    ")
  })

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
        unit = "—",
        year = "—",
        semester = "—",
        student_count = "—",
        sources = list(canvas = FALSE, consids = FALSE, plans = FALSE)
      ))
    }

    # Extract unit code
    unit <- if ("unit_of_study" %in% names(data) && nrow(data) > 0) {
      unique(data$unit_of_study)[1]
    } else {
      "—"
    }

    # Extract year from attribute
    year <- attr(data, "academic_year")
    if (is.null(year) || is.na(year)) {
      year <- "—"
    }

    # Extract semester from attribute (set by canvas import)
    semester <- attr(data, "semester")
    if (is.null(semester) || is.na(semester)) {
      # Fallback: try detection function
      semester <- tryCatch(
        detect_semester_from_canvas(data),
        error = function(e) "—"
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
    }
  })

  # Render dataset metadata panel
  output$dataset_metadata_panel <- renderUI({
    # Only render metadata panel after data is loaded
    if (!isLoaded()) {
      return(NULL)
    }

    meta <- datasetMetadata()
    folders <- scan_data_folders(NIGHTMARE_CONFIG$data$data_dir)
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
            tags$span(class = "unit-dropdown-indicator", HTML("&#9662;")),
            # Inline dropdown
            tags$div(
              id = "unit-dropdown",
              class = "unit-dropdown",
              lapply(folders, function(f) {
                tags$div(
                  class = paste("unit-dropdown-item", if (identical(f, active)) "active" else ""),
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
            if (meta$student_count == "—") "—" else as.character(meta$student_count)
          )
        ),
        tags$div(
          class = "metadata-item",
          tags$span(class = "metadata-label", "Semester:"),
          tags$span(class = "metadata-value", meta$semester)
        ),
        # Data source indicators
        tags$div(
          class = "metadata-item",
          tags$span(class = "metadata-label", "Data Sources:"),
          tags$div(
            class = "source-tags",
            tags$span(
              class = paste("source-tag", if (meta$sources$canvas) "active" else "inactive"),
              if (meta$sources$canvas) "Canvas Gradebook" else "Canvas Gradebook"
            ),
            tags$span(
              class = paste("source-tag", if (meta$sources$consids) "active" else "inactive"),
              if (meta$sources$consids) "Special Considerations" else "Special Considerations"
            ),
            tags$span(
              class = paste("source-tag", if (meta$sources$plans) "active" else "inactive"),
              if (meta$sources$plans) "Academic Plans" else "Academic Plans"
            )
          )
        )
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

  # Show folder picker on startup (auto-load last unit if available)
  observe({
    data_dir <- NIGHTMARE_CONFIG$data$data_dir
    folders <- scan_data_folders(data_dir)
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
    }
  })

  # Search module
  selectedStudentId <- searchModuleServer("search", studentData)

  # Extensions module
  extensionsModuleServer("extensions", studentData, dataSources, currentUnit)

  # Assessments module
  assessmentsModuleServer("assessments", studentData)

  # Notes module
  notesModuleServer("notes", studentData, studentNotes, currentUnit)

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
    save_student_notes(NIGHTMARE_CONFIG$data$data_dir, unit, updated)
  })

  # Delete a note
  observeEvent(input$delete_note, {
    req(input$delete_note)
    info <- input$delete_note
    unit <- currentUnit()
    if (is.null(unit)) return()

    updated <- delete_note(studentNotes(), info$student_id, info$note_id)
    studentNotes(updated)
    save_student_notes(NIGHTMARE_CONFIG$data$data_dir, unit, updated)
  })

  # Edit a note — show modal with pre-filled values
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

  observeEvent(input$confirm_edit_note_data, {
    req(input$confirm_edit_note_data)
    info <- input$confirm_edit_note_data
    unit <- currentUnit()
    if (is.null(unit)) return()

    updated <- edit_note(studentNotes(), info$student_id, info$note_id, info$category, info$text)
    studentNotes(updated)
    save_student_notes(NIGHTMARE_CONFIG$data$data_dir, unit, updated)
    removeModal()
  })

  # Student detail panel
  output$student_detail_panel <- renderUI({
    # Show blank until data is loaded
    if (!isLoaded()) {
      return(NULL)
    }

    if (is.null(selectedStudentId())) {
      return(div(class = "empty-state",
        tags$p("Select a student from the list to view details")))
    }

    student <- studentData() %>%
      filter(student_id == selectedStudentId())

    if (nrow(student) == 0) {
      return(div(class = "empty-state",
        tags$p("Student not found")))
    }

    student <- student[1, ]
    sid <- as.character(student$student_id)
    notes_for_student <- studentNotes()[[sid]] %||% list()
    build_student_detail_view(student, studentData(), notes_for_student)
  })

}
