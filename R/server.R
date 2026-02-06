# Server Logic for NIGHTMARE - Refactored (Phase 1)

source("R/dependencies.R")
source("R/config.R")
source("R/utils/import/canvas.R")
source("R/utils/import/special_consids.R")
source("R/utils/import/disability_plans.R")
source("R/utils/import/consolidate.R")
source("R/utils/import/file_detection.R")
source("R/utils/risk_scoring.R")
source("R/utils/history.R")
source("R/utils/exports/helpers.R")
source("R/utils/exports/canvas.R")
source("R/utils/exports/extensions.R")
source("R/utils/exports/at_risk.R")
source("R/utils/exports/comprehensive.R")
source("R/utils/ui_helpers.R")
source("R/modules/dashboard_module.R")
source("R/modules/student_detail_module.R")
source("R/modules/backup_module.R")
source("R/modules/export_module.R")
source("R/modules/search_module.R")
source("R/utils/action_handlers.R")

load_nightmare_dependencies()

server <- function(input, output, session) {

  # Reactive values
  studentData <- reactiveVal(data.frame())
  isLoaded <- reactiveVal(FALSE)
  activeView <- reactiveVal("student")
  history <- HistoryManager$new()
  undo_redo_state <- reactiveValues(
    can_undo = FALSE,
    can_redo = FALSE
  )

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
        student_count = "—"
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
      student_count = nrow(data)
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

        # Left Column (40%) - Search-focused
        tags$div(
          class = "left-column",

          # Large search box with dropdown (sticky at top)
          searchModuleUI("search")
        ),

        # Right Column (60%)
        tags$div(
          class = "right-column",
          uiOutput("student_detail_panel")
        )
      )
    } else if (activeView() == "extensions") {
      # Extensions view (placeholder)
      tags$div(
        class = "main-container",
        tags$div(
          class = "extensions-view",
          tags$div(
            class = "empty-state",
            tags$p("Extensions view")
          )
        )
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

    tags$div(
      class = "metadata-panel",
      tags$div(
        class = "metadata-grid",
        # Row 1
        tags$div(
          class = "metadata-item",
          tags$span(class = "metadata-label", "Unit:"),
          tags$span(class = "metadata-value", meta$unit)
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
        )
      )
    )
  })

  # Load sample data on startup
  observe({
    tryCatch({
      canvas_path <- file.path(NIGHTMARE_CONFIG$data$sample_data_dir,
                               NIGHTMARE_CONFIG$data$canvas_file)
      consids_path <- file.path(NIGHTMARE_CONFIG$data$sample_data_dir,
                                NIGHTMARE_CONFIG$data$consids_file)
      plans_path <- file.path(NIGHTMARE_CONFIG$data$sample_data_dir,
                              NIGHTMARE_CONFIG$data$plans_file)

      if (file.exists(canvas_path)) {
        canvas <- import_canvas_grades(canvas_path)

        # Detect year from Canvas data for filtering plans
        detected_year <- detect_year_from_canvas(canvas)

        consids <- import_special_considerations(consids_path,
                                                 unit_filter = NIGHTMARE_CONFIG$data$default_unit)
        plans <- import_disability_plans(plans_path,
                                        unit_filter = NIGHTMARE_CONFIG$data$default_unit,
                                        year_filter = detected_year)

        consolidated <- consolidate_student_data(canvas, consids, plans)
        consolidated <- apply_risk_scoring(consolidated)

        studentData(consolidated)
        isLoaded(TRUE)
        history$push(consolidated, "Initial data load")
        undo_redo_state$can_undo <- history$can_undo()
        undo_redo_state$can_redo <- history$can_redo()

        showNotification(
          sprintf("Loaded %d students from sample data", nrow(consolidated)),
          type = "message",
          duration = NIGHTMARE_CONFIG$notifications$duration_message
        )
      }
    }, error = function(e) {
      message("Could not load sample data: ", e$message)
    })
  })

  # Search module
  selectedStudentId <- searchModuleServer("search", studentData)

  # Reset search input on app startup
  session$onFlush(function() {
    shinyjs::runjs("
      // Clear selectize search input to ensure fresh start
      const searchInput = document.querySelector('[data-id=\"search-student_search\"]');
      if (searchInput) {
        searchInput.value = '';
        searchInput.dispatchEvent(new Event('change', { bubbles: true }));
      }
    ")
  }, once = TRUE)

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
    build_student_detail_view(student)
  })

  # Action handlers
  setup_undo_redo(input, output, history, studentData, undo_redo_state)
  setup_backup(input, studentData)
  setup_export(input, output, studentData)
}
