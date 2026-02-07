# Server Logic for NIGHTMARE

source("R/dependencies.R")
source("R/config.R")
source("R/utils/import/canvas.R")
source("R/utils/import/special_consids.R")
source("R/utils/import/disability_plans.R")
source("R/utils/import/consolidate.R")
source("R/utils/import/file_detection.R")
source("R/utils/import/folder_loader.R")
source("R/utils/risk_scoring.R")
source("R/utils/ui_helpers.R")
source("R/modules/search_module.R")

load_nightmare_dependencies()

server <- function(input, output, session) {

  # Reactive values
  studentData <- reactiveVal(data.frame())
  isLoaded <- reactiveVal(FALSE)
  activeView <- reactiveVal("student")

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

  # Show folder picker on startup
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

    # Pre-select last used unit, or first available
    selected <- if (!is.null(last_unit) && last_unit %in% folders) last_unit else folders[1]

    showModal(modalDialog(
      title = "Select Unit",
      selectInput("folder_select", "Unit of Study", choices = folders, selected = selected),
      footer = actionButton("folder_confirm", "Load", class = "btn-dark"),
      easyClose = FALSE
    ))
  })

  # Handle folder selection
  observeEvent(input$folder_confirm, {
    removeModal()
    data_dir <- NIGHTMARE_CONFIG$data$data_dir
    unit <- input$folder_select
    folder_path <- file.path(data_dir, unit)

    tryCatch({
      imported <- load_folder(folder_path, unit_filter = unit)

      if (is.null(imported$canvas)) {
        showNotification("No Canvas gradebook found in folder", type = "error")
        return()
      }

      consolidated <- consolidate_student_data(
        imported$canvas, imported$consids, imported$plans
      )
      consolidated <- apply_risk_scoring(consolidated)

      studentData(consolidated)
      isLoaded(TRUE)
      save_last_unit(data_dir, unit)

      showNotification(
        sprintf("Loaded %d students from %s", nrow(consolidated), unit),
        type = "message",
        duration = NIGHTMARE_CONFIG$notifications$duration_message
      )
    }, error = function(e) {
      showNotification(
        paste("Error loading data:", e$message),
        type = "error"
      )
    })
  })

  # Search module
  selectedStudentId <- searchModuleServer("search", studentData)

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

}
