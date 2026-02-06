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
  history <- HistoryManager$new()
  undo_redo_state <- reactiveValues(
    can_undo = FALSE,
    can_redo = FALSE
  )

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

  # Student detail panel
  output$student_detail_panel <- renderUI({
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
