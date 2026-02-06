# Action handlers for NIGHTMARE
# Extracted from server.R lines 386-490

# Setup undo/redo state management
setup_undo_redo <- function(input, output, history, studentData, undo_redo_state) {
  # Update button states (lines 389-392)
  observe({
    shinyjs::toggleState("undo_action", history$can_undo())
    shinyjs::toggleState("redo_action", history$can_redo())
  })

  # Undo button handler (lines 395-408)
  observeEvent(input$undo_action, {
    result <- history$undo()
    if (!is.null(result)) {
      studentData(result$state)
      undo_redo_state$can_undo <- history$can_undo()
      undo_redo_state$can_redo <- history$can_redo()

      showNotification(
        paste("Undo:", result$description),
        type = "message",
        duration = NIGHTMARE_CONFIG$notifications$duration_undo_redo
      )
    }
  })

  # Redo button handler (lines 411-424)
  observeEvent(input$redo_action, {
    result <- history$redo()
    if (!is.null(result)) {
      studentData(result$state)
      undo_redo_state$can_undo <- history$can_undo()
      undo_redo_state$can_redo <- history$can_redo()

      showNotification(
        paste("Redo:", result$description),
        type = "message",
        duration = NIGHTMARE_CONFIG$notifications$duration_undo_redo
      )
    }
  })
}

# Setup backup handler (lines 428-456)
setup_backup <- function(input, studentData) {
  observeEvent(input$backup_action, {
    req(studentData())

    tryCatch({
      if (!dir.exists(NIGHTMARE_CONFIG$backup$dir)) {
        dir.create(NIGHTMARE_CONFIG$backup$dir)
      }

      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename <- file.path(
        NIGHTMARE_CONFIG$backup$dir,
        paste0("nightmare_backup_", timestamp, ".rds")
      )

      saveRDS(studentData(), filename)

      showNotification(
        paste("Backup saved:", basename(filename)),
        type = "message",
        duration = NIGHTMARE_CONFIG$notifications$duration_message
      )
    }, error = function(e) {
      showNotification(
        paste("Backup failed:", e$message),
        type = "error",
        duration = NIGHTMARE_CONFIG$notifications$duration_message
      )
    })
  })
}

# Setup export handler (lines 461-490)
setup_export <- function(input, output, studentData) {
  output$download_export <- downloadHandler(
    filename = function() {
      paste0("nightmare_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(studentData())

      export_data <- studentData() %>%
        select(
          student_id, name, email, sis_login_id,
          unit_of_study, section, final_grade,
          risk_score, risk_category,
          total_approved_extension_days,
          has_disability_plan, has_replacement_exam, has_mark_adjustment
        )

      write_csv(export_data, file)
    }
  )

  observeEvent(input$export_action, {
    shinyjs::click("download_export")

    showNotification(
      "Export started",
      type = "message",
      duration = NIGHTMARE_CONFIG$notifications$duration_undo_redo
    )
  })
}
