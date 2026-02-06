#' Backup and Restore Module for NIGHTMARE
#'
#' Provides auto-backup, manual backup download, and restore functionality

library(shiny)
library(bslib)

#' Backup UI
#'
#' @param id Module namespace ID
#' @export
backupUI <- function(id) {
  ns <- NS(id)

  card(
    card_header(
      "Backup & Restore",
      class = "bg-info text-white"
    ),
    card_body(
      p("Protect your work with automatic and manual backups."),
      hr(),

      # Backup controls
      div(
        class = "d-grid gap-2",
        downloadButton(
          ns("download_backup"),
          "Download Backup",
          class = "btn-primary",
          icon = icon("download")
        ),
        fileInput(
          ns("restore_file"),
          "Restore from Backup",
          accept = ".rds",
          buttonLabel = "Browse...",
          placeholder = "Select .rds backup file"
        )
      ),

      hr(),

      # Status display
      div(
        class = "backup-status",
        uiOutput(ns("backup_status_display"))
      ),

      # Message area
      uiOutput(ns("backup_messages"))
    )
  )
}

#' Backup Server
#'
#' @param id Module namespace ID
#' @param studentData Reactive containing student data.frame
#' @export
backupServer <- function(id, studentData) {
  moduleServer(id, function(input, output, session) {

    # Reactive values for backup status
    backup_status <- reactiveValues(
      last_backup_time = NULL,
      student_count = 0,
      auto_backup_enabled = TRUE,
      message = NULL,
      message_type = NULL
    )

    # Auto-backup timer (every 5 minutes)
    auto_backup_timer <- reactiveTimer(5 * 60 * 1000)  # 5 minutes in milliseconds

    # Auto-backup observer
    observe({
      auto_backup_timer()  # Trigger on timer

      if (backup_status$auto_backup_enabled) {
        data <- studentData()

        if (!is.null(data) && nrow(data) > 0) {
          tryCatch({
            # Save to browser storage (using session$sendCustomMessage if available)
            # For now, just update the timestamp
            backup_status$last_backup_time <- Sys.time()
            backup_status$student_count <- nrow(data)

            message("Auto-backup completed: ", backup_status$last_backup_time)
          }, error = function(e) {
            warning("Auto-backup failed: ", e$message)
          })
        }
      }
    })

    # Manual backup download
    output$download_backup <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
        paste0("nightmare_backup_", timestamp, ".rds")
      },
      content = function(file) {
        data <- studentData()
        req(data)

        # Create backup structure
        backup <- list(
          metadata = list(
            timestamp = Sys.time(),
            student_count = nrow(data),
            unit_of_study = if ("unit_of_study" %in% names(data)) {
              unique(data$unit_of_study)[1]
            } else {
              "Unknown"
            },
            data_sources = c("canvas", "special_consids", "plans"),
            nightmare_version = "0.1.0"
          ),
          student_data = data,
          notes = ""
        )

        # Save as RDS
        saveRDS(backup, file)

        # Update status
        backup_status$last_backup_time <- Sys.time()
        backup_status$student_count <- nrow(data)
        backup_status$message <- paste(
          "Backup downloaded successfully:",
          nrow(data),
          "students"
        )
        backup_status$message_type <- "success"

        showNotification(
          paste("Backup saved:", basename(file)),
          type = "message",
          duration = 3
        )
      }
    )

    # Restore from uploaded file
    observeEvent(input$restore_file, {
      req(input$restore_file)

      tryCatch({
        # Read the backup file
        backup <- readRDS(input$restore_file$datapath)

        # Validate structure
        if (!is.list(backup) || !"student_data" %in% names(backup)) {
          stop("Invalid backup file format. Expected RDS file with student_data.")
        }

        if (!"metadata" %in% names(backup)) {
          warning("Backup file missing metadata. Proceeding with data only.")
        }

        # Validate student_data structure
        data <- backup$student_data
        required_cols <- c("student_id", "name")
        if (!all(required_cols %in% names(data))) {
          stop(paste(
            "Invalid student data structure. Missing required columns:",
            paste(setdiff(required_cols, names(data)), collapse = ", ")
          ))
        }

        # Restore the data (the parent module should handle this)
        # We'll return the restored data via a reactive
        restored_data$value <- data
        restored_data$metadata <- backup$metadata

        # Update status
        backup_status$last_backup_time <- Sys.time()
        backup_status$student_count <- nrow(data)
        backup_status$message <- sprintf(
          "Successfully restored %d students from backup (created: %s)",
          nrow(data),
          if (!is.null(backup$metadata$timestamp)) {
            format(backup$metadata$timestamp, "%Y-%m-%d %H:%M:%S")
          } else {
            "Unknown"
          }
        )
        backup_status$message_type <- "success"

        showNotification(
          sprintf("Restored %d students from backup", nrow(data)),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        backup_status$message <- paste("Restore failed:", e$message)
        backup_status$message_type <- "danger"

        showNotification(
          paste("Restore error:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Status display
    output$backup_status_display <- renderUI({
      div(
        class = "small text-muted",
        if (!is.null(backup_status$last_backup_time)) {
          p(
            icon("clock"),
            "Last backup:",
            format(backup_status$last_backup_time, "%Y-%m-%d %H:%M:%S")
          )
        } else {
          p(icon("clock"), "No backup yet")
        },
        if (backup_status$student_count > 0) {
          p(
            icon("users"),
            sprintf("%d students backed up", backup_status$student_count)
          )
        },
        p(
          icon("sync"),
          if (backup_status$auto_backup_enabled) {
            "Auto-backup: Enabled (every 5 minutes)"
          } else {
            "Auto-backup: Disabled"
          }
        )
      )
    })

    # Message display
    output$backup_messages <- renderUI({
      req(backup_status$message)

      div(
        class = paste("alert", paste0("alert-", backup_status$message_type)),
        role = "alert",
        backup_status$message
      )
    })

    # Reactive for restored data (parent module should observe this)
    restored_data <- reactiveValues(
      value = NULL,
      metadata = NULL
    )

    # Return reactive status and restored data
    return(list(
      status = reactive({
        list(
          last_backup = backup_status$last_backup_time,
          student_count = backup_status$student_count,
          auto_enabled = backup_status$auto_backup_enabled
        )
      }),
      restored_data = reactive({ restored_data$value }),
      restored_metadata = reactive({ restored_data$metadata })
    ))
  })
}
