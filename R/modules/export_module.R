#' Export Module for NIGHTMARE
#'
#' Provides UI and server logic for exporting student data in various formats

library(shiny)
library(bslib)
library(readr)

# Source export utilities
source("R/utils/exports/helpers.R")
source("R/utils/exports/canvas.R")
source("R/utils/exports/extensions.R")
source("R/utils/exports/at_risk.R")
source("R/utils/exports/comprehensive.R")

#' Export UI
#'
#' @param id Module namespace ID
#' @export
exportUI <- function(id) {
  ns <- NS(id)

  card(
    card_header(
      "Export Data",
      class = "bg-primary text-white"
    ),
    card_body(
      p("Export student data in various formats for different stakeholders."),
      hr(),

      # Export format selection
      selectInput(
        ns("export_format"),
        "Export Format",
        choices = c(
          "Canvas Bulk Upload (CSV)" = "canvas",
          "Extensions Report (CSV)" = "extensions",
          "At-Risk Students (CSV)" = "at_risk",
          "Comprehensive Report (Excel)" = "comprehensive"
        ),
        selected = "canvas"
      ),

      # Format descriptions
      uiOutput(ns("format_description")),

      hr(),

      # Download button
      div(
        class = "d-grid",
        downloadButton(
          ns("download_export"),
          "Download Export",
          class = "btn-success",
          icon = icon("file-export")
        )
      ),

      hr(),

      # Status display
      div(
        class = "export-status small text-muted",
        uiOutput(ns("export_status_display"))
      )
    )
  )
}

#' Export Server
#'
#' @param id Module namespace ID
#' @param studentData Reactive containing student data.frame
#' @export
exportServer <- function(id, studentData) {
  moduleServer(id, function(input, output, session) {

    # Reactive values for export status
    export_status <- reactiveValues(
      last_export_time = NULL,
      last_export_format = NULL,
      row_count = 0,
      message = NULL
    )

    # Format descriptions
    output$format_description <- renderUI({
      format <- input$export_format

      description <- switch(format,
        "canvas" = div(
          class = "alert alert-info",
          p(strong("Canvas Bulk Upload Format")),
          p("Exports: SIS Login ID, Student Name, Final Grade"),
          p("Use this file to upload final grades to Canvas."),
          p(em("Note: Extensions are NOT included in this export."))
        ),
        "extensions" = div(
          class = "alert alert-info",
          p(strong("Extensions Report")),
          p("Exports: Student ID, Name, Assessment, Extension Days, Reason"),
          p("One row per extension. Submit to faculty for record-keeping."),
          p(em("Includes both Special Considerations and Disability Plan extensions."))
        ),
        "at_risk" = div(
          class = "alert alert-warning",
          p(strong("At-Risk Students Report")),
          p("Exports: High and Medium risk students with recommended actions"),
          p("Use this to flag students for intervention programs."),
          p(em("Includes risk factors, extensions, and disability plan status."))
        ),
        "comprehensive" = div(
          class = "alert alert-success",
          p(strong("Comprehensive Excel Report")),
          p("5 sheets: Summary, All Students, Extensions, At-Risk, Disability Plans"),
          p("Complete multi-sheet workbook for detailed analysis."),
          p(em("Best for archival and comprehensive reporting."))
        )
      )

      return(description)
    })

    # Export download handler
    output$download_export <- downloadHandler(
      filename = function() {
        format <- input$export_format
        timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

        ext <- if (format == "comprehensive") "xlsx" else "csv"

        paste0("nightmare_", format, "_", timestamp, ".", ext)
      },
      content = function(file) {
        data <- studentData()
        req(data)

        format <- input$export_format

        tryCatch({
          # Generate export based on format
          if (format == "canvas") {
            export_data <- export_for_canvas(data)
            write_csv(export_data, file)
            row_count <- nrow(export_data)

          } else if (format == "extensions") {
            export_data <- export_extensions_report(data)
            write_csv(export_data, file)
            row_count <- nrow(export_data)

          } else if (format == "at_risk") {
            export_data <- export_at_risk_report(data)
            write_csv(export_data, file)
            row_count <- nrow(export_data)

          } else if (format == "comprehensive") {
            # Check if writexl is available
            if (!requireNamespace("writexl", quietly = TRUE)) {
              stop("Package 'writexl' is required for Excel export. Install with: install.packages('writexl')")
            }

            # Generate sheets
            sheets <- export_comprehensive_report(data)

            # Write to file
            writexl::write_xlsx(sheets, file)

            # Count total rows across all sheets
            row_count <- sum(sapply(sheets, nrow))
          }

          # Update status
          export_status$last_export_time <- Sys.time()
          export_status$last_export_format <- format
          export_status$row_count <- row_count

          showNotification(
            sprintf("Export complete: %s (%d rows)", basename(file), row_count),
            type = "message",
            duration = 5
          )

        }, error = function(e) {
          export_status$message <- paste("Export failed:", e$message)

          showNotification(
            paste("Export error:", e$message),
            type = "error",
            duration = 10
          )

          # Fail gracefully
          stop(e$message)
        })
      }
    )

    # Status display
    output$export_status_display <- renderUI({
      if (!is.null(export_status$last_export_time)) {
        div(
          p(
            icon("check-circle", class = "text-success"),
            "Last export:",
            format(export_status$last_export_time, "%Y-%m-%d %H:%M:%S")
          ),
          p(
            icon("table"),
            sprintf("Format: %s", export_status$last_export_format)
          ),
          p(
            icon("list-ol"),
            sprintf("%d rows exported", export_status$row_count)
          )
        )
      } else {
        p(icon("info-circle"), "No exports yet")
      }
    })

    # Return reactive status
    return(reactive({
      list(
        last_export = export_status$last_export_time,
        format = export_status$last_export_format,
        row_count = export_status$row_count
      )
    }))
  })
}
