# Assessments Module for NIGHTMARE
# Class-level mark distribution view with density plots and grade breakdowns.

assessmentsModuleUI <- function(id) {
  ns <- NS(id)
  ns_id <- ns("plot_type")
  tags$div(class = "assessments-view",
    tags$div(class = "assessments-toolbar",
      tags$span(class = "assessments-label", "Plot Type"),
      tags$div(class = "plot-type-toggle",
        tags$button(
          class = "plot-type-btn active",
          `data-value` = "histogram",
          onclick = sprintf(
            "document.querySelectorAll('.plot-type-btn').forEach(function(b){b.classList.remove('active')});this.classList.add('active');Shiny.setInputValue('%s',this.dataset.value,{priority:'event'});",
            ns_id
          ),
          "Histogram"
        ),
        tags$button(
          class = "plot-type-btn",
          `data-value` = "density",
          onclick = sprintf(
            "document.querySelectorAll('.plot-type-btn').forEach(function(b){b.classList.remove('active')});this.classList.add('active');Shiny.setInputValue('%s',this.dataset.value,{priority:'event'});",
            ns_id
          ),
          "Density"
        )
      )
    ),
    uiOutput(ns("assessments_grid"))
  )
}

assessmentsModuleServer <- function(id, studentData, examData = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    classScores <- reactive({
      data <- studentData()
      canvas_scores <- if (!is.null(data) && nrow(data) > 0) extract_class_scores(data) else list()

      # Merge exam scores if available
      exam <- if (!is.null(examData)) examData() else NULL
      exam_scores <- if (!is.null(exam)) extract_exam_class_scores(exam) else list()

      c(canvas_scores, exam_scores)
    })

    plotType <- reactive(input$plot_type %||% "histogram")

    output$assessments_grid <- renderUI({
      scores <- classScores()
      plot_type <- plotType()
      if (length(scores) == 0) {
        return(tags$div(class = "empty-state",
          tags$p("No completed assessments to display")
        ))
      }

      # Create a card for each assessment
      cards <- lapply(names(scores), function(aname) {
        info <- scores[[aname]]
        plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", aname))

        pcts <- info$scores
        tags$div(class = "assessment-card detail-section",
          # Header
          tags$div(class = "assessment-card-header detail-section-header",
            paste0(aname, " (", info$max_points, " pts)")
          ),
          # Summary stats
          tags$div(class = "assessment-grade-row extensions-summary",
            tags$div(
              tags$span(class = "stat-label", "n"),
              tags$span(class = "stat-value", length(pcts))
            ),
            tags$div(
              tags$span(class = "stat-label", "Mean"),
              tags$span(class = "stat-value", sprintf("%.1f%%", mean(pcts)))
            ),
            tags$div(
              tags$span(class = "stat-label", "Median"),
              tags$span(class = "stat-value", sprintf("%.1f%%", median(pcts)))
            ),
            tags$div(
              tags$span(class = "stat-label", "SD"),
              tags$span(class = "stat-value", sprintf("%.1f", sd(pcts)))
            )
          ),
          # Plot with integrated grade labels and counts
          tags$div(class = "assessment-plot",
            plotOutput(ns(plot_id), height = "160px")
          )
        )
      })

      # Create plot outputs dynamically
      lapply(names(scores), function(aname) {
        info <- scores[[aname]]
        plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", aname))
        output[[plot_id]] <- renderPlot({
          if (plot_type == "histogram") {
            render_histogram_plot(info$scores)
          } else {
            render_density_plot(info$scores)
          }
        }, bg = "transparent")
      })

      tags$div(class = "assessments-grid detail-cards-grid",
        cards
      )
    })
  })
}
