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
          `data-value` = "density",
          onclick = sprintf(
            "document.querySelectorAll('.plot-type-btn').forEach(function(b){b.classList.remove('active')});this.classList.add('active');Shiny.setInputValue('%s',this.dataset.value,{priority:'event'});",
            ns_id
          ),
          "Density"
        ),
        tags$button(
          class = "plot-type-btn",
          `data-value` = "histogram",
          onclick = sprintf(
            "document.querySelectorAll('.plot-type-btn').forEach(function(b){b.classList.remove('active')});this.classList.add('active');Shiny.setInputValue('%s',this.dataset.value,{priority:'event'});",
            ns_id
          ),
          "Histogram"
        )
      )
    ),
    uiOutput(ns("assessments_grid"))
  )
}

assessmentsModuleServer <- function(id, studentData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    classScores <- reactive({
      data <- studentData()
      if (is.null(data) || nrow(data) == 0) return(list())
      extract_class_scores(data)
    })

    plotType <- reactive(input$plot_type %||% "density")

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
        grades <- compute_grade_counts(info$scores)
        plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", aname))

        tags$div(class = "assessment-card detail-section",
          # Header
          tags$div(class = "assessment-card-header detail-section-header",
            paste0(aname, " (", info$max_points, " pts)")
          ),
          # Grade counts row
          tags$div(class = "assessment-grade-row extensions-summary",
            tags$div(
              tags$span(class = "stat-label", "F"),
              tags$span(class = "stat-value", grades[["F"]])
            ),
            tags$div(
              tags$span(class = "stat-label", "P"),
              tags$span(class = "stat-value", grades[["P"]])
            ),
            tags$div(
              tags$span(class = "stat-label", "CR"),
              tags$span(class = "stat-value", grades[["CR"]])
            ),
            tags$div(
              tags$span(class = "stat-label", "D"),
              tags$span(class = "stat-value", grades[["D"]])
            ),
            tags$div(
              tags$span(class = "stat-label", "HD"),
              tags$span(class = "stat-value", grades[["HD"]])
            )
          ),
          # Density plot
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
