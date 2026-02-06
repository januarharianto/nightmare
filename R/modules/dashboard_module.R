#' Dashboard Module for NIGHTMARE
#'
#' Displays summary metrics, filters, and student table with risk indicators

library(shiny)
library(bslib)
library(DT)
library(dplyr)

#' Dashboard UI
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
dashboardUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Summary Cards Row
    layout_columns(
      col_widths = c(4, 4, 4),

      # Card 1: Total Students
      value_box(
        title = "Total Students",
        value = textOutput(ns("total_students")),
        showcase = icon("users"),
        theme = "primary"
      ),

      # Card 2: At Risk (High)
      value_box(
        title = "High Risk Students",
        value = textOutput(ns("at_risk_high")),
        showcase = icon("exclamation-triangle"),
        theme = "danger"
      ),

      # Card 3: Average Extensions
      value_box(
        title = "Avg Extension Days",
        value = textOutput(ns("avg_extensions")),
        showcase = icon("calendar-alt"),
        theme = "info"
      )
    ),

    # Filters Card
    card(
      card_header("Filters"),
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),

          # Risk Category Filter
          selectInput(
            ns("risk_filter"),
            "Risk Category",
            choices = c("All", "Low", "Medium", "High"),
            selected = "All"
          ),

          # Disability Plan Filter
          checkboxInput(
            ns("disability_filter"),
            "Has Disability Plan Only",
            value = FALSE
          ),

          # Extension Days Threshold
          sliderInput(
            ns("extension_threshold"),
            "Min Extension Days",
            min = 0,
            max = 30,
            value = 0,
            step = 1
          ),

          # Clear All Button
          div(
            style = "padding-top: 25px;",
            actionButton(
              ns("clear_filters"),
              "Clear All Filters",
              class = "btn-secondary"
            )
          )
        )
      )
    ),

    # Student Table Card
    card(
      card_header("Student Overview"),
      card_body(
        DTOutput(ns("student_table"))
      )
    )
  )
}

#' Dashboard Server
#'
#' @param id Module namespace ID
#' @param studentData Reactive containing consolidated student data
#' @return Reactive value with selected student ID
dashboardServer <- function(id, studentData) {
  moduleServer(id, function(input, output, session) {

    # Selected student reactive
    selected_student <- reactiveVal(NULL)

    # Calculate summary metrics
    output$total_students <- renderText({
      req(studentData())
      nrow(studentData())
    })

    output$at_risk_high <- renderText({
      req(studentData())
      data <- studentData()
      high_count <- sum(data$risk_category == "High", na.rm = TRUE)
      total <- nrow(data)
      pct <- if (total > 0) round(100 * high_count / total, 1) else 0
      paste0(high_count, " (", pct, "%)")
    })

    output$avg_extensions <- renderText({
      req(studentData())
      data <- studentData()
      avg <- mean(data$total_approved_extension_days, na.rm = TRUE)
      if (is.na(avg)) {
        "0.0"
      } else {
        sprintf("%.1f days", avg)
      }
    })

    # Filtered data based on user inputs
    filtered_data <- reactive({
      req(studentData())
      data <- studentData()

      # Apply risk category filter
      if (input$risk_filter != "All") {
        data <- data %>%
          filter(risk_category == input$risk_filter)
      }

      # Apply disability plan filter
      if (input$disability_filter) {
        data <- data %>%
          filter(has_disability_plan == TRUE)
      }

      # Apply extension threshold filter
      if (input$extension_threshold > 0) {
        data <- data %>%
          filter(total_approved_extension_days >= input$extension_threshold)
      }

      return(data)
    })

    # Render student table
    output$student_table <- renderDT({
      req(filtered_data())

      # Prepare display data
      display_data <- filtered_data() %>%
        mutate(
          Name = name,
          ID = student_id,
          `Final Grade` = round(final_grade, 1),
          `Risk Score` = round(risk_score, 0),
          `Risk Category` = risk_category,
          `Total Extensions` = total_approved_extension_days
        ) %>%
        select(Name, ID, `Final Grade`, `Risk Score`, `Risk Category`, `Total Extensions`)

      # Create datatable
      dt <- datatable(
        display_data,
        options = list(
          pageLength = 25,
          autoWidth = TRUE,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          order = list(list(3, 'desc'))  # Sort by Risk Score descending
        ),
        rownames = FALSE,
        selection = 'single',
        class = 'cell-border stripe hover'
      )

      # Apply conditional formatting based on risk category
      dt <- dt %>%
        formatStyle(
          'Risk Category',
          target = 'row',
          backgroundColor = styleEqual(
            c('Low', 'Medium', 'High'),
            c('#ccffcc', '#ffffcc', '#ffcccc')
          )
        ) %>%
        formatStyle(
          'Risk Category',
          fontWeight = 'bold',
          color = styleEqual(
            c('Low', 'Medium', 'High'),
            c('#2d5016', '#7a6a00', '#7a0000')
          )
        )

      return(dt)
    })

    # Handle row selection
    observeEvent(input$student_table_rows_selected, {
      req(input$student_table_rows_selected)

      # Get the student_id from filtered data
      row_index <- input$student_table_rows_selected
      data <- filtered_data()

      if (row_index <= nrow(data)) {
        student_id <- data$student_id[row_index]
        selected_student(student_id)
      }
    })

    # Clear all filters
    observeEvent(input$clear_filters, {
      updateSelectInput(session, "risk_filter", selected = "All")
      updateCheckboxInput(session, "disability_filter", value = FALSE)
      updateSliderInput(session, "extension_threshold", value = 0)
    })

    # Return selected student ID
    return(reactive({ selected_student() }))
  })
}
