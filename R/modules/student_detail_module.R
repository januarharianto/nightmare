#' Student Detail Module for NIGHTMARE
#'
#' Displays comprehensive student information in a modal/sidebar view

library(shiny)
library(bslib)
library(DT)
library(dplyr)

#' Student Detail UI
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
studentDetailUI <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = sidebar(
      id = ns("detail_sidebar"),
      title = "Student Details",
      width = "600px",
      position = "right",
      open = FALSE,

      # Close button
      actionButton(
        ns("close_detail"),
        "Close",
        icon = icon("times"),
        class = "btn-secondary btn-sm float-end mb-3"
      ),

      # Profile Section
      card(
        card_header("Profile"),
        card_body(
          uiOutput(ns("profile_info"))
        )
      ),

      # Academic Performance
      card(
        card_header("Academic Performance"),
        card_body(
          uiOutput(ns("performance_info")),
          DTOutput(ns("assignments_table"))
        )
      ),

      # Extensions Timeline
      card(
        card_header("Extensions Timeline"),
        card_body(
          uiOutput(ns("extensions_summary")),
          DTOutput(ns("extensions_table"))
        )
      ),

      # Support Plans
      card(
        card_header("Support Plans"),
        card_body(
          uiOutput(ns("support_plans_info"))
        )
      ),

      # Risk Assessment
      card(
        card_header("Risk Assessment"),
        card_body(
          uiOutput(ns("risk_assessment_info"))
        )
      )
    ),

    # Main content (empty - sidebar only)
    NULL
  )
}

#' Student Detail Server
#'
#' @param id Module namespace ID
#' @param studentRecord Reactive containing single student record
#' @param show Reactive logical indicating whether to show sidebar
#' @return Reactive indicating close event
studentDetailServer <- function(id, studentRecord, show) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Observe show/hide trigger
    observe({
      req(show())
      if (!is.null(studentRecord()) && nrow(studentRecord()) > 0) {
        sidebar_toggle(id = "detail_sidebar", open = TRUE)
      }
    })

    # Profile Information
    output$profile_info <- renderUI({
      req(studentRecord())
      student <- studentRecord()

      tagList(
        p(strong("Name: "), student$name),
        p(strong("Student ID: "), student$student_id),
        p(strong("Unikey: "), student$sis_login_id),
        p(strong("Email: "), student$email),
        p(strong("Unit: "), student$unit_of_study)
      )
    })

    # Performance Information
    output$performance_info <- renderUI({
      req(studentRecord())
      student <- studentRecord()

      final_grade <- if (!is.na(student$final_grade)) {
        sprintf("%.1f%%", student$final_grade)
      } else {
        "N/A"
      }

      grade_color <- if (!is.na(student$final_grade)) {
        if (student$final_grade >= 50) "success"
        else if (student$final_grade >= 30) "warning"
        else "danger"
      } else {
        "secondary"
      }

      tagList(
        div(
          class = paste0("alert alert-", grade_color),
          h4("Final Grade: ", final_grade)
        )
      )
    })

    # Assignments Table
    output$assignments_table <- renderDT({
      req(studentRecord())
      student <- studentRecord()

      assignments <- student$assignments[[1]]

      if (nrow(assignments) == 0) {
        return(datatable(
          data.frame(Message = "No assignment data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Format for display
      display_assignments <- assignments %>%
        mutate(
          Score = sprintf("%.1f", score),
          Weight = sprintf("%.1f%%", weight)
        ) %>%
        select(
          Assignment = name,
          Score,
          Weight
        )

      datatable(
        display_assignments,
        options = list(
          pageLength = 10,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
    })

    # Extensions Summary
    output$extensions_summary <- renderUI({
      req(studentRecord())
      student <- studentRecord()

      total_days <- student$total_approved_extension_days
      has_replacement <- student$has_replacement_exam
      has_adjustment <- student$has_mark_adjustment

      tagList(
        p(strong("Total Extension Days: "), total_days),
        if (has_replacement) p(icon("check"), "Replacement Exam Granted"),
        if (has_adjustment) p(icon("check"), "Mark Adjustment Granted")
      )
    })

    # Extensions Table
    output$extensions_table <- renderDT({
      req(studentRecord())
      student <- studentRecord()

      special_consids <- student$special_consids[[1]]

      if (nrow(special_consids) == 0) {
        return(datatable(
          data.frame(Message = "No extensions recorded"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Format for display
      display_extensions <- special_consids %>%
        filter(approved == TRUE) %>%
        mutate(
          Extension = ifelse(!is.na(extension_date) & extension_date != "",
                           extension_date,
                           "N/A"),
          Type = ifelse(!is.na(outcome_type), outcome_type, "Extension")
        ) %>%
        select(
          Assessment = assessment_name,
          Extension,
          Type,
          Ticket = ticket_id
        )

      if (nrow(display_extensions) == 0) {
        return(datatable(
          data.frame(Message = "No approved extensions"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      datatable(
        display_extensions,
        options = list(
          pageLength = 10,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      )
    })

    # Support Plans Information
    output$support_plans_info <- renderUI({
      req(studentRecord())
      student <- studentRecord()

      if (!student$has_disability_plan) {
        return(p(icon("info-circle"), "No disability plan on file"))
      }

      plan_adjustments <- student$plan_adjustments[[1]]

      if (nrow(plan_adjustments) == 0) {
        return(p(icon("info-circle"), "Disability plan recorded but no adjustments listed"))
      }

      # Create list of adjustments
      adjustment_list <- lapply(1:nrow(plan_adjustments), function(i) {
        adj <- plan_adjustments[i, ]
        tags$li(
          strong(adj$adjustment_type, ": "),
          adj$description
        )
      })

      tagList(
        p(icon("check-circle", class = "text-success"), strong("Has Disability Plan")),
        tags$h6("Adjustments:"),
        tags$ul(adjustment_list)
      )
    })

    # Risk Assessment Information
    output$risk_assessment_info <- renderUI({
      req(studentRecord())
      student <- studentRecord()

      risk_score <- student$risk_score
      risk_category <- as.character(student$risk_category)
      risk_factors <- student$risk_factors[[1]]

      # Color based on category
      badge_color <- switch(
        risk_category,
        "Low" = "success",
        "Medium" = "warning",
        "High" = "danger",
        "secondary"
      )

      # Create risk factors list
      factors_list <- lapply(risk_factors, function(factor) {
        tags$li(factor)
      })

      tagList(
        div(
          class = "d-flex justify-content-between align-items-center mb-3",
          div(
            strong("Risk Score: "),
            sprintf("%.0f / 100", risk_score)
          ),
          span(
            class = paste0("badge bg-", badge_color),
            risk_category
          )
        ),
        tags$h6("Risk Factors:"),
        tags$ul(factors_list)
      )
    })

    # Handle close button
    observeEvent(input$close_detail, {
      sidebar_toggle(id = "detail_sidebar", open = FALSE)
    })

    # Return close event
    return(reactive({ input$close_detail }))
  })
}
