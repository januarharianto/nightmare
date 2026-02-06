# Search Module for NIGHTMARE
# Extracted from server.R lines 73-160

searchModuleUI <- function(id) {
  ns <- NS(id)
  div(class = "search-container",
    textInput(ns("search_box_left"), label = NULL,
              placeholder = "Search Student", width = "100%"))
}

searchModuleServer <- function(id, studentData) {
  moduleServer(id, function(input, output, session) {

    # Reactive search term (lines 73-79)
    searchTerm <- reactive({
      term <- input$search_box_left
      if (is.null(term) || term == "") return("")
      tolower(trimws(term))
    })

    # Filter students based on search (lines 82-105)
    searchResults <- reactive({
      term <- searchTerm()
      if (term == "" || nchar(term) == 0) return(NULL)

      data <- studentData()
      req(data)

      data %>%
        filter(
          grepl(term, tolower(name), fixed = TRUE) |
          grepl(term, tolower(as.character(student_id)), fixed = TRUE) |
          grepl(term, tolower(sis_login_id), fixed = TRUE)
        ) %>%
        select(name, student_id, sis_login_id) %>%
        rename(
          "Name" = name,
          "SID" = student_id,
          "Unikey" = sis_login_id
        )
    })

    # Render search results (lines 108-149)
    output$search_results <- renderUI({
      results <- searchResults()

      if (is.null(results) || nrow(results) == 0) {
        if (searchTerm() == "") {
          return(NULL)
        } else {
          return(div(class = "search-empty-state",
            tags$p("No students found")))
        }
      }

      result_items <- lapply(1:nrow(results), function(i) {
        tags$button(
          class = "search-results-item",
          id = paste0("result_item_", i),
          onclick = paste0("Shiny.setInputValue('", session$ns("selected_result_index"), "', ", i, ", {priority: 'event'})"),

          div(class = "result-header",
            tags$p(class = "result-name", results$Name[i]),
            tags$p(class = "result-sid", paste0("SID: ", results$SID[i]))
          ),

          tags$p(class = "result-unikey",
            paste0("Unikey: ", results$Unikey[i]))
        )
      })

      div(
        div(class = "search-results-header",
          sprintf("▼ %d RESULTS", nrow(results))),
        tags$ul(class = "search-results-list", result_items)
      )
    })

    # Selected student ID reactive
    selectedStudentId <- reactiveVal(NULL)

    # Handle result item click (lines 152-160)
    observeEvent(input$selected_result_index, {
      results <- searchResults()
      if (!is.null(results) && !is.na(input$selected_result_index)) {
        idx <- input$selected_result_index
        if (idx >= 1 && idx <= nrow(results)) {
          selectedStudentId(results$SID[idx])
        }
      }
    })

    return(selectedStudentId)
  })
}
