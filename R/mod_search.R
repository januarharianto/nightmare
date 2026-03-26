# -- search_module.R -----------------------------------------------
# Student search with list-based results and keyboard navigation.

searchModuleUI <- function(id) {
  ns <- NS(id)
  div(class = "search-section",
    div(class = "search-container",
      tags$label("Search", class = "search-label", `for` = ns("search_box")),
      textInput(ns("search_box"), label = NULL,
                placeholder = "Search by Name, SID, or Unikey...", width = "100%")
    ),
    # Results list (shown when results exist)
    uiOutput(ns("search_results_list"))
  )
}

searchModuleServer <- function(id, studentData) {
  moduleServer(id, function(input, output, session) {

    # Reactive search term
    searchTerm <- reactive({
      term <- input$search_box
      if (is.null(term) || term == "") return("")
      tolower(trimws(term))
    })

    # Filter students based on search
    searchResults <- reactive({
      term <- searchTerm()
      if (term == "" || nchar(term) == 0) return(NULL)

      data <- studentData()
      req(data)

      results <- data %>%
        filter(
          grepl(term, tolower(name), fixed = TRUE) |
          grepl(term, tolower(as.character(student_id)), fixed = TRUE) |
          grepl(term, tolower(sis_login_id), fixed = TRUE)
        ) %>%
        select(name, student_id, sis_login_id) %>%
        arrange(name)

      if (nrow(results) == 0) return(NULL)
      results
    })

    # Render search results list
    output$search_results_list <- renderUI({
      results <- searchResults()

      # Don't show list if no search term
      if (searchTerm() == "") {
        return(NULL)
      }

      # Show "No results" message
      if (is.null(results) || nrow(results) == 0) {
        return(
          div(class = "search-results-empty",
            "No students found"
          )
        )
      }

      # Generate result items
      result_items <- lapply(1:nrow(results), function(i) {
        div(
          class = "search-result-item",
          onclick = paste0("Shiny.setInputValue('", session$ns("selected_index"), "', ", i, ", {priority: 'event'})"),
          div(class = "result-item-name", results$name[i]),
          div(class = "result-item-meta",
            span(class = "result-item-sid", paste0("SID: ", results$student_id[i])),
            span(class = "result-item-sep", " \u00b7 "),
            span(class = "result-item-unikey", results$sis_login_id[i])
          )
        )
      })

      # Results list container
      div(class = "search-results-list",
        result_items
      )
    })

    # Selected student ID reactive
    selectedStudentId <- reactiveVal(NULL)

    # Handle result item click
    observeEvent(input$selected_index, {
      results <- searchResults()
      if (!is.null(results) && !is.na(input$selected_index)) {
        idx <- input$selected_index
        if (idx >= 1 && idx <= nrow(results)) {
          selectedStudentId(results$student_id[idx])
          # Clear search box after selection
          updateTextInput(session, "search_box", value = "")
        }
      }
    })

    return(selectedStudentId)
  })
}
