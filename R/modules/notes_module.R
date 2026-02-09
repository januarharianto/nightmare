# -- notes_module.R ------------------------------------------------
# Sidebar view: browse all student notes with category filtering.

notesModuleUI <- function(id) {
  ns <- NS(id)
  ns_id <- ns("category_filter")

  # Build filter buttons from NOTE_TAGS
  filter_buttons <- tagList(
    tags$button(
      class = "notes-filter-btn active",
      `data-value` = "all",
      onclick = sprintf(
        "document.querySelectorAll('.notes-filter-btn').forEach(function(b){b.classList.remove('active')});this.classList.add('active');Shiny.setInputValue('%s','all',{priority:'event'});",
        ns_id
      ),
      "All"
    ),
    lapply(names(NOTE_TAGS), function(tag_key) {
      tag_info <- NOTE_TAGS[[tag_key]]
      tags$button(
        class = "notes-filter-btn",
        `data-value` = tag_key,
        onclick = sprintf(
          "document.querySelectorAll('.notes-filter-btn').forEach(function(b){b.classList.remove('active')});this.classList.add('active');Shiny.setInputValue('%s','%s',{priority:'event'});",
          ns_id, tag_key
        ),
        tag_info$label
      )
    })
  )

  tags$div(class = "notes-view",
    tags$div(class = "notes-toolbar",
      tags$span(class = "notes-filter-label", "Filter"),
      tags$div(class = "notes-filter-tags", filter_buttons)
    ),
    tags$div(class = "notes-feed-container",
      uiOutput(ns("notes_feed"))
    )
  )
}

notesModuleServer <- function(id, studentData, studentNotes, currentUnit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    categoryFilter <- reactive(input$category_filter %||% "all")

    output$notes_feed <- renderUI({
      notes <- studentNotes()
      data <- studentData()

      if (length(notes) == 0) {
        return(tags$div(class = "empty-state",
          tags$p("No notes yet")))
      }

      flat <- flatten_all_notes(notes, data)
      if (nrow(flat) == 0) {
        return(tags$div(class = "empty-state",
          tags$p("No notes yet")))
      }

      # Apply category filter
      cat_filter <- categoryFilter()
      if (cat_filter != "all") {
        flat <- flat[flat$category == cat_filter, , drop = FALSE]
      }

      if (nrow(flat) == 0) {
        return(tags$div(class = "empty-state",
          tags$p("No notes in this category")))
      }

      items <- lapply(seq_len(nrow(flat)), function(i) {
        row <- flat[i, ]
        tag_label <- if (!is.null(NOTE_TAGS[[row$category]])) {
          NOTE_TAGS[[row$category]]$label
        } else {
          row$category
        }

        ts_display <- tryCatch(
          format(as.POSIXct(row$timestamp), "%d %b %Y %H:%M"),
          error = function(e) row$timestamp
        )

        tags$div(
          class = "notes-feed-item",
          onclick = sprintf(
            "Shiny.setInputValue('navigate_to_student','%s',{priority:'event'});Shiny.setInputValue('active_view','student',{priority:'event'});",
            row$student_id
          ),
          tags$div(
            class = "notes-feed-item-header",
            tags$span(class = "notes-feed-student",
              paste0(row$name, " (", row$student_id, ")")
            ),
            tags$span(class = paste0("notes-tag-badge notes-tag-", row$category), tag_label),
            tags$span(class = "notes-feed-timestamp", ts_display)
          ),
          tags$div(class = "notes-feed-text", row$text)
        )
      })

      tagList(items)
    })
  })
}
