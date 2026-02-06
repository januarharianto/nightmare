# Search Module for NIGHTMARE
# Extracted from server.R lines 73-160

searchModuleUI <- function(id) {
  ns <- NS(id)
  div(class = "search-container",
    textInput(ns("search_box_left"), label = NULL,
              placeholder = "Search by Name, SID, or Unikey...", width = "100%"),
    # Dropdown container (hidden by default, shown when results exist)
    uiOutput(ns("search_dropdown"))
  )
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

    # Render search dropdown (lines 108-149)
    output$search_dropdown <- renderUI({
      results <- searchResults()

      # Hide dropdown when no search term or no results
      if (is.null(results) || nrow(results) == 0) {
        if (searchTerm() == "") {
          return(NULL)
        } else {
          # Show "No students found" in dropdown
          return(
            div(class = "search-dropdown",
              div(class = "search-dropdown-items",
                div(class = "search-dropdown-item search-dropdown-empty",
                  "No students found"
                )
              )
            )
          )
        }
      }

      # Limit to max 8 items
      total_results <- nrow(results)
      display_count <- min(8, total_results)
      show_more_indicator <- total_results > 8

      # Generate dropdown items
      result_items <- lapply(1:display_count, function(i) {
        div(
          class = "search-dropdown-item",
          `data-index` = i,
          onclick = paste0("Shiny.setInputValue('", session$ns("selected_result_index"), "', ", i, ", {priority: 'event'})"),

          # Name and SID on same line
          div(class = "dropdown-item-header",
            tags$span(class = "dropdown-item-name", results$Name[i]),
            tags$span(class = "dropdown-item-sid", paste0("SID: ", results$SID[i]))
          ),

          # Unikey below
          div(class = "dropdown-item-unikey",
            paste0("Unikey: ", results$Unikey[i])
          )
        )
      })

      # Add "Showing N of X" indicator if needed
      if (show_more_indicator) {
        result_items <- c(result_items, list(
          div(class = "search-dropdown-more",
            sprintf("Showing %d of %d matches", display_count, total_results)
          )
        ))
      }

      # Dropdown container with keyboard navigation JavaScript
      tagList(
        div(class = "search-dropdown",
          div(class = "search-dropdown-items", result_items)
        ),

        # Inline JavaScript for keyboard navigation
        tags$script(HTML(sprintf("
          (function() {
            var searchInput = document.getElementById('%s');
            var dropdown = searchInput.nextElementSibling;
            var currentIndex = -1;
            var itemCount = %d;

            // Debounce timer
            var debounceTimer = null;

            // Highlight item by index
            function highlightItem(index) {
              var items = dropdown.querySelectorAll('.search-dropdown-item:not(.search-dropdown-empty):not(.search-dropdown-more)');

              // Remove previous highlight
              items.forEach(function(item) {
                item.classList.remove('active');
              });

              // Add highlight to current item
              if (index >= 0 && index < items.length) {
                items[index].classList.add('active');
                // Scroll into view if needed
                items[index].scrollIntoView({ block: 'nearest', behavior: 'smooth' });
              }
            }

            // Close dropdown
            function closeDropdown() {
              if (dropdown) {
                dropdown.style.display = 'none';
              }
              currentIndex = -1;
            }

            // Open dropdown
            function openDropdown() {
              if (dropdown && dropdown.querySelector('.search-dropdown-items').children.length > 0) {
                dropdown.style.display = 'block';
              }
            }

            // Keyboard event handler
            searchInput.addEventListener('keydown', function(e) {
              var items = dropdown.querySelectorAll('.search-dropdown-item:not(.search-dropdown-empty):not(.search-dropdown-more)');

              if (items.length === 0) return;

              switch(e.key) {
                case 'ArrowDown':
                  e.preventDefault();
                  currentIndex = (currentIndex + 1) %% items.length;
                  highlightItem(currentIndex);
                  openDropdown();
                  break;

                case 'ArrowUp':
                  e.preventDefault();
                  currentIndex = currentIndex <= 0 ? items.length - 1 : currentIndex - 1;
                  highlightItem(currentIndex);
                  openDropdown();
                  break;

                case 'Enter':
                  e.preventDefault();
                  if (currentIndex >= 0 && currentIndex < items.length) {
                    items[currentIndex].click();
                    closeDropdown();
                  }
                  break;

                case 'Escape':
                case 'Tab':
                  closeDropdown();
                  break;
              }
            });

            // Show dropdown on input (with debounce)
            searchInput.addEventListener('input', function() {
              clearTimeout(debounceTimer);
              debounceTimer = setTimeout(function() {
                openDropdown();
                currentIndex = -1;
              }, 150);
            });

            // Close dropdown on click outside
            document.addEventListener('click', function(e) {
              if (!searchInput.contains(e.target) && !dropdown.contains(e.target)) {
                closeDropdown();
              }
            });

            // Open dropdown on focus if there's content
            searchInput.addEventListener('focus', function() {
              if (searchInput.value.length > 0) {
                openDropdown();
              }
            });
          })();
        ", session$ns("search_box_left"), display_count)))
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
          # Clear search box after selection
          updateTextInput(session, "search_box_left", value = "")
        }
      }
    })

    return(selectedStudentId)
  })
}
