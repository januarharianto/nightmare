#' @keywords internal
# -- plans_module.R -------------------------------------------------
# Sidebar view: browse all academic plans with category filtering.

plansModuleUI <- function(id) {
  ns <- NS(id)
  ns_id <- ns("group_filter")

  # Build filter buttons: All + 6 plan groups
  filter_buttons <- tagList(
    tags$button(
      class = "plans-filter-btn toggle-btn active",
      `data-value` = "all",
      onclick = sprintf(
        "document.querySelectorAll('.plans-filter-btn').forEach(function(b){b.classList.remove('active')});this.classList.add('active');Shiny.setInputValue('%s','all',{priority:'event'});",
        ns_id
      ),
      "All"
    ),
    lapply(PLAN_GROUPS, function(grp) {
      tags$button(
        class = "plans-filter-btn toggle-btn",
        `data-value` = grp,
        onclick = sprintf(
          "document.querySelectorAll('.plans-filter-btn').forEach(function(b){b.classList.remove('active')});this.classList.add('active');Shiny.setInputValue('%s','%s',{priority:'event'});",
          ns_id, grp
        ),
        PLAN_GROUP_LABELS[[grp]]
      )
    })
  )

  tags$div(class = "plans-view view-container",
    tags$div(class = "plans-search-section",
      tags$label("Search", class = "search-label meta-label", `for` = ns("search_box")),
      textInput(ns("search_box"), label = NULL,
                placeholder = "Search by Name, SID, or Unikey...", width = "100%")
    ),
    tags$div(class = "plans-toolbar toolbar",
      tags$span(class = "plans-label meta-label", "Category"),
      tags$div(class = "plans-filter-tags toggle-group", filter_buttons)
    ),
    uiOutput(ns("plan_stats")),
    tags$div(class = "plans-list-container scroll-container",
      uiOutput(ns("plans_list"))
    )
  )
}

plansModuleServer <- function(id, studentData, dataSources) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    groupFilter <- reactive(input$group_filter %||% "all")
    searchTermRaw <- reactive({
      term <- input$search_box
      if (is.null(term) || term == "") return("")
      tolower(trimws(term))
    })
    searchTerm <- searchTermRaw |> debounce(300)

    plansFlat <- reactive({
      flatten_all_plans(studentData())
    })

    filteredPlans <- reactive({
      flat <- plansFlat()
      grp <- groupFilter()
      if (grp != "all" && nrow(flat) > 0) {
        flat <- flat[flat$group == grp, , drop = FALSE]
      }
      q <- searchTerm()
      if (nzchar(q) && nrow(flat) > 0) {
        match <- grepl(q, flat$name_lower, fixed = TRUE) |
          grepl(q, flat$sid_lower, fixed = TRUE) |
          grepl(q, flat$login_lower, fixed = TRUE)
        flat <- flat[match, , drop = FALSE]
      }
      flat
    })

    output$plan_stats <- renderUI({
      flat <- plansFlat()
      filtered <- filteredPlans()
      data <- studentData()
      grp <- groupFilter()

      total_students <- if (!is.null(data)) nrow(data) else 0
      with_plans <- length(unique(flat$student_id))
      showing <- length(unique(filtered$student_id))
      adj_count <- nrow(filtered)

      showing_label <- if (grp == "all") "Showing" else PLAN_GROUP_LABELS[[grp]]

      tags$div(class = "plans-summary summary-bar",
        tags$div(class = "stat-item",
          tags$span(class = "stat-label", "With Plans"),
          tags$span(class = "stat-value", with_plans)
        ),
        tags$div(class = "stat-item",
          tags$span(class = "stat-label", "Total"),
          tags$span(class = "stat-value", total_students)
        ),
        tags$div(class = "stat-item",
          tags$span(class = "stat-label", showing_label),
          tags$span(class = "stat-value", showing)
        ),
        tags$div(class = "stat-item",
          tags$span(class = "stat-label", "Adjustments"),
          tags$span(class = "stat-value", adj_count)
        )
      )
    })

    output$plans_list <- renderUI({
      sources <- dataSources()
      if (!isTRUE(sources$plans)) {
        return(tags$div(class = "empty-state",
          tags$p("No academic plans data available")))
      }

      filtered <- filteredPlans()
      if (nrow(filtered) == 0) {
        q <- searchTerm()
        grp <- groupFilter()
        msg <- if (nzchar(q)) {
          paste0("No students matching '", q, "'")
        } else if (grp != "all") {
          paste0("No students with ", PLAN_GROUP_LABELS[[grp]], " adjustments")
        } else {
          "No students with academic plans"
        }
        return(tags$div(class = "empty-state", tags$p(msg)))
      }

      # Split once, sort by name
      by_student <- split(filtered, filtered$student_id)
      student_names <- vapply(by_student, function(df) df$name[1], character(1))
      by_student <- by_student[order(student_names)]

      items <- lapply(by_student, function(srows) {
        sid <- srows$student_id[1]
        sname <- srows$name[1]
        login <- srows$sis_login_id[1]
        adj_count <- nrow(srows)

        # Build one line per group present for this student
        groups_present <- intersect(PLAN_GROUPS, unique(srows$group))

        group_lines <- lapply(groups_present, function(grp) {
          grows <- srows[srows$group == grp, , drop = FALSE]
          tags$div(class = "plans-item-group",
            tags$span(class = "plans-item-group-label meta-label", PLAN_GROUP_LABELS[[grp]]),
            tags$span(class = "plans-item-group-detail",
              paste(grows$display_detail, collapse = ", "))
          )
        })

        tags$div(
          class = "plans-list-item list-item",
          tags$div(class = "plans-item-header",
            tags$span(class = "plans-item-name", sname),
            tags$span(class = "plans-item-meta",
              paste0(sid, if (nzchar(login)) paste0(" / ", login) else "")
            ),
            tags$span(class = "plans-item-count meta-label",
              paste0(adj_count, " adj")
            ),
            tags$button(
              class = "plans-view-btn",
              onclick = sprintf(
                "event.stopPropagation();Shiny.setInputValue('navigate_to_student','%s',{priority:'event'});Shiny.setInputValue('active_view','student',{priority:'event'});",
                sid
              ),
              "View"
            )
          ),
          tagList(group_lines)
        )
      })

      tagList(items)
    })
  })
}
