# UI Definition for NIGHTMARE - Clinical Minimal Design

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    base_font = font_collection(
      "-apple-system", "BlinkMacSystemFont",
      "Segoe UI", "Arial", "sans-serif"
    ),
    bg = "#FFFFFF",
    fg = "#000000"
  ),

  # Custom CSS and shinyjs
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "search-keyboard.js")
  ),

  # Top Navbar
  tags$div(
    class = "navbar-nightmare",

    # Logo section with subtitle and version
    tags$div(
      class = "navbar-logo-section",
      tags$div(
        class = "navbar-title",
        tags$span(class = "app-name", "NIGHTMARE"),
        tags$span(class = "app-version", "v0.0.9000")
      ),
      tags$div(
        class = "navbar-subtitle",
        HTML("<b>N</b>ew <b>I</b>nredibly <b>G</b>litchy <b>H</b>acking <b>T</b>ool to <b>M</b>anage <b>A</b>ll <b>R</b>ecords <b>E</b>ffortlessly... by Januar Harianto")
      )
    ),
  ),

  # Sidebar navigation
  tags$div(
    class = "sidebar-navigation",
    tags$div(
      class = "sidebar-item active",
      `data-view` = "student",
      onclick = "Shiny.setInputValue('active_view', 'student', {priority: 'event'})",
      "Students"
    ),
    tags$div(
      class = "sidebar-item",
      `data-view` = "extensions",
      onclick = "Shiny.setInputValue('active_view', 'extensions', {priority: 'event'})",
      "Extensions"
    ),
    tags$div(
      class = "sidebar-item",
      `data-view` = "assessments",
      onclick = "Shiny.setInputValue('active_view', 'assessments', {priority: 'event'})",
      "Assessments"
    )
  ),

  # Dataset metadata panel (top bar)
  uiOutput("dataset_metadata_panel"),

  # Main container with two-column layout or placeholder
  uiOutput("main_content_output"),

  # Footer
  tags$footer(
    class = "app-footer",
    tags$span(paste0("NIGHTMARE v", NIGHTMARE_CONFIG$ui$version)),
    tags$span(class = "footer-sep", "|"),
    tags$span(paste0("\u00A9 ", format(Sys.Date(), "%Y"), " ", NIGHTMARE_CONFIG$ui$author)),
    tags$span(class = "footer-sep", "|"),
    tags$span("MIT License")
  )
)
