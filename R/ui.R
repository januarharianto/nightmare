# UI Definition for NIGHTMARE - Clinical Minimal Design

library(shiny)
library(bslib)
library(shinyjs)

# Source modules
source("R/modules/search_module.R")

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
        "New Incredibly Glitchy Hacking Tool to Manage Records that Enrage me | Januar Harianto"
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
    )
  ),

  # Dataset metadata panel (top bar)
  uiOutput("dataset_metadata_panel"),

  # Main container with two-column layout or placeholder
  uiOutput("main_content_output"),

)
