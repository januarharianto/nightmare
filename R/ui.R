# UI Definition for NIGHTMARE - Clinical Minimal Design

library(shiny)
library(bslib)
library(DT)
library(shinyjs)

# Source modules
source("R/modules/dashboard_module.R")
source("R/modules/student_detail_module.R")
source("R/modules/backup_module.R")
source("R/modules/export_module.R")
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
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
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

    # Controls
    tags$div(
      class = "controls",
      actionButton(
        "undo_action",
        "Undo",
        icon = icon("undo")
      ),
      actionButton(
        "redo_action",
        "Redo",
        icon = icon("redo")
      ),
      actionButton(
        "backup_action",
        "Backup",
        icon = icon("save")
      ),
      actionButton(
        "export_action",
        "Export",
        icon = icon("download")
      )
    )
  ),

  # Sidebar navigation
  tags$div(
    class = "sidebar-navigation",
    tags$div(
      class = "sidebar-item",
      `data-view` = "student",
      onclick = "Shiny.setInputValue('active_view', 'student', {priority: 'event'})",
      class = "sidebar-item active",
      tags$span(class = "sidebar-icon", icon("users")),
      tags$span(class = "sidebar-label", "Students")
    ),
    tags$div(
      class = "sidebar-item",
      `data-view` = "extensions",
      onclick = "Shiny.setInputValue('active_view', 'extensions', {priority: 'event'})",
      tags$span(class = "sidebar-icon", icon("file-contract")),
      tags$span(class = "sidebar-label", "Extensions")
    )
  ),

  # Dataset metadata panel (top bar)
  uiOutput("dataset_metadata_panel"),

  # Main container with two-column layout or placeholder
  uiOutput("main_content_output"),

  # Hidden download button for export
  tags$div(
    style = "display: none;",
    downloadButton("download_export", "Download")
  )
)
