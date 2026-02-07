# NIGHTMARE - Student Data Management
# Main application entry point

# Load all dependencies first (before UI/server which use bslib, shinyjs, etc.)
source("R/dependencies.R")
load_nightmare_dependencies()

# Config needed by both UI (footer) and server
source("R/config.R")

# Source UI and server components
source("R/ui.R")
source("R/server.R")

# Create and run the Shiny app
shinyApp(ui = ui, server = server)
