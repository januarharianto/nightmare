# NIGHTMARE - Student Data Management
# Main application entry point

library(shiny)

# Source UI and server components
source("R/ui.R")
source("R/server.R")

# Create and run the Shiny app
shinyApp(ui = ui, server = server)
