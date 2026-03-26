# Thin shim — launches the nightmare Shiny app.
# Run via nightmare::run_nightmare(), not directly.
shinyApp(ui = nightmare::app_ui(), server = nightmare::app_server)
