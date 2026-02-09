# -- dependencies.R ------------------------------------------------
# Central library loading — all library() calls live here.

load_nightmare_dependencies <- function() {
  library(shiny)
  library(bslib)
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(shinyjs)
  library(jsonlite)
}
