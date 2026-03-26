#' @import shiny
#' @import dplyr
#' @importFrom bslib bs_theme font_collection page_fillable
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract str_replace str_trim
#' @importFrom shinyjs useShinyjs runjs
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom htmltools htmlDependency tags tagList HTML
#' @importFrom rlang .data
#' @importFrom stats density ecdf median sd
#' @importFrom graphics abline axis hist par plot plot.new text
#' @importFrom shinyFiles shinyDirButton shinyDirChoose parseDirPath
NULL

#' Run the NIGHTMARE Shiny app
#'
#' @param ... Arguments passed to \code{\link[shiny]{runApp}}
#' @export
run_nightmare <- function(...) {
  shiny::runApp(system.file("app", package = "nightmare"), ...)
}
