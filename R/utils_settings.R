#' @keywords internal
settings_path <- function() {
  file.path(tools::R_user_dir("nightmare", "config"), "settings.json")
}

#' @keywords internal
read_settings <- function() {
  defaults <- list(data_dir = NULL)
  path <- settings_path()
  if (!file.exists(path)) return(defaults)
  saved <- tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) list())
  modifyList(defaults, saved)
}

#' @keywords internal
save_settings <- function(settings) {
  tryCatch({
    path <- settings_path()
    dir <- dirname(path)
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    save_json(path, settings)
  }, error = function(e) {
    warning("Could not save settings: ", e$message)
  })
}
