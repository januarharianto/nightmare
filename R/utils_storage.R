#' @keywords internal
# -- storage.R ----------------------------------------------------
# Shared persistence utilities: directory setup and JSON save.

# Ensure .nightmare/ directory exists inside a unit's data folder.
# Returns the path to the .nightmare/ directory.
ensure_nightmare_dir <- function(data_dir, unit) {
  nightmare_dir <- file.path(data_dir, unit, ".nightmare")
  if (!dir.exists(nightmare_dir)) dir.create(nightmare_dir, recursive = TRUE)
  nightmare_dir
}

# Write a payload list to a JSON file with standard formatting.
save_json <- function(path, payload) {
  writeLines(toJSON(payload, auto_unbox = TRUE, null = "null", pretty = TRUE), path)
  invisible(path)
}

# Load JSON from .nightmare/ directory with error fallback.
load_json <- function(data_dir, unit, filename, default) {
  path <- file.path(data_dir, unit, ".nightmare", filename)
  if (!file.exists(path)) return(default)
  tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) default)
}

# Save payload as JSON to .nightmare/ directory with version envelope.
save_nightmare_json <- function(data_dir, unit, filename, payload, version = 1L) {
  nightmare_dir <- ensure_nightmare_dir(data_dir, unit)
  payload$version <- version
  payload$saved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  save_json(file.path(nightmare_dir, filename), payload)
}

# Bind list of data.frame rows, returning empty_df if result is NULL or empty.
rbind_or_empty <- function(rows, empty_df) {
  result <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(result) || nrow(result) == 0) empty_df else result
}

# Factory: empty special_consids data.frame schema.
empty_consids_df <- function() {
  data.frame(
    ticket_id = character(), assessment_name = character(),
    assessment_title = character(), assessment_category = character(),
    assessment_type = character(), outcome_type = character(),
    classification = character(), state = character(),
    approved = logical(), extension_date = as.Date(character()),
    due_date = as.POSIXct(character()), closing_date = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
}

# Factory: empty plan_adjustments data.frame schema.
empty_plan_adjustments_df <- function() {
  data.frame(
    category = character(), arrangement_type = character(),
    value = character(), stringsAsFactors = FALSE
  )
}
