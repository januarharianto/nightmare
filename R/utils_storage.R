# -- storage.R ----------------------------------------------------
# Shared persistence utilities: directory setup and JSON save.

# Ensure .nightmare/ directory exists inside an offering folder.
# Returns the path to the .nightmare/ directory.
ensure_nightmare_dir_for_path <- function(offering_path) {
  nightmare_dir <- file.path(offering_path, ".nightmare")
  if (!dir.exists(nightmare_dir)) dir.create(nightmare_dir, recursive = TRUE)
  nightmare_dir
}

# Ensure .nightmare/ directory exists inside a unit's data folder.
# Returns the path to the .nightmare/ directory.
ensure_nightmare_dir <- function(data_dir, unit) {
  ensure_nightmare_dir_for_path(file.path(data_dir, unit))
}

# Write a payload list to a JSON file with standard formatting.
save_json <- function(path, payload) {
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", pretty = TRUE), path)
  invisible(path)
}

# Load JSON from an offering .nightmare/ directory with error fallback.
load_json_for_path <- function(offering_path, filename, default) {
  path <- file.path(offering_path, ".nightmare", filename)
  if (!file.exists(path)) return(default)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) default)
}

# Load JSON from .nightmare/ directory with error fallback.
load_json <- function(data_dir, unit, filename, default) {
  load_json_for_path(file.path(data_dir, unit), filename, default)
}

# Save payload as JSON to an offering .nightmare/ directory with version envelope.
save_nightmare_json_for_path <- function(offering_path, filename, payload, version = 1L) {
  nightmare_dir <- ensure_nightmare_dir_for_path(offering_path)
  payload$version <- version
  payload$saved_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  save_json(file.path(nightmare_dir, filename), payload)
}

# Save payload as JSON to .nightmare/ directory with version envelope.
save_nightmare_json <- function(data_dir, unit, filename, payload, version = 1L) {
  save_nightmare_json_for_path(file.path(data_dir, unit), filename, payload, version)
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
