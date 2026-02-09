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
