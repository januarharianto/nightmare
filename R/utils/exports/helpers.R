# Export Helper Functions for NIGHTMARE

#' Helper: Format risk factors for display
#'
#' @param risk_factors_list Character vector or list of risk factors
#' @return Character string with semicolon-separated factors
#' @export
format_risk_factors <- function(risk_factors_list) {
  # INPUT: character vector of risk factors
  # OUTPUT: semicolon-separated string suitable for CSV

  if (is.null(risk_factors_list) || length(risk_factors_list) == 0) {
    return("None")
  }

  # Handle nested lists (from list columns)
  if (is.list(risk_factors_list) && !is.data.frame(risk_factors_list)) {
    risk_factors_list <- unlist(risk_factors_list)
  }

  # Remove NAs and empty strings
  risk_factors_list <- risk_factors_list[!is.na(risk_factors_list) & risk_factors_list != ""]

  if (length(risk_factors_list) == 0) {
    return("None")
  }

  # Join with semicolon
  paste(risk_factors_list, collapse = "; ")
}
