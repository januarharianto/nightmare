#' File Type Detection for NIGHTMARE
#'
#' Detect data source type based on column signatures

#' Detect file type based on column signatures
#'
#' @param file_path Path to CSV or Excel file
#' @return Character: "canvas", "special_consids", "plans", or "unknown"
detect_file_type <- function(file_path) {
  # Determine file format
  ext <- tolower(tools::file_ext(file_path))

  tryCatch({
    # Read first 100 rows
    if (ext %in% c("xlsx", "xls")) {
      sample_data <- read_excel(file_path, n_max = 100, col_names = FALSE)
      headers <- as.character(sample_data[3, ])  # Plans have headers in row 3
    } else {
      sample_data <- read_csv(file_path, n_max = 100, show_col_types = FALSE)
      headers <- names(sample_data)
    }

    # Check signatures
    # Canvas: Student, ID, SIS User ID, SIS Login ID, Section
    canvas_sig <- c("Student", "ID", "SIS User ID", "SIS Login ID", "Section")
    if (all(canvas_sig %in% headers)) {
      return("canvas")
    }

    # Special Consids: number, state, availability, student_id variations
    consid_sig <- c("number", "state", "availability")
    if (all(consid_sig %in% headers) || any(grepl("student.*id", headers, ignore.case = TRUE))) {
      return("special_consids")
    }

    # Plans: Year, Session, UoS Code, SID (in row 3 for Excel)
    plans_sig <- c("Year", "Session", "UoS Code", "SID")
    if (all(plans_sig %in% headers)) {
      return("plans")
    }

    return("unknown")
  }, error = function(e) {
    warning(paste("Error detecting file type:", e$message))
    return("unknown")
  })
}
