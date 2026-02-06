# Storage utility functions for NIGHTMARE
# Handles IndexedDB and localStorage interactions

#' Sync data to IndexedDB storage
#' @param data Data frame to store
#' @return TRUE if successful, FALSE otherwise
syncToStorage <- function(data) {
  tryCatch({
    # Stub for now - will implement JS interop for IndexedDB
    # For Phase 1, we'll just validate the function exists

    # TODO: Implement IndexedDB storage using webR IDBFS
    # session$sendCustomMessage("syncToIndexedDB", list(data = data))

    # Fallback to localStorage for small datasets
    saveToLocalStorage(data)

    return(TRUE)
  }, error = function(e) {
    warning(paste("Storage sync failed:", e$message))
    return(FALSE)
  })
}

#' Load data from IndexedDB storage
#' @return Data frame if successful, NULL otherwise
loadFromStorage <- function() {
  tryCatch({
    # Stub for now - will implement JS interop for IndexedDB

    # TODO: Implement IndexedDB retrieval using webR IDBFS
    # session$sendCustomMessage("loadFromIndexedDB", list())

    return(NULL)
  }, error = function(e) {
    warning(paste("Storage load failed:", e$message))
    return(NULL)
  })
}

#' Save data to localStorage (fallback for small datasets)
#' @param data Data frame to store
#' @return TRUE if successful, FALSE otherwise
saveToLocalStorage <- function(data) {
  tryCatch({
    # Validate data size (localStorage has ~5-10MB limit)
    data_size <- object.size(data)
    max_size <- 5 * 1024 * 1024  # 5MB

    if (data_size > max_size) {
      warning("Data too large for localStorage fallback")
      return(FALSE)
    }

    # Stub for now - will implement JS interop
    # TODO: Implement localStorage using session$sendCustomMessage

    return(TRUE)
  }, error = function(e) {
    warning(paste("localStorage save failed:", e$message))
    return(FALSE)
  })
}

#' Validate student data structure
#' @param data Data frame to validate
#' @return List with valid (TRUE/FALSE) and message
validateStudentData <- function(data) {
  required_cols <- c("student_id", "name", "email", "canvas_id", "section", "final_grade")

  # Check if data is a data frame
  if (!is.data.frame(data)) {
    return(list(valid = FALSE, message = "Data is not a data frame"))
  }

  # Check for required columns
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    return(list(
      valid = FALSE,
      message = paste("Missing columns:", paste(missing_cols, collapse = ", "))
    ))
  }

  # Check for empty data
  if (nrow(data) == 0) {
    return(list(valid = FALSE, message = "Data is empty"))
  }

  return(list(valid = TRUE, message = "Data validation passed"))
}
