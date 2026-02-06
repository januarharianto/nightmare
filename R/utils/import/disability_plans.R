#' Disability Plans Import for NIGHTMARE
#'
#' Import and process disability plans Excel files

library(readxl)

#' Import Disability Plans
#'
#' @param file_path Path to disability plans Excel file
#' @param unit_filter Optional unit code to filter by (e.g., "BIOL2022")
#' @return data.frame with student_id and disability plan data
import_disability_plans <- function(file_path, unit_filter = NULL) {
  message("Importing disability plans from: ", file_path)

  # Read Excel with no header (raw)
  raw_data <- read_excel(file_path, col_names = FALSE)

  # Extract actual headers from row 3 (index 3)
  actual_headers <- as.character(raw_data[3, ])

  # Make column names unique (handle duplicate "Arrangement" columns)
  actual_headers <- make.names(actual_headers, unique = TRUE)

  # Data starts at row 4 (index 4)
  data <- raw_data[4:nrow(raw_data), ]
  colnames(data) <- actual_headers

  # Convert to data frame and filter out empty rows
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data <- data[!is.na(data$SID) & data$SID != "", ]

  # Apply unit filter if provided
  if (!is.null(unit_filter)) {
    uos_col <- which(names(data) == "UoS.Code")
    if (length(uos_col) > 0) {
      data <- data[data[[uos_col]] == unit_filter, ]
    } else {
      warning("No 'UoS Code' column found for filtering by unit")
    }
  }

  # Identify adjustment columns (exclude metadata columns)
  # After make.names(), spaces become dots
  metadata_pattern <- "^(Year|Session|UoS\\.Code|SID|Preferred\\.Name|Family\\.Name|Unikey|Email|Name|Assessment|Date|W\\.|Category|AP\\.)"
  metadata_cols <- grep(metadata_pattern, names(data), value = TRUE)
  adjustment_cols <- setdiff(names(data), metadata_cols)

  # Process each student's plan
  plans_list <- lapply(1:nrow(data), function(i) {
    student_row <- data[i, ]
    student_id <- as.character(student_row$SID)
    student_name <- as.character(student_row$Preferred.Name)

    # Collect all non-empty adjustments
    adjustments_list <- lapply(adjustment_cols, function(col) {
      value <- student_row[[col]]
      if (!is.na(value) && value != "" && value != "NA") {
        data.frame(
          adjustment_type = col,
          description = as.character(value),
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })

    # Combine non-null adjustments
    adjustments_df <- do.call(rbind, Filter(Negate(is.null), adjustments_list))

    if (is.null(adjustments_df)) {
      adjustments_df <- data.frame(
        adjustment_type = character(),
        description = character(),
        stringsAsFactors = FALSE
      )
    }

    list(
      student_id = student_id,
      name = student_name,
      plan_adjustments = adjustments_df,
      has_disability_plan = TRUE
    )
  })

  # Convert to data frame format
  plans_by_student <- data.frame(
    student_id = sapply(plans_list, function(x) x$student_id),
    name = sapply(plans_list, function(x) x$name),
    has_disability_plan = sapply(plans_list, function(x) x$has_disability_plan),
    stringsAsFactors = FALSE
  )
  plans_by_student$plan_adjustments <- lapply(plans_list, function(x) x$plan_adjustments)

  message(sprintf("Imported disability plans for %d unique students", nrow(plans_by_student)))

  return(plans_by_student)
}
