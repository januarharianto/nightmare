# -- disability_plans.R --------------------------------------------
# Import and process disability plans Excel files (headers row 3).

#' Import Disability Plans
#' @keywords internal
#'
#' @param file_path Path to disability plans Excel file
#' @param unit_filter Optional unit code to filter by (e.g., "BIOL2022")
#' @param year_filter Optional year to filter by (e.g., "2024")
#' @return data.frame with student_id and disability plan data
import_disability_plans <- function(file_path, unit_filter = NULL, year_filter = NULL) {
  message("Importing disability plans from: ", file_path)

  # Read first 2 rows to get real headers (categories + arrangement names)
  header_rows <- read_excel(file_path, col_names = FALSE, n_max = 2,
                            .name_repair = "minimal")
  categories <- as.character(header_rows[1, ])       # Row 1 = category names
  arrangement_names <- as.character(header_rows[2, ]) # Row 2 = arrangement names

  # Read the full file with no header to get row 3 headers for metadata columns
  raw_data <- read_excel(file_path, col_names = FALSE, .name_repair = "minimal")

  # Extract row 3 headers (used for metadata column identification)
  row3_headers <- as.character(raw_data[3, ])
  row3_headers <- make.names(row3_headers, unique = TRUE)

  # Data starts at row 4 (index 4)
  data <- raw_data[4:nrow(raw_data), ]
  colnames(data) <- row3_headers

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

  # Apply year filter if provided
  if (!is.null(year_filter)) {
    data <- data[data$Year == year_filter, ]
    message(sprintf("Filtered plans to %d rows for year %s", nrow(data), year_filter))
  }

  # Identify arrangement columns (cols 16 onward are arrangements)
  # Metadata columns are 1-15
  n_metadata <- min(15, ncol(data))
  arrangement_indices <- seq(n_metadata + 1, ncol(data))

  # Filter values to exclude
  exclude_pattern <- "not required|date has passed"

  # Process each student's plan
  plans_list <- lapply(1:nrow(data), function(i) {
    student_row <- data[i, ]
    student_id <- as.character(student_row$SID)
    student_name <- as.character(student_row$Preferred.Name)

    # Pivot arrangement columns into tidy data frame
    adjustments_list <- lapply(arrangement_indices, function(col_idx) {
      value <- as.character(student_row[[col_idx]])
      if (!is.na(value) && value != "" && value != "NA" &&
          !grepl(exclude_pattern, value, ignore.case = TRUE)) {
        data.frame(
          category = ifelse(is.na(categories[col_idx]), "", categories[col_idx]),
          arrangement_type = ifelse(is.na(arrangement_names[col_idx]), "", arrangement_names[col_idx]),
          value = value,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })

    # Combine non-null adjustments
    adjustments_df <- rbind_or_empty(adjustments_list, empty_plan_adjustments_df())

    if (nrow(adjustments_df) == 0) {
      adjustments_df <- empty_plan_adjustments_df()
    } else {
      # Deduplicate adjustments (same arrangement_type + value)
      adjustments_df <- adjustments_df[!duplicated(adjustments_df[, c("arrangement_type", "value")]), ]
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

  # Group by student_id to deduplicate (keep first row per student)
  plans_by_student <- plans_by_student[!duplicated(plans_by_student$student_id), ]

  message(sprintf("Grouped plans to %d unique students", nrow(plans_by_student)))

  return(plans_by_student)
}
