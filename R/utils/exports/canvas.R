# Canvas Grade Export for NIGHTMARE

library(dplyr)

#' Export for Canvas Bulk Grade Upload
#'
#' @param student_data Consolidated student data.frame
#' @return data.frame ready for CSV export to Canvas
#' @export
export_for_canvas <- function(student_data) {
  # Export for Canvas bulk grade upload
  # COLUMNS: SIS Login ID, Student Name, Final Grade
  # PURPOSE: Upload to Canvas for grade recording
  # NOTES: Extensions are NOT imported to Canvas grades

  canvas_export <- student_data %>%
    select(
      `SIS Login ID` = sis_login_id,
      `Student Name` = name,
      `Final Grade` = final_grade
    ) %>%
    arrange(`SIS Login ID`)

  return(canvas_export)
}
