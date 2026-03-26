# -- ui_student_detail.R -------------------------------------------
# Student detail view: banner + orchestrator calling 4 card builders.

build_student_detail_banner <- function(student, all_students = NULL) {
  # Compute position in class for nav (alphabetical by surname)
  student_idx <- NULL
  total_students <- 0L
  if (!is.null(all_students) && nrow(all_students) > 0) {
    sorted <- all_students[order(all_students$name), ]
    all_sids <- as.character(sorted$student_id)
    current_sid <- as.character(student$student_id)
    student_idx <- which(all_sids == current_sid)
    total_students <- length(all_sids)
    if (length(student_idx) == 0) student_idx <- NULL else student_idx <- student_idx[1]
  }

  # Prev/next SIDs
  prev_sid <- if (!is.null(student_idx) && student_idx > 1) all_sids[student_idx - 1] else NULL
  next_sid <- if (!is.null(student_idx) && student_idx < total_students) all_sids[student_idx + 1] else NULL

  tagList(
    # Banner header with nav
    tags$div(
      class = "student-detail-header",
      tags$div(class = "student-banner-top",
        tags$div(class = "student-banner-name", student$name),
        tags$div(class = "student-banner-nav",
          tags$button(
            class = "student-nav-btn",
            disabled = if (is.null(prev_sid)) "disabled" else NULL,
            onclick = if (!is.null(prev_sid)) sprintf(
              "Shiny.setInputValue('navigate_to_student', '%s', {priority: 'event'})", prev_sid
            ) else "",
            "\u2190"
          ),
          if (!is.null(student_idx)) {
            tags$span(class = "student-nav-count",
              sprintf("%d / %d", student_idx, total_students)
            )
          },
          tags$button(
            class = "student-nav-btn",
            disabled = if (is.null(next_sid)) "disabled" else NULL,
            onclick = if (!is.null(next_sid)) sprintf(
              "Shiny.setInputValue('navigate_to_student', '%s', {priority: 'event'})", next_sid
            ) else "",
            "\u2192"
          )
        )
      ),
      tags$div(class = "student-banner-meta",
        tags$div(class = "student-banner-field",
          tags$span(class = "student-banner-label", "SID"),
          tags$span(class = "student-banner-value", as.character(student$student_id))
        ),
        tags$div(class = "student-banner-field",
          tags$span(class = "student-banner-label", "Email"),
          tags$span(class = "student-banner-value", as.character(student$email))
        ),
        tags$div(class = "student-banner-field",
          tags$span(class = "student-banner-label", "Unikey"),
          tags$span(class = "student-banner-value", as.character(student$sis_login_id))
        )
      )
    ),

    # Cards grid
    tags$div(
      class = "detail-cards-grid",
      uiOutput("card_assessments"),
      uiOutput("card_consids"),
      uiOutput("card_plans"),
      uiOutput("card_notes")
    )
  )
}
