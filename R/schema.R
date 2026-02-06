#' Student Data Schema for NIGHTMARE
#' Consolidated structure holding all student information
#'
#' This schema documents the complete structure of student records
#' after consolidating data from Canvas, Special Considerations, and Disability Plans.

student_schema <- list(
  # === Identity ===
  student_id = character(),           # SIS Student ID (e.g., "530485801") - PRIMARY KEY
  sis_login_id = character(),         # unikey (e.g., "lsax8243")
  canvas_id = integer(),              # Canvas internal ID
  name = character(),                 # Display name
  email = character(),                # Student email (from Canvas)

  # === Academic Performance (Canvas) ===
  unit_of_study = character(),        # Unit code (e.g., "BIOL2022")
  section = character(),              # Full Section string from Canvas (includes extension notes)
  final_grade = numeric(),            # Overall final score (0-100)

  # === Assignment Details (nested) ===
  assignments = list(),               # data.frame: name, score, max_points, weight, submission_status
  category_scores = list(),           # data.frame: category_name, current_score, final_score

  # === Special Considerations (nested) ===
  special_consids = list(),           # data.frame: ticket_id, assessment_name, outcome_type, extension_date, state, approved

  # === Disability/Accessibility Plans (nested) ===
  has_disability_plan = logical(),    # TRUE if student has any plan in plans.xlsx
  plan_adjustments = list(),          # data.frame: adjustment_type, description (e.g., extra time, rest breaks)

  # === Calculated Fields ===
  total_approved_extension_days = integer(),  # Sum of all calendar day extensions
  has_replacement_exam = logical(),
  has_mark_adjustment = logical(),

  # === Metadata ===
  last_updated = as.POSIXct(character()),
  data_sources = list()               # Which files contributed data
)
