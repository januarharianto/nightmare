# -- plans_data.R ---------------------------------------------------
# Shared utilities for plan data: grouping, formatting, flattening.

PLAN_GROUPS <- c("Extensions", "Exam Accommodations", "Presentation",
                 "Classroom Support", "Placement", "Other")

# Classify a single plan adjustment into one of the 6 groups.
# Priority order matches the mask chain in build_plans_card().
classify_plan_adjustment <- function(category, arrangement_type) {
  if (category == "Assessment Adjustment" &&
      grepl("Extension|Take Home", arrangement_type, ignore.case = TRUE)) {
    return("Extensions")
  }
  if (grepl("Exam Adjustment", category, ignore.case = TRUE)) {
    return("Exam Accommodations")
  }
  if (grepl("Content Not|Present To|Group Presentations", arrangement_type, ignore.case = TRUE)) {
    return("Presentation")
  }
  if (category == "In-Class Support & Management") {
    return("Classroom Support")
  }
  if (grepl("Placement", category, ignore.case = TRUE)) {
    return("Placement")
  }
  "Other"
}

# Clean arrangement name for display (strips Pro Rata, Minutes/hour, Online prefix).
clean_arrangement_name <- function(name) {
  name <- gsub("\\s*\\(Pro Rata\\)\\s*\\(Minutes/hour\\)", "", name)
  name <- gsub("\\s*\\(Pro Rata\\)", "", name)
  name <- gsub("\\s*\\(Minutes/hour\\)", "", name)
  name <- gsub("^Online - ", "", name)
  trimws(name)
}

# Format plan value for display.
format_plan_value <- function(val, arrangement) {
  v <- trimws(val)
  if (grepl("Minutes/hour", arrangement, ignore.case = TRUE)) {
    return(paste0(v, " min/hour"))
  }
  v
}

# Flatten all plan adjustments across students into a single data.frame.
# Returns: student_id, name, sis_login_id, category, arrangement_type, value, group
flatten_all_plans <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame(
      student_id = character(), name = character(), sis_login_id = character(),
      category = character(), arrangement_type = character(),
      value = character(), group = character(),
      stringsAsFactors = FALSE
    ))
  }

  plan_students <- data[isTRUE(data$has_disability_plan) |
                        vapply(data$has_disability_plan, isTRUE, logical(1)), ]

  if (nrow(plan_students) == 0) {
    return(data.frame(
      student_id = character(), name = character(), sis_login_id = character(),
      category = character(), arrangement_type = character(),
      value = character(), group = character(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- list()
  for (i in seq_len(nrow(plan_students))) {
    student <- plan_students[i, ]
    adj <- student$plan_adjustments[[1]]
    if (is.null(adj) || nrow(adj) == 0) next

    for (j in seq_len(nrow(adj))) {
      grp <- classify_plan_adjustment(adj$category[j], adj$arrangement_type[j])
      rows[[length(rows) + 1L]] <- data.frame(
        student_id = as.character(student$student_id),
        name = student$name,
        sis_login_id = if ("sis_login_id" %in% names(student)) student$sis_login_id else "",
        category = adj$category[j],
        arrangement_type = adj$arrangement_type[j],
        value = adj$value[j],
        group = grp,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      student_id = character(), name = character(), sis_login_id = character(),
      category = character(), arrangement_type = character(),
      value = character(), group = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}
