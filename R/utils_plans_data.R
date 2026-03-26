#' @keywords internal
# -- plans_data.R ---------------------------------------------------
# Shared utilities for plan data: grouping, formatting, flattening.

PLAN_GROUPS <- c("Extensions", "Exam Accommodations", "Presentation",
                 "Classroom Support", "Placement", "Other")

# Short display labels for groups (used in filter buttons + list renderer)
PLAN_GROUP_LABELS <- c(
  "Extensions" = "Extensions",
  "Exam Accommodations" = "Exam Accomm.",
  "Presentation" = "Presentation",
  "Classroom Support" = "Classroom",
  "Placement" = "Placement",
  "Other" = "Other"
)

# Check if a plan value is a simple yes/flag with no specific data.
is_yes_only <- function(val) {
  tolower(trimws(val)) %in% c("yes", "y", "true", "x")
}

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

# Format a single adjustment row into a display string.
format_adjustment_detail <- function(val, arrangement_type) {
  cleaned <- clean_arrangement_name(arrangement_type)
  if (is_yes_only(val)) {
    cleaned
  } else {
    paste0(cleaned, " (", format_plan_value(val, arrangement_type), ")")
  }
}

# Flatten all plan adjustments across students into a single data.frame.
# Returns: student_id, name, sis_login_id, category, arrangement_type, value,
#          group, display_detail, name_lower, sid_lower, login_lower
flatten_all_plans <- function(data) {
  empty <- data.frame(
    student_id = character(), name = character(), sis_login_id = character(),
    category = character(), arrangement_type = character(),
    value = character(), group = character(), display_detail = character(),
    name_lower = character(), sid_lower = character(), login_lower = character(),
    stringsAsFactors = FALSE
  )

  if (is.null(data) || nrow(data) == 0) return(empty)

  has_plan <- vapply(data$has_disability_plan, isTRUE, logical(1))
  plan_students <- data[has_plan, ]

  if (nrow(plan_students) == 0) return(empty)

  has_login <- "sis_login_id" %in% names(plan_students)

  # Pre-allocate vectors instead of building data.frames row by row
  n_est <- nrow(plan_students) * 4L  # rough estimate
  sid_vec <- character(n_est)
  name_vec <- character(n_est)
  login_vec <- character(n_est)
  cat_vec <- character(n_est)
  arr_vec <- character(n_est)
  val_vec <- character(n_est)
  grp_vec <- character(n_est)
  detail_vec <- character(n_est)
  k <- 0L

  for (i in seq_len(nrow(plan_students))) {
    adj <- plan_students$plan_adjustments[[i]]
    if (is.null(adj) || nrow(adj) == 0) next

    s_id <- as.character(plan_students$student_id[i])
    s_name <- plan_students$name[i]
    s_login <- if (has_login) plan_students$sis_login_id[i] else ""

    for (j in seq_len(nrow(adj))) {
      k <- k + 1L
      if (k > length(sid_vec)) {
        # Grow vectors
        sid_vec <- c(sid_vec, character(n_est))
        name_vec <- c(name_vec, character(n_est))
        login_vec <- c(login_vec, character(n_est))
        cat_vec <- c(cat_vec, character(n_est))
        arr_vec <- c(arr_vec, character(n_est))
        val_vec <- c(val_vec, character(n_est))
        grp_vec <- c(grp_vec, character(n_est))
        detail_vec <- c(detail_vec, character(n_est))
      }
      sid_vec[k] <- s_id
      name_vec[k] <- s_name
      login_vec[k] <- s_login
      cat_vec[k] <- adj$category[j]
      arr_vec[k] <- adj$arrangement_type[j]
      val_vec[k] <- adj$value[j]
      grp_vec[k] <- classify_plan_adjustment(adj$category[j], adj$arrangement_type[j])
      detail_vec[k] <- format_adjustment_detail(adj$value[j], adj$arrangement_type[j])
    }
  }

  if (k == 0L) return(empty)

  idx <- seq_len(k)
  result <- data.frame(
    student_id = sid_vec[idx],
    name = name_vec[idx],
    sis_login_id = login_vec[idx],
    category = cat_vec[idx],
    arrangement_type = arr_vec[idx],
    value = val_vec[idx],
    group = grp_vec[idx],
    display_detail = detail_vec[idx],
    stringsAsFactors = FALSE
  )
  # Pre-compute lowercase columns for search
  result$name_lower <- tolower(result$name)
  result$sid_lower <- tolower(result$student_id)
  result$login_lower <- tolower(result$sis_login_id)
  result
}
