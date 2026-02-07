# Extensions data utilities for NIGHTMARE
# Pure data-processing functions — no Shiny reactives or side effects.

# Flatten extensions from nested student data.
# Un-nests special_consids, filters to extension types, and returns a flat
# data.frame with one row per extension record.
flatten_extensions <- function(data) {
  # Schema for empty result
  empty_df <- data.frame(
    student_id = character(),
    name = character(),
    sis_login_id = character(),
    ticket_id = character(),
    assessment_name = character(),
    outcome_type = character(),
    state = character(),
    approved = logical(),
    extension_date = as.Date(character()),
    due_date = as.POSIXct(character()),
    closing_date = as.POSIXct(character()),
    has_plan_extension = logical(),
    stringsAsFactors = FALSE
  )

  if (is.null(data) || nrow(data) == 0) return(empty_df)

  extension_types <- c("Simple Extension", "Extension of time")

  rows <- lapply(seq_len(nrow(data)), function(i) {
    consids <- data$special_consids[[i]]
    if (is.null(consids) || nrow(consids) == 0) return(NULL)

    # Filter to extension types
    ext <- consids[consids$outcome_type %in% extension_types, , drop = FALSE]
    if (nrow(ext) == 0) return(NULL)

    # Determine if student has a plan-based extension adjustment
    plan_adj <- data$plan_adjustments[[i]]
    has_plan <- FALSE
    if (!is.null(plan_adj) && nrow(plan_adj) > 0) {
      aa_rows <- plan_adj[plan_adj$category == "Assessment Adjustment", , drop = FALSE]
      if (nrow(aa_rows) > 0) {
        has_plan <- any(grepl("Extension|Take Home", aa_rows$arrangement_type,
                              ignore.case = TRUE))
      }
    }

    data.frame(
      student_id = rep(as.character(data$student_id[i]), nrow(ext)),
      name = rep(as.character(data$name[i]), nrow(ext)),
      sis_login_id = rep(as.character(data$sis_login_id[i]), nrow(ext)),
      ticket_id = as.character(ext$ticket_id),
      assessment_name = as.character(ext$assessment_name),
      outcome_type = as.character(ext$outcome_type),
      state = as.character(ext$state),
      approved = as.logical(ext$approved),
      extension_date = as.Date(ext$extension_date),
      due_date = as.POSIXct(ext$due_date),
      closing_date = as.POSIXct(ext$closing_date),
      has_plan_extension = rep(has_plan, nrow(ext)),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(result) || nrow(result) == 0) return(empty_df)
  result
}

# Extract unique sorted assignment names from Canvas data.
# All students share the same assignment list, so we use the first student.
get_canvas_assignment_names <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(character())
  if (!"assignments" %in% names(data)) return(character())

  assignments <- data$assignments[[1]]
  if (is.null(assignments) || nrow(assignments) == 0) return(character())

  sort(unique(assignments$name))
}

# Match spec cons assessment names to Canvas assignment names.
# Uses three-step matching: exact (case-insensitive), substring, then fuzzy.
match_assessments <- function(spec_cons_names, canvas_names, max_distance = 0.3) {
  matched <- data.frame(
    spec_cons_name = character(),
    canvas_name = character(),
    stringsAsFactors = FALSE
  )
  unmatched <- character()
  ambiguous <- list()

  if (length(spec_cons_names) == 0 || length(canvas_names) == 0) {
    return(list(matched = matched, unmatched = spec_cons_names, ambiguous = ambiguous))
  }

  canvas_lower <- tolower(canvas_names)

  for (sc_name in spec_cons_names) {
    sc_lower <- tolower(sc_name)

    # Step 1: Exact case-insensitive match
    exact <- which(canvas_lower == sc_lower)
    if (length(exact) == 1) {
      matched <- rbind(matched, data.frame(
        spec_cons_name = sc_name,
        canvas_name = canvas_names[exact],
        stringsAsFactors = FALSE
      ))
      next
    }

    # Step 2: Substring containment (either direction)
    substring_hits <- which(
      grepl(sc_lower, canvas_lower, fixed = TRUE) |
      sapply(canvas_lower, function(cn) grepl(cn, sc_lower, fixed = TRUE))
    )
    if (length(substring_hits) == 1) {
      matched <- rbind(matched, data.frame(
        spec_cons_name = sc_name,
        canvas_name = canvas_names[substring_hits],
        stringsAsFactors = FALSE
      ))
      next
    }
    if (length(substring_hits) > 1) {
      ambiguous[[sc_name]] <- canvas_names[substring_hits]
      next
    }

    # Step 3: Fuzzy match via agrep
    fuzzy_hits <- agrep(sc_name, canvas_names, max.distance = max_distance,
                        ignore.case = TRUE)
    if (length(fuzzy_hits) == 1) {
      matched <- rbind(matched, data.frame(
        spec_cons_name = sc_name,
        canvas_name = canvas_names[fuzzy_hits],
        stringsAsFactors = FALSE
      ))
      next
    }
    if (length(fuzzy_hits) > 1) {
      ambiguous[[sc_name]] <- canvas_names[fuzzy_hits]
      next
    }

    # No match at all
    unmatched <- c(unmatched, sc_name)
  }

  list(matched = matched, unmatched = unmatched, ambiguous = ambiguous)
}

# Apply manual overrides to a match result.
# overrides: named list of spec_cons_name -> chosen canvas_name (or NA to skip).
apply_match_overrides <- function(match_result, overrides) {
  if (is.null(overrides) || length(overrides) == 0) return(match_result)

  matched <- match_result$matched
  unmatched <- match_result$unmatched
  ambiguous <- match_result$ambiguous

  for (sc_name in names(overrides)) {
    chosen <- overrides[[sc_name]]

    # Remove from ambiguous
    ambiguous[[sc_name]] <- NULL
    # Remove from unmatched
    unmatched <- unmatched[unmatched != sc_name]

    # Add to matched if not NA
    if (!is.na(chosen)) {
      matched <- rbind(matched, data.frame(
        spec_cons_name = sc_name,
        canvas_name = chosen,
        stringsAsFactors = FALSE
      ))
    }
  }

  list(matched = matched, unmatched = unmatched, ambiguous = ambiguous)
}

# Save match overrides to a JSON dotfile in the unit's data folder.
# overrides: named list of spec_cons_name -> canvas_name (NA for skipped).
save_match_overrides <- function(data_dir, unit, overrides) {
  folder <- file.path(data_dir, unit)
  if (!dir.exists(folder)) return(invisible(NULL))

  # Convert NA values to JSON null via explicit NULL assignment
  overrides_json <- lapply(overrides, function(x) if (is.na(x)) NULL else x)

  payload <- list(
    version = 1L,
    saved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    overrides = overrides_json
  )

  path <- file.path(folder, ".match_overrides.json")
  writeLines(toJSON(payload, auto_unbox = TRUE, null = "null", pretty = TRUE), path)
  invisible(path)
}

# Load match overrides from a JSON dotfile.
# Returns a named list (spec_cons_name -> canvas_name, or NA for skipped).
# Returns empty list if the file is missing or corrupt.
load_match_overrides <- function(data_dir, unit) {
  path <- file.path(data_dir, unit, ".match_overrides.json")
  if (!file.exists(path)) return(list())

  tryCatch({
    payload <- fromJSON(path, simplifyVector = FALSE)
    if (is.null(payload$overrides)) return(list())

    # Convert JSON null back to NA
    result <- lapply(payload$overrides, function(x) if (is.null(x)) NA else x)
    attr(result, "saved_at") <- payload$saved_at
    result
  }, error = function(e) {
    list()
  })
}

# Validate overrides against current spec cons + canvas names.
# Drops entries where the spec cons name no longer exists, or where the
# canvas name no longer exists (unless the override is NA/skip).
validate_match_overrides <- function(overrides, spec_cons_names, canvas_names) {
  if (length(overrides) == 0) return(list())

  saved_at <- attr(overrides, "saved_at")
  keep <- list()

  for (nm in names(overrides)) {
    # Spec cons name must still exist
    if (!(nm %in% spec_cons_names)) next
    val <- overrides[[nm]]
    # NA (skip) is always valid; otherwise canvas name must still exist
    if (is.na(val) || val %in% canvas_names) {
      keep[[nm]] <- val
    }
  }

  attr(keep, "saved_at") <- saved_at
  keep
}

# Compute summary statistics for extensions on a given Canvas assignment.
compute_extension_stats <- function(ext_flat, canvas_name, match_result) {
  # Find which spec_cons_names map to this canvas assignment
  sc_names <- match_result$matched$spec_cons_name[
    match_result$matched$canvas_name == canvas_name
  ]

  if (length(sc_names) == 0 || is.null(ext_flat) || nrow(ext_flat) == 0) {
    return(list(total = 0L, approved = 0L, pending = 0L, with_plan = 0L,
                avg_days = NA_real_))
  }

  filtered <- ext_flat[ext_flat$assessment_name %in% sc_names, , drop = FALSE]
  if (nrow(filtered) == 0) {
    return(list(total = 0L, approved = 0L, pending = 0L, with_plan = 0L,
                avg_days = NA_real_))
  }

  days_diff <- as.numeric(difftime(filtered$extension_date,
                                   as.Date(filtered$due_date),
                                   units = "days"))
  avg_days <- if (all(is.na(days_diff))) NA_real_ else mean(days_diff, na.rm = TRUE)

  list(
    total = nrow(filtered),
    approved = sum(filtered$approved, na.rm = TRUE),
    pending = sum(filtered$state == "Pending", na.rm = TRUE),
    with_plan = sum(filtered$has_plan_extension, na.rm = TRUE),
    avg_days = avg_days
  )
}

# Build a display table of extensions for a given Canvas assignment.
# Returns a data.frame with display columns plus hidden columns prefixed with ".".
build_extensions_table <- function(ext_flat, canvas_name, match_result) {
  # Empty table schema
  empty_table <- data.frame(
    Student = character(),
    UniKey = character(),
    `Due Date` = character(),
    `Extended To` = character(),
    `+Days` = character(),
    Outcome = character(),
    Status = character(),
    Plan = character(),
    .approved = logical(),
    .extension_date = as.Date(character()),
    .student_id = character(),
    .ticket_id = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Find matching spec_cons_names
  sc_names <- match_result$matched$spec_cons_name[
    match_result$matched$canvas_name == canvas_name
  ]

  if (length(sc_names) == 0 || is.null(ext_flat) || nrow(ext_flat) == 0) {
    return(empty_table)
  }

  filtered <- ext_flat[ext_flat$assessment_name %in% sc_names, , drop = FALSE]
  if (nrow(filtered) == 0) return(empty_table)

  # Format dates
  fmt_date <- function(d) {
    ifelse(is.na(d), "TBC", format(d, "%d %b %Y"))
  }

  due_dates_formatted <- fmt_date(as.Date(filtered$due_date))
  ext_dates_formatted <- fmt_date(filtered$extension_date)

  # Compute +Days
  days_diff <- as.numeric(difftime(filtered$extension_date,
                                   as.Date(filtered$due_date),
                                   units = "days"))
  plus_days <- ifelse(is.na(days_diff), "TBC", as.character(days_diff))

  # Plan indicator
  plan_col <- ifelse(filtered$has_plan_extension, "Yes", "")

  tbl <- data.frame(
    Student = filtered$name,
    UniKey = filtered$sis_login_id,
    `Due Date` = due_dates_formatted,
    `Extended To` = ext_dates_formatted,
    `+Days` = plus_days,
    Outcome = filtered$outcome_type,
    Status = filtered$state,
    Plan = plan_col,
    .approved = filtered$approved,
    .extension_date = filtered$extension_date,
    .student_id = filtered$student_id,
    .ticket_id = filtered$ticket_id,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Sort: pending first, then by name, then by extension_date
  is_pending <- tbl$Status == "Pending"
  tbl <- tbl[order(!is_pending, tbl$Student, tbl$.extension_date), ]
  rownames(tbl) <- NULL
  tbl
}

# Build a SEAMS2-compatible export data.frame for approved extensions.
# Deduplicates by student, keeping the latest extension_date per student.
build_seams2_export <- function(display_table, canvas_name) {
  empty_export <- data.frame(
    `Section Id` = character(),
    `Section name` = character(),
    `Student name` = character(),
    UniKey = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (is.null(display_table) || nrow(display_table) == 0) return(empty_export)

  # Filter to approved only
  approved <- display_table[display_table$.approved == TRUE, , drop = FALSE]
  if (nrow(approved) == 0) return(empty_export)

  # Deduplicate by student_id, keeping latest extension_date
  approved <- approved[order(approved$.extension_date, decreasing = TRUE), ]
  approved <- approved[!duplicated(approved$.student_id), ]

  # Build section name: "{canvas_name} - {Mon-DD} - {outcome_type}"
  date_part <- ifelse(
    is.na(approved$.extension_date),
    "TBC",
    format(approved$.extension_date, "%b-%d")
  )
  section_name <- paste0(canvas_name, " - ", date_part, " - ", approved$Outcome)

  export <- data.frame(
    `Section Id` = rep("", nrow(approved)),
    `Section name` = section_name,
    `Student name` = rep("", nrow(approved)),
    UniKey = approved$UniKey,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  export
}
