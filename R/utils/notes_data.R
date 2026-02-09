# -- notes_data.R --------------------------------------------------
# Note CRUD, NOTE_TAGS definitions, JSON persistence.

# Tag dictionary: category -> list(label, description)
NOTE_TAGS <- list(
  `follow-up` = list(label = "Follow-up", description = "Action required from you"),
  `at-risk`   = list(label = "At-Risk", description = "Performance or wellbeing concern"),
  contacted   = list(label = "Contacted", description = "Communication sent or received"),
  resolved    = list(label = "Resolved", description = "Issue closed, no further action"),
  general     = list(label = "General", description = "General observation")
)

# Generate a unique note ID: timestamp + 6-char random suffix.
generate_note_id <- function() {
  ts <- format(Sys.time(), "%Y%m%d%H%M%S")
  suffix <- paste0(sample(c(letters, 0:9), 6, replace = TRUE), collapse = "")
  paste0(ts, "_", suffix)
}

# Load student notes from .nightmare/student_notes.json.
# Returns a named list keyed by student_id, each value a list of note lists.
load_student_notes <- function(data_dir, unit) {
  path <- file.path(data_dir, unit, ".nightmare", "student_notes.json")
  if (!file.exists(path)) return(list())

  tryCatch({
    payload <- fromJSON(path, simplifyVector = FALSE)
    if (is.null(payload$notes)) return(list())
    payload$notes
  }, error = function(e) {
    list()
  })
}

# Save student notes to .nightmare/student_notes.json.
save_student_notes <- function(data_dir, unit, notes) {
  nightmare_dir <- ensure_nightmare_dir(data_dir, unit)

  payload <- list(
    version = 1L,
    saved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    notes = notes
  )

  path <- file.path(nightmare_dir, "student_notes.json")
  save_json(path, payload)
}

# Add a note for a student. Returns updated notes list.
add_note <- function(notes, student_id, category, text) {
  sid <- as.character(student_id)
  note <- list(
    id = generate_note_id(),
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    category = category,
    text = text
  )
  if (is.null(notes[[sid]])) {
    notes[[sid]] <- list(note)
  } else {
    notes[[sid]] <- c(list(note), notes[[sid]])
  }
  notes
}

# Edit an existing note. Returns updated notes list.
edit_note <- function(notes, student_id, note_id, category, text) {
  sid <- as.character(student_id)
  if (is.null(notes[[sid]])) return(notes)

  for (i in seq_along(notes[[sid]])) {
    if (identical(notes[[sid]][[i]]$id, note_id)) {
      notes[[sid]][[i]]$category <- category
      notes[[sid]][[i]]$text <- text
      break
    }
  }
  notes
}

# Delete a note. Returns updated notes list.
delete_note <- function(notes, student_id, note_id) {
  sid <- as.character(student_id)
  if (is.null(notes[[sid]])) return(notes)

  notes[[sid]] <- Filter(function(n) !identical(n$id, note_id), notes[[sid]])
  if (length(notes[[sid]]) == 0) notes[[sid]] <- NULL
  notes
}

# Flatten all notes into a reverse-chronological data.frame.
# student_data: the full studentData() data.frame (for name lookup).
flatten_all_notes <- function(notes, student_data) {
  empty_df <- data.frame(
    student_id = character(),
    name = character(),
    note_id = character(),
    timestamp = character(),
    category = character(),
    text = character(),
    stringsAsFactors = FALSE
  )

  if (length(notes) == 0) return(empty_df)

  rows <- lapply(names(notes), function(sid) {
    student_notes <- notes[[sid]]
    if (length(student_notes) == 0) return(NULL)

    # Look up student name
    name <- ""
    if (!is.null(student_data) && nrow(student_data) > 0) {
      match_idx <- which(as.character(student_data$student_id) == sid)
      if (length(match_idx) > 0) name <- student_data$name[match_idx[1]]
    }

    do.call(rbind, lapply(student_notes, function(n) {
      data.frame(
        student_id = sid,
        name = name,
        note_id = n$id,
        timestamp = n$timestamp,
        category = n$category,
        text = n$text,
        stringsAsFactors = FALSE
      )
    }))
  })

  result <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(result) || nrow(result) == 0) return(empty_df)

  # Sort reverse-chronological
  result <- result[order(result$timestamp, decreasing = TRUE), ]
  rownames(result) <- NULL
  result
}
