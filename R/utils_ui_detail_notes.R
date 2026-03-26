#' @keywords internal
# -- ui_detail_notes.R ---------------------------------------------
# Notes card for student detail view: tag selector, input form, notes list.

build_notes_card <- function(student, student_notes = list()) {
  sid <- as.character(student$student_id)

  # Tag selector buttons + description
  tag_buttons <- lapply(names(NOTE_TAGS), function(tag_key) {
    tag_info <- NOTE_TAGS[[tag_key]]
    tags$button(
      class = "notes-tag-btn btn-secondary",
      `data-tag` = tag_key,
      `data-description` = tag_info$description,
      onclick = sprintf(
        "document.querySelectorAll('.notes-card-content .notes-tag-btn').forEach(function(b){b.classList.remove('selected')});this.classList.add('selected');Shiny.setInputValue('note_tag_selected','%s',{priority:'event'});document.querySelector('.notes-tag-description').textContent=this.dataset.description;",
        tag_key
      ),
      onmouseenter = "document.querySelector('.notes-tag-description').textContent=this.dataset.description;",
      onmouseleave = "var sel=document.querySelector('.notes-tag-btn.selected');document.querySelector('.notes-tag-description').textContent=sel?sel.dataset.description:'Select a category';",
      tag_info$label
    )
  })

  # Notes form
  form <- tags$div(
    class = "notes-form",
    tags$div(class = "notes-tag-selector", tag_buttons),
    tags$div(class = "notes-tag-description", "Select a category"),
    tags$div(
      class = "notes-input-row",
      tags$textarea(
        class = "notes-textarea",
        id = "note_text_input",
        placeholder = "Add a note...",
        rows = "2"
      ),
      tags$button(
        class = "notes-save-btn btn-primary",
        onclick = sprintf(
          "var text=document.getElementById('note_text_input').value;var tag=document.querySelector('.notes-tag-btn.selected');if(!tag||!text.trim()){return;}Shiny.setInputValue('save_note',{student_id:'%s',category:tag.dataset.tag,text:text},{priority:'event'});document.getElementById('note_text_input').value='';",
          sid
        ),
        "Save"
      )
    )
  )

  # Notes list (reverse-chronological)
  notes_list <- if (length(student_notes) == 0) {
    tags$div(class = "notes-list",
      tags$div(
        style = "padding: 12px 0; color: #AAAAAA; font-size: 12px; text-align: center;",
        "No notes yet"
      )
    )
  } else {
    items <- lapply(student_notes, function(note) {
      tag_label <- if (!is.null(NOTE_TAGS[[note$category]])) {
        NOTE_TAGS[[note$category]]$label
      } else {
        note$category
      }

      ts_display <- tryCatch(
        format(as.POSIXct(note$timestamp), "%d %b %Y %H:%M"),
        error = function(e) note$timestamp
      )

      tags$div(
        class = "note-item",
        tags$div(
          class = "note-item-header",
          tags$span(class = paste0("notes-tag-badge notes-tag-", note$category), tag_label),
          tags$span(class = "note-item-timestamp meta-label", ts_display),
          tags$span(class = "note-item-actions",
            tags$button(
              class = "note-action-btn btn-secondary",
              onclick = sprintf(
                "Shiny.setInputValue('edit_note',{student_id:'%s',note_id:'%s'},{priority:'event'});",
                sid, note$id
              ),
              "Edit"
            ),
            tags$button(
              class = "note-action-btn note-delete-btn",
              onclick = sprintf(
                "if(confirm('Delete this note?')){Shiny.setInputValue('delete_note',{student_id:'%s',note_id:'%s'},{priority:'event'});}",
                sid, note$id
              ),
              "Delete"
            )
          )
        ),
        tags$div(class = "note-item-text", note$text)
      )
    })
    tags$div(class = "notes-list", items)
  }

  tags$div(
    class = "detail-section",
    tags$div(class = "detail-section-header", "Notes"),
    tags$div(
      class = "detail-section-content notes-card-content",
      tagList(form, notes_list)
    )
  )
}
