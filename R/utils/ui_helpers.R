# UI helper functions for NIGHTMARE
# Extracted from server.R lines 164-384

build_student_detail_view <- function(student, all_students = NULL, student_notes = list(), exam_data = NULL) {
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

      # Assessments Section
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Assessments"),
        tags$div(
          class = "detail-section-content",
          {
            assignments <- student$assignments[[1]]
              percentiles <- if (!is.null(all_students)) compute_percentile_ranks(student, all_students) else numeric(0)

            if (nrow(assignments) == 0) {
              tags$div(
                class = "empty-state",
                tags$p("No assessment data available")
              )
            } else {
              # Compute summary stats
              completed <- assignments[!assignments$is_ongoing, ]
              n_total <- nrow(assignments)
              has_score <- completed[!is.na(completed$score), ]
              n_completed <- nrow(has_score)
              n_ongoing <- sum(assignments$is_ongoing)
              n_failing <- sum(!is.na(completed$score) & completed$percentage < 50) +
                sum(is.na(completed$score))
              n_spec_cons <- nrow(student$special_consids[[1]])

              all_ongoing <- n_total == n_ongoing

              tagList(
                # Summary statistics
                tags$div(
                  class = "assessment-summary",
                  tags$div(
                    tags$div(class = "stat-label", "Completed"),
                    tags$div(class = "stat-value",
                      if (all_ongoing) "--" else sprintf("%d of %d", n_completed, n_total)
                    )
                  ),
                  tags$div(
                    tags$div(class = "stat-label", "Failing"),
                    tags$div(class = "stat-value",
                      if (all_ongoing) "--" else as.character(n_failing)
                    )
                  ),
                  tags$div(
                    tags$div(class = "stat-label", "Spec Cons"),
                    tags$div(class = "stat-value", as.character(n_spec_cons))
                  ),
                  if (length(percentiles) > 0) {
                    overall_pctl <- round(median(percentiles, na.rm = TRUE))
                    tags$div(
                      tags$div(class = "stat-label", "Percentile"),
                      tags$div(class = "stat-value", paste0(overall_pctl, "%"))
                    )
                  }
                ),

                if (all_ongoing) {
                  tags$div(
                    style = "padding: 8px 0; color: #AAAAAA; font-size: 12px;",
                    "All assessments pending"
                  )
                },

                # Assessment table
                tags$table(
                  class = "detail-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Assessment"),
                      tags$th("Score"),
                      tags$th("Percentage")
                    )
                  ),
                  tags$tbody(
                    # Canvas assignment rows
                    lapply(1:nrow(assignments), function(i) {
                      a <- assignments[i, ]
                      has_sc <- !is.na(a$score)
                      ongoing <- isTRUE(a$is_ongoing)
                      missing <- !has_sc && !ongoing

                      # Score column
                      score_display <- if (has_sc) {
                        sprintf("%g / %g", a$score, a$max_points)
                      } else if (ongoing) {
                        "--"
                      } else {
                        sprintf("-- / %g", a$max_points)
                      }

                      # Percentage column
                      pct_display <- if (has_sc) {
                        sprintf("%.0f%%", a$percentage)
                      } else if (ongoing) {
                        "--"
                      } else {
                        "Missing"
                      }

                      row_class <- if (ongoing) {
                        "assessment-pending"
                      } else if (missing) {
                        "assessment-missing"
                      } else if (a$percentage < 50) {
                        "assessment-failing"
                      } else {
                        ""
                      }

                      tags$tr(
                        class = row_class,
                        tags$td(a$name),
                        tags$td(score_display),
                        tags$td(class = "assessment-pct", pct_display)
                      )
                    }),

                    # Exam data rows (all uploaded assessments, missing if no score)
                    if (!is.null(exam_data) && length(exam_data$assessments) > 0) {
                      sid <- as.character(student$student_id)
                      scores_df <- get_student_exam_scores(exam_data, sid)
                      lapply(names(exam_data$assessments), function(aname) {
                        a <- exam_data$assessments[[aname]]
                        mp <- a$max_points
                        row <- if (nrow(scores_df) > 0) scores_df[scores_df$assessment == aname, , drop = FALSE] else scores_df
                        has_score <- nrow(row) > 0

                        # Source type tag
                        src_types <- unique(vapply(a$sittings, function(s) s$source_type %||% "manual", character(1)))
                        src_label <- if (any(src_types == "gradescope")) "Gradescope" else "Manual"
                        src_tag <- tags$span(class = "exam-source-tag", src_label)

                        if (has_score) {
                          pct <- (row$score[1] / mp) * 100
                          row_class <- if (pct < 50) "assessment-failing" else ""
                          tags$tr(
                            class = row_class,
                            tags$td(aname, src_tag),
                            tags$td(sprintf("%g / %g", row$score[1], mp)),
                            tags$td(class = "assessment-pct", sprintf("%.0f%%", pct))
                          )
                        } else {
                          tags$tr(
                            class = "assessment-missing",
                            tags$td(aname, src_tag),
                            tags$td(sprintf("-- / %g", mp)),
                            tags$td(class = "assessment-pct", "Missing")
                          )
                        }
                      })
                    }
                  )
                )
              )
            }
          }
        )
      ),

      # Special Considerations Section
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Special Considerations"),
        tags$div(
          class = "detail-section-content",
          {
            consids <- student$special_consids[[1]]
            n_consids <- nrow(consids)

            if (n_consids == 0) {
              tags$div(class = "empty-state",
                tags$p("No special considerations"))
            } else {
              # Helper: format date or return "--"
              fmt_date <- function(d) {
                if (!is.na(d)) format(as.Date(d), "%d %b %Y") else "--"
              }

              # Split by outcome type groups (handle NA outcome_type)
              has_outcome <- !is.na(consids$outcome_type)
              extensions <- consids[has_outcome & consids$outcome_type %in%
                c("Simple Extension", "Extension of time"), ]
              replacements <- consids[has_outcome &
                grepl("replacement.*exam", consids$outcome_type, ignore.case = TRUE), ]
              mark_adj <- consids[has_outcome &
                grepl("mark.*adjustment", consids$outcome_type, ignore.case = TRUE), ]
              ext_types <- c("Simple Extension", "Extension of time")
              other <- consids[has_outcome &
                !(consids$outcome_type %in% ext_types) &
                !grepl("replacement.*exam", consids$outcome_type, ignore.case = TRUE) &
                !grepl("mark.*adjustment", consids$outcome_type, ignore.case = TRUE), ]
              # Pending with no outcome type
              pending_no_outcome <- consids[!has_outcome & consids$state == "Pending", ]

              n_approved <- sum(consids$approved, na.rm = TRUE)
              n_pending <- sum(consids$state == "Pending", na.rm = TRUE)

              tagList(
                # Summary bar
                tags$div(
                  class = "consids-summary",
                  tags$div(
                    class = "consids-summary-primary",
                    tags$div(
                      tags$div(class = "stat-label", "Approved"),
                      tags$div(class = "stat-value", as.character(n_approved))
                    ),
                    if (n_pending > 0) {
                      tags$div(
                        tags$div(class = "stat-label", "Pending"),
                        tags$div(class = "stat-value consids-pending-value",
                                 as.character(n_pending))
                      )
                    }
                  ),
                  tags$div(
                    class = "consids-summary-breakdown",
                    tags$div(
                      tags$div(class = "stat-label", "Extensions"),
                      tags$div(class = "stat-value", as.character(nrow(extensions)))
                    ),
                    if (nrow(replacements) > 0 || nrow(pending_no_outcome) > 0) {
                      tags$div(
                        tags$div(class = "stat-label", "Repl. Exams"),
                        tags$div(class = "stat-value",
                                 as.character(nrow(replacements) + nrow(pending_no_outcome)))
                      )
                    },
                    if (nrow(mark_adj) > 0) {
                      tags$div(
                        tags$div(class = "stat-label", "Mark Adj."),
                        tags$div(class = "stat-value", as.character(nrow(mark_adj)))
                      )
                    }
                  )
                ),

                # Replacement Exams group (shown first — most actionable)
                if (nrow(replacements) > 0 || nrow(pending_no_outcome) > 0) {
                  exam_rows <- rbind(
                    if (nrow(replacements) > 0) replacements else NULL,
                    if (nrow(pending_no_outcome) > 0) pending_no_outcome else NULL
                  )

                  tagList(
                    tags$div(class = "consids-group-header consids-alert-header",
                             "Exam Considerations"),
                    tags$table(
                      class = "detail-table",
                      tags$thead(tags$tr(
                        tags$th("Assessment"),
                        tags$th("Due Date"),
                        tags$th("Outcome"),
                        tags$th("Status")
                      )),
                      tags$tbody(
                        lapply(1:nrow(exam_rows), function(i) {
                          r <- exam_rows[i, ]
                          due_str <- fmt_date(r$due_date)
                          outcome_str <- if (!is.na(r$outcome_type)) {
                            r$outcome_type
                          } else {
                            "--"
                          }
                          state_class <- if (r$state == "Pending") {
                            "consids-state-pending"
                          } else {
                            "consids-state-approved"
                          }

                          tags$tr(
                            tags$td(r$assessment_name),
                            tags$td(due_str),
                            tags$td(outcome_str),
                            tags$td(tags$span(class = state_class, r$state))
                          )
                        })
                      )
                    )
                  )
                },

                # Extensions group
                if (nrow(extensions) > 0) {
                  tagList(
                    tags$div(class = "consids-group-header", "Extensions"),
                    tags$table(
                      class = "detail-table",
                      tags$thead(tags$tr(
                        tags$th("Assessment"),
                        tags$th("Extended To"),
                        tags$th("Due Date")
                      )),
                      tags$tbody(
                        lapply(1:nrow(extensions), function(i) {
                          e <- extensions[i, ]
                          ext_str <- fmt_date(e$extension_date)
                          due_str <- fmt_date(e$due_date)
                          tags$tr(
                            tags$td(e$assessment_name),
                            tags$td(ext_str),
                            tags$td(due_str)
                          )
                        })
                      )
                    )
                  )
                },

                # Mark Adjustments group
                if (nrow(mark_adj) > 0) {
                  tagList(
                    tags$div(class = "consids-group-header", "Mark Adjustments"),
                    tags$table(
                      class = "detail-table",
                      tags$thead(tags$tr(
                        tags$th("Assessment"),
                        tags$th("Type"),
                        tags$th("Ticket")
                      )),
                      tags$tbody(
                        lapply(1:nrow(mark_adj), function(i) {
                          m <- mark_adj[i, ]
                          tags$tr(
                            tags$td(m$assessment_name),
                            tags$td(m$assessment_type),
                            tags$td(m$ticket_id)
                          )
                        })
                      )
                    )
                  )
                },

                # Other outcomes
                if (nrow(other) > 0) {
                  tagList(
                    tags$div(class = "consids-group-header", "Other"),
                    tags$table(
                      class = "detail-table",
                      tags$thead(tags$tr(
                        tags$th("Assessment"),
                        tags$th("Outcome"),
                        tags$th("Status")
                      )),
                      tags$tbody(
                        lapply(1:nrow(other), function(i) {
                          o <- other[i, ]
                          state_class <- if (o$state == "Pending") {
                            "consids-state-pending"
                          } else {
                            "consids-state-approved"
                          }
                          tags$tr(
                            tags$td(o$assessment_name),
                            tags$td(o$outcome_type),
                            tags$td(tags$span(class = state_class, o$state))
                          )
                        })
                      )
                    )
                  )
                }
              )
            }
          }
        )
      ),

      # Support Plans Section
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Support Plans"),
        tags$div(
          class = "detail-section-content",
          {
            has_plan <- isTRUE(student$has_disability_plan)
            adjustments <- if (has_plan) student$plan_adjustments[[1]] else data.frame(
              category = character(), arrangement_type = character(),
              value = character(), stringsAsFactors = FALSE
            )
            n_adjustments <- nrow(adjustments)

            # Summary row: Disability Plan status + count
            summary_tags <- tagList(
              tags$div(
                class = "detail-row",
                tags$div(class = "detail-label", "Disability Plan"),
                tags$div(class = "detail-value",
                  if (has_plan) {
                    if (n_adjustments > 0) {
                      paste0("Yes (", n_adjustments, " active adjustment",
                             if (n_adjustments != 1) "s" else "", ")")
                    } else {
                      "Yes"
                    }
                  } else {
                    "No"
                  }
                )
              )
            )

            if (!has_plan || n_adjustments == 0) {
              summary_tags
            } else {
              # Helper: clean arrangement name for display
              clean_name <- function(name) {
                name <- gsub("\\s*\\(Pro Rata\\)\\s*\\(Minutes/hour\\)", "", name)
                name <- gsub("\\s*\\(Pro Rata\\)", "", name)
                name <- gsub("\\s*\\(Minutes/hour\\)", "", name)
                name <- gsub("^Online - ", "", name)
                name <- trimws(name)
                name
              }

              # Helper: determine if value is a simple yes/flag or has specific data
              is_yes_only <- function(val) {
                tolower(trimws(val)) %in% c("yes", "y", "true", "x")
              }

              # Helper: format value for display
              format_value <- function(val, arrangement) {
                v <- trimws(val)
                # If arrangement mentions minutes/hour, add suffix
                if (grepl("Minutes/hour", arrangement, ignore.case = TRUE)) {
                  return(paste0(v, " min/hour"))
                }
                v
              }

              # Build groups from adjustments
              # 1. Extensions
              ext_mask <- adjustments$category == "Assessment Adjustment" &
                grepl("Extension|Take Home", adjustments$arrangement_type, ignore.case = TRUE)
              ext_df <- adjustments[ext_mask, ]

              # 2. Exam Accommodations
              exam_mask <- grepl("Exam Adjustment", adjustments$category, ignore.case = TRUE)
              exam_df <- adjustments[exam_mask, ]

              # Merge in-person and online exam adjustments
              if (nrow(exam_df) > 0) {
                exam_df$clean_name <- sapply(exam_df$arrangement_type, clean_name)
                # Deduplicate: if same clean_name + same value exists, keep once
                exam_df <- exam_df[!duplicated(paste(exam_df$clean_name, exam_df$value)), ]
                exam_df$arrangement_type <- exam_df$clean_name
                exam_df$clean_name <- NULL
              }

              # 3. Presentation
              pres_mask <- grepl("Content Not|Present To|Group Presentations",
                                adjustments$arrangement_type, ignore.case = TRUE)
              pres_df <- adjustments[pres_mask, ]

              # 4. Classroom Support
              class_mask <- adjustments$category == "In-Class Support & Management"
              class_df <- adjustments[class_mask, ]

              # 5. Placement
              place_mask <- grepl("Placement", adjustments$category, ignore.case = TRUE)
              place_df <- adjustments[place_mask, ]

              # Collect already-assigned rows
              assigned <- ext_mask | exam_mask | pres_mask | class_mask | place_mask

              groups <- list(
                list(label = "Extensions", df = ext_df),
                list(label = "Exam Accommodations", df = exam_df),
                list(label = "Presentation", df = pres_df),
                list(label = "Classroom Support", df = class_df),
                list(label = "Placement", df = place_df)
              )

              # Add uncategorised group for anything not matched
              other_df <- adjustments[!assigned, ]
              if (nrow(other_df) > 0) {
                groups <- c(groups, list(list(label = "Other", df = other_df)))
              }

              # Build UI for each group
              group_tags <- lapply(groups, function(g) {
                df <- g$df
                if (nrow(df) == 0) return(NULL)

                # Split into value rows vs yes-only tags
                yes_mask <- sapply(df$value, is_yes_only)
                value_rows <- df[!yes_mask, ]
                tag_rows <- df[yes_mask, ]

                tagList(
                  tags$div(
                    class = "plan-group",
                    tags$div(class = "plan-group-header", g$label),

                    # Value rows (label-value flex layout)
                    if (nrow(value_rows) > 0) {
                      lapply(1:nrow(value_rows), function(j) {
                        row <- value_rows[j, ]
                        tags$div(
                          class = "plan-adjustment-row",
                          tags$span(class = "plan-adjustment-label",
                                    clean_name(row$arrangement_type)),
                          tags$span(class = "plan-adjustment-value",
                                    format_value(row$value, row$arrangement_type))
                        )
                      })
                    },

                    # Yes-only tags (inline)
                    if (nrow(tag_rows) > 0) {
                      tags$div(
                        class = "plan-tags",
                        lapply(1:nrow(tag_rows), function(j) {
                          tags$span(class = "plan-tag",
                                    clean_name(tag_rows[j, "arrangement_type"]))
                        })
                      )
                    }
                  )
                )
              })

              tagList(summary_tags, Filter(Negate(is.null), group_tags))
            }
          }
        )
      ),

      # Notes Section (4th card)
      tags$div(
        class = "detail-section",
        tags$div(class = "detail-section-header", "Notes"),
        tags$div(
          class = "detail-section-content notes-card-content",
          {
            sid <- as.character(student$student_id)

            # Tag selector buttons + description
            tag_buttons <- lapply(names(NOTE_TAGS), function(tag_key) {
              tag_info <- NOTE_TAGS[[tag_key]]
              tags$button(
                class = "notes-tag-btn",
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
                  class = "notes-save-btn",
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
                    tags$span(class = "note-item-timestamp", ts_display),
                    tags$span(class = "note-item-actions",
                      tags$button(
                        class = "note-action-btn",
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

            tagList(form, notes_list)
          }
        )
      )
    )
  )
}
