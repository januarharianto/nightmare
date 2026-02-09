# -- ui_detail_plans.R ---------------------------------------------
# Support plans card for student detail view: disability plan adjustments.

build_plans_card <- function(student) {
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
  )
}
