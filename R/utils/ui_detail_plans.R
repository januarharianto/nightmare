# -- ui_detail_plans.R ---------------------------------------------
# Support plans card for student detail view: disability plan adjustments.

build_plans_card <- function(student) {
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
          # Classify each adjustment into a group
          adjustments$group <- mapply(
            classify_plan_adjustment,
            adjustments$category,
            adjustments$arrangement_type,
            USE.NAMES = FALSE
          )

          # Deduplicate exam accommodations (merge in-person and online)
          exam_rows <- adjustments$group == "Exam Accommodations"
          if (any(exam_rows)) {
            exam_df <- adjustments[exam_rows, ]
            exam_df$clean_name <- sapply(exam_df$arrangement_type, clean_arrangement_name)
            exam_df <- exam_df[!duplicated(paste(exam_df$clean_name, exam_df$value)), ]
            exam_df$arrangement_type <- exam_df$clean_name
            exam_df$clean_name <- NULL
            adjustments <- rbind(adjustments[!exam_rows, ], exam_df)
          }

          # Build groups in display order
          groups <- lapply(PLAN_GROUPS, function(grp) {
            df <- adjustments[adjustments$group == grp, , drop = FALSE]
            list(label = grp, df = df)
          })

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
                tags$div(class = "plan-group-header meta-label", g$label),

                # Value rows (label-value flex layout)
                if (nrow(value_rows) > 0) {
                  lapply(1:nrow(value_rows), function(j) {
                    row <- value_rows[j, ]
                    tags$div(
                      class = "plan-adjustment-row",
                      tags$span(class = "plan-adjustment-label",
                                clean_arrangement_name(row$arrangement_type)),
                      tags$span(class = "plan-adjustment-value",
                                format_plan_value(row$value, row$arrangement_type))
                    )
                  })
                },

                # Yes-only tags (inline)
                if (nrow(tag_rows) > 0) {
                  tags$div(
                    class = "plan-tags",
                    lapply(1:nrow(tag_rows), function(j) {
                      tags$span(class = "plan-tag",
                                clean_arrangement_name(tag_rows[j, "arrangement_type"]))
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
