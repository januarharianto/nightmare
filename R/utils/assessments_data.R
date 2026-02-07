# Assessments data utilities for NIGHTMARE
# Pure data-processing functions — no Shiny reactives or side effects.

# Extract class-level scores per completed assessment.
# Returns a named list of data.frames, keyed by assignment name.
# Each data.frame has: name, max_points, scores (numeric vector of percentages).
# Only includes completed assessments (is_ongoing == FALSE).
extract_class_scores <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(list())
  if (!"assignments" %in% names(data)) return(list())

  # All students share the same assignment list — use the first to get names
  ref <- data$assignments[[1]]
  if (is.null(ref) || nrow(ref) == 0) return(list())

  # Only completed assessments
  completed <- ref[!ref$is_ongoing, , drop = FALSE]
  if (nrow(completed) == 0) return(list())

  result <- list()
  for (i in seq_len(nrow(completed))) {
    aname <- completed$name[i]
    max_pts <- completed$max_points[i]

    # Collect percentages across all students
    pcts <- vapply(data$assignments, function(a) {
      row <- a[a$name == aname, , drop = FALSE]
      if (nrow(row) == 0) return(NA_real_)
      row$percentage[1]
    }, numeric(1))

    # Remove NAs
    pcts <- pcts[!is.na(pcts)]
    if (length(pcts) == 0) next

    result[[aname]] <- list(
      name = aname,
      max_points = max_pts,
      scores = pcts
    )
  }

  result
}

# Compute grade counts using USYD boundaries.
# Takes a numeric vector of percentages, returns a named integer vector.
compute_grade_counts <- function(percentages) {
  c(
    F  = sum(percentages < 50),
    P  = sum(percentages >= 50 & percentages < 65),
    CR = sum(percentages >= 65 & percentages < 75),
    D  = sum(percentages >= 75 & percentages < 85),
    HD = sum(percentages >= 85)
  )
}

# Draw a density plot for an assessment's percentage distribution.
# Black outline, no fill, grade boundary lines, minimal axes.
# Grade labels and counts integrated at top of plot.
# Call inside renderPlot().
render_density_plot <- function(percentages) {
  if (length(percentages) < 2) {
    plot.new()
    text(0.5, 0.5, "Insufficient data", col = "#AAAAAA", cex = 1.2)
    return(invisible(NULL))
  }

  d <- density(percentages, from = 0, to = 100, bw = "SJ")
  grades <- compute_grade_counts(percentages)
  y_max <- max(d$y)

  par(mar = c(2.5, 0, 0, 0), bg = "#FFFFFF")
  plot(d, main = "", xlab = "", ylab = "", axes = FALSE,
       xlim = c(0, 100), ylim = c(0, y_max * 1.6),
       col = "#000000", lwd = 1.5,
       zero.line = FALSE, frame.plot = FALSE)

  # Grade boundary lines
  boundaries <- c(50, 65, 75, 85)
  abline(v = boundaries, lty = 2, col = "#CCCCCC")

  # X-axis with boundary ticks only
  axis(1, at = c(0, 50, 65, 75, 85, 100), labels = c(0, 50, 65, 75, 85, 100),
       lwd = 0, lwd.ticks = 0.5, col.ticks = "#CCCCCC", col.axis = "#000000",
       cex.axis = 1.0, padj = -0.5)

  # Grade labels (light, top) and counts (black, below) stacked vertically
  label_pos <- c(25, 57.5, 70, 80, 92.5)
  label_txt <- c("F", "P", "CR", "D", "HD")
  count_txt <- as.character(grades[label_txt])
  text(label_pos, rep(y_max * 1.47, 5), label_txt, col = "#AAAAAA",
       cex = 0.9, font = 2)
  text(label_pos, rep(y_max * 1.25, 5), count_txt, col = "#000000",
       cex = 1.15, font = 2)

  invisible(NULL)
}

# Draw a histogram for an assessment's percentage distribution.
# F range (0-50) split into 10-wide sub-bins for finer detail;
# P/CR/D/HD use grade boundaries. Black outline, #F5F5F5 fill.
# Grade labels and counts integrated at top of plot.
# Call inside renderPlot().
render_histogram_plot <- function(percentages) {
  if (length(percentages) < 2) {
    plot.new()
    text(0.5, 0.5, "Insufficient data", col = "#AAAAAA", cex = 1.2)
    return(invisible(NULL))
  }

  breaks <- c(seq(0, 50, by = 10), 65, 75, 85, 100)
  grades <- compute_grade_counts(percentages)
  y_max <- max(hist(percentages, breaks = breaks, plot = FALSE)$density)

  par(mar = c(2.5, 0, 0, 0), bg = "#FFFFFF")
  h <- hist(percentages, breaks = breaks, plot = FALSE)
  plot(h, main = "", xlab = "", ylab = "", axes = FALSE,
       xlim = c(0, 100), ylim = c(0, y_max * 1.6),
       col = "#F5F5F5", border = "#000000",
       freq = FALSE)

  # Grade boundary lines
  boundaries <- c(50, 65, 75, 85)
  abline(v = boundaries, lty = 2, col = "#CCCCCC")

  # X-axis with boundary ticks only
  axis(1, at = c(0, 50, 65, 75, 85, 100), labels = c(0, 50, 65, 75, 85, 100),
       lwd = 0, lwd.ticks = 0.5, col.ticks = "#CCCCCC", col.axis = "#000000",
       cex.axis = 1.0, padj = -0.5)

  # Grade labels (light, top) and counts (black, below) stacked vertically
  label_pos <- c(25, 57.5, 70, 80, 92.5)
  label_txt <- c("F", "P", "CR", "D", "HD")
  count_txt <- as.character(grades[label_txt])
  text(label_pos, rep(y_max * 1.47, 5), label_txt, col = "#AAAAAA",
       cex = 0.9, font = 2)
  text(label_pos, rep(y_max * 1.25, 5), count_txt, col = "#000000",
       cex = 1.15, font = 2)

  invisible(NULL)
}

# Compute percentile rank for a student's assessments against the class.
# Returns a named numeric vector (0-100) keyed by assessment name.
# Only completed assessments with scores get a rank.
compute_percentile_ranks <- function(student, all_students) {
  assignments <- student$assignments[[1]]
  if (is.null(assignments) || nrow(assignments) == 0) return(numeric(0))

  completed <- assignments[!assignments$is_ongoing & !is.na(assignments$score), ]
  if (nrow(completed) == 0) return(numeric(0))

  ranks <- vapply(seq_len(nrow(completed)), function(i) {
    aname <- completed$name[i]
    pct <- completed$percentage[i]

    # Collect all class scores for this assessment
    class_scores <- vapply(all_students$assignments, function(a) {
      row <- a[a$name == aname, , drop = FALSE]
      if (nrow(row) == 0 || is.na(row$percentage[1])) return(NA_real_)
      row$percentage[1]
    }, numeric(1))
    class_scores <- class_scores[!is.na(class_scores)]

    if (length(class_scores) < 2) return(NA_real_)
    round(ecdf(class_scores)(pct) * 100)
  }, numeric(1))

  names(ranks) <- completed$name
  ranks
}
