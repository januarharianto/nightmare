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
# Call inside renderPlot().
render_density_plot <- function(percentages) {
  if (length(percentages) < 2) {
    plot.new()
    text(0.5, 0.5, "Insufficient data", col = "#AAAAAA", cex = 1.2)
    return(invisible(NULL))
  }

  d <- density(percentages, from = 0, to = 100, bw = "SJ")

  par(mar = c(2, 0, 0, 0), bg = "#FFFFFF")
  plot(d, main = "", xlab = "", ylab = "", axes = FALSE,
       xlim = c(0, 100), col = "#000000", lwd = 1.5,
       zero.line = FALSE, frame.plot = FALSE)

  # Grade boundary lines
  boundaries <- c(50, 65, 75, 85)
  abline(v = boundaries, lty = 2, col = "#CCCCCC")

  # X-axis with boundary ticks only
  axis(1, at = c(0, 50, 65, 75, 85, 100), labels = c(0, 50, 65, 75, 85, 100),
       lwd = 0, lwd.ticks = 0.5, col.ticks = "#CCCCCC", col.axis = "#666666",
       cex.axis = 0.8, padj = -1)

  # Grade labels at top
  y_top <- max(d$y) * 0.95
  label_pos <- c(25, 57.5, 70, 80, 92.5)
  label_txt <- c("F", "P", "CR", "D", "HD")
  text(label_pos, rep(y_top, 5), label_txt, col = "#AAAAAA",
       cex = 0.75, font = 2)

  invisible(NULL)
}

# Draw a histogram for an assessment's percentage distribution.
# Bins at grade boundaries: 0-50, 50-65, 65-75, 75-85, 85-100.
# Black outline bars, #F5F5F5 fill, grade boundary lines, minimal axes.
# Call inside renderPlot().
render_histogram_plot <- function(percentages) {
  if (length(percentages) < 2) {
    plot.new()
    text(0.5, 0.5, "Insufficient data", col = "#AAAAAA", cex = 1.2)
    return(invisible(NULL))
  }

  breaks <- c(0, 50, 65, 75, 85, 100)

  par(mar = c(2, 0, 0, 0), bg = "#FFFFFF")
  h <- hist(percentages, breaks = breaks, plot = FALSE)
  plot(h, main = "", xlab = "", ylab = "", axes = FALSE,
       xlim = c(0, 100), col = "#F5F5F5", border = "#000000",
       freq = FALSE)

  # Grade boundary lines
  boundaries <- c(50, 65, 75, 85)
  abline(v = boundaries, lty = 2, col = "#CCCCCC")

  # X-axis with boundary ticks only
  axis(1, at = c(0, 50, 65, 75, 85, 100), labels = c(0, 50, 65, 75, 85, 100),
       lwd = 0, lwd.ticks = 0.5, col.ticks = "#CCCCCC", col.axis = "#666666",
       cex.axis = 0.8, padj = -1)

  # Grade labels at top
  y_top <- max(h$density) * 0.95
  label_pos <- c(25, 57.5, 70, 80, 92.5)
  label_txt <- c("F", "P", "CR", "D", "HD")
  text(label_pos, rep(y_top, 5), label_txt, col = "#AAAAAA",
       cex = 0.75, font = 2)

  invisible(NULL)
}
