#!/usr/bin/env Rscript
#' NIGHTMARE Installation & Functionality Verification Script
#'
#' Run this script to verify that all components of the NIGHTMARE application
#' are working correctly before deployment.
#'
#' Usage:
#'   source("VERIFY_INSTALLATION.R")
#'   or
#'   Rscript VERIFY_INSTALLATION.R

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║          NIGHTMARE Installation & Verification Script          ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Check working directory
wd <- getwd()
cat(sprintf("Working Directory: %s\n", wd))

# 1. Check for required files
cat("\n--- Checking Project Structure ---\n")
required_files <- c(
  "app.R",
  "DESCRIPTION",
  "R/ui.R",
  "R/server.R",
  "R/schema.R",
  "R/utils/import/canvas.R",
  "R/utils/import/special_consids.R",
  "R/utils/import/disability_plans.R",
  "R/utils/import/consolidate.R",
  "R/utils/import/file_detection.R",
  "R/utils/extensions.R",
  "R/utils/risk_scoring.R",
  "R/utils/history.R",
  "R/utils/export.R",
  "R/modules/dashboard_module.R",
  "R/modules/student_detail_module.R",
  "R/modules/backup_module.R",
  "R/modules/export_module.R",
  "data/sample_data.rds",
  "_sample-data/canvas gradebook.csv",
  "_sample-data/special considerations.csv",
  "_sample-data/plans.xlsx"
)

missing_files <- c()
for (file in required_files) {
  if (file.exists(file)) {
    cat(sprintf("  ✓ %s\n", file))
  } else {
    cat(sprintf("  ✗ MISSING: %s\n", file))
    missing_files <- c(missing_files, file)
  }
}

if (length(missing_files) > 0) {
  cat("\nWARNING: Missing files detected!\n")
}

# 2. Check required R packages
cat("\n--- Checking Required Packages ---\n")
required_packages <- c(
  "shiny", "bslib", "dplyr", "readr", "readxl", "writexl",
  "DT", "reactable", "R6", "testthat"
)

missing_packages <- c()
for (pkg in required_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("  ✓ %s\n", pkg))
  } else {
    cat(sprintf("  ✗ MISSING: %s\n", pkg))
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat(sprintf("\nInstalling missing packages: %s\n", paste(missing_packages, collapse = ", ")))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

# 3. Load and test core modules
cat("\n--- Testing Core Modules ---\n")

tryCatch({
  source("R/schema.R")
  cat("  ✓ schema.R loaded\n")
}, error = function(e) {
  cat(sprintf("  ✗ Error loading schema.R: %s\n", e$message))
})

tryCatch({
  source("R/utils/import/canvas.R")
  source("R/utils/import/special_consids.R")
  source("R/utils/import/disability_plans.R")
  source("R/utils/import/consolidate.R")
  source("R/utils/import/file_detection.R")
  cat("  ✓ import modules loaded\n")
}, error = function(e) {
  cat(sprintf("  ✗ Error loading import modules: %s\n", e$message))
})

tryCatch({
  source("R/utils/extensions.R")
  cat("  ✓ extensions.R loaded\n")
}, error = function(e) {
  cat(sprintf("  ✗ Error loading extensions.R: %s\n", e$message))
})

tryCatch({
  source("R/utils/risk_scoring.R")
  cat("  ✓ risk_scoring.R loaded\n")
}, error = function(e) {
  cat(sprintf("  ✗ Error loading risk_scoring.R: %s\n", e$message))
})

tryCatch({
  source("R/utils/history.R")
  cat("  ✓ history.R loaded\n")
}, error = function(e) {
  cat(sprintf("  ✗ Error loading history.R: %s\n", e$message))
})

tryCatch({
  source("R/utils/export.R")
  cat("  ✓ export.R loaded\n")
}, error = function(e) {
  cat(sprintf("  ✗ Error loading export.R: %s\n", e$message))
})

# 4. Test data import functionality
cat("\n--- Testing Data Import ---\n")

tryCatch({
  cat("  Loading Canvas gradebook...")
  canvas <- import_canvas_grades("_sample-data/canvas gradebook.csv")
  cat(sprintf(" ✓ (%d students)\n", nrow(canvas)))
}, error = function(e) {
  cat(sprintf(" ✗ Error: %s\n", e$message))
})

tryCatch({
  cat("  Loading special considerations...")
  consids <- import_special_considerations("_sample-data/special considerations.csv")
  cat(sprintf(" ✓ (%d entries)\n", nrow(consids)))
}, error = function(e) {
  cat(sprintf(" ✗ Error: %s\n", e$message))
})

tryCatch({
  cat("  Loading disability plans...")
  plans <- import_disability_plans("_sample-data/plans.xlsx")
  cat(sprintf(" ✓ (%d rows)\n", nrow(plans)))
}, error = function(e) {
  cat(sprintf(" ✗ Error: %s\n", e$message))
})

# 5. Test consolidation
cat("\n--- Testing Data Consolidation ---\n")

tryCatch({
  cat("  Consolidating data...")
  consolidated <- consolidate_student_data(canvas, consids, plans)
  cat(sprintf(" ✓ (%d students)\n", nrow(consolidated)))

  cat("  Checking required columns...\n")
  required_cols <- c("student_id", "name", "final_grade", "unit_of_study")
  for (col in required_cols) {
    if (col %in% names(consolidated)) {
      cat(sprintf("    ✓ %s\n", col))
    } else {
      cat(sprintf("    ✗ Missing: %s\n", col))
    }
  }
}, error = function(e) {
  cat(sprintf(" ✗ Error: %s\n", e$message))
})

# 6. Test risk scoring
cat("\n--- Testing Risk Scoring ---\n")

tryCatch({
  cat("  Applying risk scores...")
  scored <- apply_risk_scoring(consolidated)

  risk_dist <- table(scored$risk_category)
  cat(sprintf(" ✓ Scoring complete\n")
  cat(sprintf("  Risk distribution:\n")
  cat(sprintf("    Low: %d\n", risk_dist[1] %||% 0))
  cat(sprintf("    Medium: %d\n", risk_dist[2] %||% 0))
  cat(sprintf("    High: %d\n", risk_dist[3] %||% 0))
}, error = function(e) {
  cat(sprintf(" ✗ Error: %s\n", e$message))
})

# 7. Test export functions
cat("\n--- Testing Export Functions ---\n")

tryCatch({
  cat("  Canvas export...")
  export_canvas <- export_for_canvas(scored)
  cat(sprintf(" ✓ (%d rows)\n", nrow(export_canvas)))
}, error = function(e) {
  cat(sprintf(" ✗ Error: %s\n", e$message))
})

tryCatch({
  cat("  Extensions report...")
  export_ext <- export_extensions_report(scored)
  cat(sprintf(" ✓ (%d rows)\n", nrow(export_ext)))
}, error = function(e) {
  cat(sprintf(" ✗ Error: %s\n", e$message))
})

tryCatch({
  cat("  At-risk report...")
  export_risk <- export_at_risk_report(scored)
  cat(sprintf(" ✓ (%d rows)\n", nrow(export_risk)))
}, error = function(e) {
  cat(sprintf(" ✗ Error: %s\n", e$message))
})

# 8. Test undo/redo
cat("\n--- Testing Undo/Redo ---\n")

tryCatch({
  cat("  Creating HistoryManager...")
  history <- HistoryManager$new()
  cat(" ✓\n")
  cat("  Testing push/undo/redo...")
  history$push(list(data = 1), "Test state")
  history$undo()
  history$redo()
  cat(" ✓\n")
}, error = function(e) {
  cat(sprintf(" ✗ Error: %s\n", e$message))
})

# 9. Summary
cat("\n╔════════════════════════════════════════════════════════════════╗\n")
cat("║                    VERIFICATION COMPLETE                       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

cat("\nNext Steps:\n")
cat("  1. Review any warnings or errors above\n")
cat("  2. Install missing packages if needed\n")
cat("  3. Run the application: shiny::runApp()\n")
cat("  4. Test in browser at http://127.0.0.1:3838/\n")
cat("  5. Import sample data and verify functionality\n")
cat("\nFor deployment to shinylive:\n")
cat("  shinylive::export('.', 'site')\n")
cat("  httpuv::runStaticServer('site/')\n")

cat("\n")
