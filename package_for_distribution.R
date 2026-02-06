#!/usr/bin/env Rscript

# =============================================================================
# NIGHTMARE Portable Distribution Packaging Script
# =============================================================================
#
# Purpose: Creates a portable distribution package of NIGHTMARE that users
#          can run without installation (R + packages only).
#
# Output:  dist/NIGHTMARE_Portable.zip
#          Contains app, launchers (Windows/Mac), README, optional sample data
#
# Usage:   source("package_for_distribution.R")
#
# Requirements:
#   - All source files must exist (app.R, DESCRIPTION, R/, www/)
#   - Base R only (no external dependencies for packaging)
#   - Idempotent (safe to run multiple times)
#
# =============================================================================

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

APP_CONFIG <- list(
  name = "NIGHTMARE",
  version = "1.0.0",
  description = "Student Data Management System",
  port = 8888,

  # Dependencies (from DESCRIPTION)
  dependencies = c(
    "shiny", "bslib", "readxl", "writexl", "DT",
    "R6", "dplyr", "tidyr", "purrr", "lubridate"
  ),

  # Source files/directories to include
  required_files = c("app.R", "DESCRIPTION"),
  required_dirs = c("R", "www"),
  optional_dirs = c("_sample-data"),

  # Output configuration
  output_dir = "dist",
  package_name = "NIGHTMARE_Portable"
)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Print formatted message with timestamp
msg <- function(..., prefix = "[INFO]") {
  message(sprintf("%s %s %s",
                  prefix,
                  format(Sys.time(), "%H:%M:%S"),
                  paste0(...)))
}

#' Print error message and stop
stop_with_error <- function(...) {
  msg(..., prefix = "[ERROR]")
  stop(paste0(...), call. = FALSE)
}

#' Check if file or directory exists
check_exists <- function(path, type = "file") {
  if (type == "file") {
    if (!file.exists(path)) {
      stop_with_error("Required ", type, " not found: ", path)
    }
  } else if (type == "dir") {
    if (!dir.exists(path)) {
      stop_with_error("Required directory not found: ", path)
    }
  }
  return(TRUE)
}

#' Recursively copy directory
copy_dir <- function(from, to) {
  if (!dir.exists(from)) {
    return(FALSE)
  }

  # Create destination directory
  if (!dir.exists(to)) {
    dir.create(to, recursive = TRUE)
  }

  # Get all files and subdirectories
  items <- list.files(from, full.names = TRUE, recursive = FALSE,
                      all.files = FALSE, include.dirs = TRUE)

  for (item in items) {
    item_name <- basename(item)
    dest_path <- file.path(to, item_name)

    if (dir.exists(item)) {
      # Recursively copy subdirectory
      copy_dir(item, dest_path)
    } else {
      # Copy file
      file.copy(item, dest_path, overwrite = TRUE)
    }
  }

  return(TRUE)
}

#' Get human-readable file size
format_bytes <- function(bytes) {
  if (bytes < 1024) {
    return(sprintf("%d B", bytes))
  } else if (bytes < 1024^2) {
    return(sprintf("%.1f KB", bytes / 1024))
  } else if (bytes < 1024^3) {
    return(sprintf("%.1f MB", bytes / 1024^2))
  } else {
    return(sprintf("%.1f GB", bytes / 1024^3))
  }
}

# -----------------------------------------------------------------------------
# Validation
# -----------------------------------------------------------------------------

validate_source_files <- function(config) {
  msg("Validating source files...")

  # Check required files
  for (file in config$required_files) {
    check_exists(file, type = "file")
    msg("  ✓ ", file)
  }

  # Check required directories
  for (dir in config$required_dirs) {
    check_exists(dir, type = "dir")
    msg("  ✓ ", dir, "/")
  }

  # Check optional directories (warn if missing)
  for (dir in config$optional_dirs) {
    if (dir.exists(dir)) {
      msg("  ✓ ", dir, "/ (optional)")
    } else {
      msg("  ⚠ ", dir, "/ not found (optional, skipping)", prefix = "[WARN]")
    }
  }

  msg("All required source files validated")
}

# -----------------------------------------------------------------------------
# Directory Structure Creation
# -----------------------------------------------------------------------------

create_directory_structure <- function(config) {
  msg("Creating directory structure...")

  # Build paths
  dist_root <- file.path(config$output_dir, config$package_name)

  # Clean existing distribution
  if (dir.exists(config$output_dir)) {
    msg("  Removing old dist/ directory...")
    unlink(config$output_dir, recursive = TRUE)
  }

  # Create fresh structure
  dir.create(dist_root, recursive = TRUE)
  msg("  Created: ", dist_root)

  return(dist_root)
}

# -----------------------------------------------------------------------------
# Copy Source Files
# -----------------------------------------------------------------------------

copy_source_files <- function(config, dist_root) {
  msg("Copying source files...")

  # Copy required files
  for (file in config$required_files) {
    dest <- file.path(dist_root, file)
    file.copy(file, dest, overwrite = TRUE)
    msg("  Copied: ", file)
  }

  # Copy required directories
  for (dir in config$required_dirs) {
    dest <- file.path(dist_root, dir)
    copy_dir(dir, dest)
    msg("  Copied: ", dir, "/")
  }

  # Copy optional directories if present
  for (dir in config$optional_dirs) {
    if (dir.exists(dir)) {
      dest <- file.path(dist_root, dir)
      copy_dir(dir, dest)
      msg("  Copied: ", dir, "/ (optional)")
    }
  }

  msg("Source files copied successfully")
}

# -----------------------------------------------------------------------------
# Generate Windows Launcher
# -----------------------------------------------------------------------------

generate_windows_launcher <- function(config, dist_root) {
  msg("Generating Windows launcher...")

  launcher_path <- file.path(dist_root, "START_NIGHTMARE.bat")

  # Build package installation command
  pkg_install_cmd <- sprintf(
    'R -e "install.packages(c(%s), repos=\\"https://cran.rstudio.com\\", quiet=TRUE)"',
    paste0("'", config$dependencies, "'", collapse = ", ")
  )

  launcher_content <- sprintf('@echo off
REM =============================================================================
REM NIGHTMARE - Student Data Management System
REM Windows Launcher
REM =============================================================================

title NIGHTMARE - Student Data Manager

echo.
echo ========================================
echo   NIGHTMARE v%s
echo   Student Data Management System
echo ========================================
echo.

REM Check if R is installed
where R >nul 2>nul
if %%ERRORLEVEL%% NEQ 0 (
    echo [ERROR] R is not installed or not in PATH
    echo.
    echo Please install R from: https://cran.r-project.org/
    echo.
    pause
    exit /b 1
)

echo [INFO] R installation found
echo.

REM Check if this is first run (packages not installed)
if not exist ".nightmare_initialized" (
    echo [INFO] First run detected - installing required packages...
    echo [INFO] This may take 3-5 minutes and requires internet connection.
    echo.

    %s

    if %%ERRORLEVEL%% NEQ 0 (
        echo.
        echo [ERROR] Package installation failed
        echo [ERROR] Please check your internet connection and try again
        echo.
        pause
        exit /b 1
    )

    REM Create marker file to skip package installation on subsequent runs
    echo. > .nightmare_initialized
    echo [INFO] Packages installed successfully
    echo.
)

REM Find first available port starting from %d
setlocal enabledelayedexpansion
set PORT=%d
:check_port
netstat -ano | findstr :%%PORT%% >nul 2>nul
if %%ERRORLEVEL%% EQU 0 (
    set /a PORT+=1
    if %%PORT%% LSS 8920 goto check_port
    echo [WARN] All ports 8888-8920 are in use, trying default port anyway...
    set PORT=%d
)

echo [INFO] Starting NIGHTMARE...
echo [INFO] Your browser will open automatically
echo [INFO] Server running at: http://127.0.0.1:!PORT!
echo.
echo [INFO] Press Ctrl+C to stop the server
echo.

REM Launch Shiny app
R -e "shiny::runApp(port=!PORT!, launch.browser=TRUE, quiet=TRUE)"

if %%ERRORLEVEL%% NEQ 0 (
    echo.
    echo [ERROR] Failed to start NIGHTMARE
    echo [ERROR] Port !PORT! may already be in use
    echo.
    pause
    exit /b 1
)
',
    config$version,
    pkg_install_cmd,
    config$port,
    config$port,
    config$port
  )

  writeLines(launcher_content, launcher_path)
  msg("  Created: START_NIGHTMARE.bat")
}

# -----------------------------------------------------------------------------
# Generate Mac Launcher
# -----------------------------------------------------------------------------

generate_mac_launcher <- function(config, dist_root) {
  msg("Generating Mac launcher...")

  launcher_path <- file.path(dist_root, "START_NIGHTMARE.command")

  # Build package installation command
  pkg_install_cmd <- sprintf(
    'R -e "install.packages(c(%s), repos=\\"https://cran.rstudio.com\\", quiet=TRUE)"',
    paste0("'", config$dependencies, "'", collapse = ", ")
  )

  launcher_content <- sprintf('#!/bin/bash

# =============================================================================
# NIGHTMARE - Student Data Management System
# macOS Launcher
# =============================================================================

# Get the directory where this script is located
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPT_DIR"

echo ""
echo "========================================"
echo "  NIGHTMARE v%s"
echo "  Student Data Management System"
echo "========================================"
echo ""

# Check if R is installed
if ! command -v R &> /dev/null; then
    echo "[ERROR] R is not installed or not in PATH"
    echo ""
    echo "Please install R from: https://cran.r-project.org/"
    echo ""
    read -p "Press Enter to exit..."
    exit 1
fi

echo "[INFO] R installation found"
echo ""

# Check if this is first run (packages not installed)
if [ ! -f ".nightmare_initialized" ]; then
    echo "[INFO] First run detected - installing required packages..."
    echo "[INFO] This may take 3-5 minutes and requires internet connection."
    echo ""

    %s

    if [ $? -ne 0 ]; then
        echo ""
        echo "[ERROR] Package installation failed"
        echo "[ERROR] Please check your internet connection and try again"
        echo ""
        read -p "Press Enter to exit..."
        exit 1
    fi

    # Create marker file to skip package installation on subsequent runs
    touch .nightmare_initialized
    echo "[INFO] Packages installed successfully"
    echo ""
fi

# Find first available port starting from %d
find_available_port() {
    local port=%d
    while lsof -i :$port >/dev/null 2>&1; do
        port=$((port + 1))
        if [ $port -ge 8920 ]; then
            echo "[WARN] All ports 8888-8920 are in use, trying default port anyway..."
            echo %d
            return 1
        fi
    done
    echo $port
}

PORT=$(find_available_port)

echo "[INFO] Starting NIGHTMARE..."
echo "[INFO] Your browser will open automatically"
echo "[INFO] Server running at: http://127.0.0.1:$PORT"
echo ""
echo "[INFO] Press Ctrl+C to stop the server"
echo ""

# Launch Shiny app
R -e "shiny::runApp(port=$PORT, launch.browser=TRUE, quiet=TRUE)"

if [ $? -ne 0 ]; then
    echo ""
    echo "[ERROR] Failed to start NIGHTMARE"
    echo "[ERROR] Port $PORT may already be in use"
    echo ""
    read -p "Press Enter to exit..."
    exit 1
fi
',
    config$version,
    pkg_install_cmd,
    config$port,
    config$port,
    config$port
  )

  writeLines(launcher_content, launcher_path)

  # Make executable (chmod 755)
  Sys.chmod(launcher_path, mode = "0755")
  msg("  Created: START_NIGHTMARE.command (executable)")
}

# -----------------------------------------------------------------------------
# Generate README
# -----------------------------------------------------------------------------

generate_readme <- function(config, dist_root) {
  msg("Generating README...")

  readme_path <- file.path(dist_root, "README.txt")

  readme_content <- sprintf('===============================================================================
NIGHTMARE - Student Data Management System
Version %s
Generated: %s
===============================================================================

QUICK START
-----------

WINDOWS USERS:
  1. Double-click "START_NIGHTMARE.bat"
  2. Wait for your browser to open automatically
  3. On first run, packages will install (3-5 minutes, internet required)

MAC USERS:
  1. Double-click "START_NIGHTMARE.command"
  2. If prompted, allow the script to run (System Preferences > Security)
  3. Wait for your browser to open automatically
  4. On first run, packages will install (3-5 minutes, internet required)


FIRST RUN NOTICE
----------------

The first time you launch NIGHTMARE, it will automatically install required
R packages. This process:
  - Takes 3-5 minutes
  - Requires internet connection
  - Only happens once (creates .nightmare_initialized marker file)

Subsequent launches will be much faster (a few seconds).


WHAT IS NIGHTMARE?
------------------

NIGHTMARE is a Shiny-based data management system designed for university
unit coordinators. It consolidates data from multiple sources:

  - Canvas gradebooks (student grades, assignments)
  - Special Considerations (extension requests, approvals)
  - Disability Plans (adjustments, accommodations)

Key Features:
  - Multi-source data consolidation with unit filtering
  - Extension calculation with conflict detection
  - At-risk student identification
  - Professional exports (Canvas CSV, Extensions CSV, At-Risk List, Excel)
  - Minimal high-contrast UI for reduced cognitive load


HOW TO USE
----------

1. Launch NIGHTMARE using the appropriate launcher for your OS
2. Import your data files:
   - Canvas Gradebook (CSV)
   - Special Considerations (CSV)
   - Disability Plans (XLSX)
3. Use filters to identify at-risk students or those with extensions
4. View detailed student information in the right panel
5. Export data in your preferred format

Sample data is included in the _sample-data/ directory for testing.


TROUBLESHOOTING
---------------

PROBLEM: "R is not installed or not in PATH"
SOLUTION: Install R from https://cran.r-project.org/
          Make sure to add R to your system PATH during installation.

PROBLEM: "Package installation failed"
SOLUTION: Check your internet connection and try again.
          If the problem persists, manually install packages in R:
          install.packages(c(%s))

PROBLEM: "Port %d may already be in use"
SOLUTION: Close any other applications using port %d, or edit the
          launcher file to use a different port number.

PROBLEM: (Mac) "Permission denied"
SOLUTION: Right-click START_NIGHTMARE.command, select "Open With" > Terminal
          Or run in Terminal: chmod +x START_NIGHTMARE.command

PROBLEM: Browser does not open automatically
SOLUTION: Manually open your browser and navigate to:
          http://127.0.0.1:%d


SYSTEM REQUIREMENTS
-------------------

- R version 4.0.0 or higher
- Modern web browser (Chrome, Firefox, Safari, Edge)
- Internet connection (first run only)
- 4GB RAM minimum (8GB recommended for large datasets)


FILE STRUCTURE
--------------

NIGHTMARE_Portable/
  ├── app.R                      # Main application entry point
  ├── DESCRIPTION                # Package metadata
  ├── R/                         # Application logic
  │   ├── ui.R                  # User interface
  │   ├── server.R              # Server logic
  │   ├── config.R              # Configuration
  │   ├── dependencies.R        # Dependency loading
  │   ├── schema.R              # Data schema
  │   ├── modules/              # UI modules
  │   └── utils/                # Utilities (import, export, etc.)
  ├── www/                       # Static assets
  │   └── custom.css            # Clinical minimal theme
  ├── _sample-data/             # Sample data for testing (optional)
  ├── START_NIGHTMARE.bat       # Windows launcher
  ├── START_NIGHTMARE.command   # Mac launcher
  └── README.txt                # This file


SUPPORT
-------

For issues, questions, or feedback, contact your system administrator
or the NIGHTMARE development team.

Project Location: /Users/jhar8696/Sydney Uni Dropbox/Januar Harianto/projects/automation/nightmare

===============================================================================
',
    config$version,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    paste0("'", config$dependencies, "'", collapse = ", "),
    config$port,
    config$port,
    config$port
  )

  writeLines(readme_content, readme_path)
  msg("  Created: README.txt")
}

# -----------------------------------------------------------------------------
# Create Zip Package
# -----------------------------------------------------------------------------

create_zip_package <- function(config, dist_root) {
  msg("Creating zip package...")

  # Build zip file path
  zip_file <- file.path(config$output_dir, paste0(config$package_name, ".zip"))

  # Get all files in dist_root
  files_to_zip <- list.files(dist_root, full.names = TRUE, recursive = TRUE)

  # Create zip archive
  # Use relative paths inside zip
  current_dir <- getwd()
  setwd(config$output_dir)

  tryCatch({
    zip::zip(
      zipfile = basename(zip_file),
      files = list.files(config$package_name, recursive = TRUE),
      mode = "cherry-pick"
    )
  }, error = function(e) {
    # Fallback to utils::zip if zip package not available
    utils::zip(
      zipfile = basename(zip_file),
      files = config$package_name,
      flags = "-r9Xq"
    )
  })

  setwd(current_dir)

  # Verify zip was created
  if (!file.exists(zip_file)) {
    stop_with_error("Failed to create zip package")
  }

  msg("  Created: ", basename(zip_file))
  return(zip_file)
}

# -----------------------------------------------------------------------------
# Main Execution
# -----------------------------------------------------------------------------

main <- function() {
  cat("\n")
  cat("=============================================================================\n")
  cat("  NIGHTMARE Packaging Script\n")
  cat("=============================================================================\n")
  cat("\n")

  tryCatch({
    # Step 1: Validate source files
    validate_source_files(APP_CONFIG)
    cat("\n")

    # Step 2: Create directory structure
    dist_root <- create_directory_structure(APP_CONFIG)
    cat("\n")

    # Step 3: Copy source files
    copy_source_files(APP_CONFIG, dist_root)
    cat("\n")

    # Step 4: Generate launchers
    generate_windows_launcher(APP_CONFIG, dist_root)
    cat("\n")

    generate_mac_launcher(APP_CONFIG, dist_root)
    cat("\n")

    # Step 5: Generate README
    generate_readme(APP_CONFIG, dist_root)
    cat("\n")

    # Step 6: Create zip package
    zip_file <- create_zip_package(APP_CONFIG, dist_root)
    cat("\n")

    # Success message
    zip_size <- file.info(zip_file)$size
    cat("=============================================================================\n")
    cat("  ✓ SUCCESS\n")
    cat("=============================================================================\n")
    cat("\n")
    msg("Package created successfully", prefix = "[SUCCESS]")
    msg("Location: ", normalizePath(zip_file), prefix = "[SUCCESS]")
    msg("Size: ", format_bytes(zip_size), prefix = "[SUCCESS]")
    cat("\n")
    cat("Distribution package is ready for deployment.\n")
    cat("\n")

  }, error = function(e) {
    cat("\n")
    cat("=============================================================================\n")
    cat("  ✗ PACKAGING FAILED\n")
    cat("=============================================================================\n")
    cat("\n")
    msg("Error: ", conditionMessage(e), prefix = "[ERROR]")
    cat("\n")
    stop("Packaging aborted", call. = FALSE)
  })
}

# Run main function
main()
