@echo off
REM NIGHTMARE - Double-click launcher for Windows
REM This script checks for R, installs packages if needed, and launches the app

title NIGHTMARE - Student Data Manager
color 0A

echo ========================================
echo    NIGHTMARE - Student Data Manager
echo ========================================
echo.

REM Check if R is installed
where R >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo [ERROR] R is not installed on this computer.
    echo.
    echo Please install R from: https://cran.r-project.org/bin/windows/base/
    echo After installing R, run this script again.
    echo.
    pause
    exit /b 1
)

echo [OK] R is installed
echo.

REM Check if this is the first run (packages need installation)
if not exist ".nightmare_initialized" (
    echo [FIRST RUN] Installing required packages...
    echo This will take 3-5 minutes. Please wait...
    echo.

    R --vanilla --quiet -e "install.packages(c('shiny', 'bslib', 'dplyr', 'readr', 'readxl', 'writexl', 'DT', 'htmltools'), repos='https://cran.rstudio.com/', quiet=TRUE)"

    if %ERRORLEVEL% NEQ 0 (
        echo.
        echo [ERROR] Package installation failed.
        echo Please check your internet connection and try again.
        echo.
        pause
        exit /b 1
    )

    REM Mark as initialized
    echo. > .nightmare_initialized
    echo [OK] Packages installed successfully!
    echo.
)

echo Starting NIGHTMARE...
echo The app will open in your browser at: http://localhost:8888
echo.
echo *** IMPORTANT ***
echo Do NOT close this window while using the app!
echo To stop the app, close your browser and press Ctrl+C here.
echo.

REM Launch the Shiny app
R -e "shiny::runApp(port=8888, launch.browser=TRUE, quiet=TRUE)"

echo.
echo App closed.
pause
