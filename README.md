# NIGHTMARE - Student Data Management System

**N**o-nonsense **I**ntegrated **G**rades **H**andling with **T**ransparent **M**anagement, **A**ccessibility **R**eview, and **E**xtension tracking

A Shiny web application for consolidating and managing student data from multiple University of Sydney systems (Canvas, Special Considerations, Disability Plans).

---

## Features

### 🎯 Core Functionality

- **Multi-Source Data Import**: Seamlessly import from Canvas, Special Considerations, and Disability Plans
- **Automatic Consolidation**: Merge data from all sources into a unified student view
- **Risk Assessment**: Identify at-risk students using multi-factor analysis
- **Extension Tracking**: Track and manage assessment extensions and disability accommodations
- **Flexible Exports**: Export to Canvas, CSV, or comprehensive Excel reports
- **Browser Storage**: Persistent local storage with backup/restore capabilities

### 📊 Data Sources

| Source | Format | Purpose |
|--------|--------|---------|
| **Canvas Gradebook** | CSV | Student grades, assignments, enrollment |
| **Special Considerations** | CSV | Approved extensions, replacement exams |
| **Disability Plans** | XLSX | Accessibility adjustments, accommodations |

### 🔍 Risk Scoring Algorithm

Students are automatically scored (0-100) and categorized based on:

- **Multiple Extensions** (>2 assessments) → +25 points
- **Failing Assessments** (grade <50 or weighted assignments <40) → +35 points
- **Missing Work** (zero scores without extensions) → +40 points
- **Near Policy Limit** (≥4 extensions granted) → +30 points

**Risk Categories:**
- **Low Risk** (0-24): Students on track
- **Medium Risk** (25-50): Monitor closely
- **High Risk** (51-100): Urgent intervention needed

---

## Quick Start

### Prerequisites

- R 4.0.0 or higher
- RStudio (recommended)
- Required R packages:
  ```r
  install.packages(c(
    "shiny",
    "shinydashboard",
    "shinyjs",
    "dplyr",
    "readr",
    "readxl",
    "writexl",
    "stringr",
    "tidyr",
    "purrr",
    "DT",
    "plotly",
    "testthat"
  ))
  ```

### Installation

1. **Clone or download** this repository
2. **Prepare sample data** in `_sample-data/` directory:
   - `canvas gradebook.csv`
   - `special considerations.csv`
   - `plans.xlsx`
3. **Run the app**:
   ```r
   setwd("/path/to/nightmare")
   shiny::runApp()
   ```

### First-Time Setup

1. Launch the app
2. Navigate to **"Import Data"** tab
3. Upload your three data files
4. Click **"Consolidate Data"**
5. Explore results in other tabs

---

## Usage Guide

### 1. Import Data

**Upload Files:**
- Drag and drop or click to select files
- App auto-detects file types
- Status indicators show upload progress

**Supported Formats:**
- Canvas: CSV export from Canvas gradebook
- Special Considerations: CSV export from ServiceNow
- Disability Plans: Excel (.xlsx) from Disability Services

**Validation:**
- Missing columns → Informative error
- Invalid formats → Suggestions for fixing
- Empty files → Clear error message

### 2. Data Summary

View consolidated statistics:
- Total students enrolled
- Students with extensions
- Students with disability plans
- At-risk student counts
- Average final grades

**Filters:**
- Unit of study dropdown
- Search by student ID or name
- Risk category filter

### 3. Student Table

**Interactive Table Features:**
- Search across all columns
- Sort by any column
- Click row to see detailed view
- Export table to CSV

**Columns Displayed:**
- Student ID, Name, Email
- Final Grade
- Risk Score & Category
- Extension count
- Disability plan status
- Risk factors (expandable)

### 4. At-Risk Students

**Focused View:**
- Only High and Medium risk students
- Sorted by risk score (highest first)
- Detailed risk factor breakdown
- Recommended actions

**Risk Factor Examples:**
- "3 assessments with extensions (Quiz 1, Assignment 2, Lab Report 3)"
- "Final grade: 45.0% (failing)"
- "Missing: Assignment 4 (15% weight, no extension)"
- "4 total extensions granted (approaching policy limit)"

### 5. Extensions & Plans

**Extensions Report:**
- One row per extension
- Assessment name
- Extension days/date
- Reason (Special Consid or Disability Plan)
- Exportable to CSV

**Disability Plans:**
- Active plans only
- Adjustment types listed
- Filterable by unit

### 6. Data Visualization

**Charts Included:**
- Risk category distribution (pie chart)
- Grade distribution histogram
- Extensions by student (bar chart)
- Risk score vs. final grade (scatter plot)

**Interactive:**
- Hover for details
- Click to filter
- Zoom and pan
- Export as PNG

### 7. Export Data

**Export Options:**

| Export Type | Format | Purpose |
|-------------|--------|---------|
| **For Canvas** | CSV | Bulk grade upload to Canvas |
| **Extensions Report** | CSV | Detailed extension breakdown |
| **At-Risk Report** | CSV | High/Medium risk students with actions |
| **Comprehensive Report** | XLSX | Multi-sheet workbook with all data |
| **Backup (RDS)** | RDS | Full data backup with metadata |

**Comprehensive Report Sheets:**
1. Summary - Key statistics
2. All Students - Complete student list
3. Extensions - Extension breakdown
4. At-Risk Students - Risk analysis
5. Disability Plans - Accommodation details

---

## Data Format Requirements

### Canvas Gradebook CSV

**Required Columns:**
- `Student` - Student name
- `ID` - Canvas internal ID
- `SIS User ID` - Student ID (e.g., "530485801")
- `SIS Login ID` - Unikey (e.g., "lsax8243")
- `Section` - Section including unit code

**Assignment Columns:**
- Format: `Assignment Name [weight%] (id)`
- Example: `Quiz 1 [10%] (12345)`

**Export from Canvas:**
1. Open Gradebook
2. Click "Export" → "Export Entire Gradebook"
3. Save as CSV

### Special Considerations CSV

**Required Columns:**
- `number` - Ticket ID
- `state` - Must be "Approved"
- `student_id` - Student ID
- `assessment_title` - Assessment name
- `u_outcome_type` - Outcome (extension/replacement exam)
- `extension_in_calendar_days` - Extension date (dd-mm-yyyy)
- `availability` - Contains unit code

**Export from ServiceNow:**
1. Filter to approved considerations
2. Export with all columns included
3. Save as CSV

### Disability Plans XLSX

**Required Structure:**
- Headers in **row 3**
- Data starts in **row 4**

**Required Columns:**
- `Year` - Academic year
- `Session` - Semester
- `UoS Code` - Unit code (e.g., "BIOL2022")
- `SID` - Student ID
- `Preferred Name` - Student name
- Adjustment columns (variable names)

**Notes:**
- Adjustment columns have variable names (e.g., "Extra Time", "Rest Breaks")
- Non-empty cells indicate active adjustments
- File format must be `.xlsx` (not `.xls`)

---

## Browser Compatibility

### Recommended Browsers

| Browser | Version | Status | Notes |
|---------|---------|--------|-------|
| **Chrome** | 120+ | ✅ Recommended | Best performance |
| **Firefox** | 115+ | ✅ Supported | Good performance |
| **Safari** | 17+ | ✅ Supported | Good performance |
| **Edge** | 120+ | ✅ Recommended | Best performance |

### Browser Requirements

- **JavaScript enabled**
- **Cookies/local storage enabled**
- **Normal browsing mode** (not private/incognito)
- **Modern browser** (released within last 2 years)

### Storage Limits

- **IndexedDB quota**: ~50MB (browser-dependent)
- **Recommended**: Export backups regularly
- **Warning shown**: When storage approaches limit

---

## Performance

### Target Times

All operations tested with ~255 students:

| Operation | Target | Typical |
|-----------|--------|---------|
| Canvas import | <5s | ~1-2s |
| Special considerations import | <5s | ~1-2s |
| Disability plans import | <5s | ~1-2s |
| Data consolidation | <2s | ~0.5s |
| Risk scoring | <2s | ~0.5s |
| All exports | <5s | ~1-2s |
| **Full pipeline** | **<15s** | **~5-8s** |

### Performance Tips

- **Close other tabs** to free memory
- **Use Chrome/Edge** for best performance
- **Export backups** to reduce storage usage
- **Filter data** before large exports

---

## Architecture

### Project Structure

```
nightmare/
├── app.R                    # Main Shiny app entry point
├── R/
│   ├── schema.R            # Student data schema definition
│   └── utils/
│       ├── data_import.R   # Import functions (Canvas, Special Consids, Plans)
│       ├── extensions.R    # Extension calculation logic
│       ├── risk_scoring.R  # Risk assessment algorithm
│       └── export.R        # Export functions (CSV, XLSX)
├── _sample-data/           # Sample data files (not in production)
│   ├── canvas gradebook.csv
│   ├── special considerations.csv
│   └── plans.xlsx
├── tests/                  # Test suite (testthat)
│   ├── test_data_import.R
│   ├── test_storage.R
│   ├── test_risk_scoring.R
│   ├── test_exports.R
│   └── test_performance.R
├── docs/
│   └── TESTING_GUIDE.md   # Comprehensive testing documentation
└── README.md              # This file
```

### Data Flow

```
[Canvas CSV] ─┐
              ├─→ [Import] ─→ [Consolidate] ─→ [Risk Score] ─→ [Display/Export]
[Consids CSV]─┤                    ↓
              │              [IndexedDB Storage]
[Plans XLSX] ─┘                    ↓
                            [Backup/Restore]
```

### Key Design Decisions

1. **Client-side storage**: IndexedDB for privacy and offline capability
2. **No server required**: Pure Shiny app, no backend database
3. **Immutable imports**: Original data never modified, only consolidated view
4. **Left joins**: All Canvas students preserved, other sources supplementary
5. **Nested structures**: Assignments and extensions stored as list-columns

---

## Testing

### Run All Tests

```r
# Set working directory
setwd("/path/to/nightmare")

# Run full test suite
testthat::test_dir("tests/")
```

### Test Coverage

- **47 total tests** across 5 test files
- **Data Import** (16 tests): Validation, file detection, consolidation
- **Storage** (6 tests): RDS/CSV cycles, integrity
- **Risk Scoring** (7 tests): Score ranges, categories, factors
- **Exports** (9 tests): Format validation, data integrity
- **Performance** (9 tests): All operations <target times

See **[TESTING_GUIDE.md](docs/TESTING_GUIDE.md)** for comprehensive testing documentation.

---

## Troubleshooting

### Common Issues

**"Cannot find required columns"**
- Verify CSV/XLSX structure matches requirements
- Check that Canvas export includes all assignment columns
- Ensure headers are in correct row (row 3 for plans.xlsx)

**"Storage quota exceeded"**
- Export backup RDS file
- Clear browser storage
- Use browser storage settings to increase quota

**"File upload not working"**
- Check file size (<50MB recommended)
- Verify file format (CSV or XLSX)
- Try different browser
- Disable browser extensions

**"Risk scores all zero"**
- Ensure special considerations data loaded
- Check that assignment scores are present in Canvas data
- Verify data consolidation completed successfully

**"Data disappeared after refresh"**
- Check browser storage enabled
- Not in private/incognito mode
- Import backup RDS if available

### Getting Help

1. Check **[TESTING_GUIDE.md](docs/TESTING_GUIDE.md)**
2. Review error messages (often include solutions)
3. Check browser console for JavaScript errors
4. Contact developer (see below)

---

## Known Limitations

1. **Browser Storage**: ~50MB limit, cleared with browser cache
2. **Testing**: Only verified on macOS with Chrome/Firefox/Safari
3. **Disability Plans**: Rigid XLSX structure required
4. **Manual Edits**: No built-in validation for manual corrections
5. **Large Datasets**: Not tested with >1000 students

---

## Security & Privacy

### Data Handling

- **Local storage only**: Data never sent to external servers
- **Browser storage**: IndexedDB confined to browser
- **No tracking**: No analytics or telemetry
- **No authentication**: Suitable for trusted users only

### Recommendations

- **Run locally**: Don't deploy on public server without authentication
- **Clear storage**: After each session on shared computers
- **Export backups**: Keep RDS backups in secure location
- **Access control**: Restrict app access to authorized users

---

## Contributing

### Development Setup

1. Clone repository
2. Install dependencies
3. Run tests: `testthat::test_dir("tests/")`
4. Make changes
5. Verify tests pass
6. Submit pull request

### Code Style

- Follow tidyverse style guide
- Document all functions with roxygen comments
- Write tests for new features
- Update README and TESTING_GUIDE

---

## License

[Specify license here]

---

## Contact

**Developer:** Januar Harianto
**Institution:** University of Sydney
**Email:** [email]
**GitHub:** [repository URL]

---

## Changelog

### Version 0.1.0 (2024)
- Initial release
- Canvas, Special Considerations, and Disability Plans import
- Risk scoring algorithm
- Multi-format exports
- IndexedDB storage
- Comprehensive test suite (47 tests)
- Full documentation
