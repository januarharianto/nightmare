# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Verify app loads (primary smoke test)
Rscript -e "source('app.R')"

# Run individual tests
Rscript tests/test_data_import.R
Rscript tests/test_storage.R

# Run app locally
Rscript -e "shiny::runApp()"
```

Tests require `testthat`. Sample data lives in `data/` subfolders (gitignored) and `_sample-data/` — must exist locally.

## Architecture

**Entry point:** `app.R` → `dependencies.R` → `config.R` → `ui.R` → `server.R`

Load order matters: `ui.R` uses bslib/shinyjs/NIGHTMARE_CONFIG at source time, so dependencies and config must load first. `server.R` sources all utils and modules internally.

### Data Flow

```
CSV/XLSX files in data/UNIT/
  → import (R/utils/import/*.R)
  → consolidate (left-join Canvas + consids + plans)
  → studentData reactiveVal
  → modules render views
  → JSON persistence in data/UNIT/.nightmare/
```

Each student row has nested list-columns: `assignments`, `special_consids`, `plan_adjustments`.

### Key Files

| File | Role |
|------|------|
| `R/server.R` | Main server: reactive state, module orchestration, event handling |
| `R/utils/ui_helpers.R` | Builds student detail view (not a module — called directly) |
| `R/utils/import/consolidate.R` | Merges all data sources via left joins |
| `R/utils/weights_data.R` | Grade projection + risk calculation (Arnold & Pistilli model) |
| `R/utils/exam_data.R` | Exam sittings lifecycle, conflict resolution |
| `R/utils/notes_data.R` | Note CRUD, NOTE_TAGS definitions, JSON persistence |
| `R/modules/search_module.R` | Student search (only true Shiny module with NS/moduleServer) |
| `R/modules/extensions_module.R` | Special considerations view + SEAMS2 export |
| `R/modules/exams_module.R` | Exam upload wizard (4-step) |
| `www/custom.css` | All styling |
| `www/search-keyboard.js` | Arrow key navigation for search results (jQuery) |

### Storage Layer

Transient data persists as JSON in `data/UNIT/.nightmare/`:
- `student_notes.json` — notes per student (5 categories: follow-up, at-risk, contacted, resolved, general)
- `exams.json` — exam sittings with conflict resolution (active sitting per student)
- `weights.json` — assessment weight percentages for grade projection
- `match_overrides.json` — manual extension↔assignment matches

## Patterns

### Styling

Clinical minimal aesthetic — no deviations:
- **Colours:** `#000`, `#FFF`, `#CCC`, `#EEE`, `#F5F5F5` only. Metadata labels: `#AAA`
- **No border-radius** anywhere (sharp corners throughout)
- **Layout:** `--sidebar-width: 140px`, `--navbar-height: 72px`
- **Metadata labels:** 10px, uppercase, `#AAAAAA`, 700 weight, 0.5px letter-spacing
- Tag badges (`.source-tag`): black/white active, grey inactive

### Code Conventions

- All `library()` calls go in `R/dependencies.R` only — never in individual files
- Shiny modules use `NS(id)` / `moduleServer(id, ...)` pattern
- `ui_helpers.R` builds the student detail view as a plain function (not a module)
- jQuery is available via Shiny (used in `search-keyboard.js`)

## Gotchas

- **Canvas Section field format varies:** some units start with the code (`BIOL2022 Biology...`), others prefix with `(activity) 2025-UNITCODE-...` — don't anchor regexes to `^`
- **Canvas CSV student names contain commas** (`Last, First`) — always use `read_csv()`, never awk/cut
- **`data/` subfolders are gitignored** — app needs them at runtime
- **`.gitignore` blocks `*.md`** — exceptions exist for `README.md`, `CLAUDE.md`, and `docs/**/*.md`
- **`textInput(label = NULL)`** still generates an empty `<label>` tag — hide with CSS
- **Shiny `.shiny-input-container label`** rule overrides custom label styles — use higher specificity
- **macOS `timeout` command** doesn't exist — use R-based timeouts or backgrounding
- **Disability plans XLSX** has headers in row 3, data from row 4
