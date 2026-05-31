# NIGHTMARE

**N**ew **I**ncredibly **G**litchy **H**acking **T**ool to **M**anage **A**ll **R**ecords **E**ffortlessly
*...or, a New Incredibly Glitchy Hacking Tool to Manage Records that Enrage me*

A Shiny app that wrangles student data from Canvas, special considerations, and disability plans into one place. It's held together with duct tape and spite, but it mostly works. This is a personal implementation of what might be a multi-million dollar system at USYD (I think?), but imagine that the budget is $0.

## Install

```r
# install from local source
devtools::install("path/to/nightmare")

# or just load during development
devtools::load_all()
```

## Usage

From an installed package:

```r
nightmare::run_nightmare()
```

From a local checkout during development:

```r
pkgload::load_all()
run_nightmare()
```

Or from the shell:

```bash
Rscript -e "pkgload::load_all(); run_nightmare()"
```

Choose a data directory, then drop each unit offering into a unit-first folder:

```text
data/
  ENVX2001/
    2025-S1C/
    2026-S1C/
  BIOL2022/
    2026-S2C/
```

Each offering folder contains the Canvas gradebook export (CSV), special considerations (CSV), disability plans (XLSX), and its own `.nightmare/` state. Keeping `.nightmare/` inside the offering folder keeps notes, exams, weights, match overrides, Canvas API snapshots, and import caches separate across years.

Legacy flat folders such as `data/ENVX2001/` still load. To migrate one safely:

1. Create the offering folder, e.g. `data/ENVX2001/2026-S1C/`.
2. Move the CSV/XLSX files and the existing `.nightmare/` folder into it.
3. Start NIGHTMARE and verify the offering loads.
4. Remove any remaining source files from `data/ENVX2001/` so the parent is not detected as a separate legacy offering.

## Canvas Refresh

NIGHTMARE can refresh an offering's Canvas gradebook directly from the Canvas API. Click `Configure` in the Canvas metadata control, then enter:

- Canvas URL, e.g. `https://canvas.sydney.edu.au`
- Canvas course ID for the current offering
- a Canvas API token

The Canvas base URL and token are user-level settings, but each offering has its own Canvas course ID. Configure `ENVX2001 · 2025 S1C` and `ENVX2001 · 2026 S1C` separately if they map to different Canvas courses.

The token is stored in your system keychain through the `keyring` package. It is not written to `.nightmare/`, `settings.json`, or the data folder. Refreshed gradebook snapshots are saved to the selected offering folder, for example `data/ENVX2001/2026-S1C/.nightmare/canvas_api_snapshot.rds`, so the refreshed data is reused on the next app load.

The refresh uses read-only Canvas API endpoints for assignments, student enrollments, and submissions, then reconstructs the same nested Canvas gradebook shape used by the CSV importer.

## What it does

- Consolidates student records from three university systems. More might be added later
- Tracks extensions, special considerations, grades and academic plans
- Projects grades and flags at-risk students
- Exports SEAMS2-compatible CSV for bulk extension sections in Canvas

## What it doesn't do

- Look pretty (clinical minimal is a feature... for now)
- Replace Canvas or SEAMS2 - Canvas refresh is read-only and local state stays on your machine
- Work without R - you have to use it!

## Requirements

R >= 4.1.0 and the usual suspects: shiny, bslib, dplyr, readr, readxl, stringr, shinyjs, jsonlite, httr2, keyring. All declared in DESCRIPTION so `devtools::install()` handles the app dependencies.

## Licence

MIT
