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

```r
nightmare::run_nightmare()
```

Drop your data files into `data/UNITCODE/` (e.g. `data/BIOL2022/`). The app auto-detects Canvas gradebooks (CSV), special considerations (CSV), and disability plans (XLSX).

## What it does

- Consolidates student records from three university systems. More might be added later
- Tracks extensions, special considerations, grades and academic plans
- Projects grades and flags at-risk students
- Exports SEAMS2-compatible CSV for bulk extension sections in Canvas

## What it doesn't do

- Look pretty (clinical minimal is a feature... for now)
- Connect to any API (local files only, your data stays on your machine)
- Work without R - you have to use it!

## Requirements

R >= 4.1.0 and the usual suspects: shiny, bslib, dplyr, readr, readxl, stringr, shinyjs, jsonlite. All declared in DESCRIPTION so `devtools::install()` handles it.

## Licence

MIT
