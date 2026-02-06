# NIGHTMARE Packaging - Quick Start Guide

## One-Minute Overview

You have a fully automated packaging script that creates a portable distribution for end users in seconds.

```r
# Run this whenever you update NIGHTMARE code
source("package_for_distribution.R")

# Output: dist/NIGHTMARE_Portable.zip (156 KB)
# Ready to distribute to users
```

That's it. Everything else is automated.

---

## What Gets Generated

✅ **Portable application** with all source code
✅ **Windows launcher** (`START_NIGHTMARE.bat`) - run on Windows
✅ **macOS launcher** (`START_NIGHTMARE.command`) - run on macOS
✅ **README** with complete instructions and troubleshooting
✅ **Sample data** for testing (optional)

## What Users Get

Users extract the zip and double-click the launcher for their OS:

**Windows:** `START_NIGHTMARE.bat` → R checks → Packages install (first run) → Browser opens
**macOS:** `START_NIGHTMARE.command` → R checks → Packages install (first run) → Browser opens

No installation needed. Just R + internet (first run only).

---

## Customization

Want to change something? Edit `APP_CONFIG` at the top of `package_for_distribution.R`:

```r
APP_CONFIG <- list(
  name = "NIGHTMARE",           # App name
  version = "1.0.0",            # Update this when releasing
  port = 8888,                  # Default port (8888-8920 auto-tried)

  dependencies = c(             # R packages to install
    "shiny", "bslib", "readxl", "writexl", "DT",
    "R6", "dplyr", "tidyr", "purrr", "lubridate"
  ),

  required_files = c("app.R", "DESCRIPTION"),
  required_dirs = c("R", "www"),
  optional_dirs = c("_sample-data"),

  output_dir = "dist",
  package_name = "NIGHTMARE_Portable"
)
```

After editing, re-run the script. Done.

---

## Distribution

Send `dist/NIGHTMARE_Portable.zip` to users via:
- Email (small file)
- Cloud storage (OneDrive, Google Drive)
- Shared drive
- USB stick
- Website download

---

## Key Features

| Feature | Benefit |
|---------|---------|
| **Idempotent** | Safe to run 100 times - always creates same package |
| **Fast** | Script runs in seconds |
| **Cross-platform** | Single command generates both Windows & macOS launchers |
| **Auto Port Detection** | Finds available port automatically (8888-8920) |
| **First-Run Setup** | Packages install automatically, skipped on subsequent runs |
| **Error Handling** | Clear messages if something goes wrong |
| **Zero Dependencies** | Script uses base R only |

---

## Workflow

1. **Update code** as usual
2. **Run script** when ready to distribute:
   ```r
   source("package_for_distribution.R")
   ```
3. **Get zip file** at `dist/NIGHTMARE_Portable.zip`
4. **Send to users** via email/cloud/etc
5. **Users extract** and double-click launcher
6. **Done** - app works, no install needed

---

## Troubleshooting

**"zip package not found"**
→ Script has fallback to `utils::zip()` - should work

**"R not found in PATH (Windows)"**
→ User needs to install R and add to PATH

**"Port 8888 already in use"**
→ Script auto-finds next available port (8888-8920)

**"Permission denied (macOS)"**
→ User right-clicks launcher and selects "Open"

See `PACKAGING_COMPLETE.md` for full troubleshooting guide.

---

## Files

| File | Purpose |
|------|---------|
| `package_for_distribution.R` | The packaging script (run this) |
| `dist/NIGHTMARE_Portable.zip` | Distribution package (send to users) |
| `dist/NIGHTMARE_Portable/` | Unzipped contents (for testing) |
| `PACKAGING_COMPLETE.md` | Full documentation |
| `PACKAGING_QUICKSTART.md` | This file (quick reference) |

---

## Testing Checklist

Before sending to users:

- [ ] Extract `dist/NIGHTMARE_Portable.zip`
- [ ] Double-click appropriate launcher for your OS
- [ ] Wait for package installation (first run)
- [ ] Verify browser opens to `http://127.0.0.1:8888`
- [ ] Test app functionality (import data, filter, export)
- [ ] Close browser, close launcher window
- [ ] Run launcher again (verify packages skip, app starts faster)

---

**Ready to distribute?** Send `dist/NIGHTMARE_Portable.zip` to users.
