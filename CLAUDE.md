# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development commands

All commands run inside an R session with the project root as working directory.

```r
# Reload package after edits (no restart needed)
devtools::load_all()

# Run R CMD CHECK (must pass 0 errors, 0 warnings before merging to main)
devtools::check()

# Rebuild documentation from roxygen2 comments
devtools::document()

# Run tests (tests/ folder, not testthat — see below)
source("tests/test_howdirty_20260511.R")
```

## Branch strategy

- `main` — stable releases
- `dev` — integration branch; all PRs target `dev` first
- CI runs `R CMD CHECK` on Windows and Ubuntu on every push/PR to both branches (`.github/workflows/R-CMD-check.yaml`)
- Merging to `main` requires passing CI on both platforms

## Architecture

HowDirty is a **report-generation package**: it provides R functions that are orchestrated inside a parameterized RMarkdown template (`inst/rmarkdown/templates/howdirty/skeleton/skeleton.Rmd`) to produce a self-contained HTML contamination report.

### Data flow

```
Skyline CSV export
      │
      ▼
read_conta_results()        # normalises column names (3 Skyline export formats),
                            # zero-fills NAs, computes Abundance = TotalAreaMS1/TICA,
                            # pads PEG/PPG numbers (PEG1 → PEG01) for correct ordering
      │
      ▼
read_samples_annotation()   # reads user-supplied sample annotation (Condition, Sample,
+ annotate_conta_samples()  # DilutionFactor); joins to conta_raw

      │
      ▼
read_conta_thresholds()     # reads quantile thresholds from a reference HowDirty Excel
  OR                        # output, or falls back to get_simple_thresholds_*() when no
get_simple_thresholds_*()   # reference is available

      │
      ▼
annotate_conta_thresholds() # assigns RiskLevel 0-6 by comparing Abundance to
                            # Tshd_abundance_quantile{25,50,75,90}

      │
      ▼
summarize_conta()           # aggregates by arbitrary grouping vars; always produces
                            # Abundance_{min,quantile25,median,quantile75,quantile90,max,total}

      │
      ▼
plot_*() functions          # ggplot2-based; wrapped with ggplotly() when
                            # params$PlotsInteractive == TRUE

      │
      ▼
write.xlsx()                # exports all summary tables to a multi-sheet Excel file
```

### Key design decisions

- **Abundance** is always `TotalAreaMS1 / TotalIonCurrentArea` (TIC-normalised). Raw area is never used for thresholding.
- **RiskLevel** is an ordered integer factor 0–6 that maps to human-readable labels ("0) Not Detected" … "5) Very High", "6) No threshold in reference"). The integer is used for colour-scaling; the label for display.
- `summarize_conta()` uses `...` (dot-dot-dot) to accept arbitrary grouping variables, making it reusable across contaminant, sample, condition, and dataset-level summaries.
- The Rmd template embeds `print_plot_or_plotly()` — a local wrapper — to toggle between static (`ggplot2`) and interactive (`plotly`) output via `params$PlotsInteractive`.
- Thresholds sourced from a reference run are the quantiles of that reference set; when none is provided, `get_simple_thresholds_*()` sets uniform arbitrary thresholds and warns the user.

### Tests

Tests live in `tests/` but are **not** a `testthat` suite — they are plain R scripts that exercise the full pipeline using example data in `example/data/`. Run them by sourcing the script after `devtools::load_all()`.

### Package imports

`tidyverse` was replaced with the three specific sub-packages actually used: `dplyr`, `tidyr`, `forcats`. The `%>%` pipe is re-exported by `dplyr`. Internal `require()` calls have been removed — all dependencies are declared in `DESCRIPTION` and imported via `@import` in `R/help.R`.
