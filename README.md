# labscorer

A standardised pipeline for computing psychometric scale scores in R.
Designed as the **single source of truth** for questionnaire scoring in our lab.

## Installation

```r
# From GitHub
devtools::install_github("cyplessen/labscorer", build_vignettes = TRUE)

# From a local clone
devtools::install("path/to/labscorer", build_vignettes = TRUE)
```

## Quick start

```r
library(labscorer)

# 1. Set your timepoint prefix
options(labscorer.timepoint_prefix = "t")

# 2. Load the master scale registry (ships with the package)
data(scale_specs_all)

# 3. Score everything — scales not in your data are skipped automatically
result <- create_sum_scores(df_raw, scale_specs_all)

df_scored      <- result$df
scale_database <- result$database

# 4. Check for problems
scale_database |>
  tidyr::unnest(created_score_stats) |>
  dplyr::filter(out_of_range)
```

## New to the lab?

Read the **Getting Started** tutorial:

```r
vignette("getting-started", package = "labscorer")
```

It walks you through scoring a dataset, reading diagnostics, looking up scale
specs, and adding a new questionnaire — with a simulated dataset you can run
yourself.

## Column naming convention

| Data type | Pattern | Example |
|---|---|---|
| With timepoints | `<prefix><digit>_<scale>_<item>` | `t1_phq_3` |
| Without timepoints | `<scale>_<item>` | `phq_3` |

## Adding a new questionnaire

1. Open `data-raw/scale_specs_all.R`
2. Add your entry (see existing examples for the format)
3. Run `validate_specs(scale_specs_all)` to check for mistakes
4. Run `source("data-raw/scale_specs_all.R")` to rebuild the data
5. Reinstall: `devtools::install()`
6. Submit a pull request

## What's in the package

### Core scoring

| Function | Purpose |
|---|---|
| `create_sum_scores(df, specs)` | Score all detected scales, return `list(df, database)` |
| `calculate_sum_scores(df, ...)` | Score one scale (low-level engine) |
| `validate_specs(specs)` | Check specs for errors and typos |
| `detect_timepoints(df, scale, prefix)` | Find timepoints in column names |
| `invert_item(x, min, max)` | Reverse-score an item vector |
| `unpad_scale_items(df, scales)` | Remove zero-padding from item numbers |

### Scale-specific recoding

| Function | Purpose |
|---|---|
| `recode_phq(df)` | PHQ: 0 → NA, 1–4 → 0–3 |
| `recode_mdrs(df)` | MDRS-22: 1–8 → 0–7 |
| `recode_ams(df)` | AMS: 0 → NA (scale 1–5) |
| `recode_ipss(df)` | IPSS: 1–6 → 0–5 |
| `recode_mcsd(df)` | MCSD: 1–2 → 0–1 |
| `recode_sb_pni(df, items)` | SB-PNI: 0 → NA, 1–6 → 0–5 |
| `recode_cgi(df, items)` | CGI-SI: 0 → NA (scale 1–7) |
| `recode_ctq_bagatellization(x)` | CTQ items 10/16/22: 1–2 → 0, 3–5 → 1 |
| `rename_scales_os_to_redcap(df)` | Rename OS prefixes to REDCap format |

### Data cleaning

| Function | Purpose |
|---|---|
| `recode_missing(df)` | Replace sentinel values (-66, -77, -99) with NA |
| `recode_binary(x)` | Recode 1/2 (yes/no) to 1/0 |
| `not_all_na(x)` | Check if a column has any non-NA values |
| `clear_labels(df)` | Strip label attributes from labelled data |
| `inspect_missings(df)` | Find columns containing sentinel values |

### Display / QC

| Function | Purpose |
|---|---|
| `summarize_range(df, pattern)` | Quick min/max/n summary for matching columns |
| `fmt_vec(x)` | Format a vector as comma-separated string |
| `fmt_subscales(subscales)` | Format subscale definitions for display |

### Data

| Object | Description |
|---|---|
| `scale_specs_all` | Master registry of all questionnaire specifications |

### Options

| Option | Purpose | Example |
|---|---|---|
| `labscorer.timepoint_prefix` | Set the timepoint prefix | `options(labscorer.timepoint_prefix = "t")` |

## Package structure

```
labscorer/
├── R/
│   ├── calculate_sum_scores.R   # Core scoring engine
│   ├── create_sum_scores.R      # Wrapper with diagnostic database
│   ├── utils.R                  # Helpers: invert, detect, validate, unpad
│   ├── recode_scales.R          # Scale-specific recode functions
│   ├── data_cleaning.R          # Generic cleaning: recode_missing, clear_labels, etc.
│   ├── display_helpers.R        # QC display: summarize_range, fmt_vec, etc.
│   ├── data.R                   # Documentation for scale_specs_all
│   └── labscorer-package.R      # Package-level docs
├── data/
│   └── scale_specs_all.rda      # The scale registry (built from data-raw/)
├── data-raw/
│   └── scale_specs_all.R        # ← EDIT THIS to add/change scales
├── tests/testthat/
│   ├── test-scoring.R           # Tests for scoring functions
│   └── test-helpers.R           # Tests for recode, cleaning, display helpers
├── vignettes/
│   └── getting-started.Rmd      # Tutorial for new lab members
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

## License

MIT
