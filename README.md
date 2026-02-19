# labscorer

A standardised pipeline for computing psychometric scale scores in R.
Designed as the **single source of truth** for questionnaire scoring in our lab.

## Installation

```r
# From GitHub
devtools::install_github("yourlab/labscorer")

# From a local clone
devtools::install("path/to/labscorer")
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

### Functions

| Function | Purpose |
|---|---|
| `create_sum_scores(df, specs)` | Score all detected scales, return `list(df, database)` |
| `calculate_sum_scores(df, ...)` | Score one scale (low-level engine) |
| `validate_specs(specs)` | Check specs for errors and typos |
| `detect_timepoints(df, scale, prefix)` | Find timepoints in column names |
| `invert_item(x, min, max)` | Reverse-score an item vector |
| `unpad_scale_items(df, scales)` | Remove zero-padding from item numbers |
| `recode_ctq_bagatellization(x)` | Recode CTQ bagatellisation items |

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
│   ├── recode_helpers.R         # Custom recode functions (e.g. CTQ)
│   ├── data.R                   # Documentation for scale_specs_all
│   └── labscorer-package.R      # Package-level docs
├── data/
│   └── scale_specs_all.rda      # The scale registry (built from data-raw/)
├── data-raw/
│   └── scale_specs_all.R        # ← EDIT THIS to add/change scales
├── tests/testthat/
│   └── test-scoring.R           # Unit tests
├── vignettes/
│   └── getting-started.Rmd      # Tutorial for new lab members
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

## License

MIT
