# ── Utility functions ─────────────────────────────────────────────────────────

#' Invert a psychometric item
#'
#' Reverses the scoring direction of a numeric item vector using
#' `max_score + min_score - x`. Values outside the valid range after inversion
#' are clamped. `NA`s are preserved.
#'
#' @param x Numeric vector of item scores.
#' @param min_score Minimum possible score (default `1`).
#' @param max_score Maximum possible score (**required**).
#'
#' @return Numeric vector of inverted scores, same length as `x`.
#' @export
#'
#' @examples
#' invert_item(c(1, 2, 3, NA, 5), min_score = 1, max_score = 5)
#' # [1] 5 4 3 NA 1
invert_item <- function(x, min_score = 1, max_score) {
  if (missing(max_score)) {
    cli::cli_abort("{.arg max_score} must be provided.")
  }
  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be a numeric vector, not {.cls {class(x)}}.")
  }

  inverted <- ifelse(is.na(x), NA_real_, max_score + min_score - x)
  inverted <- pmin(pmax(inverted, min_score), max_score)
  inverted
}


#' Remove zero-padding from item numbers in column names
#'
#' Renames columns like `phq_01`, `phq_02` to `phq_1`, `phq_2` for the
#' specified scale prefixes. Useful when survey software exports zero-padded
#' item numbers.
#'
#' @param df A data frame.
#' @param scales Character vector of scale prefixes to unpad.
#'
#' @return The data frame with renamed columns.
#' @export
#'
#' @examples
#' df <- data.frame(phq_01 = 1, phq_02 = 2, age = 30)
#' unpad_scale_items(df, scales = "phq")
#' # columns: phq_1, phq_2, age
unpad_scale_items <- function(df, scales = character()) {
  if (length(scales) == 0) return(df)

  pat <- paste0("(", paste(scales, collapse = "|"), ")_0+([1-9][0-9]*)$")
  rep <- "\\1_\\2"

  df %>%
    dplyr::rename_with(
      \(nm) stringr::str_replace(nm, pat, rep),
      .cols = dplyr::matches(pat)
    )
}


#' Detect timepoints present in a data frame for a given scale
#'
#' Looks for columns matching the pattern `<prefix><digit>_<scale>_*` and
#' returns the sorted unique timepoint labels (e.g. `c("t1", "t2")`).
#'
#' @param df A data frame.
#' @param scale Character; scale prefix (e.g. `"phq"`).
#' @param timepoint_prefix Character or `NULL`. The prefix used for timepoints
#'   (e.g. `"t"`). If `NULL`, returns `""` (no timepoints).
#'
#' @return Character vector of timepoint labels, or `""` for cross-sectional
#'   data.
#' @export
#'
#' @examples
#' df <- data.frame(t1_phq_1 = 1, t1_phq_2 = 2, t2_phq_1 = 3, t2_phq_2 = 4)
#' detect_timepoints(df, "phq", timepoint_prefix = "t")
#' # [1] "t1" "t2"
detect_timepoints <- function(df, scale, timepoint_prefix = NULL) {
  if (is.null(timepoint_prefix)) return("")

  tp_regex <- paste0("^", timepoint_prefix, "\\d+")

  timepoints <- df %>%
    dplyr::select(dplyr::contains(paste0("_", scale, "_"))) %>%
    names() %>%
    stringr::str_extract(tp_regex) %>%
    unique() %>%
    stats::na.omit() %>%
    sort()

  if (length(timepoints) == 0) {
    cli::cli_abort(
      "No timepoints detected for prefix {.val {timepoint_prefix}} and scale {.val {scale}}."
    )
  }

  as.character(timepoints)
}


#' Validate a list of scale specifications
#'
#' Checks each spec for required fields (`scale`, `item_indices`), warns about
#' unknown fields, and flags likely mistakes (e.g. `inverse_items` without
#' `max_score`).
#'
#' @param specs A named list of scale specifications.
#'
#' @return `TRUE` invisibly if all specs pass. Emits warnings for issues found.
#' @export
#'
#' @examples
#' specs <- list(
#'   phq = list(scale = "phq", item_indices = 1:9, min_score = 0, max_score = 3)
#' )
#' validate_specs(specs)
validate_specs <- function(specs) {
  if (!is.list(specs) || is.null(names(specs))) {
    cli::cli_abort("{.arg specs} must be a named list.")
  }

  known_fields <- c(
    "scale", "item_indices", "min_score", "max_score",
    "inverse_items", "recode_items", "subscales",
    "calc_sum", "calc_mean", "subscale_suffix"
  )

  all_ok <- TRUE

  for (nm in names(specs)) {
    s <- specs[[nm]]
    prefix <- cli::format_inline("Spec {.val {nm}}:")

    # Required fields
    if (is.null(s$scale)) {
      cli::cli_warn("{prefix} missing required field {.field scale}.")
      all_ok <- FALSE
    }
    if (is.null(s$item_indices)) {
      cli::cli_warn("{prefix} missing required field {.field item_indices}.")
      all_ok <- FALSE
    }

    # Logical consistency
    if (!is.null(s$inverse_items) && is.null(s$max_score)) {
      cli::cli_warn("{prefix} {.field inverse_items} supplied without {.field max_score}.")
      all_ok <- FALSE
    }

    # Unknown fields (typos)
    unknown <- setdiff(names(s), known_fields)
    if (length(unknown) > 0) {
      cli::cli_warn(
        "{prefix} unknown field{?s}: {.field {unknown}}. Possible typo?"
      )
      all_ok <- FALSE
    }

    # Subscale items should be subsets of item_indices
    if (!is.null(s$subscales) && !is.null(s$item_indices)) {
      for (sc in names(s$subscales)) {
        bad <- setdiff(s$subscales[[sc]], s$item_indices)
        if (length(bad) > 0) {
          cli::cli_warn(
            "{prefix} subscale {.val {sc}} contains item{?s} {.val {bad}} not in {.field item_indices}."
          )
          all_ok <- FALSE
        }
      }
    }
  }

  if (all_ok) cli::cli_alert_success("All {length(specs)} spec{?s} passed validation.")
  invisible(all_ok)
}
