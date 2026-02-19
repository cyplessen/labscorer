# ── Generic data-cleaning helpers ─────────────────────────────────────────────
#
# Functions for cleaning raw survey/REDCap data before scoring.
# Not tied to any specific questionnaire.
# ─────────────────────────────────────────────────────────────────────────────


#' Replace numeric missing codes with NA
#'
#' Survey platforms and REDCap often use sentinel values like -66, -77, -99 to
#' indicate different types of missingness. This function replaces them with
#' `NA` across all columns (numeric and character).
#'
#' @param .data A data frame.
#' @param missing_codes Numeric vector of sentinel values to treat as missing
#'   (default: `c(-66, -77, -99)`).
#'
#' @return The data frame with sentinel values replaced by `NA`.
#' @export
#'
#' @examples
#' df <- data.frame(x = c(1, -99, 3), y = c("a", "-77", "c"))
#' recode_missing(df)
recode_missing <- function(.data, missing_codes = c(-66, -77, -99)) {
  missing_codes_chr <- as.character(missing_codes)

  .data %>%
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      \(x) {
        if (is.numeric(x)) {
          replace(x, x %in% missing_codes, NA)
        } else if (is.character(x)) {
          replace(x, x %in% missing_codes_chr, NA)
        } else {
          x
        }
      }
    ))
}


#' Recode binary survey responses: 1 = yes, 2 = no → 1/0
#'
#' Many survey platforms code "yes" as 1 and "no" as 2. This recodes to
#' standard 1/0. All other values become `NA`.
#'
#' @param x Numeric vector.
#' @return Integer vector (1, 0, or `NA`).
#' @export
#'
#' @examples
#' recode_binary(c(1, 2, NA, 1))
#' # [1]  1  0 NA  1
recode_binary <- function(x) {
  dplyr::case_when(x == 1 ~ 1L, x == 2 ~ 0L, TRUE ~ NA_integer_)
}


#' Check whether a vector contains at least one non-NA value
#'
#' Useful as a column selection predicate:
#' `df %>% select(where(not_all_na))`.
#'
#' @param x A vector.
#' @return Logical scalar.
#' @export
#'
#' @examples
#' not_all_na(c(NA, NA, 1))
#' # [1] TRUE
#' not_all_na(c(NA, NA))
#' # [1] FALSE
not_all_na <- function(x) any(!is.na(x))


#' Remove all label attributes from a data frame
#'
#' Strips `"label"` attributes that packages like `haven` and `labelled`
#' attach to columns. Useful before writing to CSV or binding labelled and
#' unlabelled data frames.
#'
#' @param df A data frame.
#' @return The data frame with all column labels removed.
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:3)
#' attr(df$x, "label") <- "My variable"
#' clear_labels(df)
clear_labels <- function(df) {
  df[] <- lapply(df, function(x) {
    attr(x, "label") <- NULL
    x
  })
  df
}


#' Inspect columns that contain missing sentinel codes
#'
#' Scans all numeric and character columns for sentinel values (e.g. -66, -77,
#' -99) and returns a summary of how many such values each column contains.
#' Run this *before* [recode_missing()] to understand the scope of sentinel
#' usage.
#'
#' @param data A data frame.
#' @param missing_codes Numeric vector of sentinel values to look for
#'   (default: `c(-66, -77, -99)`).
#'
#' @return A tibble with columns `variable`, `n_missing_codes`, and `type`,
#'   filtered to only columns that contain at least one sentinel value.
#' @export
#'
#' @examples
#' df <- data.frame(x = c(1, -99, 3), y = c(4, 5, 6))
#' inspect_missings(df)
inspect_missings <- function(data, missing_codes = c(-66, -77, -99)) {
  missing_codes_chr <- as.character(missing_codes)

  num_summary <- data %>%
    dplyr::select(where(is.numeric))
  chr_summary <- data %>%
    dplyr::select(where(is.character))

  parts <- list()

  if (ncol(num_summary) > 0) {
    parts$num <- num_summary %>%
      dplyr::summarise(dplyr::across(
        dplyr::everything(),
        ~ sum(.x %in% missing_codes, na.rm = TRUE)
      )) %>%
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "variable",
        values_to = "n_missing_codes"
      ) %>%
      dplyr::mutate(type = "numeric")
  }

  if (ncol(chr_summary) > 0) {
    parts$chr <- chr_summary %>%
      dplyr::summarise(dplyr::across(
        dplyr::everything(),
        ~ sum(.x %in% missing_codes_chr, na.rm = TRUE)
      )) %>%
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "variable",
        values_to = "n_missing_codes"
      ) %>%
      dplyr::mutate(type = "character")
  }

  dplyr::bind_rows(parts) %>%
    dplyr::filter(.data$n_missing_codes > 0) %>%
    dplyr::arrange(dplyr::desc(.data$n_missing_codes))
}
