# ── Display and QC helpers ────────────────────────────────────────────────────
#
# Functions for inspecting and formatting scored data and the scale_database.
# ─────────────────────────────────────────────────────────────────────────────


#' Summarise the range of variables matching a pattern
#'
#' Quick inspection tool: selects columns matching a string pattern and returns
#' min, max, and n for each.
#'
#' @param data A data frame.
#' @param pattern Character string to match column names against (via
#'   `contains()`). Columns containing `"timestamp"` or `"_factor"` are
#'   excluded.
#'
#' @return A tibble with columns `variable`, `min`, `max`, `n`.
#' @export
#'
#' @examples
#' df <- data.frame(phq_sum = c(5, 12, 20), phq_mean = c(0.6, 1.3, 2.2))
#' summarize_range(df, "phq")
summarize_range <- function(data, pattern) {
  data %>%
    dplyr::select(
      dplyr::contains(pattern),
      -dplyr::contains(c("timestamp", "_factor"))
    ) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::summarise(
      min = if (all(is.na(.data$value))) NA_real_ else min(.data$value, na.rm = TRUE),
      max = if (all(is.na(.data$value))) NA_real_ else max(.data$value, na.rm = TRUE),
      n   = sum(!is.na(.data$value)),
      .groups = "drop"
    )
}


#' Format a vector as a compact comma-separated string
#'
#' Used to display `item_indices` and `inverse_items` from the scale database
#' in a readable table format.
#'
#' @param x A vector (or `NULL`).
#' @return A character string.
#' @export
#'
#' @examples
#' fmt_vec(c(1, 3, 5, 7))
#' # [1] "1, 3, 5, 7"
#' fmt_vec(NULL)
#' # [1] ""
fmt_vec <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  paste(x, collapse = ", ")
}


#' Format subscale definitions as a compact string
#'
#' Used to display the `subscales` list-column from the scale database in a
#' readable table format.
#'
#' @param subscales A named list of item vectors (or `NULL`).
#' @return A character string.
#' @export
#'
#' @examples
#' fmt_subscales(list(reappraisal = c(1, 3, 5), suppression = c(2, 4, 6)))
#' # [1] "reappraisal: 1, 3, 5 ; suppression: 2, 4, 6"
fmt_subscales <- function(subscales) {
  if (is.null(subscales) || length(subscales) == 0) return("")
  paste0(
    names(subscales), ": ",
    purrr::map_chr(subscales, fmt_vec),
    collapse = " ; "
  )
}
