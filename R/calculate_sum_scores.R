# ── Core scoring engine ──────────────────────────────────────────────────────

#' Calculate sum and mean scores for a psychometric scale
#'
#' Computes total sum scores, mean scores, and optional subscale scores for a
#' single questionnaire. Handles item inversion, custom recoding, and automatic
#' timepoint detection.
#'
#' @section Column naming convention:
#' Item columns should be named `<prefix><scale>_<item>` where `<prefix>` is
#' either empty (no timepoints) or `<tp>_` (e.g. `t1_`). Created score columns
#' follow the same convention:
#' - `<prefix><scale>_sum` — total sum score
#' - `<prefix><scale>_mean` — total mean score
#' - `<prefix><scale>_<subscale><subscale_suffix>` — subscale sum
#' - `<prefix><scale>_<subscale>_mean` — subscale mean
#'
#' @section Processing order:
#' For each timepoint: (1) recoding → (2) inversion (excluding already-recoded
#' items) → (3) total sum/mean → (4) subscale sum/mean.
#'
#' @param df A data frame with item-level columns.
#' @param scale Character; scale prefix in column names (e.g. `"phq"`).
#' @param item_indices Vector of item numbers (numeric or character).
#' @param na_rm Logical; ignore `NA`s in `rowSums`/`rowMeans`? Default `FALSE`.
#'   **Note:** when `TRUE`, rows where *all* items are `NA` return `0` for sums
#'   and `NaN` for means. Consider using a minimum-response threshold in your
#'   analysis instead.
#' @param inverse_items Optional vector of item indices to invert.
#' @param min_score Minimum possible item score (default `1`).
#' @param max_score Maximum possible item score; **required** when
#'   `inverse_items` is supplied.
#' @param recode_items Optional named list of recoding functions keyed by item
#'   index as character (e.g. `list("10" = my_fn)`). Recoded items are excluded
#'   from inversion.
#' @param subscales Optional named list of subscale definitions; each element is
#'   a vector of item indices.
#' @param timepoint_prefix Character prefix for timepoints (e.g. `"t"`) or
#'   `NULL` for cross-sectional data. Defaults to
#'   `getOption("labscorer.timepoint_prefix", NULL)`.
#' @param calc_sum Logical; compute total sum? Default `TRUE`.
#' @param calc_mean Logical; compute total mean? Default `TRUE`.
#' @param subscale_suffix Character suffix for subscale sum columns (default
#'   `"_sub_sum"`).
#'
#' @return The input data frame with additional score columns appended.
#' @export
#'
#' @examples
#' options(labscorer.timepoint_prefix = "t")
#' df <- data.frame(t1_phq_1 = c(0, 1, 2), t1_phq_2 = c(3, 2, 1))
#' calculate_sum_scores(df, scale = "phq", item_indices = 1:2,
#'                      min_score = 0, max_score = 3)
calculate_sum_scores <- function(
    df,
    scale,
    item_indices,
    na_rm = FALSE,
    inverse_items = NULL,
    min_score = 1,
    max_score = NULL,
    recode_items = NULL,
    subscales = NULL,
    timepoint_prefix = getOption("labscorer.timepoint_prefix", NULL),
    calc_sum  = TRUE,
    calc_mean = TRUE,
    subscale_suffix = "_sub_sum"
) {

  # --- Detect timepoints -------------------------------------------------
  timepoints <- detect_timepoints(df, scale, timepoint_prefix)

  # --- Score each timepoint ----------------------------------------------
  for (tp in timepoints) {
    prefix <- if (tp == "") "" else paste0(tp, "_")

    item_names <- paste0(prefix, scale, "_", item_indices)
    item_names <- intersect(item_names, names(df))

    if (length(item_names) == 0) {
      cli::cli_warn(
        "No matching columns for scale {.val {scale}} at timepoint {.val {tp}}. Skipping."
      )
      next
    }

    # 1. Recoding -----------------------------------------------------------
    if (!is.null(recode_items)) {
      for (i in names(recode_items)) {
        nm <- paste0(prefix, scale, "_", i)
        if (nm %in% names(df)) {
          df[[nm]] <- recode_items[[i]](df[[nm]])
        }
      }
    }

    # 2. Inversion (skip recoded items) ------------------------------------
    if (!is.null(inverse_items)) {
      inv_items <- if (!is.null(recode_items)) {
        setdiff(inverse_items, names(recode_items))
      } else {
        inverse_items
      }

      inv_names <- paste0(prefix, scale, "_", inv_items)
      inv_names <- intersect(inv_names, names(df))

      if (length(inv_names) > 0) {
        if (is.null(max_score)) {
          cli::cli_abort("{.arg max_score} must be supplied when {.arg inverse_items} are used.")
        }
        df[inv_names] <- lapply(
          df[inv_names], invert_item,
          min_score = min_score, max_score = max_score
        )
      }
    }

    # 3. Total scores -------------------------------------------------------
    if (calc_sum) {
      df[[paste0(prefix, scale, "_sum")]] <-
        rowSums(df[item_names], na.rm = na_rm)
    }
    if (calc_mean) {
      df[[paste0(prefix, scale, "_mean")]] <-
        rowMeans(df[item_names], na.rm = na_rm)
    }

    # 4. Subscale scores ----------------------------------------------------
    if (!is.null(subscales)) {
      for (sc in names(subscales)) {
        sc_items <- paste0(prefix, scale, "_", subscales[[sc]])
        sc_items <- intersect(sc_items, names(df))

        if (length(sc_items) == 0) {
          cli::cli_warn(
            "No columns for subscale {.val {sc}} of {.val {scale}} at {.val {tp}}. Skipping."
          )
          next
        }

        df[[paste0(prefix, scale, "_", sc, subscale_suffix)]] <-
          rowSums(df[sc_items], na.rm = na_rm)

        if (calc_mean) {
          df[[paste0(prefix, scale, "_", sc, "_mean")]] <-
            rowMeans(df[sc_items], na.rm = na_rm)
        }
      }
    }
  }

  df
}
