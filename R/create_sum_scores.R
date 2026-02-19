# ── Wrapper with diagnostic database ─────────────────────────────────────────

#' Score all detected scales and build a diagnostic database
#'
#' Iterates over a named list of scale specifications, detects which scales have
#' matching columns in `df`, calls [calculate_sum_scores()] for each, and
#' returns the scored data frame together with a diagnostic `scale_database`.
#'
#' The database contains one row per scale × timepoint, with a nested
#' `created_score_stats` tibble that records observed vs. theoretical score
#' ranges and an `out_of_range` flag for automatic quality control.
#'
#' @section Typical workflow:
#' ```r
#' options(labscorer.timepoint_prefix = "t")
#'
#' result <- create_sum_scores(df_raw, scale_specs_all)
#' df_scored      <- result$df
#' scale_database <- result$database
#'
#' # Check for problems
#' scale_database |>
#'   tidyr::unnest(created_score_stats) |>
#'   dplyr::filter(out_of_range)
#' ```
#'
#' @param df A data frame with item-level columns.
#' @param specs A named list of scale specifications. Each element should be a
#'   list with at least `scale` and `item_indices`, plus optional fields
#'   accepted by [calculate_sum_scores()].
#' @param na_rm Logical; passed through to [calculate_sum_scores()]. Default
#'   `FALSE`.
#' @param verbose Logical; print detected scales to the console? Default `TRUE`.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{`df`}{The input data frame with all score columns appended.}
#'     \item{`database`}{A tibble with one row per scale × timepoint containing
#'       spec metadata and a nested `created_score_stats` diagnostics table.}
#'   }
#' @export
#'
#' @examples
#' options(labscorer.timepoint_prefix = "t")
#' specs <- list(
#'   phq = list(scale = "phq", item_indices = 1:9, min_score = 0, max_score = 3)
#' )
#' # result <- create_sum_scores(df_raw, specs)
#' # result$df
#' # result$database
create_sum_scores <- function(df, specs, na_rm = FALSE, verbose = TRUE) {

  # ---- Validate specs ----------------------------------------------------
  validate_specs(specs)

  # ---- Initialise database -----------------------------------------------
  database <- tibble::tibble()

  # ---- Detect which scales have columns in df ----------------------------
  scale_present <- function(df, s) {
    nms <- names(df)
    tp <- getOption("labscorer.timepoint_prefix", NULL)
    if (is.null(tp)) {
      any(paste0(s$scale, "_", s$item_indices) %in% nms)
    } else {
      any(stringr::str_detect(nms, paste0("^", tp, "\\d+_", s$scale, "_")))
    }
  }

  present <- names(specs)[vapply(specs, \(s) scale_present(df, s), logical(1))]

  if (verbose) {
    cli::cli_alert_info(
      "{length(present)} scale{?s} detected: {.val {if (length(present) == 0) '<none>' else present}}"
    )
  }
  if (length(present) == 0) {
    return(list(df = df, database = database))
  }

  # ---- Internal helpers --------------------------------------------------
  obs_minmax <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    c(
      min = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      max = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    )
  }

  theo_range <- function(score_type, n_items, min_score, max_score) {
    if (is.na(min_score) || is.na(max_score) || is.na(n_items)) {
      return(c(theo_min = NA_real_, theo_max = NA_real_))
    }
    if (score_type == "sum") {
      return(c(theo_min = min_score * n_items, theo_max = max_score * n_items))
    }
    if (score_type == "mean") {
      return(c(theo_min = min_score, theo_max = max_score))
    }
    c(theo_min = NA_real_, theo_max = NA_real_)
  }

  # ---- Main loop ---------------------------------------------------------
  for (nm in present) {
    s <- specs[[nm]]
    tp_prefix <- getOption("labscorer.timepoint_prefix", NULL)

    # 1) Score
    df <- calculate_sum_scores(
      df              = df,
      scale           = s$scale,
      item_indices    = s$item_indices,
      na_rm           = na_rm,
      inverse_items   = s$inverse_items   %||% NULL,
      min_score       = s$min_score       %||% 1,
      max_score       = s$max_score       %||% NULL,
      recode_items    = s$recode_items    %||% NULL,
      subscales       = s$subscales       %||% NULL,
      timepoint_prefix = tp_prefix,
      calc_sum        = s$calc_sum        %||% TRUE,
      calc_mean       = s$calc_mean       %||% TRUE,
      subscale_suffix = s$subscale_suffix %||% "_sub_sum"
    )

    # 2) Detect timepoints
    timepoints <- detect_timepoints(df, s$scale, tp_prefix)

    # 3) Register diagnostics per timepoint
    for (tp in timepoints) {
      prefix <- if (tp == "") "" else paste0(tp, "_")
      sfx    <- s$subscale_suffix %||% "_sub_sum"

      # Enumerate created columns
      created <- character(0)
      if (isTRUE(s$calc_sum  %||% TRUE))
        created <- c(created, paste0(prefix, s$scale, "_sum"))
      if (isTRUE(s$calc_mean %||% TRUE))
        created <- c(created, paste0(prefix, s$scale, "_mean"))

      if (!is.null(s$subscales) && length(s$subscales) > 0) {
        created <- c(created,
                     paste0(prefix, s$scale, "_", names(s$subscales), sfx))
        if (isTRUE(s$calc_mean %||% TRUE)) {
          created <- c(created,
                       paste0(prefix, s$scale, "_", names(s$subscales), "_mean"))
        }
      }
      created <- created[created %in% names(df)]

      min_score <- s$min_score %||% NA_real_
      max_score <- s$max_score %||% NA_real_

      # Build per-score diagnostics
      if (length(created) > 0) {
        stats_tbl <- tibble::tibble(score_var = created) %>%
          dplyr::mutate(
            score_type = dplyr::case_when(
              stringr::str_ends(.data$score_var, "_sum")  ~ "sum",
              stringr::str_ends(.data$score_var, "_mean") ~ "mean",
              TRUE ~ NA_character_
            ),
            n_items = dplyr::case_when(
              .data$score_var %in% c(paste0(prefix, s$scale, "_sum"),
                                     paste0(prefix, s$scale, "_mean")) ~
                length(s$item_indices),
              TRUE ~ NA_integer_
            )
          )

        # Fill subscale n_items
        if (!is.null(s$subscales)) {
          for (sc in names(s$subscales)) {
            sum_nm  <- paste0(prefix, s$scale, "_", sc, sfx)
            mean_nm <- paste0(prefix, s$scale, "_", sc, "_mean")
            stats_tbl$n_items[stats_tbl$score_var %in% c(sum_nm, mean_nm)] <-
              length(s$subscales[[sc]])
          }
        }

        stats_tbl <- stats_tbl %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            score_min    = obs_minmax(df[[.data$score_var]])["min"],
            score_max    = obs_minmax(df[[.data$score_var]])["max"],
            theo_min     = theo_range(.data$score_type, .data$n_items,
                                      min_score, max_score)["theo_min"],
            theo_max     = theo_range(.data$score_type, .data$n_items,
                                      min_score, max_score)["theo_max"],
            out_of_range = dplyr::if_else(
              !is.na(.data$theo_min) & !is.na(.data$theo_max) &
                (!is.na(.data$score_min) | !is.na(.data$score_max)),
              (.data$score_min < .data$theo_min) |
                (.data$score_max > .data$theo_max),
              FALSE
            )
          ) %>%
          dplyr::ungroup()
      } else {
        stats_tbl <- tibble::tibble(
          score_var = character(), score_type = character(),
          n_items = integer(), score_min = numeric(), score_max = numeric(),
          theo_min = numeric(), theo_max = numeric(), out_of_range = logical()
        )
      }

      # Append row to database
      database <- dplyr::bind_rows(
        database,
        tibble::tibble(
          scale           = s$scale,
          timepoint       = ifelse(tp == "", NA_character_, tp),
          item_indices    = list(s$item_indices),
          min_score       = min_score,
          max_score       = max_score,
          inverse_items   = list(s$inverse_items %||% NULL),
          subscales       = list(s$subscales %||% NULL),
          calc_sum        = s$calc_sum  %||% TRUE,
          calc_mean       = s$calc_mean %||% TRUE,
          subscale_suffix = sfx,
          created_score_stats = list(stats_tbl)
        )
      )
    }
  }

  list(df = df, database = database)
}
