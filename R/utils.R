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

    # Subscale items should be subsets of item_indices (or recode_items)
    if (!is.null(s$subscales) && !is.null(s$item_indices)) {
      recode_idx <- if (!is.null(s$recode_items)) {
        as.numeric(names(s$recode_items))
      } else {
        numeric(0)
      }
      known_items <- union(s$item_indices, recode_idx)

      for (sc in names(s$subscales)) {
        bad <- setdiff(s$subscales[[sc]], known_items)
        if (length(bad) > 0) {
          cli::cli_warn(
            "{prefix} subscale {.val {sc}} contains item{?s} {.val {bad}} not in {.field item_indices} or {.field recode_items}."
          )
          all_ok <- FALSE
        }
      }
    }
  }

  if (all_ok) cli::cli_alert_success("All {length(specs)} spec{?s} passed validation.")
  invisible(all_ok)
}

#' Process REDCap data for a specific examination timepoint
#'
#' Filters and merges REDCap data (observations, disease, medication) for a
#' given timepoint. Timepoints map to event names as: t1 = `"arm_1"`,
#' t2 = `"arm_1b"`, t3 = `"arm_1c"`, etc.
#'
#' @param data The full REDCap data frame.
#' @param timepoint Integer 1–5 indicating the examination appointment.
#' @param arm Character; `"rct"` or `"hc"`.
#' @param require_obs_assess_dt Logical; require non-missing `obs_assess_dt`?
#'   Default `TRUE`.
#'
#' @return A merged data frame containing all non-missing observations, disease,
#'   and medication data, with columns prefixed by timepoint (e.g. `t1_`).
#' @export
process_timepoint <- function(
    data,
    timepoint = 1,
    arm = c("rct", "hc"),
    require_obs_assess_dt = TRUE
) {
  arm <- match.arg(arm)

  meta_vars <- c(
    "study_id_raw",
    "elig_final",
    "elig_group",
    "randomized_arm",
    "redcap_event_name",
    "redcap_event_name_factor",
    "arm",
    "redcap_repeat_instrument",
    "redcap_repeat_instance",
    "redcap_repeat_instrument_factor"
  )

  # ----------------------------
  # Decide event names
  # ----------------------------
  if (arm == "rct") {
    suffix_map <- c("1", "1b", "1c", "1d", "1e")
    if (!timepoint %in% 1:5) stop("For arm='rct', timepoint must be 1..5.")
    exam_event <- paste0("examination_appoin_arm_", suffix_map[timepoint])
    dis_event <- exam_event
    med_event <- exam_event
    tp_prefix <- paste0("t", timepoint, "_")
  } else {
    exam_event <- "examination_appoin_arm_2"
    dis_event  <- "diagnostic_intervi_arm_2"
    med_event  <- "diagnostic_intervi_arm_2"
    tp_prefix  <- "t1_"
  }

  # ----------------------------
  # Pull data blocks
  # ----------------------------
  filtered <- data %>%
    dplyr::filter(.data$redcap_event_name == exam_event)

  if (require_obs_assess_dt) {
    filtered <- filtered %>%
      dplyr::filter(!is.na(.data$obs_assess_dt))
  }

  filtered <- filtered %>%
    dplyr::select(where(not_all_na))

  disease <- data %>%
    dplyr::filter(
      .data$redcap_event_name == dis_event,
      .data$redcap_repeat_instrument == "concomitant_disease"
    ) %>%
    dplyr::select(where(not_all_na), -dplyr::any_of(meta_vars))

  medication <- data %>%
    dplyr::filter(
      .data$redcap_event_name == med_event,
      .data$redcap_repeat_instrument == "concomitant_medication"
    ) %>%
    dplyr::select(where(not_all_na), -dplyr::any_of(meta_vars))

  # ----------------------------
  # Join + prefix
  # ----------------------------
  out <- filtered %>%
    dplyr::left_join(disease, by = "study_id") %>%
    dplyr::left_join(medication, by = "study_id") %>%
    dplyr::rename_with(~ paste0(tp_prefix, .x), -dplyr::all_of("study_id"))

  out
}


#' Move timepoint prefixes to column name postfixes
#'
#' Converts column names from prefix format (`t1_phq_sum`) to postfix format
#' (`phq_sum_t0`). Handles four patterns:
#'
#' - `t0_*` (diagnostic) → `*_td`
#' - `t1_*` through `t5_*` → `*_t0` through `*_t4` (reindexed)
#' - `s1_*` through `s18_*` → `*_s1` through `*_s18`
#' - `os1_*`, `os2_*` → `*_os1`, `*_os2`
#'
#' @param df A data frame with timepoint-prefixed column names.
#'
#' @return The data frame with postfix-style column names.
#' @export
move_timepoint_to_postfix <- function(df) {
  df %>%
    # t0 (diagnostic) → _td postfix
    dplyr::rename_with(
      ~ stringr::str_replace(.x, "^t0_(.+)$", "\\1_td"),
      .cols = dplyr::starts_with("t0_")
    ) %>%
    # t1-t5 → t0-t4 postfix (reindexed)
    dplyr::rename_with(
      ~ {
        n <- as.integer(stringr::str_extract(.x, "(?<=^t)\\d+"))
        varname <- stringr::str_replace(.x, "^t\\d+_", "")
        paste0(varname, "_t", n - 1)
      },
      .cols = dplyr::matches("^t[1-9]\\d?_")
    ) %>%
    # Sessions: s1-s18 move to postfix
    dplyr::rename_with(
      ~ {
        snum <- stringr::str_extract(.x, "^s\\d+")
        varname <- stringr::str_replace(.x, "^s\\d+_", "")
        paste0(varname, "_", snum)
      },
      .cols = dplyr::matches("^s\\d+_")
    ) %>%
    # Online screenings: os1_, os2_ move to postfix
    dplyr::rename_with(
      ~ {
        osnum <- stringr::str_extract(.x, "^os\\d+")
        varname <- stringr::str_replace(.x, "^os\\d+_", "")
        paste0(varname, "_", osnum)
      },
      .cols = dplyr::matches("^os\\d+_")
    )
}

#' Quick column inspection by name pattern
#'
#' Selects all columns whose names contain the given string. Useful for
#' interactively checking which columns exist for a scale or variable group.
#'
#' @param df A data frame.
#' @param var Character string to match against column names (passed to
#'   `dplyr::contains()`).
#'
#' @return A data frame containing only the matching columns.
#' @export
#'
#' @examples
#' df <- data.frame(phq_1 = 1, phq_2 = 2, bdi_1 = 3)
#' check(df, "phq")
check <- function(df, var) {
  df %>% dplyr::select(dplyr::contains(var))
}
