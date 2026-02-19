# ── Scale-specific recode functions ───────────────────────────────────────────
#
# Each function recodes raw survey responses into the scale's expected range.
# These are applied BEFORE scoring (i.e. before create_sum_scores()).
#
# Naming convention: recode_<scale>()
# ─────────────────────────────────────────────────────────────────────────────


#' Recode MDRS-22 items from 1–8 to 0–7
#'
#' @param df A data frame.
#' @param items Item indices to recode (default: `1:22`).
#' @return Data frame with recoded columns.
#' @export
recode_mdrs <- function(df, items = 1:22) {
  cols <- intersect(paste0("mdrs22_", items), names(df))
  if (length(cols) == 0) return(df)
  df %>% dplyr::mutate(dplyr::across(dplyr::all_of(cols), \(x) x - 1))
}


#' Recode IPSS items from 1–6 to 0–5
#'
#' @param df A data frame.
#' @param items Item indices to recode (default: `1:7`).
#' @return Data frame with recoded columns.
#' @export
recode_ipss <- function(df, items = 1:7) {
  cols <- intersect(paste0("ipss_", items), names(df))
  if (length(cols) == 0) return(df)
  df %>% dplyr::mutate(dplyr::across(dplyr::all_of(cols), \(x) x - 1))
}


#' Recode MCSD items from 1–2 (no/yes) to 0–1
#'
#' @param df A data frame.
#' @param items Item indices to recode (default: `1:10`).
#' @return Data frame with recoded columns.
#' @export
recode_mcsd <- function(df, items = 1:10) {
  cols <- intersect(paste0("mcsd_", items), names(df))
  if (length(cols) == 0) return(df)
  df %>% dplyr::mutate(dplyr::across(dplyr::all_of(cols), \(x) x - 1))
}


#' Recode PHQ-9 items: 0 → NA, 1–4 → 0–3
#'
#' In some survey exports, PHQ items are coded 0 (not answered) and 1–4
#' (response options). This recodes to the standard 0–3 range.
#'
#' @param df A data frame.
#' @param items Item indices to recode (default: `1:10`).
#' @return Data frame with recoded columns.
#' @export
recode_phq <- function(df, items = 1:10) {
  cols <- intersect(paste0("phq_", items), names(df))
  if (length(cols) == 0) return(df)
  df %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      \(x) dplyr::case_when(
        x == 0    ~ NA_real_,
        x %in% 1:4 ~ x - 1,
        TRUE      ~ NA_real_
      )
    ))
}


#' Recode AMS items: 0 → NA (scale remains 1–5)
#'
#' @param df A data frame.
#' @param items Item indices to recode (default: `1:17`).
#' @return Data frame with recoded columns.
#' @export
recode_ams <- function(df, items = 1:17) {
  cols <- intersect(paste0("ams_", items), names(df))
  if (length(cols) == 0) return(df)
  df %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      \(x) dplyr::if_else(x == 0, NA_real_, as.numeric(x))
    ))
}


#' Recode CTQ bagatellisation items (10, 16, 22)
#'
#' Recodes CTQ-28 minimisation items: responses 1–2 → 0, responses 3–5 → 1.
#'
#' @param x Numeric vector of raw item scores.
#' @return Numeric vector of recoded scores (0 or 1).
#' @export
#'
#' @examples
#' recode_ctq_bagatellization(c(1, 2, 3, 4, 5, NA))
#' # [1] 0 0 1 1 1 NA
recode_ctq_bagatellization <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_real_,
    x <= 2   ~ 0,
    x >= 3   ~ 1
  )
}


#' Recode SB-PNI items: 0 → NA, 1–6 → 0–5
#'
#' @param df A data frame.
#' @param items Item indices to recode.
#' @return Data frame with recoded columns.
#' @export
recode_sb_pni <- function(df, items) {
  cols <- intersect(paste0("sb_pni_", items), names(df))
  if (length(cols) == 0) return(df)
  df %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      \(x) dplyr::case_when(
        x == 0    ~ NA_real_,
        x %in% 1:6 ~ x - 1,
        TRUE      ~ NA_real_
      )
    ))
}


#' Recode CGI-SI items: 0 → NA (scale remains 1–7)
#'
#' @param df A data frame.
#' @param items Item indices to recode.
#' @return Data frame with recoded columns.
#' @export
recode_cgi <- function(df, items) {
  cols <- intersect(paste0("cgi_si_", items), names(df))
  if (length(cols) == 0) return(df)
  df %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(cols),
      \(x) dplyr::case_when(
        x == 0      ~ NA_real_,
        x %in% 1:7  ~ as.numeric(x),
        TRUE        ~ NA_real_
      )
    ))
}


#' Rename scale prefixes from online screening format to REDCap format
#'
#' Renames column prefixes: `cmni_` → `cmni30_`, `mdrs_` → `mdrs22_`,
#' `grcs_` → `grcs_sf_`. Use this when merging online screening data with
#' REDCap exports that use different scale prefixes.
#'
#' @param df A data frame.
#' @return Data frame with renamed columns.
#' @export
rename_scales_os_to_redcap <- function(df) {
  df %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(
        .x,
        c(
          "^cmni_"  = "cmni30_",
          "^mdrs_"  = "mdrs22_",
          "^grcs_"  = "grcs_sf_"
        )
      )
    )
}
