# ── Custom recode functions ──────────────────────────────────────────────────

#' Recode CTQ bagatellisation items
#'
#' Recodes CTQ-28 items 10, 16, and 22 (the bagatellisation / minimisation
#' scale). These items use a different response format and need to be recoded
#' before scoring.
#'
#' The standard recode is: 1 → 0, 2 → 0, 3 → 1, 4 → 1, 5 → 1.
#' (i.e. responses 1–2 become 0, responses 3–5 become 1.)
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
