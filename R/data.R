#' Master registry of all psychometric scale specifications
#'
#' A named list containing one entry per questionnaire used in the lab's
#' research. Each entry defines the scale prefix, item indices, score range,
#' inverse items, subscales, and other parameters needed by
#' [create_sum_scores()].
#'
#' This object is the **single source of truth** for how every scale is scored.
#' To add or modify a scale, edit `data-raw/scale_specs_all.R` and rebuild the
#' package data.
#'
#' @format A named list with one element per scale. Each element is a list with:
#' \describe{
#'   \item{scale}{Character. Column name prefix (e.g. `"phq"`).}
#'   \item{item_indices}{Numeric or character vector of item numbers.}
#'   \item{min_score}{Minimum possible item score.}
#'   \item{max_score}{Maximum possible item score.}
#'   \item{inverse_items}{Numeric vector of items to reverse-score, or `NULL`.}
#'   \item{recode_items}{Named list of recoding functions, or `NULL`.}
#'   \item{subscales}{Named list of subscale definitions, or `NULL`.}
#'   \item{calc_sum}{Logical; compute sum score?}
#'   \item{calc_mean}{Logical; compute mean score?}
#'   \item{subscale_suffix}{Character suffix for subscale sum columns.}
#' }
#'
#' @section Scales included:
#' The registry currently contains specifications for the following instruments
#' (grouped by assessment phase):
#'
#' **Online Screening 1:** MCSD, MDRS-22, PHQ-9, AMS, IPSS
#'
#' **Online Screening 2:** SB-PNI, CMNI-30, MRNI, GRCS-SF, PMB, GRDS
#'
#' **Clinical Assessment:** CTQ-28, BDI-II, HDRS-21, SSEV, SCS-18, AUDIT,
#' PPCS-18, IIEF, PSS-10, GAD-7, LS-20 Short, SCS-Self-Compassion, ERQ, GBS,
#' CIDI T12, ITQ-18-II, AF-BP, FBK-6, MBAS-R15, BAS-2-10, STIG-9, SSOSH,
#' SHAME, TAS-20, Rosenberg, AISS-D, NTBS, PRIUSS-3, IGDS-9, BSMAS-6, CGI-SI
#'
#' **Session Measures:** BPSR-Therapist, BPSR-Patient, WAI-Therapist,
#' WAI-Patient
#'
#' @source Lab internal documentation.
#'
#' @examples
#' data(scale_specs_all)
#' names(scale_specs_all)
#' scale_specs_all$phq
"scale_specs_all"
