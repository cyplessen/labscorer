# ══════════════════════════════════════════════════════════════════════════════
# data-raw/scale_specs_all.R
#
# MASTER REGISTRY OF ALL PSYCHOMETRIC SCALES
#
# This file is the single source of truth for every questionnaire scored in
# our lab. To add a new scale, append a new entry to the list below and re-run
# this script to update the package data.
#
# After editing, rebuild the .rda file by running:
#
#   source("data-raw/scale_specs_all.R")
#
# Then reinstall the package:
#
#   devtools::install()
#
# ══════════════════════════════════════════════════════════════════════════════

# Load recode helpers (needed before the package is built)
source("R/recode_helpers.R")
#
# FIELD REFERENCE
# ───────────────────────────────────────────────────────────────────────────
#
#   scale           (required) Column name prefix, e.g. "phq"
#   item_indices    (required) Item numbers — numeric or character vector
#   min_score       Minimum possible item score (default: 1)
#   max_score       Maximum possible item score
#   inverse_items   Items to reverse-score before computing totals
#   recode_items    Named list of recoding functions, keyed by item index
#   subscales       Named list of subscale definitions (item index vectors)
#   calc_sum        Compute total sum score? (default: TRUE)
#   calc_mean       Compute total mean score? (default: TRUE)
#   subscale_suffix Suffix for subscale sum columns (default: "_sub_sum")
#
# ══════════════════════════════════════════════════════════════════════════════


scale_specs_all <- list(

  # ═══════════════════════════════════════════════════════════════════════════
  # ONLINE SCREENING 1 (OS1)
  # ═══════════════════════════════════════════════════════════════════════════

  mcsd = list(
    scale        = "mcsd",
    item_indices = 1:10,
    min_score    = 0,
    max_score    = 1,
    inverse_items = c(2, 4, 6, 7, 8)
  ),

  mdrs22 = list(
    scale        = "mdrs22",
    item_indices = 1:22,
    min_score    = 0,
    max_score    = 7,      # after recode 1–8 → 0–7
    subscales    = list(
      emot_supp        = c(1, 2, 8, 17),
      drug_use         = c(13, 18, 22),
      alcohol_use      = c(3, 10, 11, 15),
      anger_aggr       = c(12, 19, 20, 21),
      somatic_symptoms = c(5, 6, 7, 9),
      risk_taking      = c(4, 14, 16)
    )
  ),

  phq = list(
    scale        = "phq",
    item_indices = 1:9,
    min_score    = 0,
    max_score    = 3       # after recode 1–4 → 0–3 (0 → NA)
  ),

  ams = list(
    scale        = "ams",
    item_indices = 1:17,
    min_score    = 1,
    max_score    = 5,      # after recode: 0 → NA, scale 1–5
    subscales    = list(
      somat  = c(1, 2, 3, 4, 5, 9, 10),
      psych  = c(6, 7, 8, 11, 13),
      sexual = c(12, 14, 15, 16, 17)
    )
  ),

  ipss = list(
    scale        = "ipss",
    item_indices = 1:7,
    min_score    = 0,
    max_score    = 5
  ),


  # ═══════════════════════════════════════════════════════════════════════════
  # ONLINE SCREENING 2 (OS2)
  # ═══════════════════════════════════════════════════════════════════════════

  sb_pni = list(
    scale        = "sb_pni",
    item_indices = 1:12,
    min_score    = 0,
    max_score    = 5,
    subscales    = list(
      grandiosity   = 1:6,
      vulnerability = 7:12
    )
  ),

  cmni30 = list(
    scale         = "cmni30",
    item_indices  = 1:30,
    min_score     = 1,
    max_score     = 6,
    inverse_items = c(1, 3, 5, 9, 13, 15, 18, 22, 28),
    subscales     = list(
      emotional    = c(1, 5, 13),
      winning      = c(7, 16, 23),
      playboy      = c(12, 14, 29),
      violence     = c(3, 15, 24),
      presentation = c(2, 11, 17),
      status       = c(9, 18, 22),
      work         = c(6, 21, 26),
      power        = c(10, 20, 27),
      reliance     = c(4, 25, 28),
      risk         = c(8, 19, 30)
    )
  ),

  mrni = list(
    scale        = "mrni",
    item_indices = 1:21,
    min_score    = 1,
    max_score    = 7,
    subscales    = list(
      er = c(1, 2, 3),
      sr = c(4, 5, 6),
      nt = c(7, 8, 9),
      af = c(10, 11, 12),
      is = c(13, 14, 15),
      do = c(16, 17, 18),
      t  = c(19, 20, 21)
    )
  ),

  grcs_sf = list(
    scale        = "grcs_sf",
    item_indices = 1:16,
    min_score    = 0,
    max_score    = 5,
    subscales    = list(
      re    = 1:4,
      spc   = 5:8,
      rabbm = 9:12,
      cbwfr = 13:16
    )
  ),

  pmb = list(
    scale        = "pmb",
    item_indices = 1:4,
    min_score    = 1,
    max_score    = 7
  ),

  grds = list(
    scale        = "grds",
    item_indices = 1:10,
    min_score    = 1,
    max_score    = 7,
    subscales    = list(
      discrepancy = c(1, 2, 4, 6, 8),
      stress      = c(3, 5, 9, 10)
    )
  ),


  # ═══════════════════════════════════════════════════════════════════════════
  # CLINICAL ASSESSMENT ("t" timepoints)
  # ═══════════════════════════════════════════════════════════════════════════

  ctq28 = list(
    scale         = "ctq28",
    item_indices  = setdiff(1:28, c(10, 16, 22)),
    min_score     = 1,
    max_score     = 5,
    inverse_items = c(2, 5, 7, 13, 19, 26, 28),
    recode_items  = list(
      "10" = recode_ctq_bagatellization,
      "16" = recode_ctq_bagatellization,
      "22" = recode_ctq_bagatellization
    ),
    subscales = list(
      emot_abuse      = c(3, 8, 14, 18, 25),
      phys_abuse      = c(9, 11, 12, 15, 17),
      sex_abuse       = c(20, 21, 23, 24, 27),
      emot_neglect    = c(5, 7, 13, 19, 28),
      phys_neglect    = c(1, 2, 4, 6, 26),
      bagatellization = c(10, 16, 22)
    )
  ),

  bdi_ii = list(
    scale        = "bdi_ii",
    item_indices = 1:21,
    min_score    = 0,
    max_score    = 3
  ),

  hdrs21 = list(
    scale        = "hdrs21",
    item_indices = c(1:17, "18b", 19:21),
    min_score    = 0,
    max_score    = 4
  ),

  ssev = list(
    scale        = "ssev",
    item_indices = 1:6,
    min_score    = 0,
    max_score    = 5
  ),

  scs18 = list(
    scale        = "scs18",
    item_indices = 1:18,
    min_score    = 1,
    max_score    = 5
  ),

  audit = list(
    scale        = "audit",
    item_indices = 1:10,
    min_score    = 0,
    max_score    = 4
  ),

  ppcs18 = list(
    scale        = "ppcs18",
    item_indices = 1:18,
    min_score    = 1,
    max_score    = 7
  ),

  iiief = list(
    scale        = "iiief",
    item_indices = 1:15,
    min_score    = 0,
    max_score    = 5
  ),

  pss10 = list(
    scale        = "pss10",
    item_indices = 1:10,
    min_score    = 0,
    max_score    = 4
  ),

  gad7 = list(
    scale        = "gad7",
    item_indices = 1:7,
    min_score    = 0,
    max_score    = 3
  ),

  ls20_short = list(
    scale        = "ls20_short",
    item_indices = 1:3,
    min_score    = 0,
    max_score    = 2
  ),

  scs_selfcomp = list(
    scale         = "scs_selfcomp",
    item_indices  = 1:12,
    min_score     = 1,
    max_score     = 5,
    inverse_items = c(1, 9, 11, 12),
    subscales     = list(
      selfkindness       = c(2, 6),
      selfjudgment       = c(11, 12),
      commonhumanity     = c(5, 10),
      isolation          = c(4, 8),
      mindfulness        = c(3, 7),
      overidentification = c(1, 9)
    )
  ),

  erq = list(
    scale        = "erq",
    item_indices = 1:10,
    min_score    = 1,
    max_score    = 7,
    subscales    = list(
      reappraisal = c(1, 3, 5, 7, 8, 10),
      suppression = c(2, 4, 6, 9)
    )
  ),

  gbs = list(
    scale         = "gbs",
    item_indices  = 1:12,
    min_score     = 1,
    max_score     = 5,
    inverse_items = c(3, 4, 6, 7, 9, 12)
  ),

  cidi_t12 = list(
    scale        = "cidi_t12",
    item_indices = c(1:8, "9a"),
    min_score    = 0,
    max_score    = 1
  ),

  itq18_ii = list(
    scale        = "itq18_ii",
    item_indices = 1:6,
    min_score    = 0,
    max_score    = 4
  ),

  af_bp = list(
    scale        = "af_bp",
    item_indices = setdiff(1:29, c(14, 22)),
    min_score    = 1,
    max_score    = 4,
    subscales    = list(
      verbaggression = c(2, 6, 13, 17, 29),
      physaggression = c(1, 5, 9, 12, 16, 18, 19, 25),
      anger          = c(3, 7, 10, 20, 23, 26),
      hostility      = c(4, 8, 11, 15, 21, 24, 27, 28)
    )
  ),

  fbk6 = list(
    scale        = "fbk6",
    item_indices = 1:12,
    min_score    = 1,
    max_score    = 5,
    subscales    = list(
      realbodyimage  = 1:6,
      idealbodyimage = 7:12
    )
  ),

  mbas_r15 = list(
    scale         = "mbas_r15",
    item_indices  = 1:15,
    min_score     = 1,
    max_score     = 5,
    inverse_items = c(7, 14),
    subscales     = list(
      muscle  = 1:7,
      bodyfat = 8:12,
      height  = 13:15
    )
  ),

  bas_2_10 = list(
    scale        = "bas_2_10",
    item_indices = 1:10,
    min_score    = 1,
    max_score    = 5
  ),

  stig9 = list(
    scale        = "stig9",
    item_indices = 1:9,
    min_score    = 0,
    max_score    = 3
  ),

  ssosh = list(
    scale         = "ssosh",
    item_indices  = 1:10,
    min_score     = 1,
    max_score     = 5,
    inverse_items = c(2, 4, 5, 7, 9)
  ),

  shame = list(
    scale        = "shame",
    item_indices = 1:21,
    min_score    = 0,
    max_score    = 5,
    subscales    = list(
      body        = 1:7,
      cognitive   = 8:14,
      existential = 15:21
    )
  ),

  tas20 = list(
    scale         = "tas20",
    item_indices  = 1:20,
    min_score     = 1,
    max_score     = 5,
    inverse_items = c(4, 5, 10, 18, 19),
    subscales     = list(
      novelty   = c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19),
      intensity = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
    )
  ),

  rosenberg = list(
    scale         = "rosenberg",
    item_indices  = 1:10,
    min_score     = 1,
    max_score     = 4,
    inverse_items = c(2, 5, 6, 8, 9)
  ),

  aiss_d = list(
    scale         = "aiss_d",
    item_indices  = 1:20,
    min_score     = 1,
    max_score     = 4,
    inverse_items = c(2, 6, 10, 13, 17)
  ),

  ntbs = list(
    scale         = "ntbs",
    item_indices  = 1:10,
    min_score     = 1,
    max_score     = 5,
    inverse_items = c(1, 3, 7)
  ),

  priuss3 = list(
    scale        = "priuss3",
    item_indices = 1:3,
    min_score    = 0,
    max_score    = 4
  ),

  igds9 = list(
    scale        = "igds9",
    item_indices = 1:9,
    min_score    = 0,
    max_score    = 1
  ),

  bsmas6 = list(
    scale        = "bsmas6",
    item_indices = 1:6,
    min_score    = 0,
    max_score    = 4
  ),

  cgi_si = list(
    scale        = "cgi_si",
    item_indices = 1:2,
    min_score    = 1,
    max_score    = 7,
    calc_sum     = FALSE,
    calc_mean    = FALSE
  ),


  # ═══════════════════════════════════════════════════════════════════════════
  # SESSION MEASURES ("s" timepoints)
  # ═══════════════════════════════════════════════════════════════════════════

  t_bpsrt = list(
    scale        = "t_bpsrt",
    item_indices = 1:27,
    min_score    = -3,
    max_score    = 3,
    subscales    = list(
      therapiebeziehung               = c(1, 2, 3),
      offenheit                       = c(5, 9, 10, 11),
      therapiefortschritte            = c(6, 8),
      interaktionelle_schwierigkeiten = c(7),
      problembewaeltigung             = c(15, 17, 21),
      motivationale_klaerung          = c(13, 24, 25),
      ressourcenaktivierung           = c(14, 20, 22),
      problemaktualisierung           = c(16, 19, 23),
      anstrengungsbereitschaft        = c(4, 12),
      interaktionelle_perspektive     = c(18, 26),
      bezug_real                      = c(27)
    ),
    calc_sum  = TRUE,
    calc_mean = FALSE
  ),

  p_bpsr = list(
    scale        = "p_bpsr",
    item_indices = 1:22,
    min_score    = -3,
    max_score    = 3,
    subscales    = list(
      therapiebeziehung        = c(1, 7, 9),
      selbstwerterfahrungen    = c(5, 10, 16),
      bewaeltigungserfahrungen = c(11, 13, 18),
      klaerungserfahrungen     = c(2, 3, 6),
      therapiefortschritte     = c(4),
      aufgehobensein           = c(8, 12, 14, 19),
      direktivitaet_kontrolle  = c(20, 21, 22),
      problemaktualisierung    = c(15, 17)
    ),
    calc_sum  = TRUE,
    calc_mean = FALSE
  ),

  t_workallinv = list(
    scale        = "t_workallinv",
    item_indices = 1:12,
    min_score    = 1,
    max_score    = 5,
    subscales    = list(
      bindung = c(3, 5, 7, 9),
      prozess = c(1, 2, 8, 12),
      ziele   = c(4, 6, 10, 11)
    ),
    calc_sum  = TRUE,
    calc_mean = FALSE
  ),

  p_workallinv = list(
    scale        = "p_workallinv",
    item_indices = 1:12,
    min_score    = 1,
    max_score    = 5,
    subscales    = list(
      bindung = c(3, 5, 7, 9),
      prozess = c(1, 2, 8, 12),
      ziele   = c(4, 6, 10, 11)
    ),
    calc_sum  = TRUE,
    calc_mean = FALSE
  )

)


# ── Save as package data ─────────────────────────────────────────────────────
usethis::use_data(scale_specs_all, overwrite = TRUE)
