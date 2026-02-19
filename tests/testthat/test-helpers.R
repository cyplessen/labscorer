# tests/testthat/test-helpers.R

# ── Data cleaning ────────────────────────────────────────────────────────────

test_that("recode_missing replaces sentinel values with NA", {
  df <- data.frame(x = c(1, -99, 3, -77), y = c("a", "-66", "c", "d"))
  out <- recode_missing(df)
  expect_equal(out$x, c(1, NA, 3, NA))
  expect_equal(out$y, c("a", NA, "c", "d"))
})

test_that("recode_missing accepts custom codes", {
  df <- data.frame(x = c(1, -999, 3))
  out <- recode_missing(df, missing_codes = -999)
  expect_equal(out$x, c(1, NA, 3))
})

test_that("recode_binary converts 1/2 to 1/0", {
  expect_equal(recode_binary(c(1, 2, NA, 1)), c(1L, 0L, NA, 1L))
  expect_equal(recode_binary(c(3)), NA_integer_)
})

test_that("not_all_na works", {
  expect_true(not_all_na(c(NA, 1, NA)))
  expect_false(not_all_na(c(NA, NA)))
})

test_that("clear_labels removes label attributes", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  attr(df$x, "label") <- "My X"
  attr(df$y, "label") <- "My Y"
  out <- clear_labels(df)
  expect_null(attr(out$x, "label"))
  expect_null(attr(out$y, "label"))
})

test_that("inspect_missings finds sentinel values", {
  df <- data.frame(a = c(1, -99, 3), b = c(-77, 5, 6), c = c(1, 2, 3))
  out <- inspect_missings(df)
  expect_true("a" %in% out$variable)
  expect_true("b" %in% out$variable)
  expect_false("c" %in% out$variable)
})


# ── Scale recoding ───────────────────────────────────────────────────────────

test_that("recode_mdrs shifts 1-8 to 0-7", {
  df <- data.frame(mdrs22_1 = c(1, 4, 8), mdrs22_2 = c(2, 5, 7))
  out <- recode_mdrs(df, items = 1:2)
  expect_equal(out$mdrs22_1, c(0, 3, 7))
  expect_equal(out$mdrs22_2, c(1, 4, 6))
})

test_that("recode_phq converts 0->NA, 1:4->0:3", {
  df <- data.frame(phq_1 = c(0, 1, 2, 3, 4))
  out <- recode_phq(df, items = 1)
  expect_equal(out$phq_1, c(NA, 0, 1, 2, 3))
})

test_that("recode_mcsd shifts 1-2 to 0-1", {
  df <- data.frame(mcsd_1 = c(1, 2), mcsd_2 = c(2, 1))
  out <- recode_mcsd(df, items = 1:2)
  expect_equal(out$mcsd_1, c(0, 1))
  expect_equal(out$mcsd_2, c(1, 0))
})

test_that("recode_ipss shifts 1-6 to 0-5", {
  df <- data.frame(ipss_1 = c(1, 6))
  out <- recode_ipss(df, items = 1)
  expect_equal(out$ipss_1, c(0, 5))
})

test_that("recode_ams sets 0 to NA", {
  df <- data.frame(ams_1 = c(0, 1, 3, 5))
  out <- recode_ams(df, items = 1)
  expect_equal(out$ams_1, c(NA, 1, 3, 5))
})

test_that("recode_sb_pni converts 0->NA, 1:6->0:5", {
  df <- data.frame(sb_pni_1 = c(0, 1, 6))
  out <- recode_sb_pni(df, items = 1)
  expect_equal(out$sb_pni_1, c(NA, 0, 5))
})

test_that("recode_cgi converts 0->NA, keeps 1:7", {
  df <- data.frame(cgi_si_1 = c(0, 1, 7))
  out <- recode_cgi(df, items = 1)
  expect_equal(out$cgi_si_1, c(NA, 1, 7))
})

test_that("recode functions return df unchanged when no columns match", {
  df <- data.frame(x = 1:3)
  expect_equal(recode_mdrs(df), df)
  expect_equal(recode_phq(df), df)
  expect_equal(recode_mcsd(df), df)
})

test_that("rename_scales_os_to_redcap renames prefixes", {
  df <- data.frame(cmni_1 = 1, mdrs_5 = 2, grcs_3 = 3, other = 4)
  out <- rename_scales_os_to_redcap(df)
  expect_true("cmni30_1" %in% names(out))
  expect_true("mdrs22_5" %in% names(out))
  expect_true("grcs_sf_3" %in% names(out))
  expect_true("other" %in% names(out))
})


# ── Display helpers ──────────────────────────────────────────────────────────

test_that("fmt_vec formats vectors and handles NULL", {
  expect_equal(fmt_vec(c(1, 3, 5)), "1, 3, 5")
  expect_equal(fmt_vec(NULL), "")
  expect_equal(fmt_vec(character(0)), "")
})

test_that("fmt_subscales formats named lists", {
  subs <- list(reapp = c(1, 3), supp = c(2, 4))
  out <- fmt_subscales(subs)
  expect_true(grepl("reapp: 1, 3", out))
  expect_true(grepl("supp: 2, 4", out))
  expect_equal(fmt_subscales(NULL), "")
})

test_that("summarize_range returns min, max, n", {
  df <- data.frame(phq_sum = c(5, 12, 20, NA), phq_mean = c(0.6, 1.3, 2.2, NA))
  out <- summarize_range(df, "phq")
  expect_true(all(c("variable", "min", "max", "n") %in% names(out)))
  expect_equal(out$n[out$variable == "phq_sum"], 3)
})
