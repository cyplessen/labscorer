# tests/testthat/test-scoring.R

test_that("invert_item reverses scores correctly", {
  expect_equal(invert_item(c(1, 3, 5), min_score = 1, max_score = 5), c(5, 3, 1))
  expect_equal(invert_item(c(0, 1, 2, 3), min_score = 0, max_score = 3), c(3, 2, 1, 0))
})

test_that("invert_item preserves NA", {
  expect_equal(invert_item(c(1, NA, 5), min_score = 1, max_score = 5), c(5, NA, 1))
})

test_that("invert_item clamps out-of-range values", {
  # score of 0 with min=1 max=5: inverted = 5+1-0 = 6, clamped to 5

  expect_equal(invert_item(c(0), min_score = 1, max_score = 5), 5)
})

test_that("invert_item errors without max_score", {
  expect_error(invert_item(c(1, 2, 3)), "max_score")
})


test_that("detect_timepoints finds t1, t2", {
  df <- data.frame(t1_phq_1 = 1, t1_phq_2 = 2, t2_phq_1 = 3, t2_phq_2 = 4)
  expect_equal(detect_timepoints(df, "phq", "t"), c("t1", "t2"))
})

test_that("detect_timepoints returns empty string for NULL prefix", {
  df <- data.frame(phq_1 = 1, phq_2 = 2)
  expect_equal(detect_timepoints(df, "phq", NULL), "")
})

test_that("detect_timepoints errors if no matches", {
  df <- data.frame(x = 1, y = 2)
  expect_error(detect_timepoints(df, "phq", "t"), "No timepoints")
})


test_that("validate_specs catches missing fields", {
  bad <- list(oops = list(scale = "x"))
  expect_warning(validate_specs(bad), "item_indices")
})

test_that("validate_specs catches inverse_items without max_score", {
  bad <- list(x = list(scale = "x", item_indices = 1:5, inverse_items = c(1, 3)))
  expect_warning(validate_specs(bad), "max_score")
})

test_that("validate_specs catches unknown fields (typos)", {
  bad <- list(x = list(scale = "x", item_indices = 1:5, min_scor = 0))
  expect_warning(validate_specs(bad), "unknown")
})

test_that("validate_specs catches subscale items not in item_indices", {
  bad <- list(x = list(
    scale = "x", item_indices = 1:5, min_score = 1, max_score = 5,
    subscales = list(sub1 = c(1, 2, 99))
  ))
  expect_warning(validate_specs(bad), "99")
})


test_that("calculate_sum_scores computes sum and mean without timepoints", {
  df <- data.frame(phq_1 = c(0, 1, 2), phq_2 = c(3, 2, 1))
  out <- calculate_sum_scores(df, "phq", 1:2, min_score = 0, max_score = 3,
                              timepoint_prefix = NULL)
  expect_equal(out$phq_sum,  c(3, 3, 3))
  expect_equal(out$phq_mean, c(1.5, 1.5, 1.5))
})

test_that("calculate_sum_scores handles timepoints", {
  df <- data.frame(
    t1_phq_1 = c(0, 3), t1_phq_2 = c(3, 0),
    t2_phq_1 = c(1, 1), t2_phq_2 = c(1, 1)
  )
  out <- calculate_sum_scores(df, "phq", 1:2, min_score = 0, max_score = 3,
                              timepoint_prefix = "t")
  expect_equal(out$t1_phq_sum, c(3, 3))
  expect_equal(out$t2_phq_sum, c(2, 2))
})

test_that("calculate_sum_scores inverts items", {
  df <- data.frame(x_1 = c(1, 5), x_2 = c(5, 1))
  out <- calculate_sum_scores(df, "x", 1:2, min_score = 1, max_score = 5,
                              inverse_items = 1, timepoint_prefix = NULL)
  # item 1 inverted: 1->5, 5->1
  expect_equal(out$x_1, c(5, 1))
  # item 2 untouched
  expect_equal(out$x_2, c(5, 1))
  expect_equal(out$x_sum, c(10, 2))
})

test_that("calculate_sum_scores computes subscales", {
  df <- data.frame(x_1 = 1, x_2 = 2, x_3 = 3, x_4 = 4)
  out <- calculate_sum_scores(
    df, "x", 1:4, min_score = 1, max_score = 4,
    subscales = list(ab = c(1, 2), cd = c(3, 4)),
    timepoint_prefix = NULL
  )
  expect_equal(out$x_ab_sub_sum, 3)
  expect_equal(out$x_cd_sub_sum, 7)
  expect_equal(out$x_ab_mean, 1.5)
  expect_equal(out$x_cd_mean, 3.5)
})

test_that("calculate_sum_scores propagates NA when na_rm = FALSE", {
  df <- data.frame(phq_1 = c(1, NA), phq_2 = c(2, 3))
  out <- calculate_sum_scores(df, "phq", 1:2, min_score = 0, max_score = 3,
                              na_rm = FALSE, timepoint_prefix = NULL)
  expect_equal(out$phq_sum, c(3, NA))
})


test_that("create_sum_scores returns list with df and database", {
  df <- data.frame(phq_1 = c(0, 1), phq_2 = c(3, 2))
  specs <- list(phq = list(scale = "phq", item_indices = 1:2,
                           min_score = 0, max_score = 3))
  result <- create_sum_scores(df, specs, verbose = FALSE)

  expect_type(result, "list")
  expect_named(result, c("df", "database"))
  expect_true("phq_sum" %in% names(result$df))
  expect_true(nrow(result$database) > 0)
})

test_that("create_sum_scores database flags out-of-range correctly", {
  # All values in range → no flags
  df <- data.frame(phq_1 = c(0, 1, 2), phq_2 = c(3, 2, 1))
  specs <- list(phq = list(scale = "phq", item_indices = 1:2,
                           min_score = 0, max_score = 3))
  result <- create_sum_scores(df, specs, verbose = FALSE)
  stats <- tidyr::unnest(result$database, created_score_stats)
  expect_false(any(stats$out_of_range))
})

test_that("create_sum_scores skips scales not in data", {
  df <- data.frame(phq_1 = 1, phq_2 = 2)
  specs <- list(
    phq = list(scale = "phq", item_indices = 1:2, min_score = 0, max_score = 3),
    gad = list(scale = "gad", item_indices = 1:7, min_score = 0, max_score = 3)
  )
  result <- create_sum_scores(df, specs, verbose = FALSE)
  expect_true("phq_sum" %in% names(result$df))
  expect_false(any(grepl("gad", names(result$df))))
})
