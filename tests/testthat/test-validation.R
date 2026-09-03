test_that("constructors reject out-of-range parameters", {
  expect_error(dfr_kswin(alpha = 2), "between 0 and 1")
  expect_error(dfr_kswin(window_size = 10, stat_size = 20), "smaller than window_size")
  expect_error(dfr_mcdd(alpha = -1), "between 0 and 1")
  expect_error(dfr_lbdd(window_size = 1), "greater than or equal to")
  expect_error(dfr_kldist(p_th = -0.1), "non-negative")
  expect_error(dfr_hddm(drift_confidence = 5), "between 0 and 1")
  expect_error(dfr_eddm(warning_level = 3), "between 0 and 1")
  expect_error(mt_fscore(f = 0), "positive")
})

test_that("ecdd rejects an average run length it cannot calibrate", {
  expect_error(dfr_ecdd(average_run_length = 5000), "lower than or equal to 1000")
  expect_error(dfr_ecdd(average_run_length = 1000), NA)
})

test_that("ecdd computes a control limit for every accepted run length", {
  for (arl in c(50, 100, 400, 1000)) {
    model <- dfr_ecdd(average_run_length = arl, min_run_instances = 5)
    expect_error(fit(model, rep(c(0, 1), 40)), NA, info = as.character(arl))
  }
})

test_that("multi criteria validates its arguments", {
  expect_error(dfr_multi_criteria(drifter_list = list()), "non-empty list")
  expect_error(
    dfr_multi_criteria(drifter_list = list(bad = dfr_ddm())),
    "dist_based"
  )
  expect_error(
    dfr_multi_criteria(drifter_list = list(ph = dfr_page_hinkley()), combination = "xor"),
    NULL
  )
})

test_that("aedd validates the criteria argument", {
  expect_error(dfr_aedd(encoding_size = 2, criteria = "not_a_test"), NULL)
})
