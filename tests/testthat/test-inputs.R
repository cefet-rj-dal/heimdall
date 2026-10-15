# Input handling shared by every detector: monitored column selection,
# coercion of single observations and warm-start windows.

test_that("fit uses only the monitored column of a data frame", {
  data <- data.frame(a = rep(0, 300), b = c(rep(0, 150), rep(100, 150)))
  for (make in list(dfr_page_hinkley, dfr_adwin, dfr_kldist, dfr_mcdd)) {
    expect_false(fit(make(target_feat = "a"), data)$drifted)
  }
  model <- fit(dfr_page_hinkley(target_feat = "a", threshold = 1e9), data)
  expect_equal(model$state$sample_count - 1, 300)
})

test_that("fit asks for target_feat when the data has several columns", {
  data <- data.frame(a = 1:10, b = 1:10)
  expect_error(fit(dfr_page_hinkley(), data), "set 'target_feat'")
  expect_error(fit(dfr_ddm(), data), "set 'target_feat'")
  expect_error(fit(dfr_page_hinkley(target_feat = "c"), data), "'c' is not in the data")
  expect_error(fit(dfr_page_hinkley(), data[, "a", drop = FALSE]), NA)
})

test_that("update_state reads the monitored column of a one-row data frame", {
  model <- dfr_page_hinkley(target_feat = "b")
  output <- update_state(model, data.frame(a = 100, b = 1))
  expect_equal(output$obj$state$x_mean, 1)
})

test_that("observations are coerced by value, not by factor code", {
  expect_equal(fit(dfr_ddm(), factor(c("0", "1", "1")))$state$sample_count, 4)
  expect_equal(
    fit(dfr_ddm(), factor(c("0", "1", "1")))$state$miss_prob,
    fit(dfr_ddm(), c(0, 1, 1))$state$miss_prob
  )
  expect_equal(
    fit(dfr_ddm(), c(TRUE, FALSE))$state$miss_prob,
    fit(dfr_ddm(), c(1, 0))$state$miss_prob
  )
})

test_that("invalid observations are rejected instead of silently ignored", {
  expect_error(update_state(dfr_page_hinkley(), "abc"), "not a numeric observation")
  expect_error(update_state(dfr_page_hinkley(), c(1, 100)), "single observation")
  expect_error(update_state(dfr_ddm(), 2), "expect 1 \\(error\\) or 0")
  expect_error(fit(dfr_cusum(), rep(0.5, 10)), "expect 1 \\(error\\) or 0")
})

test_that("non-finite observations are treated as missing", {
  expect_warning(output <- update_state(dfr_page_hinkley(), Inf), "non-finite")
  expect_false(output$drift)
  expect_equal(output$obj$state$sample_count, 1)

  expect_warning(model <- fit(dfr_adwin(), c(Inf, 1:50)), "non-finite")
  expect_true(is.finite(model$state$variance))
})

test_that("warm-start data never exceeds the window", {
  expect_length(dfr_kswin(window_size = 20, stat_size = 5, data = 1:100)$state$window, 20)
  expect_lte(length(fit(dfr_mcdd(window_size = 20, data = 1:100), 1:10)$state$window), 20)
  expect_equal(dfr_lbdd(window_size = 5, data = 1:100)$state$window, 96:100)
  expect_length(dfr_kldist(window_size = 10, data = c(1, NA, Inf, 2))$state$window, 2)
})

test_that("parameters are validated", {
  expect_error(dfr_kswin(window_size = 10, stat_size = 10), "smaller than window_size")
  expect_error(dfr_ddm(min_instances = 2.5), "whole number")
  expect_error(dfr_eddm(warning_level = 0.5, out_control_level = 0.9), "greater than or equal to out_control_level")
  expect_error(dfr_kldist(p_th = "a"), "numeric")
  expect_error(dfr_page_hinkley(target_feat = 1), "target_feat")
  expect_error(dfr_page_hinkley(direction = "up"), NULL)
  expect_error(dfr_cusum(lambda = -1), "greater than or equal to 0")
  expect_error(dfr_ddm(warning_level = 2.5, out_control_level = 3.5), NA)
})
