# Behaviour of individual detectors that is not covered by the shared
# contract tests.

test_that("every detector reports a drift once, even without reset_state", {
  set.seed(1)
  errors <- c(stats::rbinom(300, 1, 0.1), rep(1, 300))
  for (name in names(error_based_detectors())) {
    output <- list(obj = error_based_detectors()[[name]], drift = FALSE)
    calls <- 0
    for (value in errors) {
      output <- update_state(output$obj, value)
      calls <- calls + output$drift
    }
    expect_lte(calls, 3, label = name)
  }
})

test_that("cusum detects a moderate increase of the error rate", {
  set.seed(42)
  errors <- c(stats::rbinom(1000, 1, 0.1), stats::rbinom(1000, 1, 0.4))
  detections <- detect_indices(dfr_cusum(), errors)
  expect_length(detections, 1)
  expect_gt(detections, 1000)
  expect_lt(detections, 1300)

  set.seed(7)
  expect_length(detect_indices(dfr_cusum(), stats::rbinom(5000, 1, 0.2)), 0)
})

test_that("ecdd raises fewer false alarms with a longer average run length", {
  set.seed(10)
  errors <- stats::rbinom(5000, 1, 0.2)
  short <- length(detect_indices(dfr_ecdd(average_run_length = 100), errors))
  long <- length(detect_indices(dfr_ecdd(average_run_length = 400), errors))
  expect_lt(long, short)
})

test_that("page-hinkley detects decreases when asked to", {
  set.seed(2)
  down <- c(stats::rnorm(300, 10), stats::rnorm(300))
  up <- c(stats::rnorm(300), stats::rnorm(300, 10))

  expect_length(detect_indices(dfr_page_hinkley(), down), 0)
  expect_gt(detect_indices(dfr_page_hinkley(direction = "decrease"), down)[1], 300)
  expect_gt(detect_indices(dfr_page_hinkley(direction = "both"), down)[1], 300)
  expect_gt(detect_indices(dfr_page_hinkley(direction = "both"), up)[1], 300)
  expect_length(detect_indices(dfr_page_hinkley(direction = "decrease"), up), 0)
})

test_that("page-hinkley keeps its direction after a reset", {
  model <- reset_state(dfr_page_hinkley(direction = "both"))
  expect_equal(model$state$direction, "both")
})

test_that("kl distance rarely alarms on a stationary stream", {
  set.seed(4)
  expect_lte(length(detect_indices(dfr_kldist(), stats::rnorm(5000))), 5)
})

test_that("kl distance reacts to a shift of the mean", {
  set.seed(5)
  detections <- detect_indices(dfr_kldist(threshold = 0.15), data.frame(a = c(stats::rnorm(500), stats::rnorm(500, 2))))
  expect_true(any(detections > 500 & detections < 600))
})

test_that("window detectors with default parameters stay quiet on stationary data", {
  set.seed(6)
  stream <- stats::rnorm(3000)
  expect_lte(length(detect_indices(dfr_kswin(), stream)), 1)
  expect_lte(length(detect_indices(dfr_lbdd(), stream)), 1)
  expect_lte(length(detect_indices(dfr_mcdd(), stream)), 1)
})
