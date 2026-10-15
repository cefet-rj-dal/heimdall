test_that("adwin detects the change of the scikit-multiflow example", {
  # Same scenario as the scikit-multiflow documentation: noise in {0, 1}
  # followed by values in {4, ..., 7}. The reference implementation reports the
  # change at the first check after the shift (observation 1024).
  set.seed(1)
  stream <- c(sample(0:1, 999, TRUE), sample(4:7, 1001, TRUE))
  detections <- detect_indices(dfr_adwin(delta = 0.002), stream)
  expect_equal(detections[1], 1024L)
})

test_that("adwin only checks for changes every 32 observations", {
  set.seed(1)
  stream <- c(stats::rnorm(500), stats::rnorm(500, mean = 5))
  detections <- detect_indices(dfr_adwin(), stream)
  expect_true(length(detections) > 0)
  expect_true(all(detections %% 32 == 0))
})

test_that("adwin stays quiet on a stationary stream", {
  set.seed(2)
  expect_false(fit(dfr_adwin(), stats::rnorm(3000))$drifted)
})

test_that("adwin shrinks its window after a change", {
  set.seed(3)
  stream <- c(stats::rnorm(1000), stats::rnorm(200, mean = 10))
  model <- fit(dfr_adwin(), stream)
  expect_true(model$drifted)
  expect_lt(model$state$width, length(stream))
  expect_equal(model$state$width, sum(model$state$sizes * 2^(seq_along(model$state$sizes) - 1)))
})

test_that("adwin keeps its statistics consistent with the window", {
  model <- fit(dfr_adwin(), c(1, 2, 3, 4))
  expect_equal(model$state$width, 4)
  expect_equal(model$state$total, 10)
  expect_equal(model$state$variance, sum((1:4 - 2.5)^2))
})

test_that("adwin can be serialized and restored", {
  set.seed(4)
  model <- fit(dfr_adwin(), stats::rnorm(200))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(model, path)
  restored <- readRDS(path)

  expect_identical(restored$state, model$state)
  expect_identical(update_state(restored, 0)$obj$state, update_state(model, 0)$obj$state)
})

test_that("adwin reset keeps delta and clears the window", {
  model <- fit(dfr_adwin(delta = 0.01), 1:100)
  model$drifted <- TRUE
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_equal(model$state$delta, 0.01)
  expect_equal(model$state$width, 0)
})

test_that("adwin validates delta", {
  expect_error(dfr_adwin(delta = 2), "between 0 and 1")
})
