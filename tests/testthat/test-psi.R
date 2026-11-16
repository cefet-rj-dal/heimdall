test_that("psi follows the quantile groups of PROC RANK and the paper's formula", {
  base <- c(5, 1, 3, 3, 9, 7, 2, 8, 6, 4)
  result <- psi(base, base, bins = 5)
  # floor(rank * 5 / 11): ranks 1, 2 -> 0; the tied 3s (rank 3.5) -> 1; ranks 5, 6 -> 2; ...
  expect_equal(result$bins$lower, c(1, 3, 4, 6, 8))
  expect_equal(result$bins$upper, c(2, 3, 5, 7, 9))
  expect_equal(result$bins$expected, rep(0.2, 5))
  expect_equal(result$summary$psi, 0)
  expect_equal(result$summary$level, "little")
  # The parts are (P - Q) ln(P / Q), as in Table 1 of the paper; empty bins give 0.
  new <- c(1, 1, 1, 1, 2, 3, 9, 9, 9, 9)
  out <- psi(base, new, bins = 5)
  p <- c(5, 1, 0, 0, 4) / 10
  q <- rep(0.2, 5)
  expect_equal(out$bins$actual, p)
  expect_equal(out$bins$part, ifelse(p > 0, (p - q) * log(p / q), 0))
  expect_equal(out$summary$psi, sum(out$bins$part))
})

test_that("psi clamps new values, keeps missing values in the size and handles gaps", {
  base <- seq(10, 100, by = 10)
  out <- psi(base, c(-50, 500, NA, 15), bins = 2)
  # -50 and 15 go to the first bin, 500 to the last, NA to none.
  expect_equal(out$bins$actual, c(2, 1) / 4)
  expect_equal(sum(out$bins$actual), 0.75)
  # 52 lies between the bins (10-50 and 60-100) and goes to the lower one.
  expect_equal(psi(base, 52, bins = 2)$bins$actual, c(1, 0))
})

test_that("psi works feature by feature and classifies the drift", {
  set.seed(1)
  base <- data.frame(a = stats::rnorm(5000), b = stats::rnorm(5000))
  new <- data.frame(b = stats::rnorm(5000, 0.5), a = stats::rnorm(5000))
  result <- psi(base, new)
  expect_s3_class(result, "psi")
  expect_equal(result$summary$feature, c("a", "b"))
  expect_lt(result$summary$psi[1], 0.01)
  expect_gt(result$summary$psi[2], 0.2)
  expect_equal(nrow(result$bins), 20L)
  expect_output(print(result), "Population Stability Index")
  expect_equal(psi(base$a, base$a * 3)$summary$level, "significant")
  expect_equal(psi(base$a, new$b, thresholds = c(0.5, 1))$summary$level, "little")
  expect_error(psi(base, new["a"]), "lacks the features b")
  expect_error(psi("a", 1), "numeric")
  expect_error(psi(1:10, 1:10, bins = 1), "bins")
  expect_error(psi(1:10, 1:10, thresholds = c(0.3, 0.1)), "thresholds")
  expect_error(psi(c(NA_real_, NA_real_), 1), "no values")
})

test_that("dfr_psi takes a baseline and compares the next windows", {
  set.seed(2)
  stream <- data.frame(x = c(stats::rnorm(3000), stats::rnorm(2000, 1)), y = stats::rnorm(5000))
  model <- fit(dfr_psi(n_reference = 1000, window_size = 500), stream)
  expect_equal(nrow(model$reference), 1000L)
  output <- model$drifter_output
  expect_equal(names(output), c("psi_x", "psi_y"))
  tested <- which(!is.na(output$psi_x))
  expect_equal(tested, 1000 + 500 * (1:8))
  expect_true(all(output$psi_x[tested[1:4]] < 0.1))
  expect_true(all(output$psi_x[tested[5:8]] > 0.25))
  expect_true(all(output$psi_y[tested] < 0.1))
  expect_true(model$drifted)
  # The same result as psi() on the same samples.
  expect_equal(output$psi_x[tested[5]], psi(stream[1:1000, ], stream[3001:3500, ])$summary$psi[1])
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_null(model$reference)
  stationary <- fit(dfr_psi(target_feat = "y", n_reference = 1000, window_size = 500), stream)
  expect_false(stationary$drifted)
  expect_equal(names(stationary$drifter_output), "psi_y")
  expect_error(dfr_psi(window_size = 0), "window_size")
  expect_error(dfr_psi(threshold = -1), "threshold")
})
