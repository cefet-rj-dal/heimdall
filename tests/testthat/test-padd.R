padd_stream <- function(n_chunks, shift_from, n_features = 10, chunk = 200) {
  x <- matrix(stats::rnorm(n_chunks * chunk * n_features), ncol = n_features)
  rows <- seq_len(nrow(x)) > (shift_from - 1) * chunk
  x[rows, 1:3] <- x[rows, 1:3] + 1.5
  as.data.frame(x)
}

test_that("the PADD network and t-test follow the reference", {
  set.seed(1)
  weights <- list(matrix(stats::rnorm(4 * 5, 0, 0.1), 4), matrix(stats::rnorm(5 * 3, 0, 0.1), 5))
  x <- matrix(stats::rnorm(30), ncol = 3)
  hidden <- pmax(cbind(x, 1) %*% weights[[1]], 0)
  out <- pmax(hidden %*% weights[[2]], 0)
  softmax <- exp(out) / rowSums(exp(out))
  activations <- heimdall:::.padd_activations(weights, x)
  expect_equal(activations, softmax)
  expect_equal(rowSums(activations), rep(1, 10))
  a <- matrix(stats::rnorm(40), 4)
  b <- matrix(stats::rnorm(40, 0.5), 4)
  p <- vapply(1:4, function(i) stats::t.test(a[i, ], b[i, ], var.equal = TRUE)$p.value, numeric(1))
  expect_equal(heimdall:::.padd_ttest(a, b), p)
  expect_true(is.na(heimdall:::.padd_ttest(matrix(1, 1, 5), matrix(1, 1, 5))))
})

test_that("dfr_padd detects a shift in the stream and stays quiet without one", {
  set.seed(2)
  settings <- list(outputs = 12, replications = 12, sample_size = 50, alpha = 0.07, threshold = 0.19)
  quiet <- fit(do.call(dfr_padd, c(settings, seed = 1)), padd_stream(10, 99))
  output <- quiet$drifter_output
  expect_equal(names(output), "significant")
  ends <- which(!is.na(output$significant))
  expect_equal(ends, seq(400L, 2000L, by = 200L))
  expect_false(quiet$drifted)
  model <- fit(do.call(dfr_padd, c(settings, seed = 1)), padd_stream(10, 6))
  expect_true(model$drifted)
  fraction <- model$drifter_output$significant
  expect_true(fraction[1200] > 0.19)
  expect_true(all(fraction[c(400, 600, 800, 1000)] <= 0.19))
  expect_equal(dim(model$weights[[1]]), c(11L, 10L))
  expect_equal(dim(model$weights[[2]]), c(10L, 12L))
  # Same seed, same result; given weights are kept.
  again <- fit(do.call(dfr_padd, c(settings, seed = 1)), padd_stream(10, 6))
  expect_equal(again$weights, model$weights)
  fixed <- fit(do.call(dfr_padd, c(settings, seed = 3, list(weights = model$weights))), padd_stream(4, 99))
  expect_identical(fixed$weights, model$weights)
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_null(model$state$past)
  expect_error(dfr_padd(weights = list(1, 2)), "weights")
  expect_error(dfr_padd(sample_size = 1), "sample_size")
  expect_error(fit(dfr_padd(chunk_size = 50, weights = model$weights), padd_stream(1, 99, n_features = 4, chunk = 50)),
               "features")
})
