test_that("cosine_score is the average cosine similarity with the baseline", {
  set.seed(1)
  baseline <- matrix(stats::rnorm(300, mean = 0.5), ncol = 6)
  x <- matrix(stats::rnorm(24), ncol = 6)
  direct <- apply(x, 1, function(v) {
    mean(apply(baseline, 1, function(e) sum(v * e) / sqrt(sum(v^2) * sum(e^2))))
  })
  expect_equal(cosine_score(baseline, x), direct)
  expect_equal(cosine_score(baseline, x[1, ]), direct[1])
  expect_equal(cosine_score(as.data.frame(baseline), as.data.frame(x)), direct)
  # Scale invariance; zero vectors.
  expect_equal(cosine_score(baseline * 3, x * 2), direct)
  expect_true(is.na(cosine_score(baseline, rep(0, 6))))
  expect_equal(cosine_score(rbind(baseline, 0), x), direct)
  expect_equal(cosine_score(baseline, baseline[1, ]), mean(apply(baseline, 1, function(e) {
    sum(baseline[1, ] * e) / sqrt(sum(baseline[1, ]^2) * sum(e^2))
  })))
  expect_error(cosine_score(baseline, x[, 1:3]), "columns")
  expect_error(cosine_score(matrix(0, 2, 2), c(1, 1)), "positive norm")
  expect_error(cosine_score(baseline, c(NA, 1:5)), "missing")
})

test_that("dfr_cosine averages the scores over windows and flags low averages", {
  set.seed(2)
  baseline <- matrix(stats::rnorm(1000, mean = 1), ncol = 10)
  near <- matrix(stats::rnorm(200, mean = 1), ncol = 10)
  far <- -matrix(abs(stats::rnorm(200, mean = 1)), ncol = 10)
  stream <- as.data.frame(rbind(near, far))
  model <- fit(dfr_cosine(reference = baseline, window_size = 5, threshold = 0.5), stream)
  output <- model$drifter_output
  expect_equal(names(output), c("score", "window_score"))
  expect_equal(output$score, cosine_score(baseline, as.matrix(stream)))
  ends <- which(!is.na(output$window_score))
  expect_equal(ends, seq(5L, 40L, by = 5L))
  expect_equal(output$window_score[5], mean(output$score[1:5]))
  expect_true(all(output$window_score[ends[ends <= 20]] > 0.5))
  expect_true(all(output$window_score[ends[ends > 20]] < 0.5))
  expect_true(model$drifted)
  # Per-observation checks (window_size = 1) as the reference code.
  single <- fit(dfr_cosine(reference = baseline, threshold = 0.5), stream)
  expect_equal(single$drifter_output$window_score, single$drifter_output$score)
  # Baseline from the stream, and reset_state.
  from_stream <- fit(dfr_cosine(n_reference = 100, window_size = 5),
                     as.data.frame(rbind(baseline, near, far)))
  expect_true(all(is.na(from_stream$drifter_output$score[1:100])))
  expect_equal(from_stream$drifter_output$score[101:140], output$score)
  from_stream <- reset_state(from_stream)
  expect_null(from_stream$direction)
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_false(is.null(model$direction))
  expect_error(dfr_cosine(window_size = 0), "window_size")
  expect_error(fit(dfr_cosine(reference = baseline), as.data.frame(near[, 1:5])), "dimensions")
})
