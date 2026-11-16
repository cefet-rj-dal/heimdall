# Two groups of three correlated features: "a" and "b".
dde_groups <- function(n, shift_a = 0, shift_b = 0) {
  a <- stats::rnorm(n, shift_a)
  b <- stats::rnorm(n, shift_b)
  data.frame(
    a1 = a + stats::rnorm(n, sd = 0.3), a2 = a + stats::rnorm(n, sd = 0.3), a3 = a + stats::rnorm(n, sd = 0.3),
    b1 = b + stats::rnorm(n, sd = 0.3), b2 = b + stats::rnorm(n, sd = 0.3), b3 = b + stats::rnorm(n, sd = 0.3)
  )
}

test_that("dde_curve counts the pairs of Eqs. 14 and 15", {
  set.seed(1)
  data <- dde_groups(500)
  curve <- dde_curve(data, c("a1", "a2", "b1"), thresholds = c(0, 0.5))
  # z = 0: every pair; m = 6 features, k = 3 drifting.
  expect_equal(curve$pairs[1], choose(6, 2))
  expect_equal(curve$drifting_pairs[1], choose(3, 2))
  # z = 0.5: only the pairs inside each group (3 + 3), of which a1-a2 drifts.
  expect_equal(curve$pairs[2], 6)
  expect_equal(curve$drifting_pairs[2], 1)
  # Eq. 20: r_n/N = (m / k) b / B; its value at 0 is (k - 1) / (m - 1).
  expect_equal(curve$probability, (6 / 3) * curve$drifting_pairs / curve$pairs)
  expect_equal(curve$probability[1], (3 - 1) / (6 - 1))
  expect_equal(curve$lift, curve$probability / curve$probability[1])
})

test_that("dde_curve reproduces Table 3 of the paper from its pair counts", {
  # Insects, first drift: m = 200 features and k = 49 drifting ones
  # (1176 = 49 * 48 / 2 drifting pairs at z = 0).
  m <- 200
  k <- 49
  pairs <- c(19900, 13755, 8401, 5532, 3332, 2158, 1501, 1105, 799, 491)
  drifting_pairs <- c(1176, 1078, 722, 581, 446, 354, 295, 240, 184, 118)
  probability <- (m / k) * drifting_pairs / pairs
  expect_equal(round(probability, 2), c(0.24, 0.32, 0.35, 0.43, 0.55, 0.67, 0.80, 0.89, 0.94, 0.98))
  expect_equal(round(probability / ((k - 1) / (m - 1)), 2), c(1.00, 1.33, 1.45, 1.78, 2.27, 2.78, 3.33, 3.68, 3.90, 4.07))
})

test_that("the lift grows when correlated features drift together and not otherwise", {
  set.seed(2)
  data <- dde_groups(500)
  together <- dde_curve(data, c("a1", "a2", "a3"))
  expect_equal(together$lift[1], 1)
  expect_equal(utils::tail(stats::na.omit(together$lift), 1), 2.5) # every strong pair drifts: 1 / (2 / 5)
  # One feature of each group: no correlated pair drifts together.
  apart <- dde_curve(data, c("a1", "b1"))
  expect_equal(apart$probability[apart$threshold >= 0.5 & !is.na(apart$probability)],
               rep(0, sum(apart$threshold >= 0.5 & !is.na(apart$probability))))
})

test_that("dde_curve validates its input", {
  set.seed(3)
  data <- dde_groups(50)
  expect_error(dde_curve(data, "a1"), "two drifting")
  expect_error(dde_curve(data, c("a1", "zz")), "not in the data")
  expect_error(dde_curve(data, c(TRUE, FALSE)), "one value per feature")
  expect_error(dde_curve(data, c("a1", "a2"), thresholds = c(0.5, 0.1)), "increasing")
  expect_error(dde_curve(data["a1"], c("a1", "a1")), "at least two features")
  logical_curve <- dde_curve(data, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(logical_curve, dde_curve(data, c("a1", "a2")))
  # A constant feature is treated as uncorrelated.
  data$a3 <- 1
  expect_silent(dde_curve(data, c("a1", "a2", "a3")))
})

test_that("dde_score is the mean of the curve over the correlations (Eq. 25)", {
  data <- data.frame(x = c(1, 2, 3, 4, 5), y = c(1, 2, 3, 5, 4), w = c(5, 1, 4, 2, 3), v = c(2, 1, 4, 3, 5))
  curve <- data.frame(threshold = c(0, 0.55, 0.85), probability = c(0.1, 0.4, 0.9))
  correlation <- abs(stats::cor(data))
  level <- function(r) curve$probability[findInterval(r, curve$threshold)]
  scores <- dde_score(curve, data, drifting = c("x", "w"), unmonitored = c("y", "v"))
  expect_equal(unname(scores["y"]), mean(c(level(correlation["y", "x"]), level(correlation["y", "w"]))))
  expect_equal(unname(scores["v"]), mean(c(level(correlation["v", "x"]), level(correlation["v", "w"]))))
  expect_equal(names(scores), c("y", "v"))
  # No drifting monitored feature: every score is 0.
  expect_equal(unname(dde_score(curve, data, character(0), c("y", "v"))), c(0, 0))
  # Thresholds without pairs carry the previous probability; scores stay in [0, 1].
  gaps <- data.frame(threshold = c(0, 0.55, 0.85), probability = c(0.2, NA, 1.4))
  gap_scores <- dde_score(gaps, data, drifting = "x", unmonitored = c("y", "w"))
  expect_true(all(gap_scores >= 0 & gap_scores <= 1))
  expect_equal(unname(gap_scores["y"]), 1) # |cor(x, y)| = 0.9
  expect_error(dde_score(data.frame(a = 1), data, "x", "y"), "DDE curve")
  expect_error(dde_score(curve, data, "x", "zz"), "not in the data")
})

test_that("dde_feature_drift matches the Evidently and scipy definitions", {
  x <- c(0.1, 0.5, 0.9, 1.3, 2.2, 2.4, 3.1, 3.3, 4.0, 0.7, 1.1, 1.9)
  y <- c(1.5, 2.5, 2.6, 3.0, 3.8, 4.4, 5.0, 5.2, 2.9, 3.6)
  reference <- data.frame(f = x[1:10])
  current <- data.frame(f = y)
  # Reference values from scipy (distance.jensenshannon on numpy.histogram
  # with Sturges bins; stats.wasserstein_distance / numpy.std).
  js <- heimdall:::.dde_js_distance(x, y)
  expect_equal(js, 0.48720876237884175, tolerance = 1e-12)
  wasserstein <- heimdall:::.dde_wasserstein(x, y) / heimdall:::.dde_population_sd(x)
  expect_equal(wasserstein, 1.4078818266458624, tolerance = 1e-12)

  set.seed(4)
  reference <- data.frame(same = stats::rnorm(400), shifted = stats::rnorm(400), constant = 1)
  current <- data.frame(same = stats::rnorm(400), shifted = stats::rnorm(400, 2), constant = 1)
  for (test in c("js", "ks", "wasserstein")) {
    result <- dde_feature_drift(reference, current, test = test)
    expect_equal(result$feature, c("same", "shifted", "constant"))
    expect_equal(result$drift, c(FALSE, TRUE, FALSE))
  }
  ks <- dde_feature_drift(reference, current, test = "ks")
  expect_equal(ks$statistic[2], stats::ks.test(reference$shifted, current$shifted)$p.value)
  strict <- dde_feature_drift(reference, current, test = "js", threshold = 0.99)
  expect_false(any(strict$drift))
  expect_error(dde_feature_drift(reference, current[1:2]), "same columns")
  short <- dde_feature_drift(data.frame(a = c(1, NA)), data.frame(a = c(1, 2)))
  expect_true(is.na(short$statistic))
  expect_false(short$drift)
})

test_that("dfr_dde fits the curve on the first drift and predicts the unmonitored features", {
  set.seed(5)
  stream <- rbind(dde_groups(600), dde_groups(600, shift_a = 3), dde_groups(600, shift_a = 3), dde_groups(600, shift_a = 6))
  model <- dfr_dde(monitored = c("a1", "b1"), test = "ks", threshold = 1e-3, window_size = 300, monitoring_step = 50)
  model <- fit(model, stream)
  expect_true(model$drifted)
  expect_gte(length(model$detections), 2L)
  # Initialization: every feature tested, group "a" drifts.
  first <- model$detections[[1]]
  expect_true(first$initialization)
  expect_setequal(first$drifting, c("a1", "a2", "a3"))
  expect_equal(model$curve$lift[1], 1)
  expect_gt(max(model$curve$lift, na.rm = TRUE), 2)
  # Prediction: a1 drifts, so a2 and a3 get high scores and b2, b3 low ones.
  last <- utils::tail(model$detections, 1)[[1]]
  expect_false(last$initialization)
  expect_equal(last$drifting, "a1")
  expect_gt(min(last$scores[c("a2", "a3")]), 0.5)
  expect_lt(max(last$scores[c("b2", "b3")]), 0.5)
  expect_equal(names(model$feature_drift), names(stream))
  expect_equal(unname(model$predicted_drift), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(names(model$drifter_output), "drifting")
  expect_equal(nrow(model$drifter_output), nrow(stream))

  model <- reset_state(model)
  expect_false(model$drifted)
  expect_null(model$curve)
  expect_length(model$detections, 0L)
})

test_that("dfr_dde draws the monitored features and supports ADWIN", {
  set.seed(6)
  stream <- rbind(dde_groups(400), dde_groups(400, shift_a = 4))
  model <- fit(dfr_dde(n_monitored = 2, window_size = 200, monitoring_step = 50, seed = 1), stream)
  expect_length(model$state$monitored, 2L)
  again <- fit(dfr_dde(n_monitored = 2, window_size = 200, monitoring_step = 50, seed = 1), stream)
  expect_equal(again$state$monitored, model$state$monitored)

  adwin <- fit(dfr_dde(test = "adwin", window_size = 100), stream)
  expect_true(adwin$drifted)
  expect_true(all(adwin$detections[[1]]$drifting %in% c("a1", "a2", "a3")))

  # Every feature monitored: no score is predicted.
  full <- fit(dfr_dde(window_size = 200, monitoring_step = 50), stream)
  expect_true(full$drifted)
  expect_true(all(full$feature_drift %in% c(0, 1)))
})

test_that("dfr_dde validates its arguments and input", {
  expect_error(dfr_dde(test = "chi"))
  expect_error(dfr_dde(window_size = 1), "window_size")
  expect_error(dfr_dde(monitored = 1), "monitored")
  expect_error(dfr_dde(score_threshold = 2), "score_threshold")
  expect_error(dfr_dde(update_curve = NA), "update_curve")
  expect_error(dfr_dde(thresholds = c(0, 2)), "thresholds")
  expect_error(update_state(dfr_dde(monitored = "zz"), data.frame(a = 1, b = 2)), "not in the data")
  expect_error(update_state(dfr_dde(n_monitored = 3), data.frame(a = 1, b = 2)), "larger")
  expect_error(update_state(dfr_dde(), data.frame(a = 1)), "two features")
  model <- update_state(dfr_dde(), data.frame(a = 1, b = 2))$obj
  expect_error(update_state(model, data.frame(a = 1, c = 2)), "features changed")
})
