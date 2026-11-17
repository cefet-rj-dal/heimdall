test_that("online_bcp finds mean changes and summarises the segments", {
  set.seed(1)
  x <- c(stats::rnorm(80, 0), stats::rnorm(80, 4), stats::rnorm(80, 0))
  result <- online_bcp(x, theta = 0.95, threshold = 0.7)
  expect_s3_class(result, "online_bcp")
  expect_equal(nrow(result$changepoints), 2L)
  expect_true(all(abs(result$changepoints$location - c(80, 160)) <= 2))
  expect_equal(nrow(result$segments), 3L)
  expect_equal(result$segments$begin[1], 1)
  expect_equal(result$segments$end[3], 240)
  seg <- result$segments
  first <- x[seg$begin[1]:seg$end[1]]
  expect_equal(seg$mean[1], mean(first))
  expect_equal(seg$`UL of CI`[1] - seg$mean[1], stats::qnorm(0.95) * stats::sd(first) / sqrt(length(first)))
  expect_equal(dim(result$trace), c(240L, 3L))
  expect_output(print(result), "2 change point")
  with_test <- online_bcp(x, theta = 0.95, threshold = 0.7, normality = TRUE)
  expect_true("normality p-value" %in% names(with_test$segments))
  quiet <- online_bcp(stats::rnorm(150), theta = 0.95, threshold = 0.7)
  expect_equal(nrow(quiet$changepoints), 0L)
  expect_equal(nrow(quiet$segments), 1L)
  expect_error(online_bcp(c(1, NA, 3)), "missing")
  expect_error(online_bcp(1:10, theta = 2), "theta")
})

test_that("the stable and reference numerics agree on short segments", {
  set.seed(2)
  x <- c(stats::rnorm(60, 0), stats::rnorm(60, 3))
  stable <- online_bcp(x, theta = 0.95, threshold = 0.7)
  reference <- online_bcp(x, theta = 0.95, threshold = 0.7, numerics = "reference")
  expect_equal(stable$trace, reference$trace, tolerance = 1e-8)
  # The reference overflows gamma() on segments longer than about 340 points
  # (with a small variance, as in the aCGH data), which gives a change point at 341.
  long <- stats::rnorm(800, sd = 0.1)
  expect_true(341 %in% online_bcp(long, theta = 0.95, threshold = 0.7, numerics = "reference")$changepoints$location)
  expect_equal(nrow(online_bcp(long, theta = 0.95, threshold = 0.7)$changepoints), 0L)
})

test_that("dfr_onlinebcp reports each new change point once", {
  set.seed(3)
  x <- c(stats::rnorm(100, 0), stats::rnorm(100, 3), stats::rnorm(100, -2))
  model <- fit(dfr_onlinebcp(theta = 0.95, threshold = 0.7), data.frame(serie = x))
  expect_true(model$drifted)
  expect_equal(nrow(model$changepoints), 2L)
  expect_true(all(abs(model$changepoints$location - c(100, 200)) <= 2))
  expect_true(all(model$changepoints$detected_at > model$changepoints$location))
  expect_equal(names(model$drifter_output), c("max_prob", "location"))
  # The trace of the stream detector is that of online_bcp.
  batch <- online_bcp(x, theta = 0.95, threshold = 0.7)
  expect_equal(model$drifter_output$max_prob[-1], unname(batch$trace[-300, "max_prob"]))
  # max_run bounds the candidates and keeps the detections on short segments.
  bounded <- fit(dfr_onlinebcp(theta = 0.95, threshold = 0.7, max_run = 150), data.frame(serie = x))
  expect_equal(bounded$changepoints$location, model$changepoints$location)
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_equal(nrow(model$changepoints), 0L)
})
