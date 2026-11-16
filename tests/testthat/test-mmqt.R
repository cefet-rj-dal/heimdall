mmqt_mode <- function(n, center, d = 2) matrix(stats::rnorm(d * n, center), ncol = d)

test_that("the QuantTree partitioning puts the target share of training points in every bin", {
  set.seed(1)
  points <- matrix(stats::rnorm(3000), ncol = 3)
  tree <- heimdall:::.qt_build(points, rep(1 / 8, 8))
  expect_s3_class(tree, "mmqt_partition")
  expect_equal(tabulate(heimdall:::.qt_find_bin(tree, points), 8), rep(125, 8))
  # Replaying the random choices gives the same partitioning.
  again <- heimdall:::.qt_build(points, rep(1 / 8, 8), dims = tree$dim, sides = tree$side)
  expect_equal(again$lower, tree$lower)
  expect_equal(again$upper, tree$upper)
  # Lower-tail bins hold (lower, upper], upper-tail bins [lower, upper).
  sides <- heimdall:::.qt_build(matrix(c(1, 2, 3, 4), ncol = 1), c(0.5, 0.5), dims = c(1, 1), sides = c(-1, 0))
  expect_equal(heimdall:::.qt_find_bin(sides, matrix(c(1, 2, 2.5, 4), ncol = 1)), c(1, 1, 2, 2))
  upper <- heimdall:::.qt_build(matrix(c(1, 2, 3, 4), ncol = 1), c(0.5, 0.5), dims = c(1, 1), sides = c(1, 0))
  expect_equal(heimdall:::.qt_find_bin(upper, matrix(c(1, 2.5, 3, 4), ncol = 1)), c(2, 2, 1, 1))
})

test_that("the thresholds match the pre-computed table of the reference code", {
  # all_distr_quanttree.pkl: total variation, K = 32, N = 4096, nu = 64.
  set.seed(2)
  threshold <- heimdall:::.qt_threshold(rep(1 / 32, 32), 4096, 64, 0.05, 20000) * 64
  expect_lte(abs(threshold - 21), 1)
  lower <- heimdall:::.qt_threshold(rep(1 / 32, 32), 4096, 64, 0.2, 20000)
  expect_lt(lower, threshold / 64)
})

test_that("the calibration follows Eqs. 12 and 13", {
  # Two interleaved modalities on a line, one split at 4 (a point of B).
  a <- matrix(c(1, 3, 5, 7), ncol = 1)
  b <- matrix(c(2, 4, 6, 8), ncol = 1)
  tree <- heimdall:::.qt_build(rbind(a, b), c(0.5, 0.5), dims = c(1, 1), sides = c(-1, 0))
  expect_equal(tree$upper[1], 4)
  # A: last point inside 3, first outside 5, so g = (4 - 3) / (5 - 3) = 0.5.
  expect_equal(heimdall:::.mmqt_calibrate(tree, a, heimdall:::.qt_find_bin(tree, a)), c(2.5 / 5, 1 - 2.5 / 5))
  # B: the boundary is its own point, g = 0.
  expect_equal(heimdall:::.mmqt_calibrate(tree, b, heimdall:::.qt_find_bin(tree, b)), c(2 / 5, 3 / 5))
  # Upper-tail bin, mirrored.
  upper <- heimdall:::.qt_build(rbind(a, b), c(0.5, 0.5), dims = c(1, 1), sides = c(1, 0))
  expect_equal(upper$lower[1], 5)
  expect_equal(heimdall:::.mmqt_calibrate(upper, b, heimdall:::.qt_find_bin(upper, b)), c(2.5 / 5, 1 - 2.5 / 5))
})

test_that("mmqt finds the modalities and controls the false positives", {
  set.seed(3)
  training <- c(lapply(1:40, function(i) mmqt_mode(64, 0)), lapply(1:40, function(i) mmqt_mode(64, 4)))
  model <- mmqt(training, bins = 8, n_repeats = 3, n_sim = 4000, seed = 1)
  expect_s3_class(model, "mmqt")
  expect_equal(model$n_modalities, 2)
  expect_equal(sort(model$n_batches), c(40, 40))
  expect_equal(nrow(model$estimation), 3L)
  expect_lte(model$training_fpr, 0.05)
  expect_equal(rowSums(model$reference), c(1, 1))
  expect_output(print(model), "2 modalities")

  stationary <- c(lapply(1:150, function(i) mmqt_mode(64, 0)), lapply(1:150, function(i) mmqt_mode(64, 4)))
  result <- mmqt_test(model, stationary)
  expect_equal(names(result), c("modality", "statistic", "threshold", "drift"))
  expect_lt(mean(result$drift), 0.12)
  # Each batch goes to the modality it was drawn from.
  expect_equal(length(unique(result$modality[1:150])), 1L)
  expect_false(result$modality[1] == result$modality[151])
  expect_gt(mean(mmqt_test(model, lapply(1:50, function(i) mmqt_mode(64, 1.5)))$drift), 0.9)

  # A single QuantTree treats batches from one modality as changes.
  single <- mmqt(training, bins = 8, n_modalities = 1, calibration = FALSE, partition = model$partition, n_sim = 4000)
  expect_gt(mean(mmqt_test(single, stationary)$drift), 0.9)
  expect_identical(single$partition, model$partition)
  expect_null(single$estimation)
})

test_that("mmqt takes a matrix or data frame cut into batches and is reproducible", {
  set.seed(4)
  data <- data.frame(x = stats::rnorm(64 * 30), y = stats::rnorm(64 * 30))
  had_seed <- exists(".Random.seed", envir = globalenv())
  before <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  first <- mmqt(data, nu = 64, bins = 4, n_repeats = 2, n_sim = 1000, seed = 5)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  second <- mmqt(as.matrix(data), nu = 64, bins = 4, n_repeats = 2, n_sim = 1000, seed = 5)
  expect_equal(first$thresholds, second$thresholds)
  expect_equal(first$reference, second$reference)
  expect_equal(first$nu, 64)
  result <- mmqt_test(first, data[1:64, c("y", "x")])
  expect_equal(nrow(result), 1L)
})

test_that("mmqt validates its input", {
  set.seed(6)
  batches <- lapply(1:4, function(i) mmqt_mode(16, 0))
  expect_error(mmqt(batches, bins = 1), "bins")
  expect_error(mmqt(batches, alpha = 2), "alpha")
  expect_error(mmqt(batches[1], bins = 2), "at least two batches")
  expect_error(mmqt(c(batches, list(mmqt_mode(10, 0))), bins = 2), "same number of rows")
  expect_error(mmqt(matrix(1, 10, 2), bins = 2), "nu is required")
  expect_error(mmqt(matrix(1, 10, 2), nu = 20, bins = 2), "fewer than nu")
  expect_error(mmqt(list(matrix("a", 2, 2), matrix("b", 2, 2))), "numeric")
  expect_error(mmqt(batches, bins = 4, n_modalities = 10, n_sim = 1000), "cannot be split")
  expect_error(mmqt(batches, partition = list()), "partition")
  model <- mmqt(batches, bins = 4, n_modalities = 1, n_sim = 1000)
  expect_error(mmqt_test(list(), batches), "mmqt object")
  expect_error(mmqt_test(model, mmqt_mode(8, 0)), "nu = 16")
  expect_error(mmqt_test(model, mmqt_mode(16, 0, d = 3)), "number of features")
})

test_that("dfr_mmqt trains on the first batches and tests the next ones", {
  set.seed(7)
  batch <- function(center) data.frame(x = stats::rnorm(32, center), y = stats::rnorm(32, center))
  centers <- c(rep(c(0, 4), 20), rep(c(0, 4), 5), rep(c(2, 4), 5))
  stream <- do.call(rbind, lapply(centers, batch))
  model <- fit(dfr_mmqt(nu = 32, n_train_batches = 40, bins = 4, n_repeats = 2, n_sim = 2000, seed = 1), stream)
  expect_s3_class(model$model, "mmqt")
  expect_equal(model$model$n_modalities, 2)
  expect_true(model$drifted)
  output <- model$drifter_output
  expect_equal(names(output), c("modality", "statistic", "threshold"))
  tested <- which(!is.na(output$statistic))
  expect_equal(tested, 32 * (41:60))
  alarms <- (output$statistic > output$threshold)[tested]
  # Batches 41-50 are stationary, the odd batches of 51-60 changed.
  expect_lte(sum(alarms[1:10]), 1)
  expect_true(all(alarms[seq(11, 20, by = 2)]))

  model <- reset_state(model)
  expect_false(model$drifted)
  expect_null(model$model)

  given <- fit(dfr_mmqt(model = mmqt(lapply(1:20, function(i) as.matrix(batch(0))), bins = 4, n_modalities = 1,
                                     n_sim = 1000)), stream[1:64, ])
  expect_equal(sum(!is.na(given$drifter_output$statistic)), 2L)
  expect_s3_class(reset_state(given)$model, "mmqt")
  expect_error(dfr_mmqt(model = list()), "mmqt object")
  expect_error(dfr_mmqt(bogus = 1), "unknown arguments")
  expect_error(dfr_mmqt(nu = 0), "nu")
})
