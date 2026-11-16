eik_clusters <- function(n, right = 5) {
  rbind(cbind(stats::rnorm(n / 2), stats::rnorm(n / 2)), cbind(stats::rnorm(n / 2, right), stats::rnorm(n / 2)))
}

test_that("the chi-square test matches chisq.test and scipy's Yates correction", {
  table <- rbind(c(120, 80, 60, 40), c(25, 30, 20, 25))
  expected <- suppressWarnings(stats::chisq.test(table))
  result <- heimdall:::.eik_chisq(table)
  expect_equal(result$statistic, unname(expected$statistic))
  expect_equal(result$p_value, expected$p.value)
  expect_equal(result$df, 3L)
  two <- rbind(c(30, 20), c(12, 18))
  expect_equal(heimdall:::.eik_chisq(two)$p_value, stats::chisq.test(two, correct = TRUE)$p.value)
  # Bins empty in both samples are left out; a single bin cannot be tested.
  expect_equal(heimdall:::.eik_chisq(cbind(table, 0))$p_value, expected$p.value)
  expect_equal(heimdall:::.eik_chisq(rbind(5, 3))$p_value, 1)
})

test_that("the k-means step converges to the Lloyd solution", {
  set.seed(1)
  x <- eik_clusters(400)
  start <- x[c(1, 2, 399, 400), ]
  ours <- heimdall:::.eik_kmeans(x, start)
  theirs <- stats::kmeans(x, start, algorithm = "Lloyd", iter.max = 300)
  expect_equal(ours$centers[order(ours$centers[, 1]), ], unname(theirs$centers[order(theirs$centers[, 1]), ]),
               tolerance = 1e-3)
  expect_length(ours$labels, 400L)
  # An empty cluster takes the point farthest from its centre.
  empty <- heimdall:::.eik_kmeans(x, rbind(x[1, ], x[2, ], c(100, 100)))
  expect_equal(length(unique(empty$labels)), 3L)
})

test_that("the greedy initialization makes groups of equal size", {
  set.seed(2)
  x <- eik_clusters(200)
  first <- heimdall:::.eik_greedy(as.matrix(stats::dist(x)), 4)
  expect_length(first, 4L)
  expect_equal(length(unique(first)), 4L)
})

test_that("eikmeans builds bins with enough points and controls the false positives", {
  set.seed(3)
  training <- eik_clusters(2000)
  model <- eikmeans(training, n_test = 200)
  expect_s3_class(model, "eikmeans")
  expect_lte(nrow(model$centers), 40L)
  expect_equal(sum(model$counts), 2000)
  expect_gt(min(model$counts), 40)
  expect_output(print(model), "bins")
  stationary <- vapply(1:200, function(i) eikmeans_test(model, eik_clusters(200))$drift, logical(1))
  expect_lt(mean(stationary), 0.1)
  shifted <- vapply(1:50, function(i) eikmeans_test(model, eik_clusters(200, right = 6))$drift, logical(1))
  expect_gt(mean(shifted), 0.8)
  result <- eikmeans_test(model, eik_clusters(200))
  expect_equal(names(result), c("statistic", "df", "p_value", "drift"))
  # The number of bins is limited by the test size and the training size.
  expect_lte(nrow(eikmeans(training, n_test = 50)$centers), 10L)
  expect_lte(nrow(eikmeans(training[1:500, ], n_test = 200)$centers), 10L)
})

test_that("eikmeans validates its input", {
  set.seed(4)
  training <- eik_clusters(400)
  expect_error(eikmeans(training, n_test = 0), "n_test")
  expect_error(eikmeans(training, n_test = 200, alpha = 2), "alpha")
  expect_error(eikmeans(training[1:20, ], n_test = 200), "too small")
  expect_error(eikmeans(matrix("a", 10, 2), n_test = 200), "numeric")
  expect_error(eikmeans(rbind(training, NA), n_test = 200), "missing")
  model <- eikmeans(data.frame(a = training[, 1], b = training[, 2]), n_test = 100)
  expect_error(eikmeans_test(list(), training), "eikmeans object")
  expect_error(eikmeans_test(model, data.frame(a = 1)), "lacks the features")
  expect_equal(eikmeans_test(model, data.frame(b = training[1:100, 2], a = training[1:100, 1]))$p_value,
               eikmeans_test(model, training[1:100, ])$p_value)
})

test_that("dfr_eikmeans trains on the first rows and tests the next windows", {
  set.seed(5)
  cluster <- function(n, x) data.frame(x1 = stats::rnorm(n, x), x2 = stats::rnorm(n))
  stream <- rbind(rbind(cluster(1500, 0), cluster(1500, 5))[sample(3000), ],
                  rbind(cluster(500, 0), cluster(500, 6))[sample(1000), ])
  model <- fit(dfr_eikmeans(n_train = 2000, window_size = 200), stream)
  expect_s3_class(model$model, "eikmeans")
  expect_true(model$drifted)
  output <- model$drifter_output
  expect_equal(names(output), c("statistic", "p_value"))
  tested <- which(!is.na(output$p_value))
  expect_equal(tested, 2000 + 200 * (1:10))
  expect_lte(sum(output$p_value[tested[1:5]] < 0.05), 1)
  expect_gte(sum(output$p_value[tested[6:10]] < 0.05), 4)
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_null(model$model)
  expect_error(dfr_eikmeans(bogus = 1), "unknown arguments")
  expect_error(dfr_eikmeans(window_size = 0), "window_size")
})
