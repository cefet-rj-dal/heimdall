toy_source <- function(...) {
  dsg_source(
    samplers = list(
      function(n) data.frame(x = stats::runif(n, 0, 1)),
      function(n) data.frame(x = stats::runif(n, 3, 4))
    ),
    labelers = list(
      function(x) as.integer(x$x > 0.5),
      function(x) as.integer(x$x <= 0.5),
      function(x) as.integer(x$x > 3.5)
    ),
    test_size = 200, n_perm = 200, ...
  )
}

test_that("the concepts combine every feature distribution with every labelling rule", {
  set.seed(1)
  source <- toy_source()
  expect_equal(nrow(source$states), 6L)
  expect_equal(source$states$virtual, rep(1:2, each = 3))
  expect_equal(source$states$real, rep(1:3, times = 2))
})

test_that("dsg_take draws from the current concept", {
  set.seed(2)
  source <- toy_source()
  source$state <- 5L # virtual 2, real 2
  drawn <- dsg_take(source, 50)
  expect_equal(nrow(drawn$x), 50L)
  expect_true(all(drawn$x$x >= 3))
  expect_equal(drawn$y, as.integer(drawn$x$x <= 0.5))
})

test_that("dsg_take keeps calling a sampler that returns few rows", {
  set.seed(3)
  source <- dsg_source(
    samplers = list(function(n) data.frame(x = stats::runif(3))),
    labelers = list(function(x) rep(1L, nrow(x)))
  )
  expect_equal(nrow(dsg_take(source, 10)$x), 10L)
})

test_that("each drift type changes exactly what it should", {
  set.seed(4)
  source <- toy_source()
  for (type in c("v", "r", "c", "a")) {
    for (i in 1:10) {
      before <- source$states[source$state, ]
      drift <- dsg_drift(source, type)
      after <- drift$source$states[drift$source$state, ]
      expect_equal(drift$virtual, after$virtual != before$virtual)
      expect_equal(drift$real, after$real != before$real)
      if (type == "v") expect_true(drift$virtual && !drift$real)
      if (type == "r") expect_true(drift$real && !drift$virtual)
      if (type == "c") expect_true(drift$real && drift$virtual)
      if (type == "a") expect_true(drift$real || drift$virtual)
      source <- drift$source
    }
  }
  unchanged <- dsg_drift(source, "-")
  expect_equal(unchanged$source$state, source$state)
  expect_false(unchanged$virtual || unchanged$real)
})

test_that("real drift is verified by the share of changed labels", {
  set.seed(5)
  # Rules 1 and 3 agree on the first feature distribution (x in [0, 1]):
  # only rule 2 is a noticeable real drift from rule 1 there.
  source <- dsg_source(
    samplers = list(function(n) data.frame(x = stats::runif(n))),
    labelers = list(
      function(x) as.integer(x$x > 0.5),
      function(x) as.integer(x$x <= 0.5),
      function(x) as.integer(x$x > 0.5 & x$x < 2)
    ),
    test_size = 200
  )
  for (i in 1:10) {
    source$state <- 1L
    drift <- dsg_drift(source, "r")
    expect_equal(drift$source$state, 2L)
    expect_equal(drift$label_change, 1)
  }
})

test_that("an undetectable virtual drift is rejected", {
  set.seed(6)
  same <- function(n) data.frame(x = stats::runif(n))
  source <- dsg_source(list(same, same), list(function(x) rep(0L, nrow(x))), test_size = 100, n_perm = 200, max_tries = 3)
  expect_error(dsg_drift(source, "v"), "unable to create a verified drift")
})

test_that("a weak real drift falls back to the strongest one with a warning", {
  set.seed(7)
  source <- dsg_source(
    samplers = list(function(n) data.frame(x = stats::runif(n))),
    labelers = list(function(x) as.integer(x$x > 0.5), function(x) as.integer(x$x > 0.502)),
    test_size = 500, max_tries = 5, real_drift_level = 0.5
  )
  source$state <- 1L
  expect_warning(drift <- dsg_drift(source, "r"), "strongest real drift")
  expect_equal(drift$source$state, 2L)
  expect_lt(drift$label_change, 0.5)
})

test_that("the two-sample test separates different distributions", {
  set.seed(8)
  same <- dsg_mmd_test(matrix(stats::rnorm(300), 150), matrix(stats::rnorm(300), 150), n_perm = 300)
  shifted <- dsg_mmd_test(matrix(stats::rnorm(300), 150), matrix(stats::rnorm(300, 0.8), 150), n_perm = 300)
  expect_gt(same$p_value, 0.01)
  expect_equal(shifted$p_value, 0)
  expect_gt(shifted$statistic, same$statistic)
})

test_that("the random-trees kernel is a valid similarity", {
  set.seed(9)
  x <- matrix(stats::runif(60), 30)
  leaves <- heimdall:::.dsg_random_tree_leaves(x, n_trees = 20, max_depth = 3)
  expect_equal(dim(leaves), c(30L, 20L))
  # Nodes of a depth-3 tree are numbered 1 to 15; a node with a single point
  # stops splitting, so leaves can sit above the last level.
  expect_true(all(leaves >= 1 & leaves <= 15))
  expect_gt(mean(leaves >= 8), 0.5)
  # Identical points always share their leaves.
  twins <- heimdall:::.dsg_random_tree_leaves(rbind(x, x[1, ]), n_trees = 20, max_depth = 3)
  expect_equal(twins[1, ], twins[31, ])
})

test_that("dsg_stream builds the segments with their drift codes", {
  set.seed(10)
  stream <- dsg_stream(toy_source(), "--v_rc", batch_size = 40)
  expect_equal(nrow(stream$data), 240L)
  expect_equal(stream$segments$drift, c(0L, 0L, 1L, 0L, 2L, 3L))
  expect_equal(stream$data$drift, rep(c(0L, 0L, 1L, 0L, 2L, 3L), each = 40))
  expect_equal(stream$data$segment, rep(1:6, each = 40))
  expect_true(all(c("x", "y") %in% names(stream$data)))
  changes <- diff(stream$segments$virtual_state) != 0
  expect_equal(changes, stream$segments$drift[-1] %in% c(1L, 3L))
  expect_error(dsg_stream(toy_source(), "x"), "pattern")
})

test_that("block boundaries follow the reference implementation", {
  expect_equal(heimdall:::.dsg_boundaries(2, 90), c(30L, 60L))
  expect_equal(heimdall:::.dsg_boundaries(c(0.25, 0.5), 100), c(25L, 50L))
  expect_equal(heimdall:::.dsg_boundaries(c(10, 70), 100), c(10L, 70L))
  expect_equal(heimdall:::.dsg_block_ids(7, c(2L, 5L)), c(0L, 0L, 1L, 1L, 1L, 2L, 2L))
  expect_error(heimdall:::.dsg_boundaries(c(0.5, 0.5), 10), "increasing")
})

test_that("label flips permute the classes inside the selected regions", {
  set.seed(11)
  x <- matrix(stats::runif(400), 200)
  y <- sample(c("a", "b", "c"), 200, replace = TRUE)
  block <- rep(0:1, each = 100)
  flips <- heimdall:::.dsg_add_flips(x, y, block, n_clusters = 12, n_ignore = 2)
  expect_true(any(flips$flipped))
  expect_true(all(flips$y[flips$flipped] != y[flips$flipped]))
  expect_equal(flips$y[!flips$flipped], y[!flips$flipped])
})

test_that("a source from a real stream separates virtual from real drift", {
  set.seed(12)
  n <- 900
  # The feature distribution changes at row 300 and the concept at row 600.
  x1 <- c(stats::rnorm(300), stats::rnorm(600, mean = 3))
  x2 <- stats::rnorm(n)
  y <- c(as.integer(x2[1:600] > 0), as.integer(x2[601:900] <= 0))
  # Exact class probabilities from the generating rule.
  model <- function(x, y) {
    function(newx) {
      p <- ifelse(newx$block >= 2, as.numeric(newx$x2 <= 0), as.numeric(newx$x2 > 0))
      cbind(`0` = 1 - p, `1` = p)
    }
  }
  source <- dsg_source_data(data.frame(x1, x2, y), "y", real_blocks = 2, virtual_blocks = 2, model = model, test_size = 300, n_perm = 300)
  expect_equal(source$virtual_boundaries, c(300L, 600L))
  expect_equal(nrow(source$states), 9L)

  # Segment 1 of the features comes from the first 300 rows.
  source$state <- which(source$states$virtual == 1 & source$states$real == 1)
  drawn <- dsg_take(source, 200)
  expect_lt(mean(drawn$x$x1), 1)
  expect_equal(drawn$y, as.integer(drawn$x$x2 > 0))

  # Labels of the third time block follow the flipped rule on any features.
  source$state <- which(source$states$virtual == 1 & source$states$real == 3)
  drawn <- dsg_take(source, 200)
  expect_equal(drawn$y, as.integer(drawn$x$x2 <= 0))

  real <- dsg_drift(source, "r")
  expect_equal(real$source$states$virtual[real$source$state], 1L)
  expect_gt(real$label_change, 0.9)
})

test_that("the default model of dsg_source_data reports its quality", {
  skip_if_not_installed("randomForest")
  set.seed(13)
  n <- 600
  x <- stats::rnorm(n)
  y <- ifelse(seq_len(n) <= 300, x > 0, x < 0)
  source <- dsg_source_data(data.frame(x, y = as.integer(y)), "y", real_blocks = 1, virtual_blocks = 1, test_size = 200, n_perm = 200)
  expect_gt(source$quality$oob_accuracy, 0.8)
  expect_gt(source$quality$block_importance, 0)
  drawn <- dsg_take(source, 20)
  expect_true(all(drawn$y %in% c(0L, 1L)))

  # A small node budget grows the forest in chunks of trees (600 points with
  # nodesize 10 need 201 nodes per tree, so chunks of 3 trees here).
  model <- heimdall:::.dsg_default_model(nodesize = 10, ntree = 10, node_budget = 700)
  block <- as.integer(seq_len(n) > 300)
  predict_fn <- model(data.frame(block = block, x = x), as.integer(y))
  probabilities <- predict_fn(data.frame(block = c(0L, 1L), x = c(2, 2)))
  expect_equal(dim(probabilities), c(2L, 2L))
  expect_equal(unname(rowSums(probabilities)), c(1, 1))
  expect_gt(probabilities[1, "1"], 0.5)
  expect_lt(probabilities[2, "1"], 0.5)
  quality <- attr(predict_fn, "quality")
  expect_gt(quality$oob_accuracy, 0.6) # out-of-bag votes of 3 trees only
  expect_gt(quality$block_importance, 0)
})

test_that("the synthetic sources reproduce the river concept functions", {
  set.seed(14)
  sea <- dsg_sea(test_size = 200, n_perm = 200)
  expect_equal(nrow(sea$states), 8L)
  sea$state <- which(sea$states$real == 4)[1]
  drawn <- dsg_take(sea, 100)
  expect_equal(drawn$y, as.integer(drawn$x$x1 + drawn$x$x2 > 9.5))

  agrawal <- dsg_agrawal(test_size = 200, n_perm = 200)
  expect_equal(length(agrawal$labelers), 9L)
  x <- data.frame(salary = 60000, commission = 0, age = c(30, 50, 70), elevel = 1, car = 1, zipcode = 0, hvalue = 1e5, hyears = 10, loan = 1e5)
  expect_equal(agrawal$labelers[[1]](x), c(1L, 0L, 1L))
  expect_equal(agrawal$labelers[[2]](x), c(1L, 0L, 1L))
  expect_equal(agrawal$labelers[[3]](x), c(1L, 1L, 0L))

  sine <- dsg_sine(test_size = 200, n_perm = 200)
  x <- data.frame(x1 = c(0.1, 0.9), x2 = c(0.5, 0.5))
  expect_equal(sine$labelers[[1]](x), c(1L, 0L))
  expect_equal(sine$labelers[[2]](x), c(0L, 1L))

  mixed <- dsg_mixed(test_size = 200, n_perm = 200)
  x <- data.frame(x1 = c(1, 0), x2 = c(1, 0), x3 = 0.5, x4 = 0.99)
  expect_equal(mixed$labelers[[2]](x), c(1L, 0L))
  expect_equal(mixed$labelers[[1]](x), c(0L, 1L))
  expect_false(any(c("y", "segment", "drift") %in% names(dsg_take(mixed, 5)$x)))
})

test_that("dsg_stream rejects features named like the stream columns", {
  source <- dsg_source(
    samplers = list(function(n) data.frame(y = stats::runif(n))),
    labelers = list(function(x) as.integer(x$y > 0.5))
  )
  expect_error(dsg_stream(source, "--"), "clash")
})

test_that("regional sub-sampling gives disjoint feature regions", {
  set.seed(15)
  source <- dsg_sine(n_variants = 2, test_size = 200, n_perm = 200)
  regions <- lapply(1:2, function(v) {
    source$state <- which(source$states$virtual == v)[1]
    x <- dsg_take(source, 300)$x
    distances <- outer(rowSums(as.matrix(x)^2), rowSums(source$centers^2), "+") - 2 * as.matrix(x) %*% t(source$centers)
    unique((max.col(-distances, ties.method = "first") - 1L) %% 2L)
  })
  expect_equal(regions, list(0L, 1L))
  drift <- dsg_drift(source, "v")
  expect_lte(drift$p_value, 0.001)
})

test_that("the constructors validate their inputs", {
  expect_error(dsg_source(list(), list(function(x) 1)), "samplers")
  expect_error(dsg_source(list(function(n) 1), list(1)), "labelers")
  expect_error(dsg_drift(toy_source(), "z"), "type")
  expect_error(dsg_source_data(data.frame(a = "x", y = 1), "y"), "numeric")
  expect_error(dsg_source_data(data.frame(a = 1, y = 1), "z"), "target")
})
