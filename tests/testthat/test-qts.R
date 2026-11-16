# Number of leaves of the quadtree built point by point as in the reference
# code (QTree_plus.Node.insert): a node splits when it holds more than one
# point, except at the maximum height.
qts_reference_leaves <- function(points, height) {
  leaves <- 0
  count_node <- function(node_points, mid, edge, depth_left) {
    if (nrow(node_points) == 0L) {
      return(0)
    }
    if ((depth_left == 0) || (nrow(node_points) == 1L)) {
      return(1)
    }
    upper <- sweep(node_points, 2, mid, ">")
    code <- apply(upper * 1L, 1, paste, collapse = "")
    total <- 0
    for (child in unique(code)) {
      side <- as.integer(strsplit(child, "")[[1]])
      child_mid <- mid + ifelse(side == 1L, edge / 2, -edge / 2)
      total <- total + count_node(node_points[code == child, , drop = FALSE], child_mid, edge / 2, depth_left - 1)
    }
    total
  }
  count_node(points, rep(0.5, ncol(points)), 0.5, height)
}

qts_keys <- function(points, height) {
  cells <- lapply(seq_len(nrow(points)), function(i) heimdall:::.qts_cells(points[i, ], height))
  matrix(unlist(cells), nrow = nrow(points), byrow = TRUE)
}

test_that("the occupancy is the number of leaves of the reference quadtree", {
  set.seed(1)
  for (d in c(1, 2, 3, 8)) {
    points <- matrix(stats::runif(300 * d), ncol = d)
    # Ties on the cell boundaries, the borders and duplicated points.
    points[1:20, ] <- round(points[1:20, ] * 8) / 8
    points[21:25, ] <- points[26:30, ]
    for (height in c(1, 2, 4)) {
      expect_equal(heimdall:::.qts_occupancy(qts_keys(points, height)), qts_reference_leaves(points, height))
    }
  }
  expect_equal(heimdall:::.qts_occupancy(qts_keys(matrix(0.3, 1, 2), 4)), 1)
  expect_equal(heimdall:::.qts_occupancy(qts_keys(matrix(0.3, 5, 2), 4)), 1)
  # Values outside the unit hypercube fall in the outermost cells.
  expect_equal(heimdall:::.qts_cells(c(-1, 2), 2), c("0 1", "0 3"))
  expect_equal(heimdall:::.qts_cells(c(0, 0.5, 0.25), 2), c("0 0 0", "0 1 0"))
})

test_that("the moving average matches pandas' ewm with adjust = TRUE", {
  # pandas.Series([1, 5, 2, 8]).ewm(span=4).mean().iloc[-1]
  expect_equal(heimdall:::.qts_ewm(c(1, 5, 2, 8)), 5.154411764705881, tolerance = 1e-12)
})

qts_stream <- function(n_before, n_after, seed) {
  set.seed(seed)
  rbind(
    data.frame(x = stats::rnorm(n_before, 0.3, 0.05), y = stats::rnorm(n_before, 0.3, 0.05)),
    data.frame(x = stats::runif(n_after), y = stats::runif(n_after))
  )
}

test_that("dfr_qts detects a change of the spatial spread", {
  stream <- qts_stream(3000, 3000, 2)
  model <- fit(dfr_qts(window_size = 200, reference_size = 600), stream)
  expect_true(model$drifted)
  expect_gte(nrow(model$detections), 1L)
  expect_true(all(model$detections$strategy %in% c("occupancy", "derivative")))
  first <- model$detections$observation[1]
  expect_gt(first, 3000)
  expect_lt(first, 3600)
  output <- model$drifter_output
  expect_equal(nrow(output), nrow(stream))
  expect_equal(names(output), c("occupancy", "occupancy_ewm", "reference_mean", "reference_sd",
                                "derivative_recent", "derivative_threshold"))
  expect_gt(mean(output$occupancy[5000:6000]), 2 * mean(output$occupancy[1000:3000]))
  # The reference exists once W and the first reference_size occupancies are full.
  expect_true(all(is.na(output$reference_mean[1:798])))
  expect_false(is.na(output$reference_mean[799]))

  model <- reset_state(model)
  expect_false(model$drifted)
  expect_equal(nrow(model$detections), 0L)
  expect_equal(model$state$n, 0)
})

test_that("dfr_qts keeps quiet on a stationary stream with the default windows", {
  set.seed(3)
  stream <- data.frame(x = stats::runif(3000), y = stats::runif(3000))
  expect_false(fit(dfr_qts(), stream)$drifted)
  expect_false(fit(dfr_qts(derivative_rule = "paper"), stream)$drifted)
})

test_that("dfr_qts scales the features with lower and upper", {
  stream <- qts_stream(1500, 1500, 4)
  scaled <- fit(dfr_qts(window_size = 200, reference_size = 600), stream)
  shifted <- fit(dfr_qts(window_size = 200, reference_size = 600, lower = c(10, -5), upper = c(12, -3)),
                 data.frame(x = 10 + 2 * stream$x, y = -5 + 2 * stream$y))
  expect_equal(shifted$drifter_output$occupancy, scaled$drifter_output$occupancy)
  expect_equal(shifted$detections, scaled$detections)
})

test_that("dfr_qts validates its arguments and input", {
  expect_error(dfr_qts(height = 0), "height")
  expect_error(dfr_qts(window_size = 1), "window_size")
  expect_error(dfr_qts(recent = 0), "recent")
  expect_error(dfr_qts(window_size = 10, recent = 0.1), "at least 2")
  expect_error(dfr_qts(lower = NA), "lower and upper")
  expect_error(dfr_qts(derivative_rule = "other"))
  expect_error(update_state(dfr_qts(lower = c(0, 0, 0)), data.frame(a = 1, b = 2)), "one value per")
  expect_error(update_state(dfr_qts(lower = 1, upper = 1), data.frame(a = 1, b = 2)), "greater than lower")
  model <- update_state(dfr_qts(), data.frame(a = 0.1, b = 0.2))$obj
  expect_error(update_state(model, data.frame(a = 0.1, c = 0.2)), "features changed")
  # A row with only missing values is skipped.
  skipped <- update_state(model, data.frame(a = NA, b = NA))$obj
  expect_equal(skipped$state$n, 1)
})
