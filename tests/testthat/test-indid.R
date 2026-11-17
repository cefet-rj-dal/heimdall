indid_numeric <- function(f, x, h = 1e-6) {
  g <- x * 0
  for (i in seq_along(x)) {
    up <- x
    down <- x
    up[i] <- up[i] + h
    down[i] <- down[i] - h
    g[i] <- (f(up) - f(down)) / (2 * h)
  }
  g
}

indid_toy <- function(n, len = 24, jump = 5) {
  change <- sample(2:len, n, replace = TRUE)
  change[seq_len(n / 2)] <- len + 1
  x <- t(vapply(change, function(c) stats::rnorm(len, ifelse(seq_len(len) < c, 1, jump)), numeric(len)))
  labels <- t(vapply(change, function(c) (seq_len(len) >= c) * 1, numeric(len)))
  list(x = x, labels = labels, change = change)
}

test_that("the expected stopping time and the InDiD loss have the right gradients", {
  set.seed(1)
  p <- stats::runif(12, 0.05, 0.9)
  delay <- heimdall:::.indid_delay(p)
  expect_equal(delay$value, 1 + sum(cumprod(1 - p)))
  expect_equal(delay$grad, indid_numeric(function(q) heimdall:::.indid_delay(q)$value, p), tolerance = 1e-7)
  expect_equal(heimdall:::.indid_delay(numeric(0))$value, 1)
  prob <- matrix(stats::runif(5 * 20, 0.05, 0.95), 5)
  labels <- matrix(0, 5, 20)
  labels[2, 2:20] <- 1
  labels[3, 8:20] <- 1
  labels[4, 20] <- 1
  loss <- heimdall:::.indid_loss(prob, labels, T = 6)
  expect_equal(loss$grad, indid_numeric(function(q) heimdall:::.indid_loss(q, labels, 6)$value, prob),
               tolerance = 1e-6)
  # Sequence 1 (no change) only pushes its probabilities down (loss -D, dD/dp < 0).
  expect_true(all(loss$grad[1, ] > 0))
  bce <- heimdall:::.indid_bce(prob, labels)
  expect_equal(bce$value, -mean(labels * log(prob) + (1 - labels) * log(1 - prob)))
  expect_equal(bce$grad, indid_numeric(function(q) heimdall:::.indid_bce(q, labels)$value, prob), tolerance = 1e-7)
})

test_that("backpropagation through time matches finite differences", {
  set.seed(2)
  net <- heimdall:::.indid_net(3, 4)
  x <- array(stats::rnorm(5 * 7 * 3), c(5, 7, 3))
  weights <- matrix(stats::rnorm(5 * 7), 5)
  value <- function(params) sum(heimdall:::.indid_forward(params, x)$p * weights)
  forward <- heimdall:::.indid_forward(net$params, x, keep = TRUE)
  analytic <- heimdall:::.indid_backward(net$params, x, forward, weights)
  for (name in names(net$params)) {
    numeric <- indid_numeric(function(q) {
      params <- net$params
      params[[name]] <- q
      value(params)
    }, net$params[[name]])
    expect_equal(analytic[[name]], numeric, tolerance = 1e-6)
  }
})

test_that("indid learns to detect a change and keeps the history of training", {
  set.seed(3)
  train <- indid_toy(200)
  test <- indid_toy(40)
  for (loss in c("indid", "bce")) {
    model <- indid(train$x, train$labels, loss = loss, T = 6, learning_rate = 0.02, epochs = 30, seed = 1)
    expect_s3_class(model, "indid")
    expect_equal(nrow(model$history), 30L)
    prob <- indid_predict(model, test$x)
    expect_equal(dim(prob), c(40L, 24L))
    before <- prob[test$labels == 0]
    after <- prob[test$labels == 1]
    expect_gt(mean(after > 0.5), 0.8)
    expect_lt(mean(before > 0.5), 0.1)
  }
  combined <- indid(train$x, train$labels, loss = "combined", T = 6, epochs = 10, seed = 1)
  expect_equal(unique(combined$history$stage), c("bce", "indid"))
  expect_output(print(combined), "combined")
  # Early stopping on the validation loss.
  stopped <- indid(train$x, train$labels, T = 6, learning_rate = 0.05, epochs = 200, patience = 2, seed = 1,
                   validation = list(x = test$x, labels = test$labels))
  expect_lt(nrow(stopped$history), 200L)
  expect_false(anyNA(stopped$history$val_loss))
  # The seed makes the training reproducible.
  again <- indid(train$x, train$labels, T = 6, epochs = 3, seed = 9)
  expect_equal(indid(train$x, train$labels, T = 6, epochs = 3, seed = 9)$params, again$params)
})

test_that("indid accepts arrays, matrices and lists and validates its input", {
  set.seed(4)
  data <- indid_toy(20, len = 8)
  as_list <- lapply(seq_len(20), function(i) matrix(data$x[i, ], ncol = 1))
  as_array <- array(data$x, c(20, 8, 1))
  m1 <- indid(data$x, data$labels, T = 3, epochs = 2, seed = 1)
  expect_equal(indid(as_list, data$labels, T = 3, epochs = 2, seed = 1)$params, m1$params)
  expect_equal(indid(as_array, data$labels, T = 3, epochs = 2, seed = 1)$params, m1$params)
  expect_error(indid(data$x, data$labels[, 1:7]), "labels")
  expect_error(indid(data$x, data$labels * 2), "labels")
  expect_error(indid(rbind(data$x[-1, ], NA), data$labels), "missing")
  expect_error(indid(data$x, data$labels, loss = "mse"), "arg")
  expect_error(indid_predict(list(), data$x), "indid object")
  expect_error(indid_predict(m1, array(0, c(2, 8, 2))), "features")
})

test_that("dfr_indid feeds the stream to the network and resets it after a drift", {
  set.seed(5)
  train <- indid_toy(200)
  model <- indid(train$x, train$labels, T = 6, learning_rate = 0.02, epochs = 30, seed = 1)
  stream <- data.frame(value = c(stats::rnorm(20, 1), stats::rnorm(10, 5)))
  detector <- fit(dfr_indid(model), stream)
  output <- detector$drifter_output
  expect_equal(names(output), "probability")
  # The online probabilities are those of indid_predict on the stream as a sequence.
  expect_equal(output$probability[1:20], as.numeric(indid_predict(model, matrix(stream$value[1:20], 1))))
  first <- which(output$probability > 0.5)[1]
  expect_gte(first, 21L)
  expect_lte(first, 23L)
  expect_true(detector$drifted)
  detector <- reset_state(detector)
  expect_false(detector$drifted)
  # horizon resets the state every few observations.
  short <- fit(dfr_indid(model, horizon = 5), data.frame(value = stream$value[1:10]))
  expect_equal(short$drifter_output$probability[6:10],
               as.numeric(indid_predict(model, matrix(stream$value[6:10], 1))))
  expect_error(dfr_indid(list()), "indid object")
  expect_error(fit(dfr_indid(model), data.frame(a = 1:3, b = 1:3)), "feature")
})
