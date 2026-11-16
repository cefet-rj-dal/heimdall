mcd_settings <- function(...) {
  utils::modifyList(dfr_mcd(window_size = 200, n_sub = 10, m = 6, k = 4, hidden_size = 7, output_size = 5)$state$settings,
                    list(...))
}

# Central finite differences of f() with respect to every entry of params.
mcd_numeric_grad <- function(params, f, h = 1e-6) {
  lapply(stats::setNames(names(params), names(params)), function(name) {
    g <- params[[name]] * 0
    for (i in seq_along(g)) {
      up <- params
      down <- params
      up[[name]][i] <- up[[name]][i] + h
      down[[name]][i] <- down[[name]][i] - h
      g[i] <- (f(up) - f(down)) / (2 * h)
    }
    g
  })
}

test_that("the analytic gradients of the encoder match finite differences", {
  set.seed(1)
  settings <- mcd_settings()
  params <- heimdall:::.mcd_net(3, settings)$params
  x <- matrix(stats::rnorm(4 * 6 * 3), ncol = 3)
  weights <- matrix(stats::rnorm(4 * 5), 4)
  # Representations: the gradient of sum(E * weights).
  value <- function(p) sum(heimdall:::.mcd_embed(heimdall:::.mcd_forward(p, x)$y, 4) * weights)
  analytic <- heimdall:::.mcd_backward(params, x, heimdall:::.mcd_forward(params, x), weights, 4)
  numeric <- mcd_numeric_grad(params, value)
  for (name in names(params)) expect_equal(analytic[[name]], numeric[[name]], tolerance = 1e-6)
  # Gradient penalty, whose gradient involves second derivatives.
  penalty <- function(p) heimdall:::.mcd_penalty(p, x, heimdall:::.mcd_forward(p, x), 4)$value
  analytic <- heimdall:::.mcd_penalty(params, x, heimdall:::.mcd_forward(params, x), 4)$grad
  numeric <- mcd_numeric_grad(params, penalty)
  for (name in names(params)) expect_equal(analytic[[name]], numeric[[name]], tolerance = 1e-5)
})

test_that("the set distances and their gradients are right", {
  set.seed(2)
  e1 <- matrix(stats::rnorm(12), 4)
  e2 <- matrix(stats::rnorm(12), 4)
  within <- heimdall:::.mcd_within(e1)
  expect_equal(within$value, mean(stats::dist(e1)))
  expect_length(within$pairs, 6L)
  between <- heimdall:::.mcd_between(e1, e2)
  d <- as.matrix(stats::dist(rbind(e1, e2)))[1:4, 5:8]
  expect_equal(between$value, mean(d[row(d) != col(d)]))
  numeric <- function(f, e) {
    g <- e * 0
    for (i in seq_along(e)) {
      up <- e
      down <- e
      up[i] <- up[i] + 1e-6
      down[i] <- down[i] - 1e-6
      g[i] <- (f(up) - f(down)) / 2e-6
    }
    g
  }
  expect_equal(within$grad, numeric(function(e) heimdall:::.mcd_within(e)$value, e1), tolerance = 1e-6)
  expect_equal(between$grad1, numeric(function(e) heimdall:::.mcd_between(e, e2)$value, e1), tolerance = 1e-6)
  expect_equal(between$grad2, numeric(function(e) heimdall:::.mcd_between(e1, e)$value, e2), tolerance = 1e-6)
})

test_that("a training step lowers the loss and the threshold is a quantile", {
  set.seed(3)
  settings <- mcd_settings(learning_rate = 0.01)
  window <- matrix(stats::rnorm(200 * 3), ncol = 3)
  net <- heimdall:::.mcd_net(3, settings)
  first <- heimdall:::.mcd_train(net, window, settings)
  expect_true(is.finite(first$loss))
  expect_gt(first$threshold, 0)
  expect_equal(first$net$adam$t, 1L)
  # Adam's first step moves every weight by about the learning rate.
  steps <- abs(first$net$params$W1 - net$params$W1)
  expect_true(all(steps < 0.0101))
  distances <- heimdall:::.mcd_test(first$net, window, settings)
  expect_length(distances, 9L)
})

test_that("dfr_mcd detects a change of the mixture weights", {
  skip_on_cran()
  set.seed(4)
  mixture <- function(n, p) {
    wide <- stats::runif(n) > p
    matrix(stats::rnorm(n * 3, 20, ifelse(wide, 50, 10)), n)
  }
  stream <- as.data.frame(rbind(mixture(4000, 0.2), mixture(2000, 0.8)))
  model <- fit(dfr_mcd(window_size = 1000, m = 30, seed = 1), stream)
  output <- model$drifter_output
  expect_equal(names(output), c("mcd", "threshold"))
  tested <- which(!is.na(output$mcd))
  expect_equal(tested, seq(1100, 6000, by = 100))
  detected <- tested[output$mcd[tested] > output$threshold[tested]]
  expect_true(4100 %in% detected)
  expect_lte(length(setdiff(detected, 4100)), 2L)
  expect_true(model$drifted)
  expect_equal(dim(model$distances), c(length(tested), 10L))
  expect_equal(model$distances[, "sub9"], output$mcd[tested])
  # The seed makes the detector reproducible.
  again <- fit(dfr_mcd(window_size = 1000, m = 30, seed = 1), stream)
  expect_equal(again$drifter_output, output)
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_null(model$net)
  expect_error(dfr_mcd(window_size = 1001), "multiple of n_sub")
  expect_error(dfr_mcd(k = 5), "even")
  expect_error(dfr_mcd(temperature = 0), "temperature")
})
