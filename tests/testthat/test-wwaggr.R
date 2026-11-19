wwaggr_sequences <- function(n, len = 16, seed = 1) {
  set.seed(seed)
  change <- sample(4:(len + 1), n, replace = TRUE)
  x <- t(vapply(change, function(c) stats::rnorm(len, ifelse(seq_len(len) < c, 0, 3)), numeric(len)))
  list(x = x, labels = t(vapply(change, function(c) (seq_len(len) >= c) * 1, numeric(len))))
}

fd_gradient <- function(params, loss_fun, name, i, h = 1e-6) {
  up <- params
  dn <- params
  up[[name]][i] <- up[[name]][i] + h
  dn[[name]][i] <- dn[[name]][i] - h
  (loss_fun(up) - loss_fun(dn)) / (2 * h)
}

test_that("automatic differentiation matches finite differences", {
  set.seed(1)
  x <- matrix(stats::rnorm(3 * 12), 3, 12)
  params <- list(c_w = matrix(stats::rnorm(4 * 9), 4, 9), c_b = stats::rnorm(4))
  r <- matrix(stats::rnorm(48), 4, 12)
  loss <- function(P) sum(.adv(heimdall:::.nn_conv1d(P, "c", x, 6, 3, 2, 4)) * r)
  grads <- heimdall:::.ad_record(function() {
    P <- heimdall:::.ad_leaves(params)
    heimdall:::.ad_grad(heimdall:::.ad_sum(heimdall:::.ad_mul(heimdall:::.nn_conv1d(P, "c", x, 6, 3, 2, 4), r)), P)
  })
  for (i in c(1, 7, 20)) expect_equal(grads$c_w[i], fd_gradient(params, loss, "c_w", i), tolerance = 1e-6)
  settings <- list(hidden_size = 3L, n_layers = 1L, layer_norm = "before", dropout = 0, loss = "bce", T = 3)
  data <- wwaggr_sequences(4, len = 6)
  xs <- array(data$x, c(4, 6, 1))
  p0 <- heimdall:::.s2s_init(1, settings)
  objective <- function(P) heimdall:::.indid_objective(heimdall:::.s2s_forward(P, settings, xs), data$labels, settings)$value
  grads <- heimdall:::.ad_record(function() {
    P <- heimdall:::.ad_leaves(p0)
    p <- heimdall:::.s2s_forward(P, settings, xs, training = TRUE)
    heimdall:::.ad_grad(p, P, heimdall:::.indid_objective(p$value, data$labels, settings)$grad)
  })
  for (name in c("Wf_w", "lnUc_gamma", "out_b")) {
    expect_equal(grads[[name]][1], fd_gradient(p0, objective, name, 1), tolerance = 1e-5)
  }
})

test_that("wwaggr aggregates ensemble scores", {
  set.seed(2)
  scores <- array(stats::runif(5 * 2 * 20, 0, 0.1), c(5, 2, 20))
  scores[, , 11:20] <- scores[, , 11:20] + 0.8
  mean_scores <- wwaggr(scores, method = "mean")
  expect_equal(dim(mean_scores), c(2, 20))
  expect_equal(mean_scores[1, ], apply(scores[, 1, ], 2, mean))
  expect_equal(wwaggr(scores, method = "max")[2, ], apply(scores[, 2, ], 2, max))
  expect_equal(wwaggr(scores, method = "quantile", q = 0.5), wwaggr(scores, method = "median"))
  w <- wwaggr(scores, window_size = 3)
  expect_equal(w[, 1:3], matrix(0, 2, 3))
  expect_true(all(w[, 4:11] < 0.1))
  expect_true(all(w[, 13:14] > 0.2))
  expect_true(all(w <= 1 & w >= 0))
  # Manual 1-Wasserstein at step 12: future window 9-11, anchor ("prev") 6-8.
  a <- sort(as.vector(scores[, 1, 6:8]))
  f <- sort(as.vector(scores[, 1, 9:11]))
  expect_equal(w[1, 12], mean(abs(a - f)))
  expect_equal(wwaggr(scores[, 1, ], window_size = 3), w[1, ])
  expect_true(all(wwaggr(scores, method = "mmd", window_size = 2)[, 15] > 0))
  expect_equal(dim(wwaggr(scores, window_size = 4, anchor = "combined")), c(2, 20))
  expect_error(wwaggr(scores, anchor = "combined", window_size = 1), "combined")
  # Steps without the score of some member are left out.
  scores[2, , 1:5] <- NA
  out <- wwaggr(scores, window_size = 2, anchor = "start")
  expect_true(all(is.na(out[, 1:5])))
  expect_equal(out[, 6:7], matrix(0, 2, 2))
})

test_that("cpd_calibrate fits beta and temperature calibrations", {
  set.seed(3)
  labels <- stats::runif(4000) < 0.4
  s <- pmin(pmax(stats::rbeta(4000, 2, 5) + 0.3 * labels, 0), 1)
  for (parameters in c("abm", "am", "ab", "a")) {
    calibrator <- heimdall:::.calibration_fit(s, labels * 1, "beta", parameters)
    p <- heimdall:::.calibration_apply(calibrator, c(0.1, 0.5, 0.9))
    expect_true(all(diff(p) > 0))
  }
  calibrator <- heimdall:::.calibration_fit(s, labels * 1, "beta", "abm")
  logloss <- function(p) -mean(labels * log(p) + (1 - labels) * log(1 - p))
  expect_lt(logloss(heimdall:::.calibration_apply(calibrator, s)), logloss(pmin(pmax(s, 1e-6), 1 - 1e-6)))
  temperature <- heimdall:::.calibration_fit(s, labels * 1, "temperature", "abm")
  expect_gt(temperature$temperature, 0)
  expect_error(heimdall:::.calibration_fit(s, rep(1, 4000), "beta", "abm"), "both labels")
  train <- wwaggr_sequences(32)
  model <- indid(train$x, train$labels, loss = "bce", epochs = 2, seed = 1)
  calibrated <- cpd_calibrate(model, train$x, train$labels, method = "temperature")
  expect_s3_class(calibrated, "cpd_calibrated")
  expect_equal(dim(cpd_scores(calibrated, train$x)), dim(train$x))
})

test_that("generalised LSTM, TS-CP2 and SN-TS2Vec train and score sequences", {
  train <- wwaggr_sequences(24)
  two <- indid(train$x, train$labels, loss = "indid", T = 4, epochs = 2, n_layers = 2, dropout = 0.2, seed = 1)
  expect_equal(nrow(two$history), 2)
  ln <- indid(train$x, train$labels, loss = "bce", epochs = 2, layer_norm = "after", seed = 1)
  expect_identical(indid(train$x, train$labels, loss = "bce", epochs = 2, layer_norm = "after", seed = 1)$params,
                   ln$params)
  p <- indid_predict(ln, train$x)
  expect_true(all(p > 0 & p < 1))
  expect_error(indid(train$x, train$labels, n_layers = 2, layer_norm = "before"), "one layer")
  x <- array(train$x, c(24, 16, 1))
  tscp <- tscp2(x, window = 4, nb_filters = 4, kernel_size = 3, dilations = c(1, 2), n_steps = 3, code_size = 3,
                epochs = 2, step = 2, alpha = 0.5, seed = 1)
  s <- cpd_scores(tscp, x[1:3, , , drop = FALSE])
  expect_true(all(is.na(s[, 1:7])))
  expect_true(all(s[, 8:16] >= 0 & s[, 8:16] <= 1))
  tv <- ts2vec(x, window = 3, output_dim = 3, hidden_dim = 4, depth = 1, epochs = 2, seed = 1)
  s <- cpd_scores(tv, x[1:3, , , drop = FALSE])
  expect_true(all(is.na(s[, 1:5])) && !anyNA(s[, 6:16]))
  expect_error(tscp2(x[, 1:6, , drop = FALSE], window = 4), "2 \\* window")
})

test_that("ensembles combine members and the stream detector matches the batch scores", {
  train <- wwaggr_sequences(32)
  bce <- cpd_ensemble_fit(train$x, train$labels, model = "bce", n_models = 2, epochs = 3, learning_rate = 0.01,
                          seed = 1)
  expect_length(bce$members, 2)
  x <- array(train$x, c(32, 16, 1))
  tscp <- tscp2(x, window = 3, nb_filters = 3, kernel_size = 2, dilations = 1, nb_stacks = 1, n_steps = 2,
                code_size = 2, epochs = 1, step = 2, alpha = 0.6, seed = 2)
  mixed <- cpd_ensemble(c(bce$members, list(cpd_calibrate(tscp, x, train$labels))))
  expect_output(print(mixed), "3 change point models")
  scores <- cpd_scores(mixed, train$x[1:3, ])
  expect_equal(dim(scores), c(3, 3, 16))
  boot <- cpd_ensemble_fit(train$x, train$labels, model = "indid", n_models = 2, boot_sample_size = 10, T = 4,
                           epochs = 1, seed = 1)
  expect_s3_class(boot, "cpd_ensemble")
  expect_error(cpd_ensemble_fit(train$x, model = "bce"), "labels")
  for (method in c("wasserstein", "mmd", "max")) {
    for (anchor in c("prev", "start")) {
      batch <- wwaggr(scores, method = method, window_size = 2, anchor = anchor)
      stream <- data.frame(value = as.vector(t(train$x[1:3, ])))
      detector <- fit(dfr_wwaggr(mixed, threshold = 1e6, method = method, window_size = 2, anchor = anchor,
                                 horizon = 16), stream)
      expect_equal(matrix(detector$drifter_output$score, 3, byrow = TRUE), batch, tolerance = 1e-10)
    }
  }
  threshold <- 0.9 * max(wwaggr(scores, window_size = 2), na.rm = TRUE)
  detector <- fit(dfr_wwaggr(mixed, threshold = threshold, window_size = 2, horizon = 16), stream)
  drifts <- which(detector$drifter_output$drift == 1)
  expect_true(length(drifts) > 0)
  expect_true(detector$drifted)
  expect_false(reset_state(detector)$drifted)
  expect_error(dfr_wwaggr(list()), "cpd_ensemble")
})

test_that("dfr_indid streams the generalised LSTM as indid_predict", {
  train <- wwaggr_sequences(16)
  model <- indid(train$x, train$labels, loss = "bce", epochs = 1, n_layers = 2, seed = 1)
  stream <- data.frame(value = as.vector(t(train$x[1:2, ])))
  detector <- fit(dfr_indid(model, threshold = 1, horizon = 16), stream)
  expect_equal(matrix(detector$drifter_output$probability, 2, byrow = TRUE), indid_predict(model, train$x[1:2, ]),
               tolerance = 1e-12)
})
