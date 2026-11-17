llr2_chain <- function(n, shift = 0) {
  a <- stats::rnorm(n)
  b <- 1.5 * a + stats::rnorm(n, sd = 0.5)
  c <- ifelse(stats::runif(n) < 0.5, b - 3, b + 3) + stats::rnorm(n, sd = 0.7) + shift
  data.frame(a = a, b = b, c = c)
}

test_that("spbn learns a chain, fits linear Gaussian CPDs and proper conditional KDEs", {
  set.seed(1)
  data <- llr2_chain(1500)
  model <- spbn(data)
  expect_s3_class(model, "spbn")
  skeleton <- sort(unlist(lapply(names(model$parents), function(v) {
    vapply(model$parents[[v]], function(p) paste(sort(c(p, v)), collapse = "-"), character(1))
  })))
  expect_true(all(c("a-b", "b-c") %in% skeleton))
  # The bimodal node gets a kernel density; the Gaussian ones a linear Gaussian CPD.
  expect_equal(model$node_types[["c"]], "ckde")
  given <- spbn(data, structure = list(a = character(0), b = "a", c = "b"), node_types = c(a = "lg", b = "lg",
                                                                                         c = "ckde"))
  fit <- stats::lm(b ~ a, data)
  expect_equal(given$cpds$b$components[[1]]$coefficients, unname(stats::coef(fit)))
  expect_equal(given$cpds$b$components[[1]]$sd, sqrt(mean(stats::residuals(fit)^2)))
  # The conditional KDE of c given b integrates to one in c.
  grid <- seq(-20, 20, by = 0.01)
  density <- exp(heimdall:::.spbn_cpd_loglik(given$cpds$c, cbind(a = 0, b = 1, c = grid)))
  expect_equal(sum(density) * 0.01, 1, tolerance = 1e-3)
  loglik <- spbn_loglik(given, data[1:10, ])
  expect_equal(dim(loglik), c(10L, 3L))
  expect_equal(loglik[, "b"], stats::dnorm(data$b[1:10], stats::predict(fit, data[1:10, ]),
                                           sqrt(mean(stats::residuals(fit)^2)), log = TRUE))
  expect_output(print(given), "2 linear Gaussian, 1 CKDE")
  expect_error(spbn(data, structure = list(a = "b", b = "a")), "cycle")
  expect_error(spbn(data, node_types = c(a = "lg")), "node_types")
  expect_error(spbn(data.frame(a = c(1, NA))), "missing")
})

test_that("dfr_llr2 detects the drifting node and stays quiet without drift", {
  set.seed(2)
  stationary <- llr2_chain(3000)
  quiet <- fit(dfr_llr2(n_baseline = 1000, window_size = 250, step = 50), stationary)
  expect_lte(length(quiet$drifts), 1L)
  shifted <- rbind(llr2_chain(2000), llr2_chain(1000, shift = 4))
  model <- fit(dfr_llr2(n_baseline = 1000, window_size = 250, step = 50), shifted)
  expect_true(model$drifted)
  first <- model$drifts[[1]]
  expect_gt(first$observation, 2000)
  # Within one window of the change.
  expect_lte(first$observation, 2250)
  expect_true("c" %in% first$nodes)
  output <- model$drifter_output
  expect_equal(names(output), c("loglik", "p_a", "p_b", "p_c"))
  expect_true(all(is.na(output$loglik[1:1000])))
  tested <- which(!is.na(output$p_a))
  expect_true(all(tested %% 50 == 0))
  # Two drifts are at least window_size + step apart.
  times <- vapply(model$drifts, `[[`, numeric(1), "observation")
  if (length(times) > 1) expect_gte(min(diff(times)), 300)
  # BH is at most as sensitive as the fixed level.
  bh <- fit(dfr_llr2(n_baseline = 1000, window_size = 250, step = 50, fdr = TRUE), shifted)
  expect_lte(length(bh$drifts), length(model$drifts))
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_null(model$network)
})

test_that("the update strategies adapt the network", {
  set.seed(3)
  shifted <- rbind(llr2_chain(1500), llr2_chain(1000, shift = 4))
  for (update in c("parameter", "parameter_sliding", "sliding", "none")) {
    model <- fit(dfr_llr2(n_baseline = 1000, window_size = 200, step = 50, update = update), shifted)
    expect_true(model$drifted)
    if (update == "parameter") {
      # The drifted node mixes the CPD of the sliding window and of the learning window.
      expect_true(any(vapply(model$network$cpds, function(cpd) length(cpd$components), integer(1)) >= 1L))
    }
  }
  expect_error(dfr_llr2(n_baseline = 100, window_size = 100), "n_baseline")
  expect_error(dfr_llr2(update = "full"), "arg")
  expect_error(dfr_llr2(fdr = NA), "fdr")
})
