test_that("ks_sf matches the p-values of ks.test", {
  set.seed(1)
  for (n in c(3, 10, 40, 100)) {
    x <- rnorm(n, 0.4)
    exact <- ks.test(x, "pnorm", exact = TRUE)
    expect_equal(ks_sf(unname(exact$statistic), n), exact$p.value, tolerance = 1e-10)
    asymptotic <- ks.test(x, "pnorm", exact = FALSE)
    expect_equal(ks_sf(unname(asymptotic$statistic), n, exact = FALSE), asymptotic$p.value, tolerance = 1e-10)
  }
  expect_equal(ks_sf(c(0, 1), 10), c(1, 0))
  expect_equal(ks_sf(0.04, 5), 1)
  expect_true(is.na(ks_sf(NA, 10)))
  expect_error(ks_sf(0.1, 0), "positive integers")
})

test_that("ks_sf covers the regions of the exact distribution", {
  # monotone in d for a large n (Durbin matrix, Pelz-Good, Smirnov tail)
  d <- c(0.002, 0.005, 0.01, 0.015, 0.02, 0.03, 0.05)
  p <- ks_sf(d, 5000)
  expect_true(all(diff(p) < 0))
  expect_true(all(p >= 0 & p <= 1))
  # close to the limiting distribution for large n
  expect_equal(ks_sf(0.02, 20000), ks_sf(0.02, 20000, exact = FALSE), tolerance = 0.05)
})

test_that("greedyks estimates the Kolmogorov-Smirnov statistic", {
  sketch <- greedyks_update(greedyks(function(q) q, m = 2), 0.5)
  expect_equal(greedyks_statistic(sketch), 0.5)

  set.seed(2)
  x <- rnorm(4000, 1.1, 1)
  cdf <- function(q) pnorm(q, 1, 1)
  sketch <- greedyks_update(greedyks(cdf, m = 1000), x)
  exact <- unname(suppressWarnings(ks.test(x, "pnorm", 1, 1))$statistic)
  expect_lt(abs(greedyks_statistic(sketch) - exact), 1e-3)
  expect_equal(sketch$n, 4000)
  expect_equal(greedyks_pvalue(sketch), ks_sf(greedyks_statistic(sketch), 4000))

  # processing in pieces is the same as all at once
  pieces <- greedyks_update(greedyks_update(greedyks(cdf, m = 50), x[1:1000]), c(x[1001:4000], NA))
  whole <- greedyks_update(greedyks(cdf, m = 50), x)
  expect_equal(greedyks_statistic(pieces), greedyks_statistic(whole))

  early <- greedyks_update(greedyks(cdf, m = 50, min_instances = 10), x[1:5])
  expect_equal(greedyks_statistic(early), 0)
  expect_output(print(whole), "GreedyKS sketch: 51 buckets")
  expect_error(greedyks(1), "must be a function")
  expect_error(greedyks_update(greedyks(function(q) q + 2), 1), "probabilities")
})

test_that("dfr_greedyks detects a shift against a fixed reference", {
  set.seed(3)
  cdf <- function(q) pnorm(q, 0, 1)
  quiet <- fit(dfr_greedyks(cdf = cdf), rnorm(2000))
  expect_false(any(quiet$drifter_output$drift))
  expect_named(quiet$drifter_output, c("D", "p_value", "drift"))
  expect_true(all(is.na(quiet$drifter_output$p_value[1:100])))

  x <- c(rnorm(500), rnorm(1500, 0.5))
  det <- fit(dfr_greedyks(cdf = cdf, m = 100), x)
  alarms <- which(det$drifter_output$drift)
  expect_gt(length(alarms), 0)
  expect_gt(alarms[1], 500)
  expect_true(det$drifted)
  expect_false(reset_state(det)$drifted)

  asym <- fit(dfr_greedyks(cdf = cdf, m = 100, exact = FALSE), x)
  expect_gt(sum(asym$drifter_output$drift), 0)
})

test_that("dfr_greedyks fits the reference distribution", {
  set.seed(4)
  x <- c(rexp(1500, 1), rexp(1500, 0.5))
  det <- fit(dfr_greedyks(cdf = "exp", reference_size = 500), x)
  out <- det$drifter_output
  expect_true(all(is.na(out$D[1:499])))
  expect_false(any(out$drift[1:1500]))
  expect_true(any(out$drift[1501:3000]))

  for (family in c("norm", "unif")) {
    det <- fit(dfr_greedyks(cdf = family, reference_size = 200), runif(1000))
    expect_s3_class(det, "dfr_greedyks")
  }
  # a constant reference waits for more observations
  det <- fit(dfr_greedyks(cdf = "norm", reference_size = 5), c(rep(1, 10), rnorm(50)))
  expect_false(all(is.na(det$drifter_output$D)))
  expect_error(dfr_greedyks(cdf = 3), "must be a function")
})
