test_that("the sliding window never grows past window_size", {
  windowed <- list(
    dfr_kldist = dfr_kldist(window_size = 10),
    dfr_kswin = dfr_kswin(window_size = 20, stat_size = 5, alpha = 0.01, exact = NULL),
    dfr_lbdd = dfr_lbdd(window_size = 10, alpha = 0.05),
    dfr_mcdd = dfr_mcdd(window_size = 10, alpha = 0.05)
  )
  for (name in names(windowed)) {
    model <- fit(windowed[[name]], seq_len(100))
    expect_lte(length(model$state$window), model$state$window_size, label = name)
  }
})

test_that("an observation is stored exactly once", {
  model <- dfr_mcdd(window_size = 10, alpha = 1e-12)
  model <- fit(model, rep(0, 5))
  expect_equal(length(model$state$window), 5)

  model <- fit(model, rep(1, 3))
  expect_equal(length(model$state$window), 8)
  expect_equal(sum(model$state$window), 3)
})

test_that("monitoring_step does not change the stored window", {
  stream <- drifting_numeric_stream()
  every <- fit(dfr_kldist(window_size = 20, p_th = 1e6), stream)
  throttled <- fit(dfr_kldist(window_size = 20, p_th = 1e6, monitoring_step = 5), stream)
  expect_equal(every$state$window, throttled$state$window)
})
