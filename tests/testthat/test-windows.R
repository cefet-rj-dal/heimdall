test_that("the sliding policy bounds the window", {
  windowed <- list(
    dfr_kswin = dfr_kswin(window_size = 20, stat_size = 5, alpha = 0.01, exact = NULL),
    dfr_lbdd = dfr_lbdd(window_size = 10, alpha = 0.05),
    dfr_mcdd = dfr_mcdd(window_size = 10, alpha = 0.05)
  )
  for (name in names(windowed)) {
    model <- fit(windowed[[name]], seq_len(200))
    expect_lte(length(model$state$window), model$state$window_size, label = name)
  }

  model <- fit(dfr_kldist(window_size = 10), data.frame(serie = seq_len(200)))
  expect_lte(nrow(model$state$window), 10)
})

test_that("the anchored policy keeps every observation", {
  model <- fit(dfr_mcdd(window_size = 10, alpha = 1e-30, window_type = "anchored"), seq_len(50))
  expect_equal(length(model$state$window), 50)

  model <- fit(dfr_kldist(window_size = 10, p_th = 1e6, window_type = "anchored"), data.frame(serie = seq_len(50)))
  expect_equal(nrow(model$state$window), 50)
})

test_that("both policies are reachable and validated", {
  expect_error(dfr_mcdd(window_type = "sliding"), NA)
  expect_error(dfr_mcdd(window_type = "anchored"), NA)
  expect_error(dfr_mcdd(window_type = "whatever"), NULL)
})

test_that("the legacy dfr_aedd window names still work, with a warning", {
  expect_warning(model <- dfr_mcdd(window_type = "fixed"), "deprecated")
  expect_equal(model$state$window_type, "sliding")

  expect_warning(model <- dfr_mcdd(window_type = "moving"), "deprecated")
  expect_equal(model$state$window_type, "anchored")
})

test_that("an observation is stored exactly once", {
  model <- dfr_mcdd(window_size = 10, alpha = 1e-30)
  model <- fit(model, rep(0, 5))
  expect_equal(length(model$state$window), 5)

  model <- fit(model, rep(1, 3))
  expect_equal(length(model$state$window), 8)
  expect_equal(sum(model$state$window), 3)
})

test_that("monitoring_step does not change the stored window", {
  stream <- drifting_numeric_stream()
  every <- fit(dfr_lbdd(window_size = 20, alpha = 1e-30), stream)
  throttled <- fit(dfr_lbdd(window_size = 20, alpha = 1e-30, monitoring_step = 5), stream)
  expect_equal(every$state$window, throttled$state$window)
})
