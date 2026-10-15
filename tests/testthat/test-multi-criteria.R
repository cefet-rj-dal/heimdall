shift_data <- function() {
  set.seed(1)
  data.frame(serie = c(stats::rnorm(100), stats::rnorm(100, mean = 10)))
}

never <- function() dfr_page_hinkley(threshold = 1e9)

drift_steps <- function(model, data) {
  output <- list(obj = model, drift = FALSE)
  steps <- integer(0)
  for (i in seq_len(nrow(data))) {
    output <- update_state(output$obj, data[i, , drop = FALSE])
    if (output$drift) {
      steps <- c(steps, i)
    }
  }
  steps
}

test_that("'or' reports a drift when any detector does", {
  model <- dfr_multi_criteria(list(ph = dfr_page_hinkley(), quiet = never()), combination = "or")
  expect_true(fit(model, shift_data())$drifted)
})

test_that("'and' requires every detector to agree within the window", {
  disagree <- dfr_multi_criteria(list(ph = dfr_page_hinkley(), quiet = never()), combination = "and")
  expect_false(fit(disagree, shift_data())$drifted)

  # the two detectors fire one observation apart
  close <- dfr_multi_criteria(
    list(a = dfr_page_hinkley(), b = dfr_page_hinkley(threshold = 40)),
    combination = "and",
    fuzzy_window = 10
  )
  expect_length(drift_steps(close, shift_data()), 1)

  strict <- dfr_multi_criteria(
    list(a = dfr_page_hinkley(), b = dfr_page_hinkley(threshold = 40)),
    combination = "and",
    fuzzy_window = 1
  )
  expect_length(drift_steps(strict, shift_data()), 0)
})

test_that("'fuzzy' combines detections that are close in time and fires once", {
  model <- dfr_multi_criteria(
    list(a = dfr_page_hinkley(), b = dfr_page_hinkley(threshold = 40)),
    combination = "fuzzy",
    fuzzy_window = 10
  )
  expect_length(drift_steps(model, shift_data()), 1)

  quiet <- dfr_multi_criteria(list(a = never(), b = never()), combination = "fuzzy")
  expect_false(fit(quiet, shift_data())$drifted)
})

test_that("multi criteria counts the detections of each detector", {
  model <- fit(
    dfr_multi_criteria(list(a = dfr_page_hinkley(), b = never())),
    shift_data()
  )
  expect_equal(model$detections, c(a = 1L, b = 0L))
  expect_equal(names(model$last_drifts), c("a", "b"))
})

test_that("multi criteria keeps a bounded state on long streams", {
  set.seed(3)
  model <- fit(
    dfr_multi_criteria(list(a = dfr_page_hinkley(), b = dfr_page_hinkley()), combination = "fuzzy"),
    data.frame(serie = stats::rnorm(2000))
  )
  expect_length(model$state$age, 2)
  expect_null(model$drifts)
})

test_that("multi criteria routes the monitored feature to each detector", {
  set.seed(2)
  data <- data.frame(a = stats::rnorm(200), b = c(stats::rnorm(100), stats::rnorm(100, mean = 10)))

  on_a <- dfr_multi_criteria(list(ph = dfr_page_hinkley(target_feat = "a")))
  on_b <- dfr_multi_criteria(list(ph = dfr_page_hinkley(target_feat = "b")))
  expect_false(fit(on_a, data)$drifted)
  expect_true(fit(on_b, data)$drifted)
})

test_that("multi criteria fails clearly when a monitored feature is missing", {
  model <- dfr_multi_criteria(list(ph = dfr_page_hinkley(target_feat = "nope")))
  expect_error(fit(model, shift_data()), "'nope' is not in the data")
})

test_that("multi criteria names unnamed detectors and resets cleanly", {
  model <- dfr_multi_criteria(list(dfr_page_hinkley(), never()), combination = "and")
  expect_equal(names(model$drifter_list), c("drifter_1", "drifter_2"))

  model <- reset_state(fit(model, shift_data()))
  expect_false(model$drifted)
  expect_equal(model$detections, c(drifter_1 = 0L, drifter_2 = 0L))
  expect_equal(model$combination, "and")
})
