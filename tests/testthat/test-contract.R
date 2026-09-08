test_that("update_state always returns the obj/drift contract", {
  detectors <- c(vector_detectors(), list(dfr_inactive = dfr_inactive(), dfr_passive = dfr_passive()))
  for (name in names(detectors)) {
    output <- update_state(detectors[[name]], 1)

    expect_true(is.list(output), info = name)
    expect_true(all(c("obj", "drift") %in% names(output)), info = name)
    expect_true(is.logical(output$drift), info = name)
    expect_length(output$drift, 1)
    expect_false(is.na(output$drift), info = name)
  }
})

test_that("the documented streaming loop works for every detector", {
  detectors <- c(vector_detectors(), list(dfr_inactive = dfr_inactive(), dfr_passive = dfr_passive()))
  for (name in names(detectors)) {
    output <- list(obj = detectors[[name]], drift = FALSE)
    expect_error(
      for (i in 1:30) {
        output <- update_state(output$obj, i %% 2)
        if (output$drift) {
          output$obj <- reset_state(output$obj)
        }
      },
      NA,
      info = name
    )
  }
})

test_that("fit accepts a stream with a single observation", {
  for (name in names(vector_detectors())) {
    expect_error(fit(vector_detectors()[[name]], 1), NA, info = name)
  }
  expect_error(fit(dfr_kldist(window_size = 10), data.frame(serie = 1)), NA)
})

test_that("fit rejects an empty stream", {
  expect_error(fit(dfr_ddm(), numeric(0)), "at least one observation")
  expect_error(fit(dfr_kldist(), data.frame(serie = numeric(0))), "at least one row")
})

test_that("the dummy baselines honour the contract", {
  inactive <- fit(dfr_inactive(), 1:20)
  expect_false(inactive$drifted)

  passive <- update_state(dfr_passive(), 1)
  expect_true(passive$drift)

  # length() on a data frame is the column count; fit must walk rows
  passive <- fit(dfr_passive(), data.frame(a = 1:7, b = 1:7))
  expect_equal(nrow(passive$drifter_output), 7)
})

test_that("fit populates the per-observation diagnostics", {
  model <- fit(dfr_kswin(window_size = 40, stat_size = 10, alpha = 0.01, exact = NULL), drifting_numeric_stream())
  expect_equal(names(model$drifter_output), c("D", "p"))
  expect_equal(nrow(model$drifter_output), 400)
})

test_that("reset_state clears the sticky drift flag", {
  for (name in names(vector_detectors())) {
    detector <- vector_detectors()[[name]]
    detector$drifted <- TRUE
    expect_false(reset_state(detector)$drifted, info = name)
  }
})

test_that("at least one error-based detector reacts to an abrupt drift", {
  stream <- drifting_error_stream()
  reacted <- vapply(
    names(error_based_detectors()),
    function(name) fit(error_based_detectors()[[name]], stream)$drifted,
    logical(1)
  )
  expect_true(any(reacted))
})
