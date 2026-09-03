test_that("update_state always returns the obj/drift contract", {
  for (name in names(all_detectors())) {
    detector <- all_detectors()[[name]]
    output <- update_state(detector, 1)

    expect_true(is.list(output), info = name)
    expect_true(all(c("obj", "drift") %in% names(output)), info = name)
    expect_true(is.logical(output$drift), info = name)
    expect_length(output$drift, 1)
    expect_false(is.na(output$drift), info = name)
  }
})

test_that("the documented streaming loop works for every detector", {
  for (name in names(all_detectors())) {
    detector <- all_detectors()[[name]]
    output <- list(obj = detector, drift = FALSE)

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
  for (name in names(c(error_based_detectors(), dist_based_detectors()))) {
    detector <- c(error_based_detectors(), dist_based_detectors())[[name]]
    expect_error(fit(detector, 1), NA, info = name)
  }
})

test_that("fit rejects an empty stream", {
  expect_error(fit(dfr_ddm(), numeric(0)), "at least one observation")
})

test_that("reset_state clears the sticky drift flag", {
  for (name in names(c(error_based_detectors(), dist_based_detectors()))) {
    detector <- c(error_based_detectors(), dist_based_detectors())[[name]]
    detector$drifted <- TRUE
    expect_false(reset_state(detector)$drifted, info = name)
  }
})

test_that("obj$drifted is sticky and a stationary stream stays clean", {
  model <- fit(dfr_ddm(), rep(0, 400))
  expect_false(model$drifted)

  model$drifted <- TRUE
  model <- update_state(model, 0)$obj
  expect_true(model$drifted)
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

test_that("hddm does not latch the per-call drift flag", {
  output <- list(obj = dfr_hddm(), drift = FALSE)
  flags <- logical(0)
  stream <- drifting_error_stream()
  for (i in seq_along(stream)) {
    output <- update_state(output$obj, stream[i])
    flags <- c(flags, output$drift)
  }
  skip_if_not(any(flags), "hddm did not report a drift on this stream")
  first <- which(flags)[1]
  expect_false(all(flags[first:length(flags)]))
})
