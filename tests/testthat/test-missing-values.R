test_that("error-based detectors treat NA as a correct prediction", {
  for (name in names(error_based_detectors())) {
    detector <- error_based_detectors()[[name]]
    expect_error(update_state(detector, NA), NA, info = name)
  }
})

test_that("distribution-based detectors skip missing observations", {
  for (name in names(dist_based_detectors())) {
    detector <- dist_based_detectors()[[name]]
    output <- update_state(detector, NA)
    expect_false(output$drift, info = name)
  }
})

test_that("streams containing NA can be fitted", {
  stream <- c(rep(0, 20), NA, rep(1, 20))
  for (name in names(c(error_based_detectors(), dist_based_detectors()))) {
    detector <- c(error_based_detectors(), dist_based_detectors())[[name]]
    expect_error(fit(detector, stream), NA, info = name)
  }
})
