test_that("error-based detectors treat NA as a correct prediction", {
  for (name in names(error_based_detectors())) {
    expect_error(update_state(error_based_detectors()[[name]], NA), NA, info = name)
  }
})

test_that("distribution-based detectors skip missing observations", {
  for (name in names(univariate_detectors())) {
    output <- update_state(univariate_detectors()[[name]], NA)
    expect_false(output$drift, info = name)
  }
})

test_that("a missing value does not poison the page hinkley statistics", {
  model <- dfr_page_hinkley()
  model <- update_state(model, 1)$obj
  before <- model$state

  model <- update_state(model, NA)$obj
  expect_equal(model$state$x_mean, before$x_mean)
  expect_equal(model$state$sum, before$sum)
  expect_false(is.nan(model$state$sum))
})

test_that("streams containing NA can be fitted", {
  stream <- c(rep(0, 20), NA, rep(1, 20))
  for (name in names(vector_detectors())) {
    expect_error(fit(vector_detectors()[[name]], stream), NA, info = name)
  }
})
