test_that("the inactive detector never reports a drift", {
  model <- dfr_inactive()
  for (i in 1:50) {
    output <- update_state(model, i)
    expect_false(output$drift)
    model <- output$obj
  }
  expect_false(model$drifted)
})

test_that("the passive detector always reports a drift", {
  model <- dfr_passive()
  for (i in 1:10) {
    output <- update_state(model, i)
    expect_true(output$drift)
    model <- output$obj
  }
})

test_that("fit.drifter does not require a prediction argument", {
  expect_error(fit(dfr_inactive(), data.frame(serie = 1:5)), NA)
})
