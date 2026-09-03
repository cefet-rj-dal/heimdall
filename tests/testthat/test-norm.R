test_that("the first fit does not inject spurious rows or warnings", {
  data <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6))

  expect_warning(model <- fit(nrm_memory(minmax()), data), NA)
  expect_equal(nrow(model$data), 3)
  expect_equal(names(model$data), c("a", "b"))
  expect_equal(model$data$a, c(1, 2, 3))
})

test_that("the history accumulates across successive fits", {
  model <- fit(nrm_memory(minmax()), data.frame(a = c(1, 2)))
  model <- fit(model, data.frame(a = c(3, 4)))
  expect_equal(sort(model$data$a), c(1, 2, 3, 4))
})

test_that("inverse_transform uses the data it is given", {
  model <- fit(nrm_memory(minmax()), data.frame(a = c(0, 10)))

  normalized_one <- transform(model, data.frame(a = 10))
  normalized_two <- transform(model, data.frame(a = 0))

  expect_false(isTRUE(all.equal(
    inverse_transform(model, normalized_one),
    inverse_transform(model, normalized_two)
  )))
})

test_that("nrm_base is exported and norm() is no longer masked", {
  expect_true(is.function(nrm_base))
  expect_identical(base::norm, norm)
})
