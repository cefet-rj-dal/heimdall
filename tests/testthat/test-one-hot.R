test_that("one-hot keeps numeric columns and expands categorical ones", {
  data <- data.frame(
    a = c(1, 2, NA),
    f = factor(c("x", "y", "x"), levels = c("x", "y", "z")),
    s = c("p", "q", "p"),
    l = c(TRUE, FALSE, NA),
    stringsAsFactors = FALSE
  )
  encoder <- heimdall:::.one_hot_fit(data)
  encoded <- heimdall:::.one_hot_apply(encoder, data)

  # same names as caret::dummyVars(" ~ .", data)
  expect_equal(names(encoded), c("a", "f.x", "f.y", "f.z", "sp", "sq", "lFALSE", "lTRUE"))
  expect_equal(encoder$feat_names, names(encoded))
  expect_equal(encoded$a, c(1, 2, NA))
  expect_equal(encoded$f.x, c(1, 0, 1))
  expect_equal(encoded$f.z, c(0, 0, 0))
  expect_equal(encoded$sq, c(0, 1, 0))
  expect_equal(encoded$lTRUE, c(1, 0, NA))
})

test_that("one-hot maps unseen levels to zeros and keeps the learned columns", {
  encoder <- heimdall:::.one_hot_fit(data.frame(f = factor(c("x", "y"))))
  encoded <- heimdall:::.one_hot_apply(encoder, data.frame(f = factor("w")))
  expect_equal(names(encoded), c("f.x", "f.y"))
  expect_equal(unlist(encoded[1, ]), c(f.x = 0, f.y = 0))
})

test_that("one-hot fails clearly when a learned column is missing", {
  encoder <- heimdall:::.one_hot_fit(data.frame(a = 1, b = 2))
  expect_error(heimdall:::.one_hot_apply(encoder, data.frame(a = 1)), "missing the columns: b")
})
