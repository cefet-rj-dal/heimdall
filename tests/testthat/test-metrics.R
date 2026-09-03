test_that("accuracy is the proportion of matches", {
  expect_equal(evaluate(mt_accuracy(), c(TRUE, FALSE), c(TRUE, TRUE)), 0.5)
})

test_that("mt_fscore honours the beta parameter", {
  # precision and recall must differ, otherwise every F-beta collapses to the
  # same value and the test would pass even with the beta argument ignored
  y_pred <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  y_true <- c(TRUE, FALSE, FALSE, FALSE, FALSE)

  precision <- evaluate(mt_precision(), y_pred, y_true)
  recall <- evaluate(mt_recall(), y_pred, y_true)
  expect_false(isTRUE(all.equal(precision, recall)))

  fbeta <- function(beta) {
    (1 + beta^2) * precision * recall / ((beta^2 * precision) + recall)
  }

  expect_equal(evaluate(mt_fscore(f = 1), y_pred, y_true), fbeta(1))
  expect_equal(evaluate(mt_fscore(f = 2), y_pred, y_true), fbeta(2))
  expect_equal(evaluate(mt_fscore(f = 0.5), y_pred, y_true), fbeta(0.5))

  # and the three values must actually differ from one another
  values <- c(fbeta(0.5), fbeta(1), fbeta(2))
  expect_equal(length(unique(values)), 3)
})

test_that("mt_fscore returns NA instead of NaN when it is undefined", {
  expect_true(is.na(evaluate(mt_fscore(), c(FALSE, FALSE), c(FALSE, FALSE))))
})

test_that("mt_rocauc returns NA when the metric is undefined", {
  expect_warning(
    result <- evaluate(mt_rocauc(), c(1, 1, 1), factor(c(TRUE, FALSE, TRUE))),
    "undefined"
  )
  expect_true(is.na(result))
})
