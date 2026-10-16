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

test_that("precision and recall follow the usual definitions", {
  y_pred <- c(TRUE, TRUE, FALSE, TRUE, FALSE)
  y_true <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
  expect_equal(evaluate(mt_precision(), y_pred, y_true), 2 / 3)
  expect_equal(evaluate(mt_recall(), y_pred, y_true), 2 / 3)
  expect_equal(evaluate(mt_recall(), c(1, 0, 0, 1), c(1, 1, 1, 1)), 0.5)
})

test_that("precision is NaN when nothing is predicted as positive", {
  expect_true(is.nan(evaluate(mt_precision(), c(FALSE, FALSE), c(TRUE, FALSE))))
})

test_that("binary metrics accept factor and character labels", {
  y_pred <- c(TRUE, TRUE, FALSE, FALSE)
  y_true <- c(TRUE, FALSE, TRUE, FALSE)
  as_factor <- function(v) factor(v, levels = c(FALSE, TRUE))

  for (metric in list(mt_precision(), mt_recall(), mt_fscore(), mt_accuracy())) {
    reference <- evaluate(metric, y_pred, y_true)
    expect_equal(evaluate(metric, y_pred, as_factor(y_true)), reference)
    expect_equal(evaluate(metric, as_factor(y_pred), y_true), reference)
    expect_equal(evaluate(metric, as.character(y_pred), as.character(y_true)), reference)
    expect_equal(evaluate(metric, as.numeric(y_pred), as_factor(y_true)), reference)
  }
})

test_that("binary metrics reject labels that are not binary", {
  expect_error(evaluate(mt_precision(), c("a", "b"), c("a", "a")), "binary labels")
})

test_that("accuracy also works with multiclass labels", {
  expect_equal(evaluate(mt_accuracy(), factor(c("a", "b", "c")), c("a", "b", "b")), 2 / 3)
})

test_that("metrics refuse vectors of different lengths", {
  expect_error(evaluate(mt_accuracy(), c(TRUE, FALSE), c(TRUE, FALSE, TRUE, FALSE)), "has 2 values")
  expect_error(evaluate(mt_rocauc(), c(0.1, 0.2), c(TRUE, FALSE, TRUE)), "has 2 values")
})

test_that("rocauc drops missing scores instead of treating them as zero", {
  y_true <- factor(c(TRUE, TRUE, FALSE, FALSE))
  expect_warning(
    result <- evaluate(mt_rocauc(), c(NA, 0.9, 0.2, 0.1), y_true),
    "1 observations with missing values"
  )
  expect_equal(result, 1)
})
