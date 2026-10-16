# A deterministic classifier that predicts TRUE whenever the first feature is
# positive. It keeps the stealthy tests independent of any learning package.
sign_classifier <- function() {
  obj <- daltoolbox::dal_base()
  obj$slevels <- c("FALSE", "TRUE")
  class(obj) <- append("sign_classifier", class(obj))
  obj
}

registerS3method("fit", "sign_classifier", function(obj, data, ...) {
  obj$n_fit <- nrow(data)
  obj
}, envir = asNamespace("daltoolbox"))

registerS3method("predict", "sign_classifier", function(object, x, ...) {
  p <- as.numeric(x[[1]] > 0)
  cbind(`FALSE` = 1 - p, `TRUE` = p)
}, envir = asNamespace("stats"))

batch <- function(n, mean, seed) {
  set.seed(seed)
  data.frame(x = stats::rnorm(n, mean = mean))
}

labels_for <- function(x, flip = FALSE) {
  y <- x$x > 0
  data.frame(y = if (flip) !y else y)
}

test_that("stealthy returns an empty prediction before the warmup", {
  model <- stealthy(sign_classifier(), dfr_inactive(), warmup_size = 100)
  x <- batch(50, 0, 1)
  model <- fit(model, x, labels_for(x))
  expect_false(model$fitted)

  prediction <- predict(model, x)
  expect_equal(dim(prediction), c(50L, 2L))
  expect_equal(names(prediction), c("FALSE", "TRUE"))
})

test_that("stealthy trains once the warmup is reached", {
  model <- stealthy(sign_classifier(), dfr_inactive(), warmup_size = 100)
  for (seed in 1:2) {
    x <- batch(50, 0, seed)
    model <- fit(model, x, labels_for(x))
  }
  expect_true(model$fitted)
  expect_equal(model$model$n_fit, 100)
  expect_equal(nrow(predict(model, batch(10, 0, 3))), 10)
})

test_that("a distribution drift discards the old data and retrains", {
  model <- stealthy(sign_classifier(), dfr_page_hinkley(target_feat = "x"), warmup_size = 100)
  drifted <- logical(0)
  for (i in 1:6) {
    x <- batch(50, if (i <= 4) 0 else 10, i)
    model <- fit(model, x, labels_for(x))
    drifted <- c(drifted, model$drifted)
  }
  expect_false(any(drifted[1:3]))
  expect_true(any(drifted[5:6]))
  expect_lt(nrow(model$x_train), 300)
})

test_that("an error-based drift is driven by the model mistakes", {
  model <- stealthy(sign_classifier(), dfr_ddm(), warmup_size = 100, incremental_memory = FALSE)
  drifted <- logical(0)
  for (i in 1:8) {
    x <- batch(50, 0, i)
    model <- fit(model, x, labels_for(x, flip = i > 4))
    drifted <- c(drifted, model$drifted)
  }
  expect_false(any(drifted[1:4]))
  expect_true(any(drifted[5:8]))
})

test_that("stealthy one-hot encodes categorical features and tolerates new levels", {
  set.seed(5)
  x <- data.frame(
    x = stats::rnorm(120),
    colour = factor(sample(c("red", "blue"), 120, TRUE))
  )
  model <- fit(stealthy(sign_classifier(), dfr_inactive(), warmup_size = 100), x, labels_for(x))
  expect_true(model$fitted)
  expect_equal(model$dummy$feat_names, c("x", "colour.blue", "colour.red"))

  unseen <- data.frame(x = c(-1, 1), colour = factor(c("green", "red")))
  expect_equal(nrow(predict(model, unseen)), 2)
})

test_that("update_state on stealthy follows the obj/drift contract", {
  model <- stealthy(sign_classifier(), dfr_passive())
  output <- update_state(model, 1)
  expect_true(output$drift)
  expect_true(output$obj$drifted)
})

test_that("reporting accumulates the drifter inputs", {
  model <- stealthy(sign_classifier(), dfr_page_hinkley(target_feat = "x"), warmup_size = 100, reporting = TRUE)
  for (i in 1:3) {
    x <- batch(50, 0, i)
    model <- fit(model, x, labels_for(x))
  }
  expect_equal(nrow(model$drifter_input), 150)
  expect_equal(nrow(model$drifter_output), 150)
})

test_that("drifter_output is exposed for every kind of drifter", {
  drifters <- list(
    error_based = dfr_ddm(),
    dist_based = dfr_page_hinkley(target_feat = "x"),
    mv_dist_based = dfr_kldist(window_size = 20),
    multi_criteria = dfr_multi_criteria(list(ph = dfr_page_hinkley(target_feat = "x"))),
    dummy = dfr_inactive()
  )
  for (name in names(drifters)) {
    model <- stealthy(sign_classifier(), drifters[[name]], warmup_size = 100)
    for (i in 1:4) {
      x <- batch(50, 0, i)
      model <- fit(model, x, labels_for(x))
    }
    expect_equal(nrow(model$drifter_output), 200, info = name)
    # The drifter only runs once the model is fitted, after the warmup.
    expect_true(all(is.na(model$drifter_output[1:100, ])), info = name)
    expect_null(model$drifter_input)
    if (name != "dummy") {
      expect_gte(ncol(model$drifter_output), 1, label = name)
      expect_false(all(is.na(model$drifter_output[101:200, ])), info = name)
    }
  }
})

test_that("error-based drifter_output stays aligned with the rows of x", {
  model <- stealthy(sign_classifier(), dfr_ddm(), warmup_size = 100)
  for (i in 1:2) {
    x <- batch(50, 0, i)
    model <- fit(model, x, labels_for(x))
  }
  x <- batch(50, 0, 3)
  y <- labels_for(x)
  y$y[c(5, 10)] <- NA
  model <- fit(model, x, y)
  expect_equal(nrow(model$drifter_output), 150)
  expect_equal(which(is.na(model$drifter_output$miss_prob[101:150])), c(5L, 10L))
})

test_that("target_uni_drifter works with a factor target", {
  model <- stealthy(sign_classifier(), dfr_page_hinkley(target_feat = "x"),
    target_uni_drifter = TRUE,
    warmup_size = 100
  )
  for (i in 1:3) {
    x <- batch(50, 0, i)
    y <- data.frame(y = factor(x$x > 0, levels = c(FALSE, TRUE)))
    model <- fit(model, x, y)
  }
  expect_gt(model$drift_method$state$sample_count, 1)
})

test_that("a multivariate drifter monitors only its target_feat columns", {
  run <- function(target_feat) {
    set.seed(8)
    model <- stealthy(sign_classifier(), dfr_kldist(target_feat = target_feat, window_size = 40), warmup_size = 100)
    for (i in 1:3) {
      x <- data.frame(a = stats::rnorm(50), b = stats::rnorm(50))
      model <- fit(model, x, data.frame(y = x$a > 0))
    }
    model
  }
  expect_equal(colnames(run("a")$drift_method$state$window), "a")
  expect_equal(colnames(run(NULL)$drift_method$state$window), c("a", "b"))
})

test_that("a missing target_feat column is reported", {
  model <- stealthy(sign_classifier(), dfr_kldist(target_feat = "nope", window_size = 20), warmup_size = 50)
  expect_error(
    for (i in 1:3) {
      x <- batch(50, 0, i)
      model <- fit(model, x, labels_for(x))
    },
    "not in the data: nope"
  )
})

test_that("history_size bounds drifter_output and drifter_input", {
  model <- stealthy(sign_classifier(), dfr_page_hinkley(target_feat = "x"),
    warmup_size = 100,
    reporting = TRUE, history_size = 120
  )
  for (i in 1:4) {
    x <- batch(50, 0, i)
    model <- fit(model, x, labels_for(x))
  }
  expect_equal(nrow(model$drifter_output), 120)
  expect_equal(nrow(model$drifter_input), 120)
  expect_error(stealthy(sign_classifier(), dfr_inactive(), history_size = 0), "greater than or equal to 1")
})
