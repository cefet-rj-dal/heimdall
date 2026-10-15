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

test_that("reporting exposes the drift input", {
  model <- stealthy(sign_classifier(), dfr_page_hinkley(target_feat = "x"), warmup_size = 100, reporting = TRUE)
  for (i in 1:3) {
    x <- batch(50, 0, i)
    model <- fit(model, x, labels_for(x))
  }
  expect_equal(length(model$drift_input), 50)
})

test_that("target_uni_drifter works with a factor target", {
  model <- stealthy(sign_classifier(), dfr_page_hinkley(target_feat = "x"), target_uni_drifter = TRUE,
                    warmup_size = 100, reporting = TRUE)
  for (i in 1:3) {
    x <- batch(50, 0, i)
    y <- data.frame(y = factor(x$x > 0, levels = c(FALSE, TRUE)))
    model <- fit(model, x, y)
  }
  expect_true(all(model$drift_input %in% c(0, 1)))
  expect_gt(model$drift_method$state$sample_count, 1)
})

test_that("monitored_features restricts what the drifter receives", {
  set.seed(8)
  make_x <- function(shift) data.frame(a = stats::rnorm(50), b = stats::rnorm(50, mean = shift))
  run <- function(features) {
    model <- stealthy(sign_classifier(), dfr_page_hinkley(), monitored_features = features,
                      warmup_size = 100, reporting = TRUE)
    for (i in 1:2) {
      x <- make_x(0)
      model <- fit(model, x, data.frame(y = x$a > 0))
    }
    x <- make_x(5)
    fit(model, x, data.frame(y = x$a > 0))
  }
  only_a <- run("a")
  both <- run(NULL)
  expect_equal(only_a$drift_input, only_a$norm_x_oh$a)
  expect_false(isTRUE(all.equal(only_a$drift_input, both$drift_input)))
})

test_that("monitored_features must exist in the data", {
  model <- stealthy(sign_classifier(), dfr_inactive(), monitored_features = "nope")
  expect_error(fit(model, batch(10, 0, 1), data.frame(y = TRUE)), "not found in x: nope")
})
