test_that("dfr_kldist accepts the deprecated p_th with a warning", {
  expect_warning(model <- dfr_kldist(p_th = 0.3), "deprecated")
  expect_equal(model$state$threshold, 0.3)
  expect_equal(dfr_kldist()$state$threshold, 0.12)
})

test_that("dfr_kldist monitors only its target_feat columns", {
  set.seed(4)
  frame <- data.frame(a = stats::rnorm(400), b = stats::rnorm(400))
  frame$b[201:400] <- frame$b[201:400] + 5
  expect_length(detect_indices(dfr_kldist(target_feat = "a"), frame), 0)
  expect_true(length(detect_indices(dfr_kldist(target_feat = "b"), frame)) > 0)
  expect_equal(colnames(fit(dfr_kldist(target_feat = "a"), frame)$state$window), "a")
})

test_that("multivariate target_feat is validated", {
  expect_error(dfr_kldist(target_feat = character(0)), "vector of column names")
  expect_error(dfr_kldist(target_feat = NA_character_), "vector of column names")
  expect_error(fit(dfr_kldist(target_feat = "z"), data.frame(a = 1:3)), "not in the data: z")
})

test_that("dfr_aedd separates alpha from threshold and selects target_feat", {
  identity_ae <- function(input_size, encoding_size, ...) {
    obj <- daltoolbox::dal_base()
    class(obj) <- append(c("identity_ae", "autoenc_ed"), class(obj))
    obj
  }
  model <- dfr_aedd(
    encoding_size = 1, ae_class = identity_ae, criteria = "psi", threshold = 0.5,
    target_feat = c("a", "b")
  )
  expect_equal(model$threshold, 0.5)
  expect_equal(model$alpha, 0.01)
  expect_error(dfr_aedd(encoding_size = 1, ae_class = identity_ae, alpha = 2), "between 0 and 1")

  output <- update_state(model, data.frame(a = 0.1, b = 0.2, c = 0.3))
  expect_equal(names(output$obj$state$data), c("a", "b"))
})

test_that("nrm_memory keeps at most max_size rows", {
  model <- fit(nrm_memory(daltoolbox::minmax(), max_size = 3), data.frame(a = 1:5))
  expect_equal(model$data$a, 3:5)
  model <- fit(model, data.frame(a = 6:7))
  expect_equal(model$data$a, 5:7)
  expect_error(nrm_memory(max_size = 0), "greater than or equal to 1")
})

test_that("kswin does not change the global random state while it runs", {
  model <- dfr_kswin(window_size = 40, stat_size = 10, seed = 1)
  stream <- stats::rnorm(200)
  set.seed(5)
  expected <- stats::runif(1)
  set.seed(5)
  model <- fit(model, stream)
  expect_equal(stats::runif(1), expected)
})

test_that("kswin is reproducible through its seed", {
  stream <- c(stats::rnorm(150), stats::rnorm(150, 1))
  first <- fit(dfr_kswin(window_size = 60, stat_size = 20, seed = 3), stream)$drifter_output
  second <- fit(dfr_kswin(window_size = 60, stat_size = 20, seed = 3), stream)$drifter_output
  expect_identical(first, second)
})
