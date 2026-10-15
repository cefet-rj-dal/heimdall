# A minimal autoencoder that reconstructs every row as the column means of the
# data it was trained on. It is enough to exercise the AEDD workflow without a
# deep learning backend.
mean_autoencoder <- function(input_size, encoding_size, batch_size, num_epochs, learning_rate) {
  obj <- daltoolbox::dal_base()
  obj$input_size <- input_size
  class(obj) <- append("mean_autoencoder", class(obj))
  obj
}

registerS3method("fit", "mean_autoencoder", function(obj, data, ...) {
  obj$means <- colMeans(as.data.frame(data))
  obj
}, envir = asNamespace("daltoolbox"))

registerS3method("transform", "mean_autoencoder", function(obj, data, ...) {
  data <- as.data.frame(data)
  as.data.frame(matrix(obj$means, nrow = nrow(data), ncol = length(obj$means), byrow = TRUE,
                       dimnames = list(NULL, names(data))))
}, envir = asNamespace("daltoolbox"))

aedd_stream <- function() {
  set.seed(10)
  stable <- data.frame(x = stats::runif(200, 0.4, 0.6), y = stats::runif(200, 0.4, 0.6))
  shifted <- data.frame(x = stats::runif(100, 0.85, 0.95), y = stats::runif(100, 0.85, 0.95))
  rbind(stable, shifted)
}

new_aedd <- function(...) {
  dfr_aedd(encoding_size = 1, ae_class = mean_autoencoder, window_size = 20, monitoring_step = 1, ...)
}

test_that("aedd follows the obj/drift contract", {
  output <- update_state(new_aedd(), data.frame(x = 0.5, y = 0.5))
  expect_true(all(c("obj", "drift") %in% names(output)))
  expect_false(output$drift)
})

test_that("aedd stays quiet on a stable stream and reacts to a shift", {
  stream <- aedd_stream()
  output <- list(obj = new_aedd(), drift = FALSE)
  flags <- logical(nrow(stream))
  for (i in seq_len(nrow(stream))) {
    output <- update_state(output$obj, stream[i, , drop = FALSE])
    flags[i] <- output$drift
  }
  expect_false(any(flags[1:200]))
  expect_true(any(flags[201:230]))
  expect_true(output$obj$drifted)
})

test_that("aedd keeps its window bounded", {
  model <- fit(new_aedd(), aedd_stream()[1:100, ])
  expect_lte(nrow(model$state$data), model$state$window_size)
})

test_that("aedd warns once when it has to clip values outside [0, 1]", {
  model <- new_aedd()
  expect_warning(
    output <- update_state(model, data.frame(x = 2, y = -1)),
    "clipped"
  )
  expect_equal(unlist(output$obj$state$data[1, ]), c(x = 1, y = 0))
  expect_warning(update_state(output$obj, data.frame(x = 3, y = 0.5)), NA)
})

test_that("aedd tolerates missing values when clipping", {
  expect_error(update_state(new_aedd(), data.frame(x = NA_real_, y = 0.5)), NA)
})

test_that("aedd reset clears the drift flag and keeps the configuration", {
  model <- fit(new_aedd(criteria = "kolmogorov_smirnov"), aedd_stream())
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_equal(model$state$criteria, "kolmogorov_smirnov")
  expect_equal(model$state$window_size, 20)
  expect_null(model$state$data)
})

test_that("every aedd criterion runs end to end", {
  stream <- aedd_stream()
  for (criteria in c("mann_whitney", "kolmogorov_smirnov", "levene",
                     "parametric_threshold", "nonparametric_threshold")) {
    expect_error(fit(new_aedd(criteria = criteria), stream), NA, info = criteria)
  }
})

test_that("aedd keeps its window bounded with the default monitoring step", {
  set.seed(9)
  data <- data.frame(x = stats::runif(1000, 0.4, 0.6), y = stats::runif(1000, 0.4, 0.6))
  model <- fit(dfr_aedd(encoding_size = 1, ae_class = mean_autoencoder), data)
  expect_equal(nrow(model$state$data), 100)
  expect_equal(model$state$monitoring_step, 50)
})

test_that("aedd reference and recent windows do not overlap", {
  model <- fit(new_aedd(), aedd_stream()[1:10, ])
  expect_equal(nrow(model$state$data), 10)

  model <- fit(dfr_aedd(encoding_size = 1, ae_class = mean_autoencoder, window_size = 10, monitoring_step = 1), data.frame(x = (1:10) / 10, y = 0.5))
  half <- floor(model$state$window_size / 2)
  reference <- utils::head(model$state$data, half)
  recent <- utils::tail(model$state$data, half)
  expect_length(intersect(reference[, "x"], recent[, "x"]), 0)
})

test_that("aedd retrains the autoencoder when new columns appear", {
  model <- fit(new_aedd(), aedd_stream()[1:30, ])
  expect_true(model$state$is_fitted)
  expect_warning(
    output <- update_state(model, data.frame(x = 0.5, y = 0.5, z = 0.5)),
    "Creating zero columns"
  )
  expect_equal(colnames(output$obj$state$data), c("x", "y", "z"))
})

test_that("aedd explains how to get the default autoencoder", {
  skip_if(requireNamespace("daltoolboxdp", quietly = TRUE), "daltoolboxdp is installed")
  expect_error(dfr_aedd(encoding_size = 1), "daltoolboxdp")
})

test_that("aedd uses daltoolboxdp::autoenc_ed by default", {
  skip_if_not_installed("daltoolboxdp")
  expect_identical(dfr_aedd(encoding_size = 1)$ae_class, daltoolboxdp::autoenc_ed)
})
