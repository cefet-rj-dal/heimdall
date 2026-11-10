# A minimal autoencoder that reconstructs every row as the column means of the
# data it was trained on. It is enough to exercise the AEDD workflow without a
# deep learning backend.
mean_autoencoder <- function(input_size, encoding_size, batch_size, num_epochs, learning_rate) {
  obj <- daltoolbox::dal_base()
  obj$input_size <- input_size
  class(obj) <- append(c("mean_autoencoder", "autoenc_ed"), class(obj))
  obj
}

registerS3method("fit", "mean_autoencoder", function(obj, data, ...) {
  obj$means <- colMeans(as.data.frame(data))
  obj
}, envir = asNamespace("daltoolbox"))

registerS3method("transform", "mean_autoencoder", function(obj, data, ...) {
  data <- as.data.frame(data)
  as.data.frame(matrix(obj$means,
    nrow = nrow(data), ncol = length(obj$means), byrow = TRUE,
    dimnames = list(NULL, names(data))
  ))
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
  # A 20-row window compares only 10 reconstruction errors per half, which is
  # too few to keep 180 tests at alpha = 0.01 free of false alarms.
  model <- dfr_aedd(encoding_size = 1, ae_class = mean_autoencoder, window_size = 40, monitoring_step = 1)
  output <- list(obj = model, drift = FALSE)
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
  for (criteria in c(
    "mann_whitney", "kolmogorov_smirnov", "levene", "psi", "diff_threshold",
    "parametric_threshold", "nonparametric_threshold"
  )) {
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

test_that("aedd reports one drifter_output row per observation", {
  model <- fit(new_aedd(), aedd_stream()[1:30, ])
  expect_equal(nrow(model$drifter_output), 30)
  expect_true(all(is.na(model$drifter_output[1:19, ])))
  expect_false(anyNA(model$drifter_output[20:30, ]))
})

test_that("aedd runs end to end with the default daltoolboxdp autoencoder", {
  skip_on_cran()
  skip_if_not_installed("daltoolboxdp")
  skip_if_not_installed("reticulate")
  skip_if_not(reticulate::py_module_available("torch"), "the Python backend of daltoolboxdp is not available")

  set.seed(11)
  stable <- data.frame(x = stats::runif(120, 0.4, 0.6), y = stats::runif(120, 0.4, 0.6))
  shifted <- data.frame(x = stats::runif(80, 0.85, 0.95), y = stats::runif(80, 0.85, 0.95))
  model <- dfr_aedd(
    encoding_size = 1,
    ae_params = list(batch_size = 16, num_epochs = 50, learning_rate = 0.01),
    window_size = 40,
    monitoring_step = 5
  )
  model <- fit(model, rbind(stable, shifted))
  expect_true(model$drifted)
  expect_equal(nrow(model$drifter_output), 200)
  # Output is reported on the evaluated rows, every monitoring_step rows.
  expect_gt(sum(!is.na(model$drifter_output[[1]])), 0)

  # torch leaves a compilation cache in the temporary directory.
  unlink(Sys.glob(file.path(dirname(tempdir()), "torchinductor_*")), recursive = TRUE)
})

# A minimal encoder whose latent representation is the input itself, so every
# input column becomes one latent dimension.
identity_encoder <- function(input_size, encoding_size, batch_size, num_epochs, learning_rate) {
  obj <- daltoolbox::dal_base()
  class(obj) <- append(c("identity_encoder", "autoenc_e"), class(obj))
  obj
}

registerS3method("fit", "identity_encoder", function(obj, data, ...) obj, envir = asNamespace("daltoolbox"))

registerS3method("transform", "identity_encoder", function(obj, data, ...) {
  as.data.frame(data)
}, envir = asNamespace("daltoolbox"))

# Four latent dimensions; only the first one shifts after row 200.
encoder_stream <- function() {
  set.seed(21)
  data <- as.data.frame(matrix(stats::runif(300 * 4, 0.4, 0.6), ncol = 4))
  data$V1[201:300] <- stats::runif(100, 0.85, 0.95)
  data
}

new_encoder_aedd <- function(...) {
  dfr_aedd(encoding_size = 4, ae_class = identity_encoder, window_size = 40, monitoring_step = 1, ...)
}

test_that("aedd tests every latent dimension separately for every hypothesis test", {
  stream <- encoder_stream()
  for (criteria in c("mann_whitney", "kolmogorov_smirnov")) {
    model <- fit(new_encoder_aedd(criteria = criteria), stream)
    expect_true(model$drifted, info = criteria)
  }
})

test_that("aedd column_threshold signals a drift only above the changed-column proportion", {
  stream <- encoder_stream()
  # One changed dimension out of four is 25%.
  quiet <- fit(new_encoder_aedd(criteria = "mann_whitney", column_threshold = 0.25), stream)
  expect_false(quiet$drifted)
  loud <- fit(new_encoder_aedd(criteria = "mann_whitney", column_threshold = 0.2), stream)
  expect_true(loud$drifted)
})

test_that("aedd column flags count the changed columns", {
  history <- data.frame(a = (1:20) / 100, b = (1:20) / 100, c = (1:20) / 100)
  recent <- data.frame(a = (1:20) / 100 + 0.5, b = (1:20) / 100 + 0.5, c = (1:20) / 100)
  flags <- heimdall:::.aedd_column_flags(history, recent, "diff_threshold", alpha = 0.01, threshold = 0.1, breaks = 4)
  expect_equal(flags, c(TRUE, TRUE, FALSE))
  flags <- heimdall:::.aedd_column_flags(history, recent, "mann_whitney", alpha = 0.01, threshold = 0.1, breaks = 4)
  expect_equal(flags, c(TRUE, TRUE, FALSE))
})

test_that("every aedd criterion runs end to end with column_threshold", {
  stream <- encoder_stream()
  for (criteria in c(
    "mann_whitney", "kolmogorov_smirnov", "levene", "psi", "diff_threshold",
    "parametric_threshold", "nonparametric_threshold"
  )) {
    expect_error(fit(new_encoder_aedd(criteria = criteria, column_threshold = 0.5), stream), NA, info = criteria)
  }
})

test_that("aedd validates and keeps column_threshold", {
  expect_error(new_aedd(column_threshold = 50), "column_threshold")
  model <- reset_state(fit(new_aedd(column_threshold = 0.5), aedd_stream()[1:30, ]))
  expect_equal(model$column_threshold, 0.5)
})

test_that("aedd rmse is the root of the mean squared reconstruction error", {
  # Errors of 0 and 0.4 on the two columns: MAE = 0.2, RMSE = sqrt(0.08).
  constant_autoencoder <- function(input_size, encoding_size, batch_size, num_epochs, learning_rate) {
    obj <- daltoolbox::dal_base()
    class(obj) <- append(c("constant_autoencoder", "autoenc_ed"), class(obj))
    obj
  }
  registerS3method("fit", "constant_autoencoder", function(obj, data, ...) obj, envir = asNamespace("daltoolbox"))
  registerS3method("transform", "constant_autoencoder", function(obj, data, ...) {
    data <- as.data.frame(data)
    data[[2]] <- data[[2]] + 0.4
    data
  }, envir = asNamespace("daltoolbox"))

  captured <- NULL
  local_mocked_bindings(
    .kldist_divergence = function(history, recent, breaks) {
      captured <<- history
      0
    }
  )
  data <- data.frame(x = rep(0.5, 10), y = rep(0.5, 10))
  for (error_function in c("mae", "rmse")) {
    model <- dfr_aedd(
      encoding_size = 1, ae_class = constant_autoencoder, window_size = 10, monitoring_step = 10,
      criteria = "psi", ae_params = list(batch_size = 1, num_epochs = 1, learning_rate = 0.1, error_function = error_function)
    )
    fit(model, data)
    expected <- if (error_function == "mae") 0.2 else sqrt(0.08)
    expect_equal(unique(captured[[1]]), expected, info = error_function)
  }
})
