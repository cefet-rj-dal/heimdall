#'@title Autoencoder-Based Drift Detection method
#'@description AEDD is an unsupervised multivariate detector that compares reconstruction errors produced by an autoencoder on reference and recent windows. Because it monitors changes in the input distribution rather than classifier performance, this implementation is primarily aimed at **virtual concept drift**. The method follows Kaminskyi, Li, and Muller (2022) <doi:10.1109/ICDMW58026.2022.00109>.
#'@param encoding_size Encoding Size
#'@param ae_class Autoencoder constructor. When `NULL` (the default), `daltoolboxdp::autoenc_ed` is used.
#'@param ae_params Named list of arguments forwarded to `ae_class`, plus `error_function`, which selects how the reconstruction error is summarised. One of `"mae"` (default) or `"rmse"`.
#'@param window_size Size of the most recent data to be used
#'@param window_type Window policy. `"sliding"` (default) keeps at most `window_size` rows, so both halves move with the stream. `"anchored"` keeps every row, which pins the reference half to the beginning of the stream and makes memory grow with the stream length.
#'@param monitoring_step The number of rows that the drifter waits to be is updated
#'@param criteria The method used to check for a drift. One of `"mann_whitney"` (default), `"kolmogorov_smirnov"`, `"levene"`, `"psi"`, `"parametric_threshold"`, `"nonparametric_threshold"` or `"diff_threshold"`.
#'@param alpha Decision threshold for `criteria`. For the hypothesis tests it is a significance level; for `"psi"` and `"diff_threshold"` it is compared directly against the statistic, so it is a raw threshold rather than a probability.
#'@details The `"levene"` criterion tests every reconstructed feature and
#'combines the per-feature p-values with a Bonferroni correction, instead of
#'looking at the first feature only.
#AEDD detection: Daniil Kaminskyi, Bin Li and Emmanuel Muller. "Reconstruction-based unsupervised drift detection over multivariate streaming data." 2022 IEEE International Conference on Data Mining Workshops (ICDMW).
#'@references Kaminskyi, D., Li, B., and Muller, E. (2022). Reconstruction-based unsupervised drift detection over multivariate streaming data. In *2022 IEEE International Conference on Data Mining Workshops (ICDMW)*. <doi:10.1109/ICDMW58026.2022.00109>
#'@return `dfr_aedd` object
#'@importFrom stats ks.test median quantile sd wilcox.test
#'@importFrom utils head tail
#'@examples
#'# Training an autoencoder is too slow for an inline example. A rendered
#'# walkthrough is available at
#'# https://github.com/cefet-rj-dal/heimdall/blob/main/examples/1_detection/dfr_aedd.md
#'@export
dfr_aedd <- function(encoding_size, ae_class=NULL, ae_params=list(batch_size = 32, num_epochs = 1000, learning_rate = 0.001, error_function = "mae"), window_size=100, window_type="sliding", monitoring_step=1700, criteria="mann_whitney", alpha=0.01) {
  criteria <- match.arg(
    criteria,
    c('mann_whitney', 'kolmogorov_smirnov', 'levene', 'psi',
      'parametric_threshold', 'nonparametric_threshold', 'diff_threshold')
  )
  .check_positive_integer(encoding_size, "encoding_size", min_value = 1L)
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  window_type <- .check_window_type(window_type)

  error_function <- ae_params[['error_function']]
  if (is.null(error_function)) {
    error_function <- 'mae'
  }
  error_function <- match.arg(error_function, c('mae', 'rmse'))
  ae_params[['error_function']] <- NULL

  if (is.null(ae_class)) {
    ae_class <- .default_autoencoder()
  }

  obj <- mv_dist_based()

  obj$ae_class <- ae_class
  obj$error_function <- error_function
  obj$ae_params <- ae_params
  obj$alpha <- alpha
  obj$encoding_size <- encoding_size
  obj$window_type <- window_type

  state <- list()

  state$window_size <- window_size
  state$window_type <- window_type
  state$monitoring_step <- monitoring_step
  state$criteria <- criteria
  state$data <- NULL
  state$n <- 0

  state$autoencoder <- NULL
  state$is_fitted <- FALSE

  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL

  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_aedd", class(obj))
  return(obj)
}

# Resolves the default autoencoder lazily so that a change in daltoolboxdp
# surfaces as an actionable message instead of an obscure lookup failure.
#' @noRd
.default_autoencoder <- function() {
  tryCatch(
    getExportedValue("daltoolboxdp", "autoenc_ed"),
    error = function(e) {
      stop(
        "dfr_aedd(): the default autoencoder 'daltoolboxdp::autoenc_ed' is not available. Pass an explicit 'ae_class'.",
        call. = FALSE
      )
    }
  )
}

#' @noRd
.aedd_build_autoencoder <- function(obj, input_size) {
  do.call(obj$ae_class, c(list(input_size = input_size, encoding_size = obj$encoding_size), obj$ae_params))
}

#'@export
update_state.dfr_aedd <- function(obj, value, ...) {
  state <- obj$state
  obj$last_drifter_output <- NULL
  has_drift <- FALSE

  value <- as.data.frame(value)
  numeric_cols <- vapply(value, is.numeric, logical(1))
  value[numeric_cols] <- lapply(value[numeric_cols], function(v) pmin(pmax(v, 0), 1))

  if (!is.null(state$data)) {
    missing_in_history <- setdiff(names(value), names(state$data))
    if (length(missing_in_history) > 0L) {
      warning('dfr_aedd::update_state: Some categories present in most recent data are not on the history dataset. Creating zero columns.')
      state$data[missing_in_history] <- 0
    }
    missing_in_recent <- setdiff(names(state$data), names(value))
    if (length(missing_in_recent) > 0L) {
      value[missing_in_recent] <- 0
    }
    value <- value[names(state$data)]
  }

  state$data <- .trim_window(rbind(state$data, value), state$window_size, state$window_type)
  rownames(state$data) <- seq_len(nrow(state$data))

  state$n <- state$n + 1
  if (state$n >= state$monitoring_step) {
    state$n <- 0
  } else {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  if (nrow(state$data) < state$window_size) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  half <- floor(state$window_size / 2)
  reference_window <- utils::tail(state$data, state$window_size)
  history_window <- utils::head(reference_window, half)
  recent_window <- utils::tail(reference_window, half)

  if (!state$is_fitted) {
    state$autoencoder <- .aedd_build_autoencoder(obj, input_size = ncol(state$data))
    state$autoencoder <- fit(state$autoencoder, history_window)
    state$is_fitted <- TRUE
  }

  history_window_output <- transform(state$autoencoder, history_window)
  recent_window_output <- transform(state$autoencoder, recent_window)

  if (any(c('autoenc_ed', 'autoenc_variational_ed') %in% class(state$autoencoder))) {
    history_rec_marker <- history_window_output - history_window
    recent_rec_marker <- recent_window_output - recent_window
    obj$last_drifter_output <- as.data.frame(utils::tail(recent_rec_marker, 1))

    if (obj$error_function == 'rmse') {
      history_rec_marker <- as.data.frame(rowMeans(sqrt(as.matrix(history_rec_marker)^2)))
      recent_rec_marker <- as.data.frame(rowMeans(sqrt(as.matrix(recent_rec_marker)^2)))
    } else {
      history_rec_marker <- as.data.frame(rowMeans(abs(as.matrix(history_rec_marker))))
      recent_rec_marker <- as.data.frame(rowMeans(abs(as.matrix(recent_rec_marker))))
    }
  } else if (any(c('autoenc_e', 'autoenc_variational_e') %in% class(state$autoencoder))) {
    history_rec_marker <- as.data.frame(history_window_output)
    recent_rec_marker <- as.data.frame(recent_window_output)
    obj$last_drifter_output <- as.data.frame(utils::tail(recent_rec_marker, 1))
  } else {
    stop(
      "dfr_aedd(): unsupported autoencoder class. Expected one of autoenc_ed, autoenc_variational_ed, autoenc_e or autoenc_variational_e.",
      call. = FALSE
    )
  }

  names(history_rec_marker) <- paste0("V", seq_len(ncol(history_rec_marker)))
  names(recent_rec_marker) <- paste0("V", seq_len(ncol(recent_rec_marker)))

  history_values <- unlist(as.vector(t(history_rec_marker)), use.names = FALSE)
  recent_values <- unlist(as.vector(t(recent_rec_marker)), use.names = FALSE)

  if (state$criteria == 'mann_whitney') {
    p_value <- suppressWarnings(stats::wilcox.test(history_values, recent_values)$p.value)
    has_drift <- !is.na(p_value) && (p_value < obj$alpha)

  } else if (state$criteria == 'kolmogorov_smirnov') {
    p_value <- suppressWarnings(stats::ks.test(history_values, recent_values)$p.value)
    has_drift <- !is.na(p_value) && (p_value < obj$alpha)

  } else if (state$criteria == 'levene') {
    p_value <- .levene_pvalue_mv(history_rec_marker, recent_rec_marker)
    has_drift <- !is.na(p_value) && (p_value < obj$alpha)

  } else if (state$criteria == 'psi') {
    breaks <- max(2, floor(state$window_size / 5))
    divergence <- .kldist_divergence(history_rec_marker, recent_rec_marker, breaks)
    has_drift <- !is.na(divergence) && (divergence >= obj$alpha)

  } else if (state$criteria == 'parametric_threshold') {
    mean_history <- abs(mean(apply(history_rec_marker, 2, mean)))
    sd_history <- abs(mean(apply(history_rec_marker, 2, stats::sd)))
    mean_recent <- abs(mean(apply(recent_rec_marker, 2, mean)))

    has_drift <- mean_recent >= (mean_history + (3 * sd_history))

  } else if (state$criteria == 'nonparametric_threshold') {
    top_limit <- as.vector(stats::quantile(history_values, 0.99))
    median_recent <- abs(stats::median(apply(recent_rec_marker, 2, stats::median)))

    has_drift <- median_recent >= top_limit

  } else if (state$criteria == 'diff_threshold') {
    error_diff <- stats::median(as.matrix(recent_rec_marker) - as.matrix(history_rec_marker))
    has_drift <- !is.na(error_diff) && (error_diff >= obj$alpha)
  }

  if (isTRUE(has_drift)) {
    obj$drifted <- TRUE
    state$is_fitted <- FALSE
  }

  obj$state <- state
  return(list(obj = obj, drift = isTRUE(has_drift)))
}

#'@export
fit.dfr_aedd <- function(obj, data, ...) {
  state <- obj$state
  data <- as.data.frame(data)

  if (!is.null(state$data) && (nrow(state$data) > 0L)) {
    missing_in_history <- setdiff(names(data), names(state$data))
    if (length(missing_in_history) > 0L) {
      warning('dfr_aedd: Some categories present in most recent data are not on the history dataset. Creating zero columns.')
      state$data[missing_in_history] <- 0
    }
    missing_in_recent <- setdiff(names(state$data), names(data))
    if (length(missing_in_recent) > 0L) {
      warning('dfr_aedd: Some categories present in history data are not on the most recent dataset. Creating zero columns.')
      data[missing_in_recent] <- 0
    }
    missing_in_recent <- setdiff(names(state$data), names(data))
    if (length(missing_in_recent) > 0L) {
      warning('dfr_aedd: Some categories present in history data are not on the most recent dataset. Creating zero columns.')
      data[missing_in_recent] <- 0
    }
  }

  obj$state <- state

  return(.fit_row_stream(obj, data))
}

#'@export
reset_state.dfr_aedd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_aedd(
    encoding_size = obj$encoding_size,
    ae_class = obj$ae_class,
    ae_params = c(obj$ae_params, list(error_function = obj$error_function)),
    window_size = obj$state$window_size,
    window_type = obj$state$window_type,
    monitoring_step = obj$state$monitoring_step,
    criteria = obj$state$criteria,
    alpha = obj$alpha
  )$state
  return(obj)
}
