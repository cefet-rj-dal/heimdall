#' @title Autoencoder-Based Drift Detection method
#' @description AEDD is an unsupervised multivariate detector that compares reconstruction errors produced by an autoencoder on reference and recent windows. Because it monitors changes in the input distribution rather than classifier performance, this implementation is primarily aimed at **virtual concept drift**. The method follows Kaminskyi, Li, and Muller (2022) <doi:10.1109/ICDMW58026.2022.00109>.
#' @param encoding_size Encoding Size
#' @param ae_class Autoencoder constructor. When `NULL` (the default),
#' `daltoolboxdp::autoenc_ed` is used, which requires the suggested package
#' `daltoolboxdp` and its Python backend.
#' @param ae_params Named list of arguments forwarded to `ae_class`, plus `error_function`, which selects how the reconstruction error is summarised. One of `"mae"` (default) or `"rmse"`.
#' @param window_size Number of most recent rows kept by the detector. The
#' oldest half is the reference window and the newest half the recent window.
#' @param window_type Window policy. `"sliding"` (default) keeps at most `window_size` rows, so both halves move with the stream. `"anchored"` keeps every row, which pins the reference half to the beginning of the stream and makes memory grow with the stream length.
#' @param monitoring_step Number of rows between two consecutive checks. The
#' default checks twice per window.
#' @param criteria The method used to check for a drift. One of `"mann_whitney"` (default), `"kolmogorov_smirnov"`, `"levene"`, `"psi"`, `"parametric_threshold"`, `"nonparametric_threshold"` or `"diff_threshold"`.
#' @param alpha Significance level of the hypothesis-test criteria (`"mann_whitney"`, `"kolmogorov_smirnov"` and `"levene"`).
#' @param threshold Drift threshold of the `"psi"` and `"diff_threshold"` criteria, compared directly against the divergence or the median error difference. It is a raw threshold, not a probability.
#' @param column_threshold `NULL` (the default) or a proportion between 0 and 1. When set, the criterion is evaluated separately on every monitored column (one per latent dimension for an encoder, a single one for an encoder-decoder), and a drift is signalled only when the proportion of columns flagged as changed is strictly greater than `column_threshold`. For example, with `column_threshold = 0.5`, 3 changed columns out of 5 (60%) signal a drift. The hypothesis tests flag a column when its uncorrected p-value is below `alpha`.
#' @param target_feat Names of the columns to be monitored. `NULL` (the default) monitors every numeric column.
#' @details The autoencoder expects inputs in the `[0, 1]` range. Values
#' outside that range are clipped to it, and the detector warns the first time
#' this happens, so the data should be normalized beforehand (for example with
#' `daltoolbox::minmax()`). `ae_class` must be a constructor accepting the
#' arguments `input_size` and `encoding_size` plus the entries of `ae_params`,
#' whose objects implement `fit()` and `transform()`.
#'
#' The autoencoder is trained on the reference window the first time the
#' window is full, and again after every drift or whenever new columns appear.
#'
#' With an encoder-decoder, the reconstruction error of each row is summarised
#' into a single value, so every criterion sees one column. With an encoder,
#' the latent representation is monitored directly and there is one column per
#' latent dimension (`encoding_size`).
#'
#' When `column_threshold` is `NULL`, the hypothesis-test criteria
#' (`"mann_whitney"`, `"kolmogorov_smirnov"` and `"levene"`) test every column
#' and combine the per-column p-values with a Bonferroni correction, instead of
#' pooling all columns into a single sample.
# AEDD detection: Daniil Kaminskyi, Bin Li and Emmanuel Muller. "Reconstruction-based unsupervised drift detection over multivariate streaming data." 2022 IEEE International Conference on Data Mining Workshops (ICDMW).
#' @references Kaminskyi, D., Li, B., and Muller, E. (2022). Reconstruction-based unsupervised drift detection over multivariate streaming data. In *2022 IEEE International Conference on Data Mining Workshops (ICDMW)*. <doi:10.1109/ICDMW58026.2022.00109>
#' @return `dfr_aedd` object
#' @importFrom stats ks.test median quantile sd wilcox.test
#' @importFrom utils head tail
#' @examples
#' # Training an autoencoder is too slow for an inline example. A rendered
#' # walkthrough is available at
#' # https://github.com/cefet-rj-dal/heimdall/blob/main/examples/1_detection/dfr_aedd.md
#' @export
dfr_aedd <- function(encoding_size, ae_class = NULL, ae_params = list(batch_size = 32, num_epochs = 1000, learning_rate = 0.001, error_function = "mae"), window_size = 100, window_type = "sliding", monitoring_step = floor(window_size / 2), criteria = "mann_whitney", alpha = 0.01, threshold = 0.01, column_threshold = NULL, target_feat = NULL) {
  criteria <- match.arg(
    criteria,
    c(
      "mann_whitney", "kolmogorov_smirnov", "levene", "psi",
      "parametric_threshold", "nonparametric_threshold", "diff_threshold"
    )
  )
  .check_probability(alpha, "alpha")
  .check_number(threshold, "threshold", min_value = 0)
  if (!is.null(column_threshold)) {
    .check_probability(column_threshold, "column_threshold")
  }
  .check_positive_integer(encoding_size, "encoding_size", min_value = 1L)
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  window_type <- .check_window_type(window_type)

  error_function <- ae_params[["error_function"]]
  if (is.null(error_function)) {
    error_function <- "mae"
  }
  error_function <- match.arg(error_function, c("mae", "rmse"))
  ae_params[["error_function"]] <- NULL

  if (is.null(ae_class)) {
    ae_class <- .default_autoencoder()
  }

  obj <- mv_dist_based(target_feat = target_feat)

  obj$ae_class <- ae_class
  obj$error_function <- error_function
  obj$ae_params <- ae_params
  obj$alpha <- alpha
  obj$threshold <- threshold
  obj$column_threshold <- column_threshold
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

# Resolves the default autoencoder constructor lazily, so that the suggested
# backend is only required when the default is actually used.
#' @noRd
.default_autoencoder <- function() {
  if (!requireNamespace("daltoolboxdp", quietly = TRUE)) {
    stop(
      "dfr_aedd(): the default autoencoder needs the 'daltoolboxdp' package. Install it or pass an explicit 'ae_class'.",
      call. = FALSE
    )
  }
  return(getExportedValue("daltoolboxdp", "autoenc_ed"))
}

#' @noRd
.aedd_build_autoencoder <- function(obj, input_size) {
  do.call(obj$ae_class, c(list(input_size = input_size, encoding_size = obj$encoding_size), obj$ae_params))
}

#' @export
update_state.dfr_aedd <- function(obj, value, ...) {
  state <- obj$state
  obj$last_drifter_output <- NULL
  has_drift <- FALSE

  value <- as.data.frame(as.list(.select_features(value, obj$target_feat)), check.names = FALSE)
  out_of_range <- !is.na(value) & ((value > 1) | (value < 0))
  if (any(out_of_range)) {
    if (!isTRUE(obj$warned_clipping)) {
      warning(
        "dfr_aedd: values outside [0, 1] were clipped. Normalize the data (e.g., with daltoolbox::minmax()) before feeding the detector.",
        call. = FALSE
      )
      obj$warned_clipping <- TRUE
    }
    value[] <- lapply(value, function(v) pmin(pmax(v, 0), 1))
  }

  if (!is.null(state$data)) {
    missing_in_history <- setdiff(names(value), names(state$data))
    if (length(missing_in_history) > 0L) {
      warning("dfr_aedd::update_state: Some categories present in most recent data are not on the history dataset. Creating zero columns.")
      state$data[missing_in_history] <- 0
      state$is_fitted <- FALSE
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

  # Reconstruction (encoder-decoder) or latent space (encoder) output. Every
  # daltoolboxdp autoencoder inherits from autoenc_base_ed or autoenc_base_e
  # (dense, denoising, adversarial, variational, LSTM, ...).
  if (any(c("autoenc_base_ed", "autoenc_ed", "autoenc_variational_ed") %in% class(state$autoencoder))) {
    history_rec_marker <- history_window_output - history_window
    recent_rec_marker <- recent_window_output - recent_window
    obj$last_drifter_output <- as.data.frame(utils::tail(recent_rec_marker, 1))

    if (obj$error_function == "rmse") {
      history_rec_marker <- as.data.frame(sqrt(rowMeans(as.matrix(history_rec_marker)^2)))
      recent_rec_marker <- as.data.frame(sqrt(rowMeans(as.matrix(recent_rec_marker)^2)))
    } else {
      history_rec_marker <- as.data.frame(rowMeans(abs(as.matrix(history_rec_marker))))
      recent_rec_marker <- as.data.frame(rowMeans(abs(as.matrix(recent_rec_marker))))
    }
  } else if (any(c("autoenc_base_e", "autoenc_e", "autoenc_variational_e") %in% class(state$autoencoder))) {
    history_rec_marker <- as.data.frame(history_window_output)
    recent_rec_marker <- as.data.frame(recent_window_output)
    obj$last_drifter_output <- as.data.frame(utils::tail(recent_rec_marker, 1))
  } else {
    stop(
      "dfr_aedd(): unsupported autoencoder class. Expected an encoder-decoder (autoenc_base_ed) or an encoder (autoenc_base_e) autoencoder.",
      call. = FALSE
    )
  }

  names(history_rec_marker) <- paste0("V", seq_len(ncol(history_rec_marker)))
  names(recent_rec_marker) <- paste0("V", seq_len(ncol(recent_rec_marker)))

  hypothesis_tests <- c("mann_whitney", "kolmogorov_smirnov", "levene")
  breaks <- max(2, floor(state$window_size / 5))

  if (!is.null(obj$column_threshold)) {
    column_flags <- .aedd_column_flags(
      history_rec_marker, recent_rec_marker, state$criteria,
      alpha = obj$alpha, threshold = obj$threshold, breaks = breaks
    )
    evaluated <- column_flags[!is.na(column_flags)]
    has_drift <- (length(evaluated) > 0L) && (mean(evaluated) > obj$column_threshold)
  } else if (state$criteria %in% hypothesis_tests) {
    p_value <- .bonferroni_pvalue(.column_pvalues(history_rec_marker, recent_rec_marker, state$criteria))
    has_drift <- !is.na(p_value) && (p_value < obj$alpha)
  } else if (state$criteria == "psi") {
    divergence <- .kldist_divergence(history_rec_marker, recent_rec_marker, breaks)
    has_drift <- !is.na(divergence) && (divergence >= obj$threshold)
  } else if (state$criteria == "parametric_threshold") {
    mean_history <- abs(mean(apply(history_rec_marker, 2, mean, na.rm = TRUE)))
    sd_history <- abs(mean(apply(history_rec_marker, 2, stats::sd, na.rm = TRUE)))
    mean_recent <- abs(mean(apply(recent_rec_marker, 2, mean, na.rm = TRUE)))

    has_drift <- mean_recent >= (mean_history + (3 * sd_history))
  } else if (state$criteria == "nonparametric_threshold") {
    history_values <- unlist(history_rec_marker, use.names = FALSE)
    top_limit <- as.vector(stats::quantile(history_values, 0.99, na.rm = TRUE))
    median_recent <- abs(stats::median(apply(recent_rec_marker, 2, stats::median, na.rm = TRUE)))

    has_drift <- median_recent >= top_limit
  } else if (state$criteria == "diff_threshold") {
    error_diff <- stats::median(as.matrix(recent_rec_marker) - as.matrix(history_rec_marker), na.rm = TRUE)
    has_drift <- !is.na(error_diff) && (error_diff >= obj$threshold)
  }

  has_drift <- isTRUE(has_drift)
  if (has_drift) {
    obj$drifted <- TRUE
    state$is_fitted <- FALSE
  }

  obj$state <- state
  return(list(obj = obj, drift = has_drift))
}

# Evaluates the criterion separately on every column of the monitored signal.
# Returns one logical per column (TRUE when that column changed), with NA for
# the columns where the criterion cannot be computed.
#' @noRd
.aedd_column_flags <- function(history, recent, criteria, alpha, threshold, breaks) {
  history <- as.data.frame(history)
  recent <- as.data.frame(recent)

  if (criteria %in% c("mann_whitney", "kolmogorov_smirnov", "levene")) {
    p_values <- .column_pvalues(history, recent, criteria)
    return(unname(p_values < alpha))
  }

  features <- intersect(names(history), names(recent))
  flags <- vapply(
    features,
    function(feat) {
      h <- history[[feat]]
      r <- recent[[feat]]
      if (all(is.na(h)) || all(is.na(r))) {
        return(NA)
      }
      if (criteria == "psi") {
        divergence <- .kldist_divergence(history[feat], recent[feat], breaks)
        return(if (is.na(divergence)) NA else divergence >= threshold)
      }
      if (criteria == "parametric_threshold") {
        sd_history <- stats::sd(h, na.rm = TRUE)
        if (is.na(sd_history)) {
          return(NA)
        }
        return(abs(mean(r, na.rm = TRUE)) >= (abs(mean(h, na.rm = TRUE)) + (3 * sd_history)))
      }
      if (criteria == "nonparametric_threshold") {
        top_limit <- as.vector(stats::quantile(h, 0.99, na.rm = TRUE))
        return(abs(stats::median(r, na.rm = TRUE)) >= top_limit)
      }
      # diff_threshold
      error_diff <- stats::median(r - h, na.rm = TRUE)
      return(if (is.na(error_diff)) NA else error_diff >= threshold)
    },
    logical(1)
  )
  return(unname(flags))
}

#' @export
fit.dfr_aedd <- function(obj, data, ...) {
  data <- as.data.frame(data)

  if (!is.null(obj$state$data) && (nrow(obj$state$data) > 0L)) {
    missing_in_recent <- setdiff(names(obj$state$data), names(data))
    if (length(missing_in_recent) > 0L) {
      warning("dfr_aedd: Some categories present in history data are not on the most recent dataset. Creating zero columns.")
    }
  }

  return(.fit_row_stream(obj, data))
}

#' @export
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
    alpha = obj$alpha,
    threshold = obj$threshold,
    column_threshold = obj$column_threshold,
    target_feat = obj$target_feat
  )$state
  return(obj)
}
