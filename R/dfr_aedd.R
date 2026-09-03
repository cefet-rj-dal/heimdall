#'@title Autoencoder-Based Drift Detection method
#'@description AEDD is an unsupervised multivariate detector that compares reconstruction errors produced by an autoencoder on reference and recent windows. Because it monitors changes in the input distribution rather than classifier performance, this implementation is primarily aimed at **virtual concept drift**. The method follows Kaminskyi, Li, and Muller (2022) <doi:10.1109/ICDMW58026.2022.00109>.
#'@param encoding_size Encoding Size
#'@param ae_class Autoencoder Class. When `NULL` (the default),
#'`daltoolbox::autoenc_encode_decode` is used.
#'@param batch_size Batch Size for batch learning
#'@param num_epochs Number of Epochs for training
#'@param learning_rate Learning Rate
#'@param window_size Size of the most recent data to be used
#'@param monitoring_step The number of rows that the drifter waits to be is updated
#'@param criteria The method to be used to check if there is a drift. One of `'mann_whitney'` (default), `'kolmogorov_smirnov'`, `'levene'`, `'parametric_threshold'` or `'nonparametric_threshold'`
#'@param alpha The significance threshold for the statistical test used in criteria
#'@param reporting If TRUE, some data are returned as norm_x_oh, drift_input, hist_proj, and recent_proj.
#AEDD detection: Daniil Kaminskyi, Bin Li and Emmanuel Muller. "Reconstruction-based unsupervised drift detection over multivariate streaming data." 2022 IEEE International Conference on Data Mining Workshops (ICDMW).
#'@references Kaminskyi, D., Li, B., and Muller, E. (2022). Reconstruction-based unsupervised drift detection over multivariate streaming data. In *2022 IEEE International Conference on Data Mining Workshops (ICDMW)*. <doi:10.1109/ICDMW58026.2022.00109>
#'@return `dfr_aedd` object
#'@importFrom stats ks.test median quantile sd wilcox.test
#'@importFrom utils head tail
#'@examples
#'# A rendered walkthrough is available at
#'# https://github.com/cefet-rj-dal/heimdall/blob/main/examples/dfr_aedd.md
#'@export
dfr_aedd <- function(encoding_size, ae_class = NULL, batch_size = 32, num_epochs = 1000, learning_rate = 0.001, window_size = 100, monitoring_step = 1700, criteria = "mann_whitney", alpha = 0.01, reporting = FALSE) {
  criteria <- match.arg(
    criteria,
    c('mann_whitney', 'kolmogorov_smirnov', 'levene', 'parametric_threshold', 'nonparametric_threshold')
  )
  .check_probability(alpha, "alpha")
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  .check_positive_integer(encoding_size, "encoding_size", min_value = 1L)

  if (is.null(ae_class)) {
    ae_class <- .default_autoencoder()
  }

  obj <- mv_dist_based()

  obj$ae_class <- ae_class
  obj$alpha <- alpha

  # Attributes
  state <- list()

  state$encoding_size <- encoding_size
  state$batch_size <- batch_size
  state$num_epochs <- num_epochs
  state$learning_rate <- learning_rate
  state$window_size <- window_size
  state$monitoring_step <- monitoring_step
  state$criteria <- criteria
  state$data <- NULL
  state$n <- 0

  state$autoencoder <- NULL
  state$is_fitted <- FALSE

  obj$reporting <- reporting
  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_aedd", class(obj))
  return(obj)
}

# Resolves the default autoencoder constructor lazily so that a change in
# daltoolbox surfaces as an actionable message instead of an obscure lookup
# failure.
#' @noRd
.default_autoencoder <- function() {
  tryCatch(
    getExportedValue("daltoolbox", "autoenc_encode_decode"),
    error = function(e) {
      stop(
        "dfr_aedd(): the default autoencoder 'daltoolbox::autoenc_encode_decode' is not available. Pass an explicit 'ae_class'.",
        call. = FALSE
      )
    }
  )
}

#'@export
update_state.dfr_aedd <- function(obj, value, ...) {
  state <- obj$state
  has_drift <- FALSE

  if (length(value) == 1) {
    if (value > 1) {
      value <- 1
    } else if (value < 0) {
      value <- 0
    }
  } else if (length(value) > 1) {
    value[value > 1] <- 1
    value[value < 0] <- 0
  }

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
  }

  state$data <- rbind(state$data, as.data.frame(value))
  rownames(state$data) <- seq_len(nrow(state$data))

  state$n <- state$n + 1
  if (state$n >= state$monitoring_step) {
    state$n <- 0
  } else {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  currentLength <- nrow(state$data)
  if (is.null(currentLength)) {
    currentLength <- 0
  }

  if (currentLength >= state$window_size) {
    state$data <- utils::tail(state$data, -1)
    half <- floor(state$window_size / 2)
    history_window <- utils::head(state$data, half)
    recent_window <- utils::tail(state$data, half)

    if (!state$is_fitted) {
      if (is.null(ncol(state$data))) {
        input_size <- 1
      } else {
        input_size <- ncol(state$data)
      }

      state$autoencoder <- obj$ae_class(input_size = input_size, encoding_size = state$encoding_size, batch_size = state$batch_size, num_epochs = state$num_epochs, learning_rate = state$learning_rate)
      state$autoencoder <- fit(state$autoencoder, history_window)

      state$is_fitted <- TRUE
    }

    history_window_proj <- transform(state$autoencoder, history_window)
    history_rec_error <- (history_window_proj - history_window)
    recent_window_proj <- transform(state$autoencoder, recent_window)
    recent_rec_error <- (recent_window_proj - recent_window)

    if (obj$reporting) {
      obj$history_window_proj <- history_window_proj
      obj$recent_window_proj <- transform(state$autoencoder, value)
    }

    history_errors <- unlist(as.vector(t(history_rec_error)), use.names = FALSE)
    recent_errors <- unlist(as.vector(t(recent_rec_error)), use.names = FALSE)

    if (state$criteria == 'mann_whitney') {
      mw_results <- suppressWarnings(stats::wilcox.test(history_errors, recent_errors))
      if (!is.na(mw_results$p.value) && (mw_results$p.value < obj$alpha)) {
        has_drift <- TRUE
      }
    }

    if (state$criteria == 'kolmogorov_smirnov') {
      ks_results <- suppressWarnings(stats::ks.test(history_errors, recent_errors))
      if (!is.na(ks_results$p.value) && (ks_results$p.value < obj$alpha)) {
        has_drift <- TRUE
      }
    }

    if (state$criteria == 'levene') {
      p_value <- .levene_pvalue(
        values = c(history_errors, recent_errors),
        group = rep(c('History', 'Recent'), c(length(history_errors), length(recent_errors)))
      )
      if (!is.na(p_value) && (p_value < obj$alpha)) {
        has_drift <- TRUE
      }
    }

    if (state$criteria == 'parametric_threshold') {
      mean_history_rec_error <- abs(mean(apply(history_rec_error, 2, mean)))
      sd_history_rec_error <- abs(mean(apply(history_rec_error, 2, stats::sd)))
      mean_recent_rec_error <- abs(mean(apply(recent_rec_error, 2, mean)))

      if (mean_recent_rec_error >= (mean_history_rec_error + (3 * sd_history_rec_error))) {
        has_drift <- TRUE
      }
    }

    if (state$criteria == 'nonparametric_threshold') {
      top_limit <- as.vector(stats::quantile(history_errors, 0.99))
      median_recent_rec_error <- abs(stats::median(apply(recent_rec_error, 2, stats::median)))

      if (median_recent_rec_error >= top_limit) {
        has_drift <- TRUE
      }
    }

    if (has_drift) {
      obj$drifted <- TRUE
      state$is_fitted <- FALSE
    }
  } else {
    if (obj$reporting) {
      obj$recent_window_proj <- 0
    }
  }

  obj$state <- state
  return(list(obj = obj, drift = has_drift))
}

#'@export
fit.dfr_aedd <- function(obj, data, ...) {
  state <- obj$state
  data <- as.data.frame(data)

  if (obj$reporting) {
    obj$hist_proj <- c()
    obj$recent_proj <- c()
  }

  if (!is.null(state$data) && (nrow(state$data) > 0L) && (!is.null(ncol(state$data)))) {
    if (!state$is_fitted) {
      missing_in_history <- setdiff(names(data), names(state$data))
      if (length(missing_in_history) > 0L) {
        warning('dfr_aedd: Some categories present in most recent data are not on the history dataset. Creating zero columns.')
        state$data[missing_in_history] <- 0
      }
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
    encoding_size = obj$state$encoding_size,
    ae_class = obj$ae_class,
    batch_size = obj$state$batch_size,
    num_epochs = obj$state$num_epochs,
    learning_rate = obj$state$learning_rate,
    window_size = obj$state$window_size,
    monitoring_step = obj$state$monitoring_step,
    criteria = obj$state$criteria,
    alpha = obj$alpha,
    reporting = obj$reporting
  )$state
  return(obj)
}
