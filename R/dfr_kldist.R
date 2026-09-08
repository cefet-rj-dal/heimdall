#'@title KL Distance method
#'@description This detector compares a reference and a recent window through the symmetrised Kullback-Leibler divergence of their binned empirical distributions, `sum((p - q) * log(p / q))`, which is the quantity also known as the Jeffreys divergence or, in the credit-risk literature, the Population Stability Index. It is multivariate: every numeric column of the monitored stream contributes to the score. Because it monitors the distribution of the observed features rather than predictive error, it is primarily used for **virtual concept drift**. The statistical foundation is the divergence introduced by Kullback and Leibler (1951).
#'@param window_size Size of the window
#'@param p_th Drift threshold applied to the divergence. Despite the name, this is not a p-value.
#'@param window_type Window policy. `"sliding"` (default) keeps at most `window_size` rows, so both halves move with the stream. `"anchored"` keeps every row, which pins the reference half to the beginning of the stream and makes memory grow with the stream length.
#'@param breaks Number of bins used to discretise each feature before the divergence is computed.
#'@param monitoring_step Number of rows between two consecutive evaluations. The default (`1`) evaluates at every row; larger values reduce the computational cost on long streams.
#'@param data Already collected data to avoid cold start.
#'@details Bins are built over the reference and recent halves together, so the
#'two histograms are always comparable. Empty bins are smoothed before the
#'logarithm is taken, and the score is averaged over bins and over features.
#'
#'Rows whose first value is missing are skipped instead of being imputed.
#'
#'This detector consumes a data frame, one row at a time, and no longer takes a
#'`target_feat` argument. Select the columns to monitor before passing the data
#'in.
#KL divergence: Solomon Kullback and Richard A. Leibler. On information and sufficiency. Annals of Mathematical Statistics, 1951.
#'@references Kullback, S., and Leibler, R. A. (1951). On information and sufficiency. *The Annals of Mathematical Statistics*, 22(1), 79-86. <doi:10.1214/aoms/1177729694>
#'@return `dfr_kldist` object
#'@importFrom utils head tail
#'@example examples/1_detection/r/dfr_kldist.R
#'@export
dfr_kldist <- function(window_size=100, p_th=0.25, window_type="sliding", breaks=20, monitoring_step=1, data=NULL) {
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(breaks, "breaks", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  if (!is.numeric(p_th) || (length(p_th) != 1L) || is.na(p_th) || (p_th < 0)) {
    stop("p_th must be a single non-negative numeric value", call. = FALSE)
  }
  window_type <- .check_window_type(window_type)

  obj <- mv_dist_based()

  state <- list()
  
  state$window_size <- window_size
  state$p_th <- p_th
  state$window_type <- window_type
  state$breaks <- breaks
  state$monitoring_step <- monitoring_step
  state$n <- 0
  state$divergence <- NA_real_

  if (is.null(data)) {
    state$window <- data.frame()
  } else {
    state$window <- as.data.frame(data)
  }

  obj$state <- state
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL

  class(obj) <- append("dfr_kldist", class(obj))
  return(obj)
}

# Symmetrised KL divergence between two binned samples, averaged over bins and
# features.
#' @noRd
.kldist_divergence <- function(history, recent, breaks) {
  features <- intersect(names(history), names(recent))
  features <- features[vapply(features, function(f) is.numeric(history[[f]]), logical(1))]
  if (length(features) == 0L) {
    return(NA_real_)
  }

  smoothing <- 0.005
  divergence <- 0
  for (feat in features) {
    combined <- c(history[[feat]], recent[[feat]])
    if (length(unique(combined[!is.na(combined)])) < 2L) {
      next
    }
    bins <- cut(combined, breaks = breaks)
    history_bins <- bins[seq_len(nrow(history))]
    recent_bins <- bins[-seq_len(nrow(history))]

    for (b in levels(bins)) {
      expected <- sum(history_bins == b, na.rm = TRUE) / length(history_bins)
      observed <- sum(recent_bins == b, na.rm = TRUE) / length(recent_bins)

      p <- (observed + smoothing) / (1 + breaks * smoothing)
      q <- (expected + smoothing) / (1 + breaks * smoothing)

      divergence <- divergence + (p - q) * log(p / q)
    }
  }

  return(divergence / breaks / length(features))
}

#'@export
update_state.dfr_kldist <- function(obj, value, ...) {
  state <- obj$state
  obj$last_drifter_output <- NA_real_

  state$n <- state$n + 1
  
  if (is.na(.as_scalar(value))) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  state$window <- .trim_window(rbind(state$window, as.data.frame(value)), state$window_size, state$window_type)

  if (nrow(state$window) < state$window_size) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  if ((state$n %% state$monitoring_step) != 0) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  half <- floor(state$window_size / 2)

  history_window <- utils::head(state$window, half)
  recent_window <- utils::tail(state$window, half)
  
  state$divergence <- .kldist_divergence(history_window, recent_window, state$breaks)
  obj$last_drifter_output <- state$divergence

  if (!is.na(state$divergence) && (state$divergence >= state$p_th)) {

    state$window <- utils::tail(state$window, half)

    obj$drifted <- TRUE
    obj$state <- state
    return(list(obj = obj, drift = TRUE))
  }

  obj$state <- state
  return(list(obj = obj, drift = FALSE))
}

#'@export
fit.dfr_kldist <- function(obj, data, ...) {
  return(.fit_row_stream(obj, data, output_names = "divergence"))
}

#'@export
reset_state.dfr_kldist <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kldist(
    window_size = obj$state$window_size,
    p_th = obj$state$p_th,
    window_type = obj$state$window_type,
    breaks = obj$state$breaks,
    monitoring_step = obj$state$monitoring_step
    # data = obj$state$window
  )$state
  return(obj)
}
