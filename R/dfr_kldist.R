#' @title KL Distance method
#' @description This detector compares a reference and a recent window through the symmetrised Kullback-Leibler divergence of their binned empirical distributions, `sum((p - q) * log(p / q))`, which is the quantity also known as the Jeffreys divergence or, in the credit-risk literature, the Population Stability Index. It is multivariate: every monitored column contributes to the score. Because it monitors the distribution of the observed features rather than predictive error, it is primarily used for **virtual concept drift**. The statistical foundation is the divergence introduced by Kullback and Leibler (1951).
#' @param target_feat Names of the columns to be monitored. `NULL` (the default)
#' monitors every numeric column.
#' @param window_size Size of the window
#' @param threshold Drift threshold applied to the divergence. The default
#' (`0.12`) was calibrated by simulation for the default window: on stationary
#' normal data the divergence stayed below 0.1, while a shift of two standard
#' deviations exceeded 0.13 in 90% of the runs. Shifts smaller than about one
#' standard deviation are not separable from noise with 50-row halves; use a
#' larger window to detect them.
#' @param window_type Window policy. `"sliding"` (default) keeps at most `window_size` rows, so both halves move with the stream. `"anchored"` keeps every row, which pins the reference half to the beginning of the stream and makes memory grow with the stream length.
#' @param breaks Number of bins used to discretise each feature before the divergence is computed.
#' @param monitoring_step Number of rows between two consecutive evaluations. The default (`1`) evaluates at every row; larger values reduce the computational cost on long streams.
#' @param data Already collected data to avoid cold start.
#' @param p_th Deprecated name of `threshold`.
#' @details Bins are built over the reference and recent halves together, so the
#' two histograms are always comparable. Empty bins are smoothed before the
#' logarithm is taken, and the score is averaged over bins and over features.
#'
#' The detector consumes one row of a data frame at a time (a single value is
#' also accepted). Rows whose monitored values are all missing are skipped
#' instead of being imputed.
# KL divergence: Solomon Kullback and Richard A. Leibler. On information and sufficiency. Annals of Mathematical Statistics, 1951.
#' @references Kullback, S., and Leibler, R. A. (1951). On information and sufficiency. *The Annals of Mathematical Statistics*, 22(1), 79-86. <doi:10.1214/aoms/1177729694>
#' @return `dfr_kldist` object
#' @importFrom utils head tail
#' @example examples/1_detection/r/dfr_kldist.R
#' @export
dfr_kldist <- function(target_feat = NULL, window_size = 100, threshold = 0.12, window_type = "sliding", breaks = 20, monitoring_step = 1, data = NULL, p_th = NULL) {
  if (!is.null(p_th)) {
    warning("dfr_kldist(): 'p_th' is deprecated; use 'threshold' instead.", call. = FALSE)
    threshold <- p_th
  }
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(breaks, "breaks", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  .check_number(threshold, "threshold", min_value = 0)
  window_type <- .check_window_type(window_type)

  obj <- mv_dist_based(target_feat = target_feat)

  state <- list()

  state$window_size <- window_size
  state$threshold <- threshold
  state$window_type <- window_type
  state$breaks <- breaks
  state$monitoring_step <- monitoring_step
  state$n <- 0
  state$divergence <- NA_real_
  # Monitored columns, fixed by the first observation, and the window itself,
  # one row per observation.
  state$features <- NULL
  state$window <- NULL

  obj$state <- state
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_kldist", class(obj))

  if (!is.null(data)) {
    data <- as.data.frame(data)
    for (i in seq_len(nrow(data))) {
      obj$state <- .kldist_append(obj, data[i, , drop = FALSE])
    }
    obj$state$window <- .trim_window(obj$state$window, window_size, window_type)
  }
  return(obj)
}

# Adds an observation to the window. Rows whose monitored values are all
# missing are left out.
#' @noRd
.kldist_append <- function(obj, value) {
  state <- obj$state
  row <- .select_features(value, obj$target_feat)
  if (is.null(state$features)) {
    state$features <- names(row)
  }
  row <- unname(row[state$features])
  if (all(is.na(row))) {
    return(state)
  }
  state$window <- rbind(state$window, row, deparse.level = 0)
  colnames(state$window) <- state$features
  return(state)
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
    history_values <- history[[feat]]
    recent_values <- recent[[feat]]
    value_range <- range(c(history_values, recent_values), na.rm = TRUE)
    if (!all(is.finite(value_range)) || (value_range[1] == value_range[2])) {
      next
    }
    # Equal-width, right-closed bins over the joint range, as cut(x, breaks).
    inner_edges <- seq(value_range[1], value_range[2], length.out = breaks + 1L)[-c(1L, breaks + 1L)]
    bin_counts <- function(values) {
      bins <- findInterval(values[!is.na(values)], inner_edges, left.open = TRUE) + 1L
      tabulate(bins, nbins = breaks)
    }
    expected <- bin_counts(history_values) / length(history_values)
    observed <- bin_counts(recent_values) / length(recent_values)

    p <- (observed + smoothing) / (1 + breaks * smoothing)
    q <- (expected + smoothing) / (1 + breaks * smoothing)

    divergence <- divergence + sum((p - q) * log(p / q))
  }

  return(divergence / breaks / length(features))
}

#' @export
update_state.dfr_kldist <- function(obj, value, ...) {
  obj$last_drifter_output <- NA_real_
  obj$state$n <- obj$state$n + 1
  rows_before <- NROW(obj$state$window)
  obj$state <- .kldist_append(obj, value)
  state <- obj$state
  if (NROW(state$window) == rows_before) {
    return(list(obj = obj, drift = FALSE))
  }

  state$window <- .trim_window(state$window, state$window_size, state$window_type)

  if (nrow(state$window) < state$window_size) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  if ((state$n %% state$monitoring_step) != 0) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  half <- floor(state$window_size / 2)

  history_window <- as.data.frame(utils::head(state$window, half))
  recent_window <- as.data.frame(utils::tail(state$window, half))

  state$divergence <- .kldist_divergence(history_window, recent_window, state$breaks)
  obj$last_drifter_output <- state$divergence

  if (!is.na(state$divergence) && (state$divergence >= state$threshold)) {
    state$window <- utils::tail(state$window, half)

    obj$drifted <- TRUE
    obj$state <- state
    return(list(obj = obj, drift = TRUE))
  }

  obj$state <- state
  return(list(obj = obj, drift = FALSE))
}

#' @export
fit.dfr_kldist <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data, output_names = "divergence"))
}

#' @export
reset_state.dfr_kldist <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kldist(
    target_feat = obj$target_feat,
    window_size = obj$state$window_size,
    threshold = obj$state$threshold,
    window_type = obj$state$window_type,
    breaks = obj$state$breaks,
    monitoring_step = obj$state$monitoring_step
  )$state
  return(obj)
}
