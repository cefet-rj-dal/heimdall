#'@title KSWIN method
#'@description KSWIN applies a Kolmogorov-Smirnov test between a recent window and a reference sample drawn from older observations. In this package, the method is primarily used for **virtual concept drift**, because it monitors distributional changes in a numeric feature stream. The method follows Raab et al. (2020) <doi:10.1016/j.neucom.2019.11.111>.
#'@param target_feat Feature to be monitored.
#'@param alpha Probability for the test statistic of the Kolmogorov-Smirnov-Test The alpha parameter is very sensitive, therefore should be set below 0.01.
#'@param window_size Size of the window (must be greater than stat_size)
#'@param stat_size Size of the statistic window
#'@param window_type Window policy. `"sliding"` (default) keeps at most `window_size` observations, so both the reference and the statistic halves move with the stream. `"anchored"` keeps every observation, which pins the reference sample to the beginning of the stream and makes memory grow with the stream length.
#'@param monitoring_step Number of observations between two consecutive tests. The default (`1`) tests at every observation; larger values reduce the computational cost on long streams.
#'@param exact Passed to [stats::ks.test()]. The default (`TRUE`) preserves the behaviour of previous versions; setting it to `NULL` lets R choose, which is considerably faster for the default window sizes.
#'@param data Already collected data to avoid cold start.
#'@details The reference window is randomly subsampled to `stat_size`
#'observations, so results depend on the state of the random number generator.
#'Call [base::set.seed()] before the stream loop if reproducibility is required.
#'
#'A drift requires both a significant p-value and a KS statistic above
#'`sqrt(-log(alpha) / stat_size)`, as defined by Raab et al. Missing
#'observations are skipped instead of being imputed.
#KSWIN detection: Christoph Raab, Moritz Heusinger, Frank-Michael Schleif, Reactive Soft Prototype Computing for Concept Drift Streams, Neurocomputing, 2020.
#KSWIN detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/kswin.py#L5
#'@references Raab, C., Heusinger, M., and Schleif, F.-M. (2020). Reactive soft prototype computing for concept drift streams. *Neurocomputing*, 416, 340-351. <doi:10.1016/j.neucom.2019.11.111>
#'@return `dfr_kswin` object
#'@importFrom stats ks.test
#'@importFrom utils head tail
#'@example examples/1_detection/r/dfr_kswin.R
#'@export
dfr_kswin <- function(target_feat=NULL, window_size=1500, stat_size=500, alpha=1e-07, window_type="sliding", monitoring_step=1, exact=TRUE, data=NULL) {
  .check_probability(alpha, "alpha")
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(stat_size, "stat_size", min_value = 1L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  window_type <- .check_window_type(window_type)
  
  if (window_size <= stat_size) {
    stop("stat_size must be smaller than window_size", call. = FALSE)
  }

  obj <- dist_based(target_feat = target_feat)

  state <- list()
  state$window_size <- window_size
  state$stat_size <- stat_size
  state$alpha <- alpha
  state$window_type <- window_type
  state$monitoring_step <- monitoring_step
  state$exact <- exact
  state$p_value <- NA_real_
  state$n <- 0

  if (is.null(data)) {
    state$window <- numeric(0)
  } else {
    state$window <- as.numeric(data)
  }

  obj$state <- state

  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL

  class(obj) <- append("dfr_kswin", class(obj))
  return(obj)
}

#'@export
update_state.dfr_kswin <- function(obj, value, ...) {
  state <- obj$state
  obj$last_drifter_output <- cbind(D = NA_real_, p = NA_real_)

  state$n <- state$n + 1
  value <- .as_scalar(value)
  if (is.na(value)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  state$window <- .trim_window(c(state$window, value), state$window_size, state$window_type)

  if (length(state$window) < state$window_size) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  if ((state$n %% state$monitoring_step) != 0) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  reference_window <- utils::head(state$window, length(state$window) - state$stat_size)
  
  if (length(reference_window) > state$stat_size) {
    reference_window <- sample(reference_window, state$stat_size)
  }
  stat_window <- utils::tail(state$window, state$stat_size)

  ks_res <- stats::ks.test(reference_window, stat_window, exact = state$exact)
  st <- as.numeric(ks_res$statistic)
  state$p_value <- as.numeric(ks_res$p.value)
  threshold <- sqrt(-log(state$alpha) / state$stat_size)

  obj$last_drifter_output <- cbind(D = st, p = state$p_value)

  if ((state$p_value < state$alpha) && (st > threshold)) {
    state$window <- utils::tail(state$window, state$stat_size)

    obj$drifted <- TRUE
    obj$state <- state
    return(list(obj = obj, drift = TRUE))
  }

  obj$state <- state
  return(list(obj = obj, drift = FALSE))
}

#'@export
fit.dfr_kswin <- function(obj, data, ...) {
  return(.fit_vector_stream(obj, data, output_names = c("D", "p")))
}

#'@export
reset_state.dfr_kswin <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kswin(
    target_feat = obj$target_feat,
    window_size = obj$state$window_size,
    stat_size = obj$state$stat_size,
    alpha = obj$state$alpha,
    window_type = obj$state$window_type,
    monitoring_step = obj$state$monitoring_step,
    exact = obj$state$exact,
    data = obj$state$window
  )$state
  return(obj)
}
