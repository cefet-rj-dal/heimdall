#'@title KSWIN method
#'@description KSWIN applies a Kolmogorov-Smirnov test between a recent window and a reference sample drawn from older observations. In this package, the method is primarily used for **virtual concept drift**, because it monitors distributional changes in a numeric feature stream. The method follows Raab et al. (2020) <doi:10.1016/j.neucom.2019.11.111>.
#'@param target_feat Feature to be monitored.
#'@param alpha Probability for the test statistic of the Kolmogorov-Smirnov-Test The alpha parameter is very sensitive, therefore should be set below 0.01.
#'@param window_size Size of the sliding window (must be > 2*stat_size)
#'@param stat_size Size of the statistic window
#'@param monitoring_step Number of observations between two consecutive tests. The default (`1`) tests at every observation; larger values reduce the computational cost on long streams.
#'@param exact Passed to [stats::ks.test()]. The default (`TRUE`) preserves the
#'behaviour of previous versions; setting it to `NULL` lets R choose, which is
#'considerably faster for the default window sizes.
#'@param data Already collected data to avoid cold start.
#'@details The reference window is randomly subsampled, so results depend on the
#'state of the random number generator. Call [base::set.seed()] before the stream loop
#'if reproducibility is required.
#'
#'Missing observations are skipped instead of being imputed. When a drift is
#'reported the window is trimmed to its most recent `stat_size` observations,
#'and `reset_state()` preserves that window so the detector does not restart
#'cold.
#KSWIN detection: Christoph Raab, Moritz Heusinger, Frank-Michael Schleif, Reactive Soft Prototype Computing for Concept Drift Streams, Neurocomputing, 2020.
#KSWIN detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/kswin.py#L5
#'@references Raab, C., Heusinger, M., and Schleif, F.-M. (2020). Reactive soft prototype computing for concept drift streams. *Neurocomputing*, 416, 340-351. <doi:10.1016/j.neucom.2019.11.111>
#'@return `dfr_kswin` object
#'@importFrom stats ks.test
#'@importFrom utils tail
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'# This example assumes a model residual where 1 is an error and 0 is a
#'# correct prediction.
#'
#'data(st_drift_examples)
#'data <- st_drift_examples$univariate
#'data$event <- NULL
#'
#'model <- dfr_kswin(target_feat='serie')
#'
#'detection <- NULL
#'output <- list(obj=model, drift=FALSE)
#'for (i in seq_along(data$serie)){
#'  output <- update_state(output$obj, data$serie[i])
#'  if (output$drift){
#'    type <- 'drift'
#'    output$obj <- reset_state(output$obj)
#'  }else{
#'    type <- ''
#'  }
#'  detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
#'}
#'
#'detection[detection$type == 'drift',]
#'@export
dfr_kswin <- function(target_feat = NULL, window_size = 1500, stat_size = 500, alpha = 1e-07, monitoring_step = 1, exact = TRUE, data = NULL) {
  .check_probability(alpha, "alpha")
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(stat_size, "stat_size", min_value = 1L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  if (window_size < stat_size) {
    stop("stat_size must be smaller than window_size", call. = FALSE)
  }

  obj <- dist_based(target_feat = target_feat)

  state <- list()
  state$window_size <- window_size
  state$stat_size <- stat_size
  state$alpha <- alpha
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

  class(obj) <- append("dfr_kswin", class(obj))
  return(obj)
}

#'@export
update_state.dfr_kswin <- function(obj, value, ...) {
  state <- obj$state

  state$n <- state$n + 1
  value <- .as_scalar(value)
  if (is.na(value)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  if (length(state$window) < state$window_size) {
    state$window <- c(state$window, value)
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  state$window <- c(state$window[-1L], value)

  if ((state$n %% state$monitoring_step) != 0) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  reference_window <- state$window[seq_len(state$window_size - state$stat_size)]
  if (length(reference_window) > state$stat_size) {
    reference_window <- sample(reference_window, state$stat_size)
  }
  stat_window <- utils::tail(state$window, state$stat_size)

  ks_res <- stats::ks.test(reference_window, stat_window, exact = state$exact)
  st <- as.numeric(ks_res$statistic)
  state$p_value <- as.numeric(ks_res$p.value)
  threshold <- sqrt(-log(state$alpha) / state$stat_size)

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
  return(.fit_vector_stream(obj, data))
}

#'@export
reset_state.dfr_kswin <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kswin(
    target_feat = obj$target_feat,
    window_size = obj$state$window_size,
    stat_size = obj$state$stat_size,
    alpha = obj$state$alpha,
    monitoring_step = obj$state$monitoring_step,
    exact = obj$state$exact,
    data = obj$state$window
  )$state
  return(obj)
}
