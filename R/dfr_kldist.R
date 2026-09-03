#'@title KL Distance method
#'@description This detector compares consecutive reference and recent windows through the Kullback-Leibler divergence estimated from their empirical distributions. In this package, it is primarily used for **virtual concept drift**, since it monitors changes in the distribution of a numeric feature stream rather than predictive error. The statistical foundation is the Kullback-Leibler divergence introduced by Kullback and Leibler (1951).
#'@param target_feat Feature to be monitored.
#'@param p_th Drift threshold applied to the KL divergence. Despite the name, this is not a p-value.
#'@param window_size Size of the sliding window
#'@param monitoring_step Number of observations between two consecutive tests. The default (`1`) tests at every observation; larger values reduce the computational cost on long streams.
#'@param data Already collected data to avoid cold start.
#'@details Missing observations are skipped instead of being imputed. When a
#'drift is reported the window is trimmed to its most recent half, and
#'`reset_state()` preserves that window so the detector does not restart cold.
#KL divergence: Solomon Kullback and Richard A. Leibler. On information and sufficiency. Annals of Mathematical Statistics, 1951.
#'@references Kullback, S., and Leibler, R. A. (1951). On information and sufficiency. *The Annals of Mathematical Statistics*, 22(1), 79-86. <doi:10.1214/aoms/1177729694>
#'@return `dfr_kldist` object
#'@importFrom graphics hist
#'@importFrom utils head tail
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
#'model <- dfr_kldist(target_feat='serie')
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
dfr_kldist <- function(target_feat = NULL, window_size = 100, p_th = 0.05, monitoring_step = 1, data = NULL) {
  if (p_th < 0) stop("p_th must be non-negative", call. = FALSE)
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)

  obj <- dist_based(target_feat = target_feat)

  state <- list()
  state$window_size <- window_size
  state$p_th <- p_th
  state$monitoring_step <- monitoring_step
  state$n <- 0
  state$kl <- NA_real_

  if (is.null(data)) {
    state$window <- numeric(0)
  } else {
    state$window <- as.numeric(data)
  }

  obj$state <- state

  class(obj) <- append("dfr_kldist", class(obj))
  return(obj)
}

#'@export
update_state.dfr_kldist <- function(obj, value, ...) {
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

  half <- floor(state$window_size / 2)
  p_window <- utils::head(state$window, half)
  q_window <- utils::tail(state$window, half)

  bins <- pretty(range(c(p_window, q_window), na.rm = TRUE), n = 10)
  if (length(unique(bins)) < 2) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  p_hist <- graphics::hist(p_window, breaks = bins, plot = FALSE)$density
  q_hist <- graphics::hist(q_window, breaks = bins, plot = FALSE)$density
  eps <- 1e-12
  p <- p_hist + eps
  q <- q_hist + eps
  p <- p / sum(p)
  q <- q / sum(q)

  state$kl <- sum(p * log(p / q, base = 2), na.rm = TRUE)

  if (state$kl >= state$p_th) {
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
  return(.fit_vector_stream(obj, data))
}

#'@export
reset_state.dfr_kldist <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kldist(
    target_feat = obj$target_feat,
    p_th = obj$state$p_th,
    window_size = obj$state$window_size,
    monitoring_step = obj$state$monitoring_step,
    data = obj$state$window
  )$state
  return(obj)
}
