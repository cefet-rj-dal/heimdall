#'@title Cumulative Sum for Concept Drift Detection (CUSUM) method
#'@description CUSUM is a sequential analysis procedure that accumulates deviations in a monitored signal and raises an alarm when the cumulative evidence exceeds a threshold. In this package, the detector is implemented as an error-based monitor, so it is primarily intended for **real concept drift** affecting predictive performance. The concept-drift adaptation follows the sequential change-detection literature discussed by Muthukrishnan, Berg, and Wu (2007) <doi:10.1109/ICDMW.2007.89>.
#'@param lambda Detection threshold: a drift is reported when the cumulative
#'sum exceeds it. Lower values react faster at the cost of more false alarms.
#'@param delta Tolerance subtracted at every step, i.e. the increase of the
#'error rate that is still considered acceptable.
#'@param min_instances Minimum number of observations before a drift can be
#'reported.
#'@details The detector follows the `CusumDM` detector of MOA: it accumulates
#'`max(0, sum + x - mean - delta)`, where `mean` is the running error rate, so
#'it reacts to an increase of the error rate whatever its level. After a drift
#'is reported the statistics restart from scratch. Missing values are treated
#'as correct predictions (`0`).
#'
#'Up to version 1.3.0, `dfr_cusum()` accumulated `+1` for every error and `-1`
#'for every correct prediction, so it could only detect error rates above 50%.
#CUMSUM: S. Muthukrishnan, Eric Berg, Yihua Wu: Sequential Change Detection on Data Streams. Seventh IEEE International Conference on Data Mining Workshops (ICDMW 2007), DOI:10.1109/ICDMW.2007.89
#'@references Muthukrishnan, S., Berg, E., and Wu, Y. (2007). Sequential change detection on data streams. In *Seventh IEEE International Conference on Data Mining Workshops (ICDMW 2007)*. <doi:10.1109/ICDMW.2007.89>
#'@return `dfr_cusum` object
#'@importFrom daltoolbox cla_nb
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'# This example uses an error-based drift detector with a synthetic
#'# model residual where 1 is an error and 0 is a correct prediction.
#'
#'data(st_drift_examples)
#'data <- st_drift_examples$univariate
#'data$event <- NULL
#'data$prediction <- st_drift_examples$univariate$serie > 4
#'
#'model <- dfr_cusum()
#'
#'detection <- NULL
#'output <- list(obj=model, drift=FALSE)
#'for (i in seq_along(data$prediction)){
#'  output <- update_state(output$obj, data$prediction[i])
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
dfr_cusum <- function(lambda = 50, delta = 0.005, min_instances = 30) {
  .check_number(lambda, "lambda", min_value = 0)
  .check_number(delta, "delta", min_value = 0)
  .check_positive_integer(min_instances, "min_instances", min_value = 1L)

  obj <- error_based()

  obj$state <- .cusum_state(lambda, delta, min_instances)
  obj$drifted <- FALSE

  class(obj) <- append("dfr_cusum", class(obj))

  return(obj)
}

#' @noRd
.cusum_state <- function(lambda, delta, min_instances) {
  list(
    lambda = lambda,
    delta = delta,
    min_instances = min_instances,
    n = 0,
    x_mean = 0,
    g = 0
  )
}

#'@export
update_state.dfr_cusum <- function(obj, value, ...) {
  value <- .as_error(value)

  state <- obj$state
  state$n <- state$n + 1
  state$x_mean <- state$x_mean + (value - state$x_mean) / state$n
  state$g <- max(0, state$g + value - state$x_mean - state$delta)

  if ((state$n >= state$min_instances) && (state$g > state$lambda)) {
    obj$state <- .cusum_state(state$lambda, state$delta, state$min_instances)
    obj$drifted <- TRUE
    return(list(obj = obj, drift = TRUE))
  }

  obj$state <- state
  return(list(obj = obj, drift = FALSE))
}

#'@export
fit.dfr_cusum <- function(obj, data, ...) {
  return(.fit_vector_stream(obj, data))
}

#'@export
reset_state.dfr_cusum <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- .cusum_state(obj$state$lambda, obj$state$delta, obj$state$min_instances)
  return(obj)
}
