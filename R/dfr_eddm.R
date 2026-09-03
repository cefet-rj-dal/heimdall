#'@title Adapted Early Drift Detection Method (EDDM) method
#'@description EDDM extends DDM by monitoring the distance between classification errors instead of only the error rate, which makes it more sensitive to gradual degradation. Because it operates on the model error stream, it is primarily intended for **real concept drift**. The method follows Baena-Garcia et al. (2006), who proposed EDDM for improved detection of gradual drift.
#'@param min_instances The minimum number of instances before detecting change
#'@param min_num_errors The minimum number of errors before detecting change
#'@param warning_level Necessary level for warning zone
#'@param out_control_level Necessary level for a positive drift detection
#'@details Missing values are treated as correct predictions (`0`).
#EDDM: Manuel Baena-Garcia, Jose Del Campo-Avila, Raul Fidalgo, Albert Bifet, Ricard Gavalda, Rafael Morales-Bueno. Early Drift Detection Method. In Fourth International Workshop on Knowledge Discovery from Data Streams, 2006.
#EDDM implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/eddm.py
#'@references Baena-Garcia, M., del Campo-Avila, J., Fidalgo, R., Bifet, A., Gavalda, R., and Morales-Bueno, R. (2006). Early drift detection method. In *Fourth International Workshop on Knowledge Discovery from Data Streams*.
#'@return `dfr_eddm` object
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
#'model <- dfr_eddm()
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
dfr_eddm <- function(min_instances = 30, min_num_errors = 30, warning_level = 0.95, out_control_level = 0.9) {
  .check_positive_integer(min_instances, "min_instances", min_value = 1L)
  .check_positive_integer(min_num_errors, "min_num_errors", min_value = 1L)
  .check_probability(warning_level, "warning_level")
  .check_probability(out_control_level, "out_control_level")

  obj <- error_based()

  state <- list()

  state$min_instances <- min_instances
  state$m_min_num_errors <- min_num_errors
  state$warning_level <- warning_level
  state$out_control_level <- out_control_level

  state$m_last_level <- NULL

  state$m_n <- 1
  state$m_num_errors <- 0
  state$m_d <- 0
  state$m_lastd <- 0
  state$m_mean <- 0.0
  state$m_std_temp <- 0.0
  state$m_m2s_max <- 0.0
  state$estimation <- 0.0
  state$concept_change <- FALSE

  obj$state <- state

  obj$drifted <- FALSE

  class(obj) <- append("dfr_eddm", class(obj))
  return(obj)
}

#'@export
update_state.dfr_eddm <- function(obj, value, ...) {
  value <- .as_scalar(value)
  if (is.na(value)) {
    value <- 0
  }

  state <- obj$state

  state$m_n <- state$m_n + 1

  if (value != 1) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  state$delay <- 0
  state$m_num_errors <- state$m_num_errors + 1
  state$m_lastd <- state$m_d
  state$m_d <- state$m_n - 1
  distance <- state$m_d - state$m_lastd
  old_mean <- state$m_mean
  state$m_mean <- state$m_mean + (distance - state$m_mean) / state$m_num_errors
  state$estimation <- state$m_mean
  state$m_std_temp <- state$m_std_temp + (distance - state$m_mean) * (distance - old_mean)
  std <- sqrt(state$m_std_temp / state$m_num_errors)
  m2s <- state$m_mean + 2 * std

  if (state$m_n < state$min_instances) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  if (m2s > state$m_m2s_max) {
    state$m_m2s_max <- m2s

    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  p <- m2s / state$m_m2s_max

  if ((state$m_num_errors > state$m_min_num_errors) && (p < state$out_control_level)) {
    state$m_n <- 1
    state$m_num_errors <- 0
    state$m_d <- 0
    state$m_lastd <- 0
    state$m_mean <- 0.0
    state$m_std_temp <- 0.0
    state$m_m2s_max <- 0.0
    state$estimation <- 0.0
    state$concept_change <- FALSE

    obj$drifted <- TRUE
    obj$state <- state
    return(list(obj = obj, drift = TRUE))
  }

  if ((state$m_num_errors > state$m_min_num_errors) && (p < state$warning_level)) {
    state$in_warning_zone <- TRUE
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  state$in_warning_zone <- FALSE
  obj$state <- state
  return(list(obj = obj, drift = FALSE))
}

#'@export
fit.dfr_eddm <- function(obj, data, ...) {
  return(.fit_vector_stream(obj, data))
}

#'@export
reset_state.dfr_eddm <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_eddm(
    min_instances = obj$state$min_instances,
    min_num_errors = obj$state$m_min_num_errors,
    warning_level = obj$state$warning_level,
    out_control_level = obj$state$out_control_level
  )$state
  return(obj)
}
