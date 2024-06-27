#'@title Adapted Early Drift Detection Method (EDDM) method
#'@description EDDM (Early Drift Detection Method) aims to improve the detection rate of gradual concept drift in DDM, while keeping a good performance against abrupt concept drift. <doi:2747577a61c70bc3874380130615e15aff76339e>
#'@param min_instances The minimum number of instances before detecting change
#'@param min_num_errors The minimum number of errors before detecting change
#'@param warning_level Necessary level for warning zone
#'@param out_control_level Necessary level for a positive drift detection
#EDDM: Early Drift Detection Method. Manuel Baena-Garcia, Jose Del Campo-Avila, Raúl Fidalgo, Albert Bifet, Ricard Gavalda, Rafael Morales-Bueno. In Fourth International Workshop on Knowledge Discovery from Data Streams, 2006.
#EDDM implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/eddm.py
#'@return `dfr_eddm` object
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'# This example uses an error-based drift detector with a synthetic a 
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
#'for (i in 1:length(data$prediction)){
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
dfr_eddm <- function(min_instances=30, min_num_errors=30, warning_level=0.95, out_control_level=0.9) {
  
  obj <- error_based()
  
  state <- list()
  
  state$min_instances <- min_instances
  state$m_min_num_errors <- min_num_errors
  state$warning_level <- warning_level
  state$out_control_level <- out_control_level
  
  state$m_num_errors <- NULL
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
update_state.dfr_eddm <- function(obj, value){
  state <- obj$state
  
  state$m_n <- state$m_n + 1
  
  if(value == 1){
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
    
    if(state$m_n < state$min_instances){
      obj$state <- state
      return(list(obj=obj, drift=FALSE))
    }
    
    if(m2s > state$m_m2s_max){
      state$m_m2s_max <- m2s
      
      obj$state <- state
      return(list(obj=obj, drift=FALSE))
    }
    else{
      p <- m2s / state$m_m2s_max
      
      if((state$m_num_errors > state$m_min_num_errors) & (p < state$out_control_level)){
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
        return(list(obj=obj, drift=TRUE))
      }
      else if((state$m_num_errors > state$m_min_num_errors) & (p < state$warning_level)){
        obj$state <- state
        return(list(obj=obj, drift=FALSE))
      }
      else{
        obj$state <- state
        return(list(obj=obj, drift=FALSE))
      }
    }
  }
  else{
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
}

#'@export
fit.dfr_eddm <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
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