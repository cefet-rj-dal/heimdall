#'@title Adapted EWMA for Concept Drift Detection (ECDD) method
#'@description ECDD is a concept change detection method that uses an exponentially weighted moving average (EWMA) chart to monitor the misclassification rate of an streaming classifier.
#'@param lambda The minimum number of instances before detecting change
#'@param min_run_instances Necessary level for warning zone (2 standard deviation)
#'@param average_run_length Necessary level for a positive drift detection
#ECDD: Gordon Ross, Niall Adams, Dimitris Tasoulis, David Hand: Exponentially weighted moving average charts for detecting concept drift. Pattern Recognition Letters 2012, Volume 33, Issue 2: 191-198, DOI:10.1016/j.patrec.2011.08.019
#ECDD Implementation: Jaime Sisniega, Álvaro García: Frouros: An open-source Python library for drift detection in machine learning systems. Neurocomputing, Volume 26, 2024, DOI: 10.1016/j.softx.2024.101733
#ECDD implementation: Frouros, https://github.com/IFCA-Advanced-Computing/frouros/blob/acde82386da735ca8e15f85112f48d5cfb10cc9a/frouros/detectors/concept_drift/streaming/statistical_process_control/ecdd.py
#'@return `dfr_ecdd` object
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'# This example uses a dist-based drift detector with a synthetic dataset.
#'
#'data(st_drift_examples)
#'data <- st_drift_examples$univariate
#'data$event <- NULL
#'
#'model <- dfr_ecdd()
#'
#'detection <- NULL
#'output <- list(obj=model, pred=FALSE)
#'for (i in 1:length(data$serie)){
#'  output <- update_state(output$obj, data$serie[i])
#'  if (output$pred){
#'    type <- 'drift'
#'    output$obj <- reset_state(output$obj)
#'  }else{
#'    type <- ''
#'  }
#'  detection <- rbind(detection, data.frame(idx=i, event=output$pred, type=type))
#'}
#'
#'detection[detection$type == 'drift',]
#'@export
dfr_ecdd <- function(lambda=0.2, min_run_instances=30, average_run_length=100) {
  obj <- error_based()
  
  state <- list()
  
  state$size <- 0
  
  state$p <- 0
  state$last_p <- NULL
  state$Z <- 0
  state$last_Z <- NULL
  
  state$lambda <- lambda
  state$min_run_instances <- min_run_instances
  state$average_run_length <- average_run_length
  
  obj$state <- state
  
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_ecdd", class(obj))
  
  return(obj)
}

#'@export
update_state.dfr_ecdd <- function(obj, value){
  if (is.na(value)){
    value <- 0
  }
  state <- obj$state
  
  state$size <- state$size + 1
  
  state$last_p <- state$p
  state$p <- (value - state$last_p) / state$size
  
  state$last_Z <- state$Z
  state$Z <- ((1 - state$lambda)*state$last_Z) + (state$lambda * value)
  
  if (state$size > state$min_run_instances){
    error_rate_variance <- state$p * (1 - state$p)
    z_variance <- sqrt(
      abs((state$lambda / (2 - state$lambda)) * (1 - (1 - state$lambda) ** (2 * state$size)) * error_rate_variance)
    )
    
    if (state$average_run_length <= 100){
      control_limit <- 2.76 - (6.23 * state$p) + (18.12 * (state$p ** 3)) - (312.45 * (state$p ** 5)) + (1002.18 * (state$p ** 7))
    }else if(state$average_run_length <= 400){
      control_limit <- 3.97 - (6.56 * state$p) + (48.73 * (state$p ** 3)) - (330.13 * (state$p ** 5)) + (987.23 * (state$p ** 7))
    }else if(state$average_run_length <= 1000){
      control_limit <- 1.17 + (7.56 * state$p) - (21.24 * (state$p ** 3)) + (112.12 * (state$p ** 5)) - (987.23 * (state$p ** 7))
    }
    
    if (state$Z > (state$p + 1 * control_limit * z_variance)){
      obj$state <- state
      obj$drifted <- TRUE
      return(list(obj=obj, pred=TRUE))
    }else{
      obj$state <- state
      return(list(obj=obj, pred=FALSE))
    }
  }else{
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
}

#'@export
fit.dfr_ecdd <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_ecdd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_ecdd(
    lambda = obj$state$lambda,
    min_run_instances = obj$state$min_run_instances,
    average_run_length = obj$state$average_run_length
    
  )$state
  return(obj)  
}