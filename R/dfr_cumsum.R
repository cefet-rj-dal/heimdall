#'@title Cumulative Sum for Concept Drift Detection (CUMSUM) method
#'@description The cumulative sum (CUSUM) is a sequential analysis technique used for change detection.
#'@param lambda Necessary level for warning zone (2 standard deviation)
#CUMSUM: S. Muthukrishnan, Eric Berg, Yihua Wu: Sequential Change Detection on Data Streams. Seventh IEEE International Conference on Data Mining Workshops (ICDMW 2007), DOI:10.1109/ICDMW.2007.89
#'@return `dfr_cumsum` object
#'@import ggplot2
#'@importFrom daltoolbox cla_nb
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
#'model <- dfr_cumsum()
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
dfr_cumsum <- function(lambda=100) {
  obj <- error_based()
  
  state <- list()
  
  state$lambda <- lambda
  
  state$g <- 0
  state$last_g <- NULL
  
  obj$state <- state
  
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_cumsum", class(obj))
  
  return(obj)
}

#'@export
update_state.dfr_cumsum <- function(obj, value){
  if (is.na(value)){
    value <- 0
  }
  
  if (value == 0){
    value = -1
  }
  
  state <- obj$state
  
  state$last_g <- state$g
  state$g <- max(0, state$last_g + value)
  
  obj$state <- state
  if (state$g > state$lambda){
    obj$drifted <- TRUE
    return(list(obj=obj, drift=TRUE))
  }else{
    return(list(obj=obj, drift=FALSE))
  }
}

#'@export
fit.dfr_cumsum <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_cumsum <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_cumsum(
    lambda = obj$state$lambda
  )$state
  return(obj)  
}