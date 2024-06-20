#'@title Adapted Page Hinkley method
#'@description Change-point detection method works by computing the observed values and their mean up to the current moment <doi:10.2307/2333009>.
#'@param target_feat Feature to be monitored.
#'@param min_instances The minimum number of instances before detecting change
#'@param delta The delta factor for the Page Hinkley test
#'@param threshold The change detection threshold (lambda)
#'@param alpha The forgetting factor, used to weight the observed value and the mean
#Page Hinkley detection: E. S. Page. (1954) Continuous Inspection Schemes, Biometrika 41(1/2), 100–115.
#Page Hinkley detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/page_hinkley.py#L4
#'@return `dfr_page_hinkley` object
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'# This example assumes a model residual where 1 is an error and 0 is a correct prediction.
#'
#'data(st_drift_examples)
#'data <- st_drift_examples$univariate
#'data$event <- NULL
#'data$prediction <- st_drift_examples$univariate$serie > 4
#'
#'
#'model <- dfr_page_hinkley(target_feat='serie')
#'
#'detection <- c()
#'output <- list(obj=model, pred=FALSE)
#'for (i in 1:length(data$serie)){
#'  output <- update_state(output$obj, data$serie[i])
#'  if (output$pred){
#'    type <- 'drift'
#'    output$obj <- reset_state(output$obj)
#'  }else{
#'    type <- ''
#'  }
#'  detection <- rbind(detection, list(idx=i, event=output$pred, type=type))
#'}
#'
#'detection <- as.data.frame(detection)
#'detection[detection$type == 'drift',]
#'@export
dfr_page_hinkley <- function(target_feat, min_instances=30, delta=0.005, threshold=50, alpha=1-0.0001) {
  obj <- dist_based(target_feat=target_feat)
  
  state <- list()
  state$min_instances <- min_instances
  state$delta <- delta
  state$threshold <- threshold
  state$alpha = alpha
  state$x_mean <- 0
  state$sum <- 0
  state$sample_count <- 1
  
  obj$state <- state
  
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_page_hinkley", class(obj))
  return(obj)
}

#'@export
update_state.dfr_page_hinkley <- function(obj, value){
  state <- obj$state
  
  state$x_mean <- state$x_mean + (value - state$x_mean)/state$sample_count
  state$sum <- max(0, state$alpha * state$sum + (value - state$x_mean - state$delta))
  state$sample_count <- state$sample_count + 1
  
  if(state$sample_count < state$min_instances){
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
  else if(state$sum > state$threshold){
    state$x_mean <- 0
    state$sum <- 0
    state$sample_count <- 1
    
    obj$drifted <- TRUE
    
    obj$state <- state
    return(list(obj=obj, pred=TRUE))
  }
  else{
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
  
  return(list(obj=obj, pred=obj$drifted))
}

#'@export
fit.dfr_page_hinkley <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_page_hinkley <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_page_hinkley(
    target_feat = obj$target_feat,
    min_instances = obj$state$min_instances,
    delta = obj$state$delta,
    threshold = obj$state$threshold,
    alpha = obj$state$alpha
  )$state
  return(obj)  
}