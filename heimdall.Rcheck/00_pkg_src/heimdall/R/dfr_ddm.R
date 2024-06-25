#'@title Adapted Drift Detection Method (DDM) method
#'@description DDM is a concept change detection method based on the PAC learning model premise, that the learner’s error rate will decrease as the number of analysed samples increase, as long as the data distribution is stationary. <doi:10.1007/978-3-540-28645-5_29>.
#'@param min_instances The minimum number of instances before detecting change
#'@param warning_level Necessary level for warning zone (2 standard deviation)
#'@param out_control_level Necessary level for a positive drift detection
#DDM: João Gama, Pedro Medas, Gladys Castillo, Pedro Pereira Rodrigues: Learning with Drift Detection. SBIA 2004: 286-295.
#DDM implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/ddm.py
#'@return `dfr_ddm` object
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
#'model <- dfr_ddm()
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
dfr_ddm <- function(min_instances=30, warning_level=2.0, out_control_level=3.0) {
  obj <- error_based()
  
  state <- list()
  
  state$min_instances <- min_instances
  state$warning_level <- warning_level
  state$out_control_level <- out_control_level
  
  state$sample_count <- 1
  state$miss_prob <- 1.0
  state$miss_std <- 0.0
  state$miss_prob_sd_min <- Inf
  state$miss_prob_min <- Inf
  state$miss_sd_min <- Inf
  
  obj$state <- state
  
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_ddm", class(obj))
  
  return(obj)
}

#'@export
update_state.dfr_ddm <- function(obj, value){
  if (is.na(value)){
    value <- 0
  }
  state <- obj$state
  state$miss_prob <- state$miss_prob + (value - state$miss_prob) / state$sample_count
  state$miss_std <- sqrt(abs(state$miss_prob * (1 - state$miss_prob))) / state$sample_count
  state$sample_count <- state$sample_count + 1
  
  state$estimation <- state$miss_prob
  state$in_concept_change <- FALSE
  state$in_warning_zone <- FALSE
  state$delay <- 0
  
  if(state$sample_count < state$min_instances){
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
  
  if((state$miss_prob + state$miss_std) <= state$miss_prob_sd_min){
    state$miss_prob_min <- state$miss_prob
    state$miss_sd_min <- state$miss_std
    state$miss_prob_sd_min <- state$miss_prob + state$miss_std
    state$sum <- 0
    state$sample_count <- 1
  }
  
  if((state$miss_prob + state$miss_std) > (state$miss_prob_min + state$out_control_level * state$miss_sd_min)){
    state$sample_count <- 1
    state$miss_prob <- 1.0
    state$miss_std <- 0.0
    state$miss_prob_sd_min <- Inf
    state$miss_prob_min <- Inf
    state$miss_sd_min <- Inf
    
    obj$drifted <- TRUE
    obj$state <- state
    return(list(obj=obj, pred=TRUE))
  }else if((state$miss_prob + state$miss_std) > (state$miss_prob_min + state$warning_level * state$miss_sd_min)){
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }else{
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
}

#'@export
fit.dfr_ddm <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_ddm <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_ddm(
    min_instances = obj$state$min_instances,
    warning_level = obj$state$warning_level,
    out_control_level = obj$state$out_control_level
    )$state
  return(obj)  
}