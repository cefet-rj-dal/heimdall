#'@title KL Distance method
#'@description Kullback Leibler Windowing method for concept drift detection.
#'@param target_feat Feature to be monitored.
#'@param p_th Probability theshold for the test statistic of the Kullback Leibler distance.
#'@param window_size Size of the sliding window (must be > 2*stat_size)
#'@param data Already collected data to avoid cold start.
#KSWIN detection: Christoph Raab, Moritz Heusinger, Frank-Michael Schleif, Reactive Soft Prototype Computing for Concept Drift Streams, Neurocomputing, 2020.
#KSWIN detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/kswin.py#L5
#'@return `dfr_kldist` object
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
#'model <- dfr_kldist(target_feat='serie')
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
dfr_kldist <- function(target_feat, window_size=100, p_th=0.9, data=NULL) {
    obj <- dist_based(target_feat=target_feat)
    
    state <- list()
    state$window_size <- window_size
    state$p_th <- p_th
    state$p_value <- 0
    state$n <- 0

    if ((state$p_th < 0) | (state$p_th > 1)) stop("Alpha must be between 0 and 1", call = FALSE)
    if (state$window_size < 0) stop("window_size must be greater than 0", call = FALSE)

    if (missing(data)){
      state$window <- c()
    }
    else{
      state$window <- data
    }
    
    obj$state <- state

    class(obj) <- append("dfr_kldist", class(obj))
    return(obj)
}

#'@importFrom utils head tail
#'@export
update_state.dfr_kldist <- function(obj, value) {
  state <- obj$state

  state$n <- state$n + 1
  currentLength <- nrow(state$window)
  if (is.null(currentLength)){
    currentLength <- 0
  }
  
  if (currentLength >= state$window_size){
    state$window <- tail(state$window, -1)
    p_window <- tail(state$window, state$window_size/2)
    q_window <- head(state$window, state$window_size/2)
    
    p <- p_window / sum(p_window)
    q <- q_window / sum(q_window)
    
    state$kl <- sum(p * log(p/q, base=2), na.rm=TRUE)
    
    if((state$kl >= state$p_th)){
      state$window <- tail(state$window, state$window_size/2)
      state$window <- rbind(state$window, value)
      
      obj$drifted <- TRUE
      
      obj$state <- state
      return(list(obj=obj, pred=TRUE))
    }
    else{
      state$window <- rbind(state$window, value)
      
      obj$state <- state
      return(list(obj=obj, pred=FALSE))
    }
  }else{
    state$window <- rbind(state$window, value)
  
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
  obj$state <- state
  return(list(obj=obj, pred=obj$drifted))
}

#'@export
fit.dfr_kldist <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_kldist <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kldist(
    target_feat = obj$target_feat,
    p_th = obj$state$p_th
  )$state
  return(obj)  
}