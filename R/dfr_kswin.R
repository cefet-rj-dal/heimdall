#'@title KSWIN method
#'@description Kolmogorov-Smirnov Windowing method for concept drift detection <doi:10.1016/j.neucom.2019.11.111>.
#'@param target_feat Feature to be monitored.
#'@param alpha Probability for the test statistic of the Kolmogorov-Smirnov-Test The alpha parameter is very sensitive, therefore should be set below 0.01.
#'@param window_size Size of the sliding window (must be > 2*stat_size)
#'@param stat_size Size of the statistic window
#'@param data Already collected data to avoid cold start.
#KSWIN detection: Christoph Raab, Moritz Heusinger, Frank-Michael Schleif, Reactive Soft Prototype Computing for Concept Drift Streams, Neurocomputing, 2020.
#KSWIN detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/kswin.py#L5
#'@return `dfr_kswin` object
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
#'model <- dfr_kswin(target_feat='serie')
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
dfr_kswin <- function(target_feat, window_size=100, stat_size=30, alpha=0.005, data=NULL) {
    obj <- dist_based(target_feat=target_feat)
    
    state <- list()
    state$window_size <- window_size
    state$stat_size <- stat_size
    state$alpha = alpha
    state$p_value <- 0
    state$n <- 0

    if ((state$alpha < 0) | (state$alpha > 1)) stop("Alpha must be between 0 and 1", call = FALSE)
    if (state$window_size < 0) stop("window_size must be greater than 0", call = FALSE)
    if (state$window_size < state$stat_size) stop("stat_size must be smaller than window_size")

    if (missing(data)){
      state$window <- c()
    }
    else{
      state$window <- data
    }
    
    obj$state <- state

    class(obj) <- append("dfr_kswin", class(obj))
    return(obj)
}

#'@importFrom stats ks.test
#'@export
update_state.dfr_kswin <- function(obj, value) {
  state <- obj$state

  state$n <- state$n + 1
  currentLength <- nrow(state$window)
  if (is.null(currentLength)){
    currentLength <- 0
  }
  
  if (currentLength >= state$window_size){
    state$window <- tail(state$window, -1)
    rnd_window <- sample(x=state$window[1:(length(state$window)-state$stat_size)], size=state$stat_size)
  
    ks_res <- stats::ks.test(rnd_window, state$window[(length(state$window)-state$stat_size):length(state$window)], exact=TRUE)
    st <- unlist(ks_res[1])
    state$p_value <- unlist(ks_res[2])
    
    if((state$p_value < state$alpha) & (st > 0.1)){
      state$window <- tail(state$window, (state$stat_size))
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
fit.dfr_kswin <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_kswin <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kswin(
    target_feat = obj$target_feat,
    window_size = obj$state$window_size,
    stat_size = obj$state$stat_size,
    alpha = obj$state$alpha,
    data = obj$state$data
  )$state
  return(obj)  
}