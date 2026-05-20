#'@title KSWIN method
#'@description KSWIN applies a Kolmogorov-Smirnov test between a recent window and a reference sample drawn from older observations. In this package, the method is primarily used for **virtual concept drift**, because it monitors distributional changes in a numeric feature stream. The method follows Raab et al. (2020) <doi:10.1016/j.neucom.2019.11.111>.
#'@param target_feat Feature to be monitored.
#'@param alpha Probability for the test statistic of the Kolmogorov-Smirnov-Test The alpha parameter is very sensitive, therefore should be set below 0.01.
#'@param window_size Size of the sliding window (must be > 2*stat_size)
#'@param stat_size Size of the statistic window
#'@param data Already collected data to avoid cold start.
#KSWIN detection: Christoph Raab, Moritz Heusinger, Frank-Michael Schleif, Reactive Soft Prototype Computing for Concept Drift Streams, Neurocomputing, 2020.
#KSWIN detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/kswin.py#L5
#'@references Raab, C., Heusinger, M., and Schleif, F.-M. (2020). Reactive soft prototype computing for concept drift streams. *Neurocomputing*, 416, 340-351. <doi:10.1016/j.neucom.2019.11.111>
#'@return `dfr_kswin` object
#'@example examples/1_detection/r/dfr_kswin.R
#'@export
dfr_kswin <- function(target_feat=NULL, window_size=1500, stat_size=500, alpha=0.0000001, data=NULL) {
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
      state$window <- numeric(0)
    }
    else{
      state$window <- as.numeric(data)
    }
    
    obj$state <- state
    
    obj$last_drifter_output <- NULL

    class(obj) <- append("dfr_kswin", class(obj))
    return(obj)
}

#'@importFrom stats ks.test
#'@export
update_state.dfr_kswin <- function(obj, value) {
  obj$last_drifter_output <- c(NA, NA)
  
  state <- obj$state

  state$n <- state$n + 1
  value <- as.numeric(value[1])
  if (is.na(value)) {
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  state$window <- c(state$window, value)
  
  currentLength <- length(state$window)
  
  if (currentLength >= state$window_size){
    rnd_window <- head(state$window, length(state$window)-state$stat_size)
    stat_window <- tail(state$window, state$stat_size)
    
    ks_res <- stats::ks.test(rnd_window, stat_window, exact=TRUE)
    st <- unlist(ks_res[1])
    state$p_value <- unlist(ks_res[2])
    obj$last_drifter_output <- cbind(st, state$p_value)
    
    if((state$p_value < state$alpha)){
      state$window <- tail(state$window, state$stat_size)
      
      obj$drifted <- TRUE
      
      obj$state <- state
      return(list(obj=obj, drift=TRUE))
    }
    else{
      obj$state <- state
      return(list(obj=obj, drift=FALSE))
    }
  }else{
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  obj$state <- state
  return(list(obj=obj, drift=obj$drifted))
}

#'@export
fit.dfr_kswin <- function(obj, data, ...){
  
  obj$drifter_output <- NULL
  obj$last_drifter_output <- NULL
  output <- update_state(obj, data[1])
  output$obj$drifter_output <- rbind(output$obj$drifter_output, output$obj$last_drifter_output)
  if (length(data) > 1){
    for (i in 2:length(data)){
      output <- update_state(output$obj, data[i])
      output$obj$drifter_output <- rbind(output$obj$drifter_output, output$obj$last_drifter_output)
    }
  }
  
  output$obj$drifter_output <- as.data.frame(output$obj$drifter_output)
  names(output$obj$drifter_output) <- c('D', 'p')
  
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
    data = obj$state$window
  )$state
  return(obj)  
}
