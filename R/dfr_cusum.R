#'@title Cumulative Sum for Concept Drift Detection (CUSUM) method
#'@description CUSUM is a sequential analysis procedure that accumulates deviations in a monitored signal and raises an alarm when the cumulative evidence exceeds a threshold. In this package, the detector is implemented as an error-based monitor, so it is primarily intended for **real concept drift** affecting predictive performance. The concept-drift adaptation follows the sequential change-detection literature discussed by Muthukrishnan, Berg, and Wu (2007) <doi:10.1109/ICDMW.2007.89>.
#'@param lambda Necessary level for warning zone (2 standard deviation)
#CUMSUM: S. Muthukrishnan, Eric Berg, Yihua Wu: Sequential Change Detection on Data Streams. Seventh IEEE International Conference on Data Mining Workshops (ICDMW 2007), DOI:10.1109/ICDMW.2007.89
#'@references Muthukrishnan, S., Berg, E., and Wu, Y. (2007). Sequential change detection on data streams. In *Seventh IEEE International Conference on Data Mining Workshops (ICDMW 2007)*. <doi:10.1109/ICDMW.2007.89>
#'@return `dfr_cusum` object
#'@example examples/1_detection/r/dfr_cusum.R
#'@import daltoolbox
#'@example examples/2_online_prediction/r/dfr_cusum.R
#'@export
dfr_cusum <- function(lambda=100) {
  obj <- error_based()
  
  state <- list()
  
  state$lambda <- lambda
  
  state$g <- 0
  state$last_g <- NULL
  
  obj$state <- state
  
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_cusum", class(obj))
  
  return(obj)
}

#'@export
update_state.dfr_cusum <- function(obj, value){
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
fit.dfr_cusum <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_cusum <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_cusum(
    lambda = obj$state$lambda
  )$state
  return(obj)  
}
