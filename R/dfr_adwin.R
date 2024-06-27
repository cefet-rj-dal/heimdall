#'@title ADWIN method
#'@description Adaptive Windowing method for concept drift detection <doi:10.1137/1.9781611972771.42>.
#'@param target_feat Feature to be monitored.
#'@param delta The significance parameter for the ADWIN algorithm.
#ADWIN detection: Bifet, Albert, and Ricard Gavalda. “Learning from time-changing data with adaptive windowing.” In Proceedings of the 2007 SIAM international conference on data mining, pp. 443-448. Society for Industrial and Applied Mathematics, 2007.
#'@return `dfr_adwin` object
#'@examples
#'#Use the same example of dfr_cumsum changing the constructor to:
#'#model <- dfr_adwin(target_feat='serie')
#'@import reticulate
#'@export
dfr_adwin <- function(target_feat, delta=0.002) {
  obj <- dist_based(target_feat=target_feat)
  
  # Attributes
  state <- list()
  
  state$delta <- delta
  
  reticulate::source_python(system.file("python", "adwin.py", package="heimdall"))
  
  state$adwin <- ADWIN(delta=delta)

  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_adwin", class(obj))
  return(obj)
}

#'@export
update_state.dfr_adwin <- function(obj, value){
  
  state <- obj$state
  
  state$adwin$add_element(value)
  
  obj$state <- state
  has_drift <- state$adwin$detected_change()
  if (has_drift){
    obj$drifted <- has_drift
    return(list(obj=obj, pred=obj$drifted))
  }
  else{
    return(list(obj=obj, pred=FALSE))
  }
}

#'@export
fit.dfr_adwin <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_adwin <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_adwin(
    target_feat = obj$target_feat,
    delta=obj$state$delta
  )$state
  return(obj) 
}
