#'@title Drifter
#'@description Ancestor class for drift detection
#'@return Drifter object
#'@examples
#'# See ?dd_ddm for an example of DDM drift detector
#'@import daltoolbox
#'@export
drifter <- function() {
  obj <- dal_base()
  obj$drifted <- FALSE
  obj$state <- list()
  class(obj) <- append("drifter", class(obj))
  return(obj)
}


#'@title Reset State 
#'@description Reset Drifter State
#'@param obj Drifter object
#'@return updated Drifter object
#'@examples
#'# See ?hcd_ddm for an example of DDM drift detector
#'@export
reset_state <- function(obj) {
  UseMethod("reset_state")
}

#'@title Update State 
#'@description Update Drifter State
#'@param obj Drifter object
#'@param value a value that represents a processed batch
#'@return updated Drifter object
#'@examples
#'# See ?hcd_ddm for an example of DDM drift detector
#'@export
update_state <- function(obj, value, ...) {
  UseMethod("update_state")
}

#'@export
update_state <- function(obj, value) {
  UseMethod("update_state")
}

#'@export
reset_state.drifter <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- list()
  return(obj)  
}

#'@export
update_state.drifter <- function(obj, value) {
  return(obj)  
}

#'@title Process Batch 
#'@description Process Batch
#'@param obj Drifter object
#'@param data data batch in data frame format
#'@param prediction prediction batch as vector format
#'@param ... opitional arguments
#'@return updated Drifter object
#'@export
fit.drifter <- function(obj, data, prediction, ...) {
  return(obj)  
}

# Basic dummy detectors

#'@title Inactive dummy detector
#'@description Implements Inactive Dummy Detector
#'@return Drifter object
#'@examples
#'# See ?hcd_ddm for an example of DDM drift detector
#'@export
dfr_inactive <- function(){
  obj <- drifter()
  obj$state <- NULL
  
  obj$drifted <- FALSE
  class(obj) <- append('dfr_inactive', class(obj))
  return(obj)
}

#'@export
reset_state.dfr_inactive <- function(obj){
  return(obj)
}

#'@title Passive dummy detector
#'@description Implements Passive Dummy Detector
#'@return Drifter object
#'@examples
#'# See ?hcd_ddm for an example of DDM drift detector
#'@export
dfr_passive <- function(){
  obj <- drifter()
  obj$state <- NULL
  
  obj$drifted <- TRUE
  class(obj) <- append('dfr_passive', class(obj))
  return(obj)
}

#'@export
reset_state.dfr_passive <- function(obj){
  return(obj)
}

#'@title Error Based Drifter sub-class
#'@description Implements Error Based drift detectors
#'@return Drifter object
#'@examples
#'# See ?hcd_ddm for an example of DDM drift detector
#'@export
error_based <- function(){
  obj <- drifter()
  class(obj) <- append('error_based', class(obj))
  return(obj)
}

#'@title Distribution Based Drifter sub-class
#'@description Implements Distribution Based drift detectors
#'@param target_feat Feature to be monitored.
#'@return Drifter object
#'@export
dist_based <- function(target_feat){
  obj <- drifter()
  obj$target_feat <- target_feat
  class(obj) <- append('dist_based', class(obj))
  return(obj)
}

#'@title Multivariate Distribution Based Drifter sub-class
#'@description Implements Multivariate Distribution Based drift detectors
#'@return Drifter object
#'@export
mv_dist_based <- function(){
  obj <- drifter()
  class(obj) <- append('mv_dist_based', class(obj))
  return(obj)
}

#'@title Multi Criteria Drifter sub-class
#'@description Implements Multi Criteria drift detectors
#'@param drifter_list List of drifters to combine.
#'@param combination How the drifters will be combined. Possible values: 'fuzzy', 'or', 'and'.
#'@param fuzzy_window Sets the fuzzy window size. Only if combination = 'fuzzy'.
#'@return Drifter object
#'@export
dfr_multi_criteria <- function(drifter_list, combination='or', fuzzy_window=10){
  obj <- drifter()
  obj$drifter_list <- drifter_list
  obj$combination <- combination
  obj$fuzzy_window <- fuzzy_window
  obj$drifts_fuzzy <- c()
  obj$drifts <- c()
  
  state <- list()
  state$drifter_list <- drifter_list
  
  obj$state <- state
  
  obj$drifted <- FALSE
  class(obj) <- append('multi_criteria', class(obj))
  return(obj)
}

#'@export
update_state.multi_criteria <- function(obj, value){
  state <- obj$state
  
  state$row_data <- c()
  for(dft in names(state$drifter_list)){
    if ('dist_based' %in% class(state$drifter_list[[dft]])){
      if (is.null(state$drifter_list[[dft]]$target_feat)){
        value_mean <- mean(as.numeric(unlist(value)))
        drifter_output <- update_state(state$drifter_list[[dft]], value_mean)
        state$drifter_list[[dft]] <- drifter_output$obj
      }else{
        drifter_output <- update_state(state$drifter_list[[dft]], value[[state$drifter_list[[dft]]$target_feat]])
        state$drifter_list[[dft]] <- drifter_output$obj
      }
    }
    
    if ('mv_dist_based' %in% class(state$drifter_list[[dft]])){
      drifter_output <- update_state(state$drifter_list[[dft]], value)
      state$drifter_list[[dft]] <- drifter_output$obj
    }
    state$row_data <- cbind(state$row_data, drifter_output$drift)
  }
  obj$drifts <- rbind(obj$drifts, state$row_data)

  if(obj$combination == 'or'){
    if(mean(state$row_data) > 0){has_drift <- TRUE}else{has_drift <- FALSE}
  }else if(obj$combination == 'and'){
    if(mean(state$row_data) == 1){has_drift <- TRUE}else{has_drift <- FALSE}
  }else if(obj$combination == 'fuzzy'){
    obj$drifts_fuzzy <- c()
    for(drifter_col in 1:ncol(obj$drifts)){
      drifter_fuzzy <- rep(0, nrow(obj$drifts))
      drifts_index <- which(obj$drifts[, drifter_col]==TRUE)
      for(i in drifts_index){
        umbrella_vector <- max(1, i-obj$fuzzy_window + 1):min(i + (obj$fuzzy_window-1), nrow(df))
        drifter_fuzzy[min(umbrella_vector):i] <- ((obj$fuzzy_window - length(min(umbrella_vector):i)) + 1):(obj$fuzzy_window) / obj$fuzzy_window
        drifter_fuzzy[i:max(umbrella_vector)] <- obj$fuzzy_window:(obj$fuzzy_window-length(i:max(umbrella_vector)) + 1) / obj$fuzzy_window
      }
      obj$drifts_fuzzy <- cbind(obj$drifts_fuzzy, drifter_fuzzy)
    }
    if(sum(rowSums(obj$drifts_fuzzy) > (length(ncol(obj$drifts))/2))){has_drift <- TRUE}else{has_drift <- FALSE}
  }
  
  obj$state <- state
  if (has_drift){
    obj$drifted <- has_drift
    return(list(obj=obj, drift=obj$drifted))
  }
  else{
    return(list(obj=obj, drift=FALSE))
  }
}

#'@export
fit.multi_criteria <- function(obj, data, ...){
  output <- update_state(obj, data[1,])
  if(nrow(data) > 1){
    for (i in 2:nrow(data)){
      output <- update_state(output$obj, data[i,])
    }
  }
  return(output$obj)
}

#'@export
reset_state.multi_criteria <- function(obj) {
  obj <- dfr_multi_criteria(
    drifter_list=obj$drifter_list,
    combination=obj$combination, 
    fuzzy_window=obj$fuzzy_window
  )
  return(obj) 
}

