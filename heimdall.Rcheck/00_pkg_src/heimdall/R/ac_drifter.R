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
inactive <- function(){
  obj <- drifter()
  obj$state <- NULL
  
  obj$drifted <- FALSE
  class(obj) <- append('inactive', class(obj))
  return(obj)
}

#'@export
reset_state.inactive <- function(obj){
  return(obj)
}

#'@title Passive dummy detector
#'@description Implements Passive Dummy Detector
#'@return Drifter object
#'@examples
#'# See ?hcd_ddm for an example of DDM drift detector
#'@export
passive <- function(){
  obj <- drifter()
  obj$state <- NULL
  
  obj$drifted <- TRUE
  class(obj) <- append('passive', class(obj))
  return(obj)
}

#'@export
reset_state.passive <- function(obj){
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

#'@title Multi Criteria Drifter sub-class
#'@description Implements Multi Criteria drift detectors
#'@return Drifter object
#'@export
multi_criteria <- function(){
  obj <- drifter()
  class(obj) <- append('multi_criteria', class(obj))
  return(obj)
}
