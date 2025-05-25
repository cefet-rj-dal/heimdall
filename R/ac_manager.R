#'@title Manager
#'@description Ancestor class for Model Management
#'@return Manager object
#'@examples
#'# See ?dd_ddm for an example of DDM drift detector
#'@import daltoolbox
#'@export
manager <- function(algorithm, model_selector, model_deleter, n_slots=1) {
  obj <- dal_base()
  
  obj$algorithm <- algorithm
  obj$n_slots <- n_slots
  obj$model_selector <- model_selector
  obj$model_deleter <- model_deleter
  
  class(obj) <- append("manager", class(obj))
  return(obj)
}


#'@title Process Batch 
#'@description Process Batch
#'@param obj manager object
#'@param data data batch in data frame format
#'@param prediction prediction batch as vector format
#'@param ... opitional arguments
#'@return updated Manager object
#'@export
fit.manager <- function(obj, x, y, ...) {
  
  print('FIT MM')
  
  obj$model <- fit(obj$algorithm, x, y)
  
  return(obj)  
}

#'@title Predict Batch 
#'@description Predict Batch
#'@param obj manager object
#'@param data data batch in data frame format
#'@param prediction prediction batch as vector format
#'@param ... opitional arguments
#'@return predicted data
#'@export
predict.manager <- function(object, data, ...) {
  
  print('PREDICT MM')
  
  y_pred <- predict(object$model, data)
  
  return(y_pred)
}
