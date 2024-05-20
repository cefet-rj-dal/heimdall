#'@title Stealthy
#'@description Ancestor class for drift adaptive models
#'@param model The algorithm object to be used for predictions
#'@param drift_method The algorithm object to detect drifts
#'@param th The threshold to be used with classification algorithms
#'@return Stealthy object
#'@examples
#'# See ?dd_ddm for an example of DDM drift detector
#'@import daltoolbox
#'@import stats
#'@importFrom caret dummyVars
#'@export
stealthy <- function(model, drift_method, th=0.5){
  obj <- dal_base()
  obj$dummy <- NULL
  obj$model <- model
  obj$model$is_fitted <- FALSE
  obj$drift_method <- drift_method
  obj$drifted <- FALSE
  obj$x_train <- c()
  obj$y_train <- c()
  obj$th <- th
  attr(obj, 'class') <- 'stealthy'
  return(obj)
}

#'@export
update_state.stealthy <- function(obj, value, ...){
  obj$drift_technique <- update_state(obj$drift_technique, value)
  return(obj)
}

#'@export
fit.stealthy <- function(obj, x, y, ...){
  # Check Drift
  obj$drifted <- FALSE
  if (obj$model$is_fitted){
    x_oh <- data.frame(predict(obj$dummy, newdata = x))
    if (!all(obj$dummy$feat_names %in% names(x_oh))){
      print('Some categories present on train are not on test dataset.')
    }
    if ('error_based' %in% class(obj$drift_method)){
      predictions <- predict(obj$model, x_oh)
      y_pred <- predictions[, 2] > obj$th
      
      model_result = y==y_pred
      model_result <- model_result[complete.cases(model_result)]
      
      obj$drift_method <- fit(obj$drift_method, model_result)
    }
    
    if ('dist_based' %in% class(obj$drift_method)){
      obj$drift_method <- fit(obj$drift_method, x_oh[,obj$drift_method$target_feat])
    }
    
    if(obj$drift_method$drifted){
      print('Stealthy detected a drift, discarding old data')
      obj$x_train <- c()
      obj$y_train <- c()  
      obj$drift_method <- reset_state(obj$drift_method)
      obj$drifted <- TRUE
    }
  }
  # Aggregate new data
  obj$x_train <- rbind(obj$x_train, x)
  obj$y_train <- rbind(obj$y_train, y)
  
  # One Hot Encoding
  obj$dummy <- caret::dummyVars(" ~ .", data=x_train)
  x_train <- data.frame(predict(obj$dummy, newdata = obj$x_train))
  obj$dummy$feat_names <- names(x_train)
  
  # Define train data
  data <- cbind(x_train, obj$y_train)
  # Fit model
  obj$model <- fit(obj$model, data)
  obj$model$is_fitted <- TRUE
  return(obj)
}

#'@export
predict.stealthy <- function(object, data, ...){
  data_oh <- data.frame(predict(object$dummy, newdata = data))
  return(predict(object$model, data_oh))
}
