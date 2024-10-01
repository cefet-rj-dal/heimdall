#'@title Stealthy
#'@description Ancestor class for drift adaptive models
#'@param model The algorithm object to be used for predictions
#'@param drift_method The algorithm object to detect drifts
#'@param th The threshold to be used with classification algorithms
#'@param target_uni_drifter Passes the prediction target to the drifts as the target feat when the drifter is univariate and dist_based.
#'@param verbose if TRUE shows drift messages
#'@return Stealthy object
#'@examples
#'# See ?dd_ddm for an example of DDM drift detector
#'@import daltoolbox
#'@import stats
#'@importFrom caret dummyVars
#'@export
stealthy <- function(model, drift_method, th=0.5, target_uni_drifter=FALSE, verbose=FALSE){
  obj <- dal_base()
  obj$dummy <- NULL
  obj$model <- model
  obj$fitted <- FALSE
  obj$drift_method <- drift_method
  obj$drifted <- FALSE
  obj$x_train <- c()
  obj$y_train <- c()
  obj$th <- th
  obj$norm_model <- minmax()
  obj$target_uni_drifter <- target_uni_drifter
  obj$verbose <- verbose
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
  if (obj$fitted){
    x_oh <- data.frame(predict(obj$dummy, newdata = x))
    norm_x_oh <- transform(obj$norm_model, x_oh)
    if (!all(obj$dummy$feat_names %in% names(x_oh))){
      warning('Some categories present on train are not on the most recent dataset. Creating zero columns.')
      for (feat in obj$dummy$feat_names){
        if (!(feat %in% names(x_oh))){
          x_oh[feat] <- 0
        }
      }
    }
    if ('error_based' %in% class(obj$drift_method)){
      predictions <- predict(obj$model, x_oh)
      y_pred <- predictions[, 2] > obj$th
      
      model_result = y==y_pred
      model_result <- model_result[complete.cases(model_result)]
      
      obj$drift_method <- fit(obj$drift_method, model_result)
    }
    
    if ('dist_based' %in% class(obj$drift_method)){
      if (is.null(obj$drift_method$target_feat)){
        norm_x_oh[,'mean'] <- rowMeans(norm_x_oh)
        obj$drift_method <- fit(obj$drift_method, norm_x_oh[,'mean'])
      }else if(obj$target_uni_drifter){
        obj$drift_method <- fit(obj$drift_method, y[, 1]*1)
      }else{
        obj$drift_method <- fit(obj$drift_method, x_oh[,obj$drift_method$target_feat])
      }
    }
    
    if ('mv_dist_based' %in% class(obj$drift_method)){
      obj$drift_method <- fit(obj$drift_method, norm_x_oh[,obj$drift_method$features])
    }
    
    if(obj$drift_method$drifted){
      if(obj$verbose){
        message('Stealthy detected a drift, discarding old data')
      }
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
  obj$dummy <- caret::dummyVars(" ~ .", data=obj$x_train)
  x_train_dummy <- data.frame(predict(obj$dummy, newdata = obj$x_train))
  obj$dummy$feat_names <- names(x_train_dummy)
  
  # Define train data
  data <- cbind(x_train_dummy, obj$y_train)
  
  # Normalize
  obj$norm_model <- fit(obj$norm_model, data)
  norm_data <- transform(obj$norm_model, data)

  # Fit model
  obj$model <- fit(obj$model, norm_data)
  obj$model$feat_names <- names(data)
  obj$fitted <- TRUE
  return(obj)
}

#'@export
predict.stealthy <- function(object, data, ...){
  data_oh <- data.frame(predict(object$dummy, newdata = data))
  for (feat in object$model$feat_names){
    if (!(feat %in% names(data_oh))){
      data_oh[feat] <- 0
    }
  }
  norm_data_oh <- transform(object$norm_model, data_oh)
  return(predict(object$model, norm_data_oh))
}
