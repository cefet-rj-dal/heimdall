#'@title Stealthy
#'@description Ancestor class for drift adaptive models
#'@param model The algorithm object to be used for predictions
#'@param drift_method The algorithm object to detect drifts
#'@param monitored_features List of features that will be monitored by the drifter
#'@param norm_class Class used to perform normalization
#'@param warmup_size Number of rows used to warmup the drifter. No drift will be detected during this phase
#'@param th The threshold to be used with classification algorithms
#'@param target_uni_drifter Passes the prediction target to the drifts as the target feat when the drifter is univariate and dist_based.
#'@param incremental_memory If true, the model will retrain with all available data whenever the fit is called. If false, it only retrains when a drift is detected.
#'@param verbose if TRUE shows drift messages
#'@param reporting If TRUE, some data are returned as norm_x_oh, drift_input, hist_proj, and recent_proj.
#'@return Stealthy object
#'@examples
#'# See ?dd_ddm for an example of DDM drift detector
#'@import daltoolbox
#'@import stats
#'@importFrom caret dummyVars
#'@export
stealthy <- function(model, drift_method, monitored_features=NULL, norm_class=daltoolbox::fixed_zscore(), warmup_size=100, th=0.5, target_uni_drifter=FALSE, incremental_memory=TRUE, verbose=FALSE, reporting=FALSE){
  obj <- dal_base()
  obj$dummy <- NULL
  obj$model <- model
  obj$fitted <- FALSE
  obj$drift_method <- drift_method
  obj$drifted <- FALSE
  obj$monitored_features <- monitored_features
  obj$x_train <- c()
  obj$y_train <- c()
  obj$th <- th
  obj$norm_model <- norm_class
  obj$warmup_size <- warmup_size
  obj$target_uni_drifter <- target_uni_drifter
  obj$incremental_memory <- incremental_memory
  obj$verbose <- verbose
  obj$reporting <- reporting
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
  if(obj$reporting){drift_input <- c()}
  if(is.null(obj$monitored_features)){
    monitored_features <- names(x)
  }else{
    monitored_features <- obj$monitored_features
  }
  # Check Drift
  obj$drifted <- FALSE
  if (obj$fitted){
    if (nrow(obj$x_train) >= obj$warmup_size){
        x_oh <- data.frame(predict(obj$dummy, newdata = x))
        if (!all(obj$dummy$feat_names %in% names(x_oh))){
          warning('Some categories present on train are not on the most recent dataset. Creating zero columns.')
          for (feat in obj$dummy$feat_names){
            if (!(feat %in% names(x_oh))){
              x_oh[feat] <- 0
            }
          }
        }
        norm_x_oh <- transform(obj$norm_model, x_oh)
        if(obj$reporting){obj$norm_x_oh <- norm_x_oh}
        
        if ('error_based' %in% class(obj$drift_method)){
          predictions <- predict(obj$model, norm_x_oh)
          y_pred <- predictions[, 2] > obj$th
          
          model_result <- !(as.logical(y[['target']])==y_pred)
          model_result <- model_result[complete.cases(model_result)]
          
          if(obj$reporting){drift_input <- model_result}
          obj$drift_method <- fit(obj$drift_method, model_result)
        }
        
        if ('dist_based' %in% class(obj$drift_method)){
          if (is.null(obj$drift_method$target_feat)){
            norm_x_oh[,'mean'] <- rowMeans(norm_x_oh)
            if(obj$reporting){drift_input <- norm_x_oh[,'mean']}
            obj$drift_method <- fit(obj$drift_method, norm_x_oh[,'mean'])
          }else if(obj$target_uni_drifter){
            if(obj$reporting){drift_input <- y[, 1]*1}
            obj$drift_method <- fit(obj$drift_method, y[, 1]*1)
          }else{
            if(obj$reporting){drift_input <- x_oh[,obj$drift_method$target_feat]}
            obj$drift_method <- fit(obj$drift_method, x_oh[,obj$drift_method$target_feat])
          }
        }
        
        if ('mv_dist_based' %in% class(obj$drift_method)){
          if(obj$reporting){drift_input <- norm_x_oh}
          obj$drift_method <- fit(obj$drift_method, norm_x_oh)
        }
        
        if ('multi_criteria' %in% class(obj$drift_method)){
          if(obj$reporting){drift_input <- norm_x_oh}
          obj$drift_method <- fit(obj$drift_method, norm_x_oh)
        }
        
        if(obj$drift_method$drifted){
          if(obj$verbose){
            message('Stealthy detected a drift, discarding old data')
          }
          obj$x_train <- x
          obj$y_train <- y  
          obj$drift_method <- reset_state(obj$drift_method)
          obj$drifted <- TRUE
          obj$fitted <- FALSE
        }
      }
    }
    # Define update models
    if(obj$incremental_memory | (!obj$fitted)){
      # Aggregate new data
      obj$x_train <- rbind(obj$x_train, x)
      obj$y_train <- rbind(obj$y_train, y)
      
      if((nrow(obj$x_train) >= obj$warmup_size)){
        # One Hot Encoding
        obj$dummy <- caret::dummyVars(" ~ .", data=obj$x_train)
        x_train_dummy <- data.frame(predict(obj$dummy, newdata = obj$x_train))
        obj$dummy$feat_names <- names(x_train_dummy)
        
        # Normalize 
        obj$norm_model <- fit(obj$norm_model, x_train_dummy)
        norm_data <- cbind(transform(obj$norm_model, x_train_dummy), obj$y_train)
        
        # Fit model
        obj$model <- fit(obj$model, norm_data)
        obj$model$feat_names <- names(norm_data)
        
        obj$fitted <- TRUE
      }
    }
  
  if(obj$reporting){obj$drift_input <- drift_input}
  
  return(obj)
}

#'@export
predict.stealthy <- function(object, data, ...){
  # Return format if not fitted
  if(!object$fitted){
    output <- c()
    for (i in 1:length(object$model$slevels)){
      output <- cbind(output, vector(mode='logical', length=nrow(data)))
    }
    output <- as.data.frame(output)
    names(output) <- object$model$slevels
    return(output)
  }
  
  # Prediction if fitted
  data_oh <- data.frame(predict(object$dummy, newdata = data))
  for (feat in object$model$feat_names){
    if (!(feat %in% names(data_oh))){
      data_oh[feat] <- 0
    }
  }
  norm_data_oh <- transform(object$norm_model, data_oh)
  return(predict(object$model, norm_data_oh))
}
