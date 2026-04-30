#'@title Stealthy
#'@description Ancestor class for drift adaptive models
#'@param model The algorithm object to be used for predictions
#'@param drift_method The algorithm object to detect drifts
#'@param norm_class Class used to perform normalization
#'@param warmup_size Number of rows used to warmup the drifter. No drift will be detected during this phase
#'@param th The threshold to be used with classification algorithms
#'@param target_uni_drifter Passes the prediction target to the drifts as the target feat when the drifter is univariate and dist_based.
#'@param incremental_memory If true, the model will retrain with all available data whenever the fit is called. If false, it only retrains when a drift is detected.
#'@param verbose if TRUE shows drift messages
#'@return Stealthy object
#'@examples
#'# See ?dd_ddm for an example of DDM drift detector
#'@import daltoolbox
#'@import stats
#'@importFrom caret dummyVars
#'@export
stealthy <- function(model, drift_method, norm_class=daltoolbox::zscore(), warmup_size=100, th=0.5, target_uni_drifter=FALSE, incremental_memory=TRUE, active_warmup=FALSE, class_balance='inactive', verbose=FALSE){
  obj <- dal_base()
  obj$dummy <- NULL
  obj$model <- model
  obj$fitted <- FALSE
  obj$drift_method <- drift_method
  obj$drifted <- FALSE
  obj$x_train <- as.data.frame(c())
  obj$y_train <- as.data.frame(c())
  obj$th <- th
  obj$norm_model <- norm_class
  obj$warmup_size <- warmup_size
  obj$target_uni_drifter <- target_uni_drifter
  obj$incremental_memory <- incremental_memory
  obj$active_warmup <- active_warmup
  obj$class_balance <- class_balance
  if(class_balance=='buffer'){
    obj$class_buffer <- list(
      true=list(x=NULL, y=NULL),
      false=list(x=NULL, y=NULL)
    )
  obj$train_model <- TRUE
  }
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
  
  fit_drifter_input <- NULL
  fit_drifter_output <- NULL
  obj$drift_method$drifter_output <- NULL
  
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
      
      if('dummy' %in% class(obj$drift_method)){
        fit_drifter_input <- norm_x_oh
        obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
      }
      
      if ('error_based' %in% class(obj$drift_method)){
        predictions <- predict(obj$model, norm_x_oh)
        y_pred <- predictions[, 2] > obj$th
        
        model_result <- !(as.logical(y[, 1])==y_pred)
        model_result <- model_result[complete.cases(model_result)]
        
        fit_drifter_input <- model_result
        obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
      }
      
      if ('dist_based' %in% class(obj$drift_method)){
        if (is.null(obj$drift_method$target_feat)){
          norm_x_oh[,'mean'] <- rowMeans(norm_x_oh)
          fit_drifter_input <- norm_x_oh[,'mean']
          obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
        }else if(obj$target_uni_drifter){
          fit_drifter_input <- y[, 1]*1
          obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
        }else{
          fit_drifter_input <- norm_x_oh[,obj$drift_method$target_feat]
          obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
        }
      }
      
      if ('mv_dist_based' %in% class(obj$drift_method)){
        fit_drifter_input <- norm_x_oh
        obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
      }
      
      if ('multi_criteria' %in% class(obj$drift_method)){
        fit_drifter_input <- norm_x_oh
        obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
      }
      norm_x_oh <- NULL
    }
  }
  # Class Balance
  if(obj$class_balance == 'buffer'){
    obj$class_buffer[['true']][['x']] <- tail(rbind(obj$class_buffer[['true']][['x']], x[y == 1,]), obj$warmup_size/2)
    obj$class_buffer[['true']][['y']] <- tail(rbind(obj$class_buffer[['true']][['y']], y[y == 1, 1, drop=FALSE]), obj$warmup_size/2)
    obj$class_buffer[['false']][['x']] <- tail(rbind(obj$class_buffer[['false']][['x']], x[y == 0,]), obj$warmup_size/2)
    obj$class_buffer[['false']][['y']] <- tail(rbind(obj$class_buffer[['false']][['y']], y[y == 0, 1, drop=FALSE]), obj$warmup_size/2)
    
    if((nrow(obj$class_buffer[['false']][['y']]) < (obj$warmup_size/2)) | (nrow(obj$class_buffer[['true']][['y']]) < (obj$warmup_size/2))){
      obj$train_model <- FALSE
    }else{
      obj$train_model <- TRUE
    }
  }
  
  # Define update models
  if(obj$incremental_memory | (!obj$fitted) | (obj$active_warmup & (nrow(obj$x_train) < obj$warmup_size))){
    # Aggregate new data
    obj$x_train <- rbind(obj$x_train, x)
    obj$y_train <- rbind(obj$y_train, y)
    
    if(((nrow(obj$x_train) >= obj$warmup_size) | (obj$active_warmup & (nrow(obj$x_train) < obj$warmup_size))) & obj$train_model){
      # Class Balance
      if(obj$class_balance == 'buffer'){
        obj$x_model_train <- rbind(obj$class_buffer[['false']][['x']], obj$class_buffer[['true']][['x']])
        obj$y_model_train <- rbind(obj$class_buffer[['false']][['y']], obj$class_buffer[['true']][['y']])
      }else if(obj$class_balance == 'inactive'){
        obj$x_model_train <- obj$x_train
        obj$y_model_train <- obj$y_train
      }
      
      # Model Training
      # One Hot Encoding
      obj$model_dummy <- caret::dummyVars(" ~ .", data=obj$x_model_train)
      x_model_train_dummy <- data.frame(predict(obj$model_dummy, newdata = obj$x_model_train))
      obj$model_dummy$feat_names <- names(x_model_train_dummy)
      
      # Normalize 
      obj$norm_model_train <- fit(obj$norm_model, x_model_train_dummy)
      norm_model_x_oh <- transform(obj$norm_model_train, x_model_train_dummy)
      norm_model_data <- cbind(norm_model_x_oh, obj$y_model_train)
      
      # Fit model
      obj$model <- fit(obj$model, norm_model_data)
      obj$model$feat_names <- names(norm_model_data)
      
      # Drift data
      # One Hot Encoding
      obj$dummy <- caret::dummyVars(" ~ .", data=obj$x_train)
      x_train_dummy <- data.frame(predict(obj$dummy, newdata = obj$x_train))
      obj$dummy$feat_names <- names(x_train_dummy)
      
      # Normalize 
      obj$norm_model <- fit(obj$norm_model, x_train_dummy)
      norm_x_oh <- transform(obj$norm_model, x_train_dummy)
      
      # Fit Drifter
      if ('dist_based' %in% class(obj$drift_method)){
        if (is.null(obj$drift_method$target_feat)){
          norm_x_oh[,'mean'] <- rowMeans(norm_x_oh)
          fit_drifter_input <- norm_x_oh[,'mean']
          obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
        }else if(obj$target_uni_drifter){
          fit_drifter_input <- obj$y_train[, 1]*1
          obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
        }else{
          fit_drifter_input <- norm_x_oh[,obj$drift_method$target_feat]
          obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
        }
      }

      if ('mv_dist_based' %in% class(obj$drift_method)){
        fit_drifter_input <- norm_x_oh
        obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
      }
      
      # obj$drift_method$drifter_output <- NULL
      norm_x_oh <- NULL
      obj$drift_method$drifted <- FALSE
      obj$fitted <- TRUE
    }
  }
  
  # Drifter Output
  
  fit_drifter_output <- tail(obj$drift_method$drifter_output, nrow(x))

  if(is.null(fit_drifter_output)){
    if(c('dfr_aedd' %in% class(obj$drift_method))){
      if(any(c('autoenc_ed', 'autoenc_variational_ed') %in% class(obj$drift_method$state$autoencoder))){
        fit_drifter_output <- as.data.frame(matrix(NA, nrow = nrow(x), ncol = ncol(x)))
        names(fit_drifter_output) <- names(x)
      }else if(any(c('autoenc_e') %in% class(obj$drift_method$state$autoencoder))){
        fit_drifter_output <- as.data.frame(matrix(NA, nrow = nrow(x), ncol = obj$drift_method$state$encoding_size))
      }else if(c('autoenc_variational_e' %in% class(obj$drift_method$state$autoencoder))){
        fit_drifter_output <- as.data.frame(matrix(NA, nrow = nrow(x), ncol = obj$drift_method$state$encoding_size * 2))
      }
    }else if(c('dfr_kswin' %in% class(obj$drift_method))){
      fit_drifter_output <- as.data.frame(matrix(NA, nrow = nrow(x), ncol = 2))
      names(fit_drifter_output) <- c('D', 'p')
    }else if(c('dfr_mcdd' %in% class(obj$drift_method))){
      fit_drifter_output <- as.data.frame(matrix(NA, nrow = nrow(x), ncol = 3))
      names(fit_drifter_output) <- c('Old Shapiro p', 'New Shapiro p', 'Comparison p')
    }else{
      fit_drifter_output <- as.data.frame(matrix(NA, nrow = nrow(x), ncol = 1))
    }
  }else{
    missing_n <- nrow(x) - nrow(fit_drifter_output)
    if(missing_n > 0){
      extra <- as.data.frame(matrix(NA, nrow = missing_n, ncol = ncol(fit_drifter_output)))
      names(extra) <- names(fit_drifter_output)
      fit_drifter_output <- rbind(fit_drifter_output, extra)
    }
  }
  
  # print(obj$drifter_output)
  if(is.null(obj$drifter_output)){
    obj$drifter_output <- fit_drifter_output
  }else{
    obj$drifter_output <- rbind(obj$drifter_output[, names(fit_drifter_output), drop=FALSE], fit_drifter_output)
  }
  rownames(obj$drifter_output) <- 1:nrow(obj$drifter_output)
  obj$drifter_input <- rbind(obj$drifter_input, cbind(x, y))

  # htmlfilename <- "//home/lucas/lucas/aels_method/results/testing/batch_plot.html"
  # if(!file.exists(htmlfilename)){
  #   batch_plot <- ggplotly(ggplot(data=fit_drifter_output, aes(x=rownames(fit_drifter_output), y=.data[['AF4']], group=1)) +
  #                            geom_line() +
  #                            xlab('') +
  #                            ylab('AF4') +
  #                            theme_minimal() +
  #                            theme(
  #                              panel.background = element_rect(fill = "white"),
  #                              panel.grid.major = element_blank(),
  #                              panel.grid.minor = element_blank(),
  #                              axis.title.x = element_blank(),
  #                              axis.text.x = element_blank(),
  #                              axis.ticks.x = element_blank()
  #                            ))
  #   saveWidget(batch_plot, htmlfilename, selfcontained = F, libdir = "lib")
  # }
  
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
