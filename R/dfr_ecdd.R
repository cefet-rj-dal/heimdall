#'@title Adapted EWMA for Concept Drift Detection (ECDD) method
#'@description ECDD is a concept change detection method that uses an exponentially weighted moving average (EWMA) chart to monitor the misclassification rate of an streaming classifier.
#'@param lambda The minimum number of instances before detecting change
#'@param min_run_instances Necessary level for warning zone (2 standard deviation)
#'@param average_run_length Necessary level for a positive drift detection
#ECDD: Gordon Ross, Niall Adams, Dimitris Tasoulis, David Hand: Exponentially weighted moving average charts for detecting concept drift. Pattern Recognition Letters 2012, Volume 33, Issue 2: 191-198, DOI:10.1016/j.patrec.2011.08.019
#ECDD implementation: Frouros, https://github.com/IFCA-Advanced-Computing/frouros/blob/acde82386da735ca8e15f85112f48d5cfb10cc9a/frouros/detectors/concept_drift/streaming/statistical_process_control/ecdd.py
#'@return `dfr_ecdd` object
#'@import ggplot2
#'@importFrom daltoolbox cla_nb
#'@examples
#'require("daltoolbox")
#'require('ggplot2')
#'require('caret')
#'
#'data("st_real_examples")
#'
#'bfd <- st_real_examples$bfd1
#'bfd['batch_index'] <- format(bfd['expected_depart'], '%V')
#'bfd <- bfd[bfd['depart'] == 'SBSP',]
#'
#'# Model features
#'features <- c(
#'  'depart_elevation', 'depart_visibility', 'depart_day_period', 'depart_pressure', 
#'  'depart_relative_humidity', 'depart_dew_point', 'depart_wind_speed_scale'
#')
#'
#'## Target
#'bfd$delay_depart_bin <- bfd$delay_depart > 0
#'target = 'delay_depart_bin'
#'bfd = bfd[complete.cases(bfd[target]),]
#'slevels <- c(TRUE, FALSE)
#'
#'# Evaluation
#'th=0.5
#'
#'results <- c()
#'ordered_batches <- sort(unique(bfd$batch_index))
#'old_start_batch <- ordered_batches[1]
#'
#'# Classification Algorithm
#'model <- stealthy(daltoolbox::cla_nb(target, slevels), dfr_ecdd())
#'
#'for (batch in ordered_batches[2:length(ordered_batches)]){
#'  print(batch)
#'  print(old_start_batch)
#'  
#'  new_batch <- bfd[bfd$batch_index == batch,]
#'  last_batch <- bfd[(bfd$batch_index < batch) & (bfd$batch_index >= old_start_batch),]
#'  
#'  old_start_batch <- batch
#'  
#'  x_train <- last_batch[, features]
#'  y_train <- last_batch[, target, drop=FALSE]
#'  
#'  x_test <- new_batch[, features]
#'  y_test <- new_batch[, target]
#'  
#'  model <- fit(model, x_train, y_train)
#'  
#'  test_predictions <- predict(model, x_test)
#'  y_pred <- test_predictions[, 2] > th
#'  
#'  # Evaluation
#'  precision <- evaluate(mt_precision(), y_pred, y_test)
#'  recall <- evaluate(mt_recall(), y_pred, y_test)
#'  f1 <- evaluate(mt_fscore(), y_pred, y_test)
#'  
#'  results <- rbind(results, 
#'                   c(
#'                     batch,
#'                     precision,
#'                     recall,
#'                     f1,
#'                     model$drifted
#'                   )
#'  )
#'  
#'  print(nrow(model$x_train))
#'  print(nrow(new_batch))
#'}
#'results <- as.data.frame(results)
#'results['index'] <- as.Date(results$index)
#'names(results) <- c('index', 'precision', 'recall', 'f1', 'drift')
#'results[, length(names(results))] <- NULL
#'
#'ggplot(data=results, aes(x=index, y=as.numeric(f1), group=1)) + 
#'  geom_line() +
#'  xlab('') +
#'  ylab('F1') +
#'  geom_vline(xintercept = results[results['drift']==TRUE, 'index'],
#'             linetype="dotted", 
#'             color = "red", linewidth=0.5) +
#'  theme_classic()
#'
#'
#'@export
dfr_ecdd <- function(lambda=0.2, min_run_instances=30, average_run_length=100) {
  obj <- error_based()
  
  state <- list()
  
  state$size <- 0
  
  state$p <- 0
  state$last_p <- NULL
  state$Z <- 0
  state$last_Z <- NULL
  
  state$lambda <- lambda
  state$min_run_instances <- min_run_instances
  state$average_run_length <- average_run_length
  
  obj$state <- state
  
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_ecdd", class(obj))
  
  return(obj)
}

#'@export
update_state.dfr_ecdd <- function(obj, value){
  if (is.na(value)){
    value <- 0
  }
  state <- obj$state
  
  state$size <- state$size + 1
  
  state$last_p <- state$p
  state$p <- (value - state$last_p) / state$size
  
  state$last_Z <- state$Z
  state$Z <- ((1 - state$lambda)*state$last_Z) + (state$lambda * value)
  
  if (state$size > state$min_run_instances){
    error_rate_variance <- state$p * (1 - state$p)
    z_variance <- sqrt(
      abs((state$lambda / (2 - state$lambda)) * (1 - (1 - state$lambda) ** (2 * state$size)) * error_rate_variance)
    )
    
    if (state$average_run_length <= 100){
      control_limit <- 2.76 - (6.23 * state$p) + (18.12 * (state$p ** 3)) - (312.45 * (state$p ** 5)) + (1002.18 * (state$p ** 7))
    }else if(state$average_run_length <= 400){
      control_limit <- 3.97 - (6.56 * state$p) + (48.73 * (state$p ** 3)) - (330.13 * (state$p ** 5)) + (987.23 * (state$p ** 7))
    }else if(state$average_run_length <= 1000){
      control_limit <- 1.17 + (7.56 * state$p) - (21.24 * (state$p ** 3)) + (112.12 * (state$p ** 5)) - (987.23 * (state$p ** 7))
    }
    
    if (state$Z > (state$p + 1 * control_limit * z_variance)){
      obj$state <- state
      obj$drifted <- TRUE
      return(list(obj=obj, pred=TRUE))
    }
  }else{
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
}

#'@export
fit.dfr_ecdd <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_ecdd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_ecdd(
    lambda = obj$state$lambda,
    min_run_instances = obj$state$min_run_instances,
    average_run_length = obj$state$average_run_length
    
  )$state
  return(obj)  
}