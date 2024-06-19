#'@title ADWIN method
#'@description Adaptive Windowing method for concept drift detection <doi:10.1137/1.9781611972771.42>.
#'@param target_feat Feature to be monitored.
#'@param delta The significance parameter for the ADWIN algorithm.
#'@param clock How often ADWIN should check for changes. 1 means every new data point, default is 32. Higher values speed up processing, but may also lead to increased delay in change detection.
#'@param max_buckets The maximum number of buckets of each size that ADWIN should keep before merging buckets. The idea of data buckets comes from the compression algorithm introduced in the ADWIN2, the second iteration of the ADWIN algorithm presented in the original research paper. This is the ADWIN version available in River.
#'@param min_window_length The minimum length allowed for a subwindow when checking for concept drift. Subwindows whose size is smaller than this value will be ignored during concept drift evaluation. Lower values may decrease delay in change detection but may also lead to more false positives.
#'@param grace_period ADWIN does not perform any change detection until at least this many data points have arrived.
#ADWIN detection: Bifet, Albert, and Ricard Gavalda. “Learning from time-changing data with adaptive windowing.” In Proceedings of the 2007 SIAM international conference on data mining, pp. 443-448. Society for Industrial and Applied Mathematics, 2007.
#'@return `dfr_adwin` object
#'@examples
#'require("daltoolbox")
#'require('ggplot2')
#'
#'data("st_real_examples")
#'
#'bfd <- st_real_examples$bfd1
#'
#'bfd['batch_index'] <- format(bfd['expected_depart'], '%Y-%m-%d')
#'bfd <- bfd[bfd['depart'] == 'SBSP',]
#'
#'# Model features
#'features <- c(
#'  'depart_elevation', 'depart_wind_direction_cat', 'depart_visibility', 
#'    'depart_day_period', 'depart_pressure', 'depart_relative_humidity', 
#'    'depart_dew_point', 'depart_sky_coverage', 'depart_wind_speed_scale'
#')
#'
#'## Target
#'bfd$delay_depart_bin <- bfd$delay_depart > 0
#'target = 'delay_depart_bin'
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
#'model <- stealthy(cla_nb(target, slevels), dfr_adwin(target_feat='depart_visibility'))
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
#'  print(nrow(new_batch))
#'  print(nrow(last_batch))
#'}
#'results <- as.data.frame(results)
#'results['index'] <- as.Date(results$index)
#'names(results) <- c('index', 'precision', 'recall', 'f1', 'drift')
#'
#'ggplot(data=results, aes(x=as.Date(index), y=as.numeric(f1), group=1)) + 
#'  geom_line() +
#'  xlab('') +
#'  ylab('F1') +
#'  theme_classic()
#'
#'@import reticulate
#'@export
dfr_adwin <- function(target_feat, delta=0.002, clock=32, max_buckets=5, min_window_length=5, grace_period=10) {
  obj <- dist_based(target_feat=target_feat)
  
  # Attributes
  state <- list()
  
  state$delta <- delta
  state$clock <- clock
  state$max_buckets <- max_buckets
  state$min_window_length <- min_window_length
  state$grace_period <- grace_period
  adwin <- import('river.drift.adwin')
  state$adwin <- adwin$ADWIN(
    delta=delta,
    clock=clock,
    max_buckets=max_buckets,
    min_window_length=min_window_length,
    grace_period=grace_period
    )

  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_adwin", class(obj))
  return(obj)
}

#'@export
update_state.dfr_adwin <- function(obj, value){
  
  state <- obj$state
  
  state$adwin$update(value)
  
  obj$state <- state
  if (state$adwin$drift_detected){
    obj$drifted <- state$adwin$drift_detected
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
    delta=obj$state$delta,
    clock=obj$state$clock,
    max_buckets=obj$state$max_buckets,
    min_window_length=obj$state$min_window_length,
    grace_period=obj$state$grace_period
  )$state
  return(obj) 
}
