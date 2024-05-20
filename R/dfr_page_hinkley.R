#'@title Adapted Page Hinkley method
#'@description Change-point detection method works by computing the observed values and their mean up to the current moment <doi:10.2307/2333009>.
#'@param target_feat Feature to be monitored.
#'@param min_instances The minimum number of instances before detecting change
#'@param delta The delta factor for the Page Hinkley test
#'@param threshold The change detection threshold (lambda)
#'@param alpha The forgetting factor, used to weight the observed value and the mean
#Page Hinkley detection: E. S. Page. (1954) Continuous Inspection Schemes, Biometrika 41(1/2), 100–115.
#Page Hinkley detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/page_hinkley.py#L4
#'@return `dfr_page_hinkley` object
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
#'model <- stealthy(daltoolbox::cla_nb(target, slevels), 
#'dfr_page_hinkley(target_feat='depart_visibility'))
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
#'@export
dfr_page_hinkley <- function(target_feat, min_instances=30, delta=0.005, threshold=50, alpha=1-0.0001) {
  obj <- dist_based(target_feat=target_feat)
  
  state <- list()
  state$min_instances <- min_instances
  state$delta <- delta
  state$threshold <- threshold
  state$alpha = alpha
  state$x_mean <- 0
  state$sum <- 0
  state$sample_count <- 1
  
  obj$state <- state
  
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_page_hinkley", class(obj))
  return(obj)
}

#'@export
update_state.dfr_page_hinkley <- function(obj, value){
  state <- obj$state
  
  state$x_mean <- state$x_mean + (value - state$x_mean)/state$sample_count
  state$sum <- max(0, state$alpha * state$sum + (value - state$x_mean - state$delta))
  state$sample_count <- state$sample_count + 1
  
  if(state$sample_count < state$min_instances){
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
  else if(state$sum > state$threshold){
    state$x_mean <- 0
    state$sum <- 0
    state$sample_count <- 1
    
    obj$drifted <- TRUE
    
    obj$state <- state
    return(list(obj=obj, pred=TRUE))
  }
  else{
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
  
  return(list(obj=obj, pred=obj$drifted))
}

#'@export
fit.dfr_page_hinkley <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_page_hinkley <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_page_hinkley(
    target_feat = obj$target_feat,
    min_instances = obj$state$min_instances,
    delta = obj$state$delta,
    threshold = obj$state$threshold,
    alpha = obj$state$alpha
  )$state
  return(obj)  
}