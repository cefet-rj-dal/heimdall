#'@title Adapted Drift Detection Method (DDM) method
#'@description DDM is a concept change detection method based on the PAC learning model premise, that the learner’s error rate will decrease as the number of analysed samples increase, as long as the data distribution is stationary. <doi:10.1007/978-3-540-28645-5_29>.
#'@param min_instances The minimum number of instances before detecting change
#'@param warning_level Necessary level for warning zone (2 standard deviation)
#'@param out_control_level Necessary level for a positive drift detection
#DDM: João Gama, Pedro Medas, Gladys Castillo, Pedro Pereira Rodrigues: Learning with Drift Detection. SBIA 2004: 286-295.
#DDM implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/ddm.py
#'@return `dfr_ddm` object
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
#'model <- stealthy(daltoolbox::cla_nb(target, slevels), dfr_ddm(out_control_level=10))
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
dfr_ddm <- function(min_instances=30, warning_level=2.0, out_control_level=3.0) {
  obj <- error_based()
  
  state <- list()
  
  state$min_instances <- min_instances
  state$warning_level <- warning_level
  state$out_control_level <- out_control_level
  
  state$sample_count <- 1
  state$miss_prob <- 1.0
  state$miss_std <- 0.0
  state$miss_prob_sd_min <- Inf
  state$miss_prob_min <- Inf
  state$miss_sd_min <- Inf
  
  obj$state <- state
  
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_ddm", class(obj))
  
  return(obj)
}

#'@export
update_state.dfr_ddm <- function(obj, value){
  if (is.na(value)){
    value <- 0
  }
  state <- obj$state
  state$miss_prob <- state$miss_prob + (value - state$miss_prob) / state$sample_count
  state$miss_std <- sqrt(state$miss_prob * (1 - state$miss_prob)) / state$sample_count
  state$sample_count <- state$sample_count + 1
  
  state$estimation <- state$miss_prob
  state$in_concept_change <- FALSE
  state$in_warning_zone <- FALSE
  state$delay <- 0
  
  if(state$sample_count < state$min_instances){
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
  
  if((state$miss_prob + state$miss_std) <= state$miss_prob_sd_min){
    state$miss_prob_min <- state$miss_prob
    state$miss_sd_min <- state$miss_std
    state$miss_prob_sd_min <- state$miss_prob + state$miss_std
    state$sum <- 0
    state$sample_count <- 1
  }
  
  if((state$miss_prob + state$miss_std) > (state$miss_prob_min + state$out_control_level * state$miss_sd_min)){
    state$sample_count <- 1
    state$miss_prob <- 1.0
    state$miss_std <- 0.0
    state$miss_prob_sd_min <- Inf
    state$miss_prob_min <- Inf
    state$miss_sd_min <- Inf
    
    obj$drifted <- TRUE
    obj$state <- state
    return(list(obj=obj, pred=TRUE))
  }else if((state$miss_prob + state$miss_std) > (state$miss_prob_min + state$warning_level * state$miss_sd_min)){
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }else{
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
}

#'@export
fit.dfr_ddm <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_ddm <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_ddm(
    min_instances = obj$state$min_instances,
    warning_level = obj$state$warning_level,
    out_control_level = obj$state$out_control_level
    )$state
  return(obj)  
}