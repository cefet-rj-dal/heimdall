#'@title Cumulative Sum for Concept Drift Detection (CUMSUM) method
#'@description The cumulative sum (CUSUM) is a sequential analysis technique used for change detection.
#'@param delta The minimum number of instances before detecting change
#'@param lambda Necessary level for warning zone (2 standard deviation)
#CUMSUM: S. Muthukrishnan, Eric Berg, Yihua Wu: Sequential Change Detection on Data Streams. Seventh IEEE International Conference on Data Mining Workshops (ICDMW 2007), DOI:10.1109/ICDMW.2007.89
#'@return `dfr_cumsum` object
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
dfr_cumsum <- function(delta=0, lambda=10**3) {
  obj <- error_based()
  
  state <- list()
  
  state$delta <- delta
  state$lambda <- lambda
  
  state$g <- 0
  state$last_g <- NULL
  
  obj$state <- state
  
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_cumsum", class(obj))
  
  return(obj)
}

#'@export
update_state.dfr_cumsum <- function(obj, value){
  if (is.na(value)){
    value <- 0
  }
  state <- obj$state
  
  state$last_g <- state$g
  state$g <- max(0, state$last_g + (value - state$delta))
  
  obj$state <- state
  if (state$g > state$lambda){
    obj$drifted <- TRUE
    return(list(obj=obj, pred=TRUE))
  }else{
    return(list(obj=obj, pred=FALSE))
  }
}

#'@export
fit.dfr_cumsum <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_cumsum <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_cumsum(
    delta = obj$state$delta,
    lambda = obj$state$lambda
    
  )$state
  return(obj)  
}