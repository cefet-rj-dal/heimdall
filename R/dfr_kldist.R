#'@title KL Distance method
#'@description Kullback Leibler Windowing method for concept drift detection.
#'@param target_feat Feature to be monitored.
#'@param p_th Probability theshold for the test statistic of the Kullback Leibler distance.
#'@param window_size Size of the sliding window (must be > 2*stat_size)
#'@param data Already collected data to avoid cold start.
#KSWIN detection: Christoph Raab, Moritz Heusinger, Frank-Michael Schleif, Reactive Soft Prototype Computing for Concept Drift Streams, Neurocomputing, 2020.
#KSWIN detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/kswin.py#L5
#'@return `dfr_kldist` object
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
#'dfr_kldist(target_feat='depart_visibility', p_th=0.2))
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
dfr_kldist <- function(target_feat, window_size=100, p_th=0.9, data=NULL) {
    obj <- dist_based(target_feat=target_feat)
    
    state <- list()
    state$window_size <- window_size
    state$p_th <- p_th
    state$p_value <- 0
    state$n <- 0

    if ((state$p_th < 0) | (state$p_th > 1)) stop("Alpha must be between 0 and 1", call = FALSE)
    if (state$window_size < 0) stop("window_size must be greater than 0", call = FALSE)

    if (missing(data)){
      state$window <- c()
    }
    else{
      state$window <- data
    }
    
    obj$state <- state

    class(obj) <- append("dfr_kldist", class(obj))
    return(obj)
}

#'@importFrom utils head tail
#'@export
update_state.dfr_kldist <- function(obj, value) {
  state <- obj$state

  state$n <- state$n + 1
  currentLength <- nrow(state$window)
  if (is.null(currentLength)){
    currentLength <- 0
  }
  
  if (currentLength >= state$window_size){
    state$window <- tail(state$window, -1)
    p_window <- tail(state$window, state$window_size/2)
    q_window <- head(state$window, state$window_size/2)
    
    p <- p_window / sum(p_window)
    q <- q_window / sum(q_window)
    
    state$kl <- sum(p * log(p/q, base=2), na.rm=TRUE)
    
    if((state$kl >= state$p_th)){
      state$window <- tail(state$window, state$window_size/2)
      state$window <- rbind(state$window, value)
      
      obj$drifted <- TRUE
      
      obj$state <- state
      return(list(obj=obj, pred=TRUE))
    }
    else{
      state$window <- rbind(state$window, value)
      
      obj$state <- state
      return(list(obj=obj, pred=FALSE))
    }
  }else{
    state$window <- rbind(state$window, value)
  
    obj$state <- state
    return(list(obj=obj, pred=FALSE))
  }
  obj$state <- state
  return(list(obj=obj, pred=obj$drifted))
}

#'@export
fit.dfr_kldist <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_kldist <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kldist(
    target_feat = obj$target_feat,
    p_th = obj$state$p_th
  )$state
  return(obj)  
}