#'@title Stacked Autoencoder-Based Drift Detection method
#'@description Stacked Autoencoder-Based method for concept drift detection <doi:0.1109/ICDMW58026.2022.00109>.
#'@param features Features to be monitored
#'@param input_size Input size
#'@param encoding_size Encoding Size
#'@param batch_size Batch Size for batch learning
#'@param num_epochs Number of Epochs for training
#'@param learning_rate Learning Rate
#'@param window_size Size of the most recent data to be used
#'@param monitoring_step The number of rows that the drifter waits to be is updated
#'@param criteria The method to be used to check if there is a drift. May be mann_whitney (default) or kolmogorov_smirnov
#SAEDD detection: Daniil Kaminskyi, Bin Li and Emmanuel Müller. “Reconstruction-based unsupervised drift detection over multivariate streaming data.” 2022 IEEE International Conference on Data Mining Workshops (ICDMW).
#'@return `dfr_saedd` object
#'@examples
#'require("daltoolbox")
#'require('ggplot2')
#'require('darch')
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
#'  'depart_elevation',
#'  'depart_visibility',
#'  'depart_pressure', 
#'  'depart_relative_humidity', 
#'  'depart_dew_point
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
#'model <- stealthy(cla_nb(target, slevels), dfr_saedd(features=features, input_size=length(features), encoding_size=3, window_size=1800))
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
#'@export
dfr_saedd <- function(features, input_size, encoding_size, batch_size = 32, num_epochs = 1000, learning_rate = 0.001, window_size=100, monitoring_step=1700, criteria='mann_whitney') {
  obj <- mv_dist_based(features=features)
  
  # Attributes
  state <- list()
  
  state$features <- features
  state$input_size <- input_size
  state$encoding_size <- encoding_size
  state$batch_size <- batch_size
  state$num_epochs <- num_epochs
  state$learning_rate <- learning_rate
  state$window_size <- window_size
  state$monitoring_step <- monitoring_step
  state$criteria <- criteria
  
  state$autoencoder <- sae_encode_decode(input_size=input_size, encoding_size=encoding_size, batch_size=batch_size, num_epochs=num_epochs, learning_rate=learning_rate)
  state$is_fitted <- FALSE
  
  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_saedd", class(obj))
  return(obj)
}

#'@export
update_state.dfr_saedd <- function(obj, value){
  #print('Update State')
  
  state <- obj$state
  
  state$n <- state$n + 1
  if (state$n >= state$monitoring_step){
    state$n <- 0
  }else{
    state$window <- rbind(state$window, value)
    
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  
  currentLength <- nrow(state$window)
  if (is.null(currentLength)){
    currentLength <- 0
  }
  
  if (currentLength >= state$window_size){
    state$window <- tail(state$window, -1)
    history_window <- tail(state$window, state$window_size/2)
    recent_window <- head(state$window, state$window_size/2)
    
    if (!state$is_fitted){
      state$autoencoder <- fit(state$autoencoder, value)
      state$is_fitted <- TRUE
    }
    
    state$drifted <- FALSE
    
    history_window_proj <- transform(state$autoencoder, history_window)
    history_rec_error <- (history_window_proj - history_window)
    recent_window_proj <- transform(state$autoencoder, recent_window)
    recent_rec_error <- (recent_window_proj - recent_window)
    
    if (state$criteria == 'mann_whitney'){
      mw_results <- wilcox.test(unlist(as.vector(t(history_rec_error))), unlist(as.vector(t(recent_rec_error))))
      
      if (mw_results['p.value'] < 0.01){
        state$drifted <- TRUE
      }
      
    }
    
    if (state$criteria == 'kolmogorov_smirnov'){
      ks_results <- ks.test(unlist(as.vector(t(history_rec_error))), unlist(as.vector(t(recent_rec_error))))
      
      if (ks_results['p.value'] < 0.01){
        state$drifted <- TRUE
      }
      
    }
    
    if (state$criteria == 'parametric_threshold'){
      mean_history_rec_error <- abs(mean(apply(history_rec_error, 2, mean)))
      sd_history_rec_error <- abs(mean(apply(history_rec_error, 2, sd)))
      mean_recent_rec_error <- abs(mean(apply(recent_rec_error, 2, mean)))
      
      if(mean_recent_rec_error >= (mean_history_rec_error + (3*sd_history_rec_error))){
        state$drifted <- TRUE
      }
    }
    
    if (state$criteria == 'nonparametric_threshold'){
      top_limit <- as.vector(quantile(unlist(as.vector(t(history_rec_error))), 0.99))
      median_recent_rec_error <- abs(median(apply(recent_rec_error, 2, median)))
      
      if(median_recent_rec_error >= top_limit){
        state$drifted <- TRUE
      }
    }
    
    
    if(state$drifted){

      obj$drifted <- TRUE
      
      obj$state <- state
      return(list(obj=obj, drift=TRUE))
    }
    else{
      state$window <- rbind(state$window, value)
      
      obj$state <- state
      return(list(obj=obj, drift=FALSE))
    }
  }else{
    state$window <- rbind(state$window, value)
    
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  
  obj$state <- state
  return(list(obj=obj, pred=obj$drifted))
}

#'@export
fit.dfr_saedd <- function(obj, data, ...){
  output <- update_state(obj, data[1,])
  for (i in 2:nrow(data)){
    output <- update_state(output$obj, data[i,])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_saedd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_saedd(
    features=obj$features,
    input_size=obj$state$input_size, 
    encoding_size=obj$state$encoding_size, 
    batch_size=obj$state$batch_size, 
    num_epochs=obj$state$num_epochs, 
    learning_rate=obj$state$learning_rate
  )$state
  return(obj) 
}
