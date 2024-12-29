#'@title Autoencoder-Based Drift Detection method
#'@description Autoencoder-Based method for concept drift detection <doi:0.1109/ICDMW58026.2022.00109>.
#'@param encoding_size Encoding Size
#'@param ae_class Autoencoder Class
#'@param batch_size Batch Size for batch learning
#'@param num_epochs Number of Epochs for training
#'@param learning_rate Learning Rate
#'@param window_size Size of the most recent data to be used
#'@param monitoring_step The number of rows that the drifter waits to be is updated
#'@param criteria The method to be used to check if there is a drift. May be mann_whitney (default) or kolmogorov_smirnov
#AEDD detection: Daniil Kaminskyi, Bin Li and Emmanuel Müller. “Reconstruction-based unsupervised drift detection over multivariate streaming data.” 2022 IEEE International Conference on Data Mining Workshops (ICDMW).
#'@return `dfr_aedd` object
#'@examples
#'#See an example of using `dfr_aedd` at this
#'#https://github.com/cefet-rj-dal/heimdall/blob/main/multivariate/dfr_aedd.md
#'@export
dfr_aedd <- function(encoding_size, ae_class=autoenc_encode_decode, batch_size = 32, num_epochs = 1000, learning_rate = 0.001, window_size=100, monitoring_step=1700, criteria='mann_whitney') {
  obj <- mv_dist_based()
  
  obj$ae_class <- ae_class
  
  # Attributes
  state <- list()
  
  state$encoding_size <- encoding_size
  state$batch_size <- batch_size
  state$num_epochs <- num_epochs
  state$learning_rate <- learning_rate
  state$window_size <- window_size
  state$monitoring_step <- monitoring_step
  state$criteria <- criteria
  state$data <- c()
  
  state$autoencoder <- NULL
  state$is_fitted <- FALSE
  
  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_aedd", class(obj))
  return(obj)
}

#'@export
update_state.dfr_aedd <- function(obj, value){
  state <- obj$state
  
  if(!is.null(state$data)){
    if(!all(names(value) %in% names(state$data))){
      warning('dfr_aedd::update_state: Some categories present in most recent data are not on the history dataset. Creating zero columns.')
      for (feat in names(value)){
        if (!(feat %in% names(state$data))){
          value[feat] <- NULL
        }
      }
    }
  }
  
  state$data <- rbind(state$data, as.data.frame(value))
  
  state$n <- state$n + 1
  if (state$n >= state$monitoring_step){
    state$n <- 0
  }else{
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  
  currentLength <- nrow(state$data)
  if (is.null(currentLength)){
    currentLength <- 0
  }
  
  if (currentLength >= state$window_size){
    state$data <- tail(state$data, -1)
    history_window <- tail(state$data, state$window_size/2)
    recent_window <- head(state$data, state$window_size/2)
    
    
    if(!state$is_fitted){
      if(is.null(ncol(state$data))){
        input_size <- 1
      }else{
        input_size <- ncol(state$data)
      }
      

      state$autoencoder <- obj$ae_class(input_size=input_size, encoding_size=state$encoding_size, batch_size=state$batch_size, num_epochs=state$num_epochs, learning_rate=state$learning_rate)
      state$autoencoder <- fit(state$autoencoder, history_window)
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
    }
  }
  obj$state <- state
  return(list(obj=obj, drift=obj$drifted))
}

#'@export
fit.dfr_aedd <- function(obj, data, ...){
  state <- obj$state
  
  if(!state$is_fitted){
    if(is.null(ncol(state$data))){
      input_size <- 1
    }else{
      input_size <- ncol(state$data)
    }

    state$autoencoder <- obj$ae_class(input_size=input_size, encoding_size=state$encoding_size, batch_size=state$batch_size, num_epochs=state$num_epochs, learning_rate=state$learning_rate)
  }
  
  if((!is.null(state$data))){
    if(nrow(state$data) & (!is.null(ncol(state$data)))){
      if(!state$is_fitted){
        if(!all(names(data) %in% names(state$data))){
          warning('dfr_aedd: Some categories present in most recent data are not on the history dataset. Creating zero columns.')
          for (feat in names(data)){
            if (!(feat %in% names(state$data))){
              state$data[feat] <- 0
            }
          }
        }
      }
      if(!all(names(state$data) %in% names(data))){
        warning('dfr_aedd: Some categories present in history data are not on the most recent dataset. Creating zero columns.')
        for (feat in names(state$data)){
          if (!(feat %in% names(data))){
            data[feat] <- 0
          }
        }
      }
    }
  }
  
  obj$state <- state
  
  output <- update_state(obj, data[1,])
  for (i in 2:nrow(data)){
    output <- update_state(output$obj, data[i,])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_aedd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_aedd(
    encoding_size=obj$state$encoding_size, 
    ae_class=obj$ae_class,
    batch_size=obj$state$batch_size, 
    num_epochs=obj$state$num_epochs, 
    learning_rate=obj$state$learning_rate
  )$state
  return(obj) 
}
