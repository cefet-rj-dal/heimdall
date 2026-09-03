#'@title Autoencoder-Based Drift Detection method
#'@description AEDD is an unsupervised multivariate detector that compares reconstruction errors produced by an autoencoder on reference and recent windows. Because it monitors changes in the input distribution rather than classifier performance, this implementation is primarily aimed at **virtual concept drift**. The method follows Kaminskyi, Li, and Muller (2022) <doi:10.1109/ICDMW58026.2022.00109>.
#'@param encoding_size Encoding Size
#'@param ae_class Autoencoder Class
#'@param batch_size Batch Size for batch learning
#'@param num_epochs Number of Epochs for training
#'@param learning_rate Learning Rate
#'@param window_size Size of the most recent data to be used
#'@param monitoring_step The number of rows that the drifter waits to be is updated
#'@param criteria The method to be used to check if there is a drift. May be mann_whitney (default), kolmogorov_smirnov, levene, parametric_threshold, nonparametric_threshold
#'@param alpha The significance threshold for the statistical test used in criteria
#AEDD detection: Daniil Kaminskyi, Bin Li and Emmanuel Müller. “Reconstruction-based unsupervised drift detection over multivariate streaming data.” 2022 IEEE International Conference on Data Mining Workshops (ICDMW).
#'@references Kaminskyi, D., Li, B., and Muller, E. (2022). Reconstruction-based unsupervised drift detection over multivariate streaming data. In *2022 IEEE International Conference on Data Mining Workshops (ICDMW)*. <doi:10.1109/ICDMW58026.2022.00109>
#'@return `dfr_aedd` object
#'@import daltoolboxdp
#'@example examples/1_detection/r/dfr_aedd.R
#'@example examples/2_online_prediction/r/dfr_aedd.R
#'@export
dfr_aedd <- function(encoding_size, ae_class=daltoolboxdp::autoenc_ed, ae_params=list(batch_size = 32, num_epochs = 1000, learning_rate = 0.001, error_function='mae'), window_size=100, window_type='fixed', monitoring_step=1700, criteria='mann_whitney', alpha=0.01) {
  obj <- mv_dist_based()
  
  obj$ae_class <- ae_class
  obj$error_function <- ae_params[['error_function']]
  ae_params[['error_function']] <- NULL
  obj$ae_params <- ae_params
  obj$alpha <- alpha
  obj$encoding_size <- encoding_size
  obj$window_type <- window_type
  
  # Attributes
  state <- list()
  state$window_size <- window_size
  state$monitoring_step <- monitoring_step
  state$criteria <- criteria
  state$data <- NULL
  state$n <- 0
  state$autoencoder <- do.call(obj$ae_class, c(
    list(
      input_size=1,
      encoding_size=obj$encoding_size
    ),
    obj$ae_params
  ))
  state$is_fitted <- FALSE
  
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_aedd", class(obj))
  return(obj)
}

#'@export
update_state.dfr_aedd <- function(obj, value){

  state <- obj$state
  
  obj$last_drifter_output <- NULL
  
  if(length(value) == 1){
    if(value > 1){value <- 1}else if(value < 0){
      value <- 0
      }
  }else if(length(value) > 1){
    value[value > 1] <- 1
    value[value < 0] <- 0
    }

  if(!is.null(state$data)){
    if(!all(names(value) %in% names(state$data))){
      warning('dfr_aedd::update_state: Some categories present in most recent data are not on the history dataset. Creating zero columns.')
      for (feat in names(value)){
        if (!(feat %in% names(state$data))){
          state$data[feat] <- 0
        }
      }
    }
    if(!all(names(state$data) %in% names(value))){
      for (feat in names(state$data)){
        if (!(feat %in% names(value))){
          value[feat] <- 0
        }
      }
    }
  }
  
  state$data <- rbind(state$data, as.data.frame(value))
  rownames(state$data) <- 1:nrow(state$data)
  
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
    if(obj$window_type == 'fixed'){
      reference_window <- tail(state$data, state$window_size)
    }else if(obj$window_type == 'moving'){
      reference_window <- state$data
    }
    
    history_window <- head(reference_window, state$window_size/2)
    recent_window <- tail(reference_window, state$window_size/2)
    
    if(!state$is_fitted){
      if(is.null(ncol(state$data))){
        input_size <- 1
      }else{
        input_size <- ncol(state$data)
      }

      state$autoencoder <- do.call(obj$ae_class, c(
        list(
        input_size=input_size,
        encoding_size=obj$encoding_size
        ),
        obj$ae_params
        ))
      
      state$autoencoder <- fit(state$autoencoder, history_window)
      
      state$is_fitted <- TRUE
    }
    
    state$drifted <- FALSE

    history_window_output <- transform(state$autoencoder, history_window)
    recent_window_output <- transform(state$autoencoder, recent_window)
    
    if(any(c('autoenc_ed', 'autoenc_variational_ed') %in% class(state$autoencoder))){
      history_rec_marker <- (history_window_output - history_window)
      recent_rec_marker <- (recent_window_output - recent_window)
      obj$last_drifter_output <- recent_rec_marker
      
      if(obj$error_function == 'rmse'){
        history_rec_marker <- rowMeans(sqrt(history_rec_marker^2))
        recent_rec_marker <- rowMeans(sqrt(recent_rec_marker^2))
      }else if(obj$error_function == 'mae'){
        history_rec_marker <- rowMeans(abs(history_rec_marker))
        recent_rec_marker <- rowMeans(abs(recent_rec_marker))
      }
    }else if(any(c('autoenc_e', 'autoenc_variational_e') %in% class(state$autoencoder))){
      history_rec_marker <- history_window_output
      recent_rec_marker <- recent_window_output
      obj$last_drifter_output <- as.data.frame(recent_rec_marker)
    }
    
    if (state$criteria == 'mann_whitney'){
      mw_results <- wilcox.test(unlist(as.vector(t(history_rec_marker))), unlist(as.vector(t(recent_rec_marker))))
      
      if (mw_results['p.value'] < obj$alpha){
        state$drifted <- TRUE
      }
    }
    
    if (state$criteria == 'kolmogorov_smirnov'){
      ks_results <- ks.test(unlist(as.vector(t(history_rec_marker))), unlist(as.vector(t(recent_rec_marker))))
      
      if (ks_results['p.value'] < obj$alpha){
        state$drifted <- TRUE
      }
      
    }
    
    if (state$criteria == 'levene'){
      history_rec_marker <- as.data.frame(history_rec_marker)
      colnames(history_rec_marker) <- paste0("V", 1:ncol(history_rec_marker))
      recent_rec_marker <- as.data.frame(recent_rec_marker)
      colnames(recent_rec_marker) <- paste0("V", 1:ncol(recent_rec_marker))
      history_rec_marker['window'] <- 'History'
      recent_rec_marker['window'] <- 'Recent'
      levene_df <- as.data.frame(rbind(history_rec_marker, recent_rec_marker))
      levene_df['window'] <- factor(levene_df[['window']])
      
      levene_results <- car::leveneTest(V1 ~ window, data=as.data.frame(levene_df))
      
      if(!is.na(levene_results['group', 'Pr(>F)'])){
        if (levene_results['group', 'Pr(>F)'] < obj$alpha){
          state$drifted <- TRUE
        }
      }
    }
    
    if (state$criteria == 'psi'){
      history_rec_marker <- as.data.frame(history_rec_marker)
      colnames(history_rec_marker) <- paste0("V", 1:ncol(history_rec_marker))
      recent_rec_marker <- as.data.frame(recent_rec_marker)
      colnames(recent_rec_marker) <- paste0("V", 1:ncol(recent_rec_marker))
      analysis_window <- rbind(history_rec_marker, recent_rec_marker)
      
      state$psi = 0
      state$breaks <- state$window_size/5
      for(c in names(analysis_window)){
        analysis_window['bin'] <- cut(analysis_window[[c]], breaks=state$breaks)
        
        p_window <- tail(analysis_window, state$window_size/2)
        q_window <- head(analysis_window, state$window_size/2)
        if(c != 'bin'){
          for(b in unique(analysis_window[['bin']])){
            ob <- sum(p_window['bin'] == b)/nrow(p_window)
            p <- (ob + 0.005)/(1 + state$breaks * 0.005)
            ex <- sum(q_window['bin'] == b)/nrow(q_window)
            q <- (ex + 0.005)/(1 + state$breaks * 0.005)
            
            psi_cb <- (p - q) * log(p/q)
            
            state$psi <- state$psi + psi_cb
          }
        }
      }
      
      state$psi <- state$psi / state$breaks / length(names(analysis_window))
      
      if((state$psi >= obj$alpha)){
        state$window <- tail(state$window, state$window_size/2)
        
        obj$drifted <- TRUE
      }
    }
    
    if (state$criteria == 'parametric_threshold'){
      mean_history_rec_error <- abs(mean(apply(history_rec_marker, 2, mean)))
      sd_history_rec_error <- abs(mean(apply(history_rec_marker, 2, sd)))
      mean_recent_rec_error <- abs(mean(apply(history_rec_marker, 2, mean)))
      
      if(mean_recent_rec_error >= (mean_history_rec_error + (3*sd_history_rec_error))){
        state$drifted <- TRUE
      }
    }
    
    if (state$criteria == 'nonparametric_threshold'){
      top_limit <- as.vector(quantile(unlist(as.vector(t(history_rec_marker))), 0.99))
      median_recent_rec_error <- abs(median(apply(recent_rec_marker, 2, median)))
      
      if(median_recent_rec_error >= top_limit){
        state$drifted <- TRUE
      }
    }
    
    if (state$criteria == 'diff_threshold'){
      rmse_diff <- median(recent_rec_marker - history_rec_marker)
      
      if(rmse_diff >= obj$alpha){
       state$drifted <- TRUE
      }    
    }
    
    if(state$drifted){
      obj$drifted <- TRUE
      state$is_fitted <- FALSE
    }
  }

  obj$state <- state
  return(list(obj=obj, drift=obj$drifted))
}

#'@export
fit.dfr_aedd <- function(obj, data, ...){
  state <- obj$state

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
  obj$drifter_output <- NULL
  obj$last_drifter_output <- NULL
  output <- update_state(obj, data[1,])
  output$obj$drifter_output <- rbind(output$obj$drifter_output, output$obj$last_drifter_output)
  if(nrow(data) >= 2){
    for (i in 2:nrow(data)){
      output <- update_state(output$obj, data[i,])
      tryCatch({
        output$obj$drifter_output <- rbind(output$obj$drifter_output, output$obj$last_drifter_output)
      },
      error = function(e) {
        print(output$obj$drifter_output)
        print(output$obj$last_drifter_output)
        message("An error occurred: ", e)
        return(NA) # Return a fallback value
      }
      )
    }
  }

  return(output$obj)
}

#'@export
reset_state.dfr_aedd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_aedd(
    encoding_size=obj$state$encoding_size,
    ae_class=obj$ae_class,
    ae_params=obj$ae_params,
    window_size=obj$state$window_size,
    monitoring_step=obj$state$monitoring_step,
    criteria=obj$state$criteria,
    alpha=obj$alpha
  )$state
  return(obj) 
}
