#'@title KL Distance method
#'@description This detector compares consecutive reference and recent windows through the Kullback-Leibler divergence estimated from their empirical distributions. In this package, it is primarily used for **virtual concept drift**, since it monitors changes in the distribution of a numeric feature stream rather than predictive error. The statistical foundation is the Kullback-Leibler divergence introduced by Kullback and Leibler (1951).
#'@param p_th Drift threshold applied to the PSI
#'@param window_size Size of the sliding window
#'@param data Already collected data to avoid cold start.
#KL divergence: Solomon Kullback and Richard A. Leibler. On information and sufficiency. Annals of Mathematical Statistics, 1951.
#'@references Kullback, S., and Leibler, R. A. (1951). On information and sufficiency. *The Annals of Mathematical Statistics*, 22(1), 79-86. <doi:10.1214/aoms/1177729694>
#'@return `dfr_kldist` object
#'@example examples/1_detection/r/dfr_kldist.R
#'@example examples/2_online_prediction/r/dfr_kldist.R
#'@export
dfr_kldist <- function(window_size=100, p_th=0.25, data=NULL) {
    obj <- mv_dist_based()
    
    state <- list()
    state$window_size <- window_size
    state$p_th <- p_th
    state$p_value <- 0
    state$n <- 0

    if (state$p_th < 0) stop("p_th must be non-negative", call = FALSE)
    if (state$window_size < 0) stop("window_size must be greater than 0", call = FALSE)

    if (missing(data)){
      state$window <- data.frame()
    }
    else{
      state$window <- as.data.frame(data)
    }
    
    obj$state <- state
    
    obj$last_drifter_output <- NULL
    obj$drifter_output <- NULL

    class(obj) <- append("dfr_kldist", class(obj))
    return(obj)
}

#'@importFrom graphics hist
#'@importFrom utils head tail
#'@export
update_state.dfr_kldist <- function(obj, value) {
  obj$last_drifter_output <- NA
  
  state <- obj$state

  state$n <- state$n + 1
  value_check <- as.numeric(value[1])
  if (is.na(value_check)) {
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  state$window <- rbind(state$window, value)
  currentLength <- nrow(state$window)
  
  if (currentLength >= state$window_size){
    analysis_window <- rbind(tail(state$window, state$window_size/2), head(state$window, state$window_size/2))
    
    state$psi = 0
    state$breaks <- 20
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
    obj$last_drifter_output <- state$psi
    
    if((state$psi >= state$p_th)){
      state$window <- tail(state$window, state$window_size/2)
      
      obj$drifted <- TRUE
      
      obj$state <- state
      return(list(obj=obj, drift=TRUE))
    }
    else{
      obj$state <- state
      return(list(obj=obj, drift=FALSE))
    }
  }else{
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  obj$state <- state
  return(list(obj=obj, drift=obj$drifted))
}

#'@export
fit.dfr_kldist <- function(obj, data, ...){
  
  obj$drifter_output <- NULL
  obj$last_drifter_output <- NULL
  output <- update_state(obj, data[1, names(data), drop=FALSE])
  output$obj$drifter_output <- rbind(output$obj$drifter_output, output$obj$last_drifter_output)
  
  if (nrow(data) > 1){
    for (i in 2:nrow(data)){
      output <- update_state(output$obj, data[i, names(data), drop=FALSE])
      output$obj$drifter_output <- rbind(output$obj$drifter_output, output$obj$last_drifter_output)
    }
  }
  
  output$obj$drifter_output <- as.data.frame(output$obj$drifter_output, col.names = c('kl'))
  
  return(output$obj)
}

#'@export
reset_state.dfr_kldist <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kldist(
    p_th = obj$state$p_th,
    window_size = obj$state$window_size,
    data = obj$state$window
  )$state
  return(obj)  
}
