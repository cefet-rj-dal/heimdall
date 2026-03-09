#'@title KL Distance method
#'@description Kullback Leibler Windowing method for concept drift detection.
#'@param target_feat Feature to be monitored.
#'@param p_th Drift threshold applied to the KL divergence
#'@param window_size Size of the sliding window
#'@param data Already collected data to avoid cold start.
#KL divergence: Solomon Kullback and Richard A. Leibler. On information and sufficiency. Annals of Mathematical Statistics, 1951.
#'@return `dfr_kldist` object
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'# This example uses a dist-based drift detector with a synthetic dataset.
#'
#'data(st_drift_examples)
#'data <- st_drift_examples$univariate
#'data$event <- NULL
#'
#'model <- dfr_kldist(target_feat='serie')
#'
#'detection <- NULL
#'output <- list(obj=model, drift=FALSE)
#'for (i in 1:length(data$serie)){
#'  output <- update_state(output$obj, data$serie[i])
#'  if (output$drift){
#'    type <- 'drift'
#'    output$obj <- reset_state(output$obj)
#'  }else{
#'    type <- ''
#'  }
#'  detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
#'}
#'
#'detection[detection$type == 'drift',]
#'@export
dfr_kldist <- function(target_feat=NULL, window_size=100, p_th=0.05, data=NULL) {
    obj <- dist_based(target_feat=target_feat)
    
    state <- list()
    state$window_size <- window_size
    state$p_th <- p_th
    state$p_value <- 0
    state$n <- 0

    if (state$p_th < 0) stop("p_th must be non-negative", call = FALSE)
    if (state$window_size < 0) stop("window_size must be greater than 0", call = FALSE)

    if (missing(data)){
      state$window <- numeric(0)
    }
    else{
      state$window <- as.numeric(data)
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
  value <- as.numeric(value[1])
  if (is.na(value)) {
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  currentLength <- length(state$window)
  
  if (currentLength >= state$window_size){
    state$window <- tail(state$window, state$window_size - 1)
    state$window <- c(state$window, value)
    p_window <- head(state$window, state$window_size/2)
    q_window <- tail(state$window, state$window_size/2)

    bins <- pretty(range(c(p_window, q_window), na.rm=TRUE), n=10)
    if (length(unique(bins)) < 2) {
      obj$state <- state
      return(list(obj=obj, drift=FALSE))
    }
    p_hist <- hist(p_window, breaks=bins, plot=FALSE)$density
    q_hist <- hist(q_window, breaks=bins, plot=FALSE)$density
    eps <- 1e-12
    p <- p_hist + eps
    q <- q_hist + eps
    p <- p / sum(p)
    q <- q / sum(q)

    state$kl <- sum(p * log(p/q, base=2), na.rm=TRUE)
    
    if((state$kl >= state$p_th)){
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
    state$window <- c(state$window, value)
  
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  obj$state <- state
  return(list(obj=obj, drift=obj$drifted))
}

#'@export
fit.dfr_kldist <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  if (length(data) > 1){
    for (i in 2:length(data)){
      output <- update_state(output$obj, data[i])
    }
  }
  return(output$obj)
}

#'@export
reset_state.dfr_kldist <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_kldist(
    target_feat = obj$target_feat,
    p_th = obj$state$p_th,
    window_size = obj$state$window_size,
    data = obj$state$window
  )$state
  return(obj)  
}
