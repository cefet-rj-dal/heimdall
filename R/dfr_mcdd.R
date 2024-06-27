#'@title Mean Comparison Distance method
#'@description Mean Comparison statistical method for concept drift detection.
#'@param target_feat Feature to be monitored
#'@param alpha Probability theshold for all test statistics
#'@param window_size Size of the sliding window
#MCDD detection: Lucas Giusti, Leonardo Carvalho, Antonio Tadeu Gomes, Rafaelli Coutinho, Jorge Soares, Eduardo Ogasawara, Analysing flight delay under concept drift, Evolving Systems, 2021, DOI:/10.1007/s12530-021-09415-z.
#'@return `dfr_mcdd` object
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
#'model <- dfr_mcdd(target_feat='depart_visibility')
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
dfr_mcdd <- function(target_feat, alpha=0.05, window_size=100) {
    obj <- dist_based(target_feat = target_feat)
    
    state <- list()
    state$window_size <- window_size
    state$alpha <- alpha
    state$n <- 0

    if ((state$alpha < 0) | (state$alpha > 1)) stop("Alpha must be between 0 and 1", call = FALSE)
    if (state$window_size < 0) stop("window_size must be greater than 0", call = FALSE)

    state$window <- c()

    obj$state <- state

    class(obj) <- append("dfr_mcdd", class(obj))
    return(obj)
}

#'@importFrom utils head tail
#'@export
update_state.dfr_mcdd <- function(obj, value) {
  state <- obj$state

  state$n <- state$n + 1
  currentLength <- nrow(state$window)
  if (is.null(currentLength)){
    currentLength <- 0
  }
  
  if (currentLength >= state$window_size){
    state$window <- tail(state$window, -1)
    new_window <- tail(state$window, state$window_size/2)
    old_window <- head(state$window, state$window_size/2)
    
    if (mean(new_window==old_window) == 1){
      state$window <- rbind(state$window, value)
      
      obj$state <- state
      return(list(obj=obj, drift=FALSE))
    }
    
    # Normality Test
    if ((nrow(unique(new_window)) >= 2) & (nrow(unique(old_window)) >= 2)){
      if ((shapiro.test(new_window)$p > state$alpha) & (shapiro.test(old_window)$p > state$alpha)){
        # T Test
        if (t.test(new_window, old_window)$p.value < state$alpha){
          obj$drifted <- TRUE
          
          obj$state <- state
          return(list(obj=obj, drift=TRUE))
          }
        }
      }
    # Mann Whitney
    if (wilcox.test(new_window, old_window)$p.value < state$alpha){
      obj$drifted <- TRUE
      
      obj$state <- state
      return(list(obj=obj, drift=TRUE))
      }
    }
  state$window <- rbind(state$window, value)
  
  obj$state <- state
  return(list(obj=obj, drift=FALSE))
}

#'@export
fit.dfr_mcdd <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  for (i in 2:length(data)){
    output <- update_state(output$obj, data[i])
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_mcdd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_mcdd(
    target_feat = obj$target_feat,
    alpha = obj$state$alpha
  )$state
  return(obj)  
}