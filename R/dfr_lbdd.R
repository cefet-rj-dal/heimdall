#'@title Levene Based Drift Detection Method method
#'@description LBDD is a window-based detector that compares the variability of reference and recent samples using Levene's test. Because it monitors changes in the distribution of an observed feature rather than model performance, it is primarily aimed at **virtual concept drift**. In this package, the detector follows the statistical-testing approach discussed by Giusti et al. (2021) for drift analysis, using Levene's variance test as its core mechanism.
#'@param target_feat Feature to be monitored
#'@param alpha Probability theshold for the test statistic
#'@param window_size Size of the sliding window
#MCDD detection: Lucas Giusti, Leonardo Carvalho, Antonio Tadeu Gomes, Rafaelli Coutinho, Jorge Soares, Eduardo Ogasawara, Analysing flight delay under concept drift, Evolving Systems, 2021, DOI:/10.1007/s12530-021-09415-z.
#'@references Giusti, L., Carvalho, L., Gomes, A. T., Coutinho, R., Soares, J., and Ogasawara, E. (2021). Analysing flight delay under concept drift. *Evolving Systems*. <doi:10.1007/s12530-021-09415-z>
#'@return `dfr_lbdd` object
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
#'model <- dfr_lbdd(target_feat='depart_visibility')
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
dfr_lbdd <- function(target_feat=NULL, alpha=0.01, window_size=1500) {
    obj <- dist_based(target_feat = target_feat)
    
    obj$drifted <- FALSE
    
    state <- list()
    state$window_size <- window_size
    state$alpha <- alpha
    state$n <- 0

    if ((state$alpha < 0) | (state$alpha > 1)) stop("Alpha must be between 0 and 1", call = FALSE)
    if (state$window_size < 0) stop("window_size must be greater than 0", call = FALSE)

    state$window <- c()

    obj$state <- state

    class(obj) <- append("dfr_lbdd", class(obj))
    return(obj)
}

#'@importFrom utils head tail
#'@export
update_state.dfr_lbdd <- function(obj, value) {
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
    
    if (mean(new_window==old_window, na.rm=TRUE) == 1){
      state$window <- rbind(state$window, value)
      
      obj$state <- state
      return(list(obj=obj, drift=FALSE))
    }
    
    # Levene Test
    old_window <- as.data.frame(old_window)
    old_window['window'] <- 'History'
    new_window <- as.data.frame(new_window)
    new_window['window'] <- 'Recent'
    levene_df <- as.data.frame(rbind(old_window, new_window))
    levene_df['window'] <- factor(levene_df[['window']])
    
    names(levene_df) <- c('V1', 'window')
    
    levene_results <- car::leveneTest(V1 ~ window, data=as.data.frame(levene_df))
    
    if (levene_results['group', 'Pr(>F)'] < state$alpha){
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
fit.dfr_lbdd <- function(obj, data, ...){
  output <- update_state(obj, data[1])
  if (length(data) > 1){
    for (i in 2:length(data)){
      output <- update_state(output$obj, data[i])
    }
  }
  
  return(output$obj)
}

#'@export
reset_state.dfr_lbdd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_lbdd(
    target_feat = obj$target_feat,
    alpha = obj$state$alpha,
    window_size = obj$state$window_size
  )$state
  return(obj)  
}
