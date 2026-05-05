#'@title Mean Comparison Distance method
#'@description MCDD is a window-based detector that compares the location of reference and recent samples by means of hypothesis tests on their central tendency. Because it monitors the distribution of observed features rather than predictive errors, it is primarily intended for **virtual concept drift**. In this package, the detector follows the statistical-testing perspective adopted by Giusti et al. (2021) for drift analysis.
#'@param target_feat Feature to be monitored
#'@param alpha Probability theshold for all test statistics
#'@param window_size Size of the sliding window
#MCDD detection: Lucas Giusti, Leonardo Carvalho, Antonio Tadeu Gomes, Rafaelli Coutinho, Jorge Soares, Eduardo Ogasawara, Analysing flight delay under concept drift, Evolving Systems, 2021, DOI:/10.1007/s12530-021-09415-z.
#'@references Giusti, L., Carvalho, L., Gomes, A. T., Coutinho, R., Soares, J., and Ogasawara, E. (2021). Analysing flight delay under concept drift. *Evolving Systems*. <doi:10.1007/s12530-021-09415-z>
#'@return `dfr_mcdd` object
#'@example examples/1_detection/r/dfr_mcdd.R
#'@export
dfr_mcdd <- function(target_feat=NULL, alpha=0.00000001, window_size=1500) {
    obj <- dist_based(target_feat = target_feat)
    
    state <- list()
    state$window_size <- window_size
    state$alpha <- alpha
    state$n <- 0

    if ((state$alpha < 0) | (state$alpha > 1)) stop("Alpha must be between 0 and 1", call = FALSE)
    if (state$window_size < 0) stop("window_size must be greater than 0", call = FALSE)

    state$window <- c()

    obj$state <- state
    obj$last_drifter_output <- NULL
    obj$drifter_output <- NULL
    
    class(obj) <- append("dfr_mcdd", class(obj))
    return(obj)
}

#'@importFrom utils head tail
#'@export
update_state.dfr_mcdd <- function(obj, value) {
  obj$last_drifter_output <- NULL
  old_p <- NA
  new_p <- NA
  comp_p <- NA
  obj$last_drifter_output <- cbind(old_p, new_p, comp_p)
  
  state <- obj$state

  state$n <- state$n + 1
  value <- as.numeric(value[1])
  if (is.na(value)) {
    obj$state <- state
    return(list(obj=obj, drift=FALSE))
  }
  
  state$window <- c(state$window, value)
  currentLength <- length(state$window)
  
  if (currentLength >= state$window_size){
    new_window <- tail(state$window, state$window_size/2)
    old_window <- head(state$window, state$window_size/2)
    
    if (mean(new_window==old_window, na.rm=TRUE) == 1){
      obj$state <- state
      return(list(obj=obj, drift=FALSE))
    }
    
    # Normality Test
    if ((length(unique(new_window)) >= 2) & (length(unique(old_window)) >= 2)){
      new_p <- shapiro.test(as.numeric(new_window))$p
      old_p <- shapiro.test(as.numeric(old_window))$p
      if ((new_p > state$alpha) & (old_p > state$alpha)){
        # T Test
        comp_p <- t.test(new_window, old_window)$p.value
        if (comp_p < state$alpha){
          obj$drifted <- TRUE
          
          obj$last_drifter_output <- cbind(old_p, new_p, comp_p)
          
          obj$state <- state
          return(list(obj=obj, drift=TRUE))
          }
        }
    }
    # Mann Whitney
    comp_p <- wilcox.test(as.numeric(new_window), as.numeric(old_window))$p.value
    if (comp_p < state$alpha){
      obj$drifted <- TRUE
      
      obj$last_drifter_output <- cbind(old_p, new_p, comp_p)
      
      obj$state <- state
      return(list(obj=obj, drift=TRUE))
    }
  }
  
  obj$state <- state
  return(list(obj=obj, drift=FALSE))
}

#'@export
fit.dfr_mcdd <- function(obj, data, ...){
  
  obj$drifter_output <- NULL
  obj$last_drifter_output <- NULL
  output <- update_state(obj, data[1])
  output$obj$drifter_output <- rbind(output$obj$drifter_output, output$obj$last_drifter_output)
  if (length(data) > 1){
    for (i in 2:length(data)){
      output <- update_state(output$obj, data[i])
      output$obj$drifter_output <- rbind(output$obj$drifter_output, output$obj$last_drifter_output)
    }
  }
  
  output$obj$drifter_output <- as.data.frame(output$obj$drifter_output)
  names(output$obj$drifter_output) <- c('Old Shapiro p', 'New Shapiro p', 'Comparison p')
  
  return(output$obj)
}

#'@export
reset_state.dfr_mcdd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_mcdd(
    target_feat = obj$target_feat,
    alpha = obj$state$alpha,
    window_size = obj$state$window_size
  )$state
  return(obj)  
}
