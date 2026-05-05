#'@title Adapted Page Hinkley method
#'@description The Page-Hinkley test is a sequential change-point detector that monitors cumulative deviations from a running mean and signals a change when those deviations grow persistently. In this package, the implementation is primarily used for **virtual concept drift** when it monitors a numeric feature stream, although the same statistic can also be applied to error streams to detect **real concept drift**. The method is based on Page (1954) and the later streaming adaptation popularized in data-stream mining.
#'@param target_feat Feature to be monitored.
#'@param min_instances The minimum number of instances before detecting change
#'@param delta The delta factor for the Page Hinkley test
#'@param threshold The change detection threshold (lambda)
#'@param alpha The forgetting factor, used to weight the observed value and the mean
#Page Hinkley detection: E. S. Page. (1954) Continuous Inspection Schemes, Biometrika 41(1/2), 100–115.
#Page Hinkley detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/page_hinkley.py#L4
#'@references Page, E. S. (1954). Continuous inspection schemes. *Biometrika*, 41(1/2), 100-115. <doi:10.2307/2333009>
#'@return `dfr_page_hinkley` object
#'@example examples/1_detection/r/dfr_page_hinkley.R
#'@export
dfr_page_hinkley <- function(target_feat=NULL, min_instances=30, delta=0.005, threshold=50, alpha=1-0.0001) {
  obj <- dist_based(target_feat=target_feat)
  
  state <- list()
  state$min_instances <- min_instances
  state$delta <- delta
  state$threshold <- threshold
  state$alpha = alpha
  state$x_mean <- 0
  state$sum <- 0
  state$min_sum <- 0
  state$sample_count <- 1
  
  obj$state <- state
  
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  obj$drifted <- FALSE
  
  class(obj) <- append("dfr_page_hinkley", class(obj))
  return(obj)
}

#'@export
update_state.dfr_page_hinkley <- function(obj, value){
  state <- obj$state
  
  state$x_mean <- state$x_mean + (value - state$x_mean)/state$sample_count
  state$sum <- state$alpha * state$sum + (value - state$x_mean - state$delta)
  state$min_sum <- min(state$min_sum, state$sum)
  state$sample_count <- state$sample_count + 1
  
  obj$last_drifter_output <- state$sum
  
  tryCatch(
    {
      if(state$sample_count < state$min_instances){
        obj$state <- state
        return(list(obj=obj, drift=FALSE))
      }
      else if(state$sum > state$threshold){
        state$x_mean <- 0
        state$sum <- 0
        state$sample_count <- 1
        
        obj$drifted <- TRUE
        
        obj$state <- state
        return(list(obj=obj, drift=TRUE))
      }
      else{
        obj$state <- state
        return(list(obj=obj, drift=FALSE))
      }
      
      return(list(obj=obj, drift=obj$drifted))
    },
    error=function(cond){
      message(conditionMessage(cond))
      if(is.na(value)){
        message('Input is null')
      }else{
        message(value)
      }
      }
    )
}

#'@export
fit.dfr_page_hinkley <- function(obj, data, ...){
  
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
  
  return(output$obj)
}

#'@export
reset_state.dfr_page_hinkley <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_page_hinkley(
    target_feat = obj$target_feat,
    min_instances = obj$state$min_instances,
    delta = obj$state$delta,
    threshold = obj$state$threshold,
    alpha = obj$state$alpha
  )$state
  return(obj)  
}
