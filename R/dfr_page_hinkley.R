#'@title Adapted Page Hinkley method
#'@description The Page-Hinkley test is a sequential change-point detector that monitors cumulative deviations from a running mean and signals a change when those deviations grow persistently. In this package, the implementation is primarily used for **virtual concept drift** when it monitors a numeric feature stream, although the same statistic can also be applied to error streams to detect **real concept drift**. The method is based on Page (1954) and the later streaming adaptation popularized in data-stream mining.
#'@param target_feat Feature to be monitored.
#'@param min_instances The minimum number of instances before detecting change
#'@param delta The delta factor for the Page Hinkley test
#'@param threshold The change detection threshold (lambda)
#'@param alpha The forgetting factor, used to weight the observed value and the mean
#'@param direction Which changes of the mean are detected: `"increase"` (the
#'default, as in the reference implementation), `"decrease"` or `"both"`.
#'@details Missing observations are skipped instead of being imputed.
#Page Hinkley detection: E. S. Page. (1954) Continuous Inspection Schemes, Biometrika 41(1/2), 100-115.
#Page Hinkley detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/page_hinkley.py#L4
#'@references Page, E. S. (1954). Continuous inspection schemes. *Biometrika*, 41(1/2), 100-115. <doi:10.2307/2333009>
#'@return `dfr_page_hinkley` object
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'# This example assumes a model residual where 1 is an error and 0 is a
#'# correct prediction.
#'
#'data(st_drift_examples)
#'data <- st_drift_examples$univariate
#'data$event <- NULL
#'
#'model <- dfr_page_hinkley(target_feat='serie')
#'
#'detection <- NULL
#'output <- list(obj=model, drift=FALSE)
#'for (i in seq_along(data$serie)){
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
dfr_page_hinkley <- function(target_feat = NULL, min_instances = 30, delta = 0.005, threshold = 50, alpha = 1 - 1e-04, direction = "increase") {
  .check_positive_integer(min_instances, "min_instances", min_value = 1L)
  .check_number(threshold, "threshold", min_value = 0)
  .check_probability(alpha, "alpha")
  .check_number(delta, "delta")
  direction <- match.arg(direction, c("increase", "decrease", "both"))

  obj <- dist_based(target_feat = target_feat)

  obj$state <- .page_hinkley_state(min_instances, delta, threshold, alpha, direction)
  obj$drifted <- FALSE

  class(obj) <- append("dfr_page_hinkley", class(obj))
  return(obj)
}

# `sum_up` accumulates deviations above the running mean and `sum_down`
# deviations below it; each one is compared with its own running minimum.
#' @noRd
.page_hinkley_state <- function(min_instances, delta, threshold, alpha, direction) {
  list(
    min_instances = min_instances,
    delta = delta,
    threshold = threshold,
    alpha = alpha,
    direction = direction,
    x_mean = 0,
    sum_up = 0,
    min_sum_up = 0,
    sum_down = 0,
    min_sum_down = 0,
    sample_count = 1
  )
}

#'@export
update_state.dfr_page_hinkley <- function(obj, value, ...) {
  state <- obj$state

  value <- .as_scalar(value, obj$target_feat)
  if (is.na(value)) {
    return(list(obj = obj, drift = FALSE))
  }

  state$x_mean <- state$x_mean + (value - state$x_mean) / state$sample_count
  state$sum_up <- state$alpha * state$sum_up + (value - state$x_mean - state$delta)
  state$min_sum_up <- min(state$min_sum_up, state$sum_up)
  state$sum_down <- state$alpha * state$sum_down + (state$x_mean - value - state$delta)
  state$min_sum_down <- min(state$min_sum_down, state$sum_down)
  state$sample_count <- state$sample_count + 1

  if (state$sample_count < state$min_instances) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  up <- (state$sum_up - state$min_sum_up) > state$threshold
  down <- (state$sum_down - state$min_sum_down) > state$threshold
  has_drift <- switch(state$direction, increase = up, decrease = down, both = up || down)

  if (has_drift) {
    obj$state <- .page_hinkley_state(state$min_instances, state$delta, state$threshold, state$alpha, state$direction)
    obj$drifted <- TRUE
    return(list(obj = obj, drift = TRUE))
  }

  obj$state <- state
  return(list(obj = obj, drift = FALSE))
}

#'@export
fit.dfr_page_hinkley <- function(obj, data, ...) {
  return(.fit_vector_stream(obj, data))
}

#'@export
reset_state.dfr_page_hinkley <- function(obj) {
  obj$drifted <- FALSE
  state <- obj$state
  obj$state <- .page_hinkley_state(state$min_instances, state$delta, state$threshold, state$alpha, state$direction)
  return(obj)
}
