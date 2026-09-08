#'@title Adapted Page Hinkley method
#'@description The Page-Hinkley test is a sequential change-point detector that monitors the cumulative deviation of a signal from its running mean and signals a change when that deviation departs from its running minimum by more than a threshold. In this package, the implementation is primarily used for **virtual concept drift** when it monitors a numeric feature stream, although the same statistic can also be applied to error streams to detect **real concept drift**. The method is based on Page (1954).
#'@param target_feat Feature to be monitored.
#'@param min_instances The minimum number of instances before detecting change
#'@param delta The delta factor for the Page Hinkley test
#'@param threshold The change detection threshold (lambda)
#'@param alpha The forgetting factor, used to weight the observed value and the mean
#'@details Missing observations are skipped instead of being imputed, so that a
#'single `NA` cannot poison the running statistics.
#Page Hinkley detection: E. S. Page. (1954) Continuous Inspection Schemes, Biometrika 41(1/2), 100-115.
#Page Hinkley detection implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/page_hinkley.py#L4
#'@references Page, E. S. (1954). Continuous inspection schemes. *Biometrika*, 41(1/2), 100-115. <doi:10.2307/2333009>
#'@return `dfr_page_hinkley` object
#'@example examples/1_detection/r/dfr_page_hinkley.R
#'@export
dfr_page_hinkley <- function(target_feat=NULL, min_instances=30, delta=0.005, threshold=50, alpha=1 - 1e-04) {
  .check_positive_integer(min_instances, "min_instances", min_value = 1L)
  .check_positive_integer(threshold, "threshold", min_value = 0L)
  .check_probability(alpha, "alpha")
  if (!is.numeric(delta) || (length(delta) != 1L) || is.na(delta)) {
    stop("delta must be a single numeric value", call. = FALSE)
  }

  obj <- dist_based(target_feat = target_feat)

  state <- list()
  state$min_instances <- min_instances
  state$delta <- delta
  state$threshold <- threshold
  state$alpha <- alpha
  state$x_mean <- 0
  state$sum <- 0
  state$min_sum <- 0
  state$sample_count <- 1

  obj$state <- state

  obj$drifted <- FALSE
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL

  class(obj) <- append("dfr_page_hinkley", class(obj))
  return(obj)
}

#'@export
update_state.dfr_page_hinkley <- function(obj, value, ...) {
  state <- obj$state

  value <- .as_scalar(value)
  if (is.na(value)) {
    obj$last_drifter_output <- NA_real_
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  state$x_mean <- state$x_mean + (value - state$x_mean) / state$sample_count
  state$sum <- state$alpha * state$sum + (value - state$x_mean - state$delta)
  state$min_sum <- min(state$min_sum, state$sum)
  state$sample_count <- state$sample_count + 1

  obj$last_drifter_output <- state$sum - state$min_sum

  if (state$sample_count < state$min_instances) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  if ((state$sum - state$min_sum) > state$threshold) {
    state$x_mean <- 0
    state$sum <- 0
    state$min_sum <- 0
    state$sample_count <- 1

    obj$drifted <- TRUE
    obj$state <- state
    return(list(obj = obj, drift = TRUE))
  }

  obj$state <- state
  return(list(obj = obj, drift = FALSE))
}

#'@export
fit.dfr_page_hinkley <- function(obj, data, ...) {
  return(.fit_vector_stream(obj, data, output_names = "ph_statistic"))
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
