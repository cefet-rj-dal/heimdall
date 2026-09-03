#'@title Mean Comparison Distance method
#'@description MCDD is a window-based detector that compares the location of reference and recent samples by means of hypothesis tests on their central tendency. Because it monitors the distribution of observed features rather than predictive errors, it is primarily intended for **virtual concept drift**. In this package, the detector follows the statistical-testing perspective adopted by Giusti et al. (2021) for drift analysis.
#'@param target_feat Feature to be monitored
#'@param alpha Probability threshold for all test statistics
#'@param window_size Size of the sliding window
#'@param monitoring_step Number of observations between two consecutive tests. The default (`1`) tests at every observation; larger values reduce the computational cost on long streams.
#'@param data Already collected data to avoid cold start.
#'@details Normality of both halves of the window is checked with
#'[stats::shapiro.test()]; when it holds, a t-test is applied, otherwise a
#'Wilcoxon rank-sum test is used. Shapiro-Wilk is only defined for samples of at
#'most 5000 observations, so larger windows always fall back to the
#'nonparametric test.
#'
#'Missing observations are skipped instead of being imputed. When a drift is
#'reported the window is trimmed to its most recent half, and `reset_state()`
#'preserves that window so the detector does not restart cold.
#MCDD detection: Lucas Giusti, Leonardo Carvalho, Antonio Tadeu Gomes, Rafaelli Coutinho, Jorge Soares, Eduardo Ogasawara, Analysing flight delay under concept drift, Evolving Systems, 2021, DOI:/10.1007/s12530-021-09415-z.
#'@references Giusti, L., Carvalho, L., Gomes, A. T., Coutinho, R., Soares, J., and Ogasawara, E. (2021). Analysing flight delay under concept drift. *Evolving Systems*. <doi:10.1007/s12530-021-09415-z>
#'@return `dfr_mcdd` object
#'@importFrom stats shapiro.test t.test wilcox.test
#'@importFrom utils head tail
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
#'model <- dfr_mcdd(target_feat='serie', window_size=100, alpha=0.05)
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
dfr_mcdd <- function(target_feat = NULL, alpha = 1e-08, window_size = 1500, monitoring_step = 1, data = NULL) {
  .check_probability(alpha, "alpha")
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)

  obj <- dist_based(target_feat = target_feat)

  state <- list()
  state$window_size <- window_size
  state$alpha <- alpha
  state$monitoring_step <- monitoring_step
  state$n <- 0
  state$p_value <- NA_real_

  if (is.null(data)) {
    state$window <- numeric(0)
  } else {
    state$window <- as.numeric(data)
  }

  obj$state <- state

  class(obj) <- append("dfr_mcdd", class(obj))
  return(obj)
}

#'@export
update_state.dfr_mcdd <- function(obj, value, ...) {
  state <- obj$state

  state$n <- state$n + 1
  value <- .as_scalar(value)
  if (is.na(value)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  if (length(state$window) < state$window_size) {
    state$window <- c(state$window, value)
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  state$window <- c(state$window[-1L], value)

  if ((state$n %% state$monitoring_step) != 0) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  half <- floor(state$window_size / 2)
  new_window <- utils::tail(state$window, half)
  old_window <- utils::head(state$window, half)

  if (isTRUE(all.equal(new_window, old_window))) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  # Choose only one hypothesis test based on normality assumptions.
  use_ttest <- FALSE
  if ((length(unique(new_window)) >= 3) && (length(unique(old_window)) >= 3) &&
      (length(new_window) <= 5000) && (length(old_window) <= 5000)) {
    if ((stats::shapiro.test(new_window)$p.value > 0.05) &&
        (stats::shapiro.test(old_window)$p.value > 0.05)) {
      use_ttest <- TRUE
    }
  }

  if (use_ttest) {
    state$p_value <- stats::t.test(new_window, old_window)$p.value
  } else {
    state$p_value <- suppressWarnings(stats::wilcox.test(new_window, old_window)$p.value)
  }

  if (!is.na(state$p_value) && (state$p_value < state$alpha)) {
    state$window <- utils::tail(state$window, half)

    obj$drifted <- TRUE
    obj$state <- state
    return(list(obj = obj, drift = TRUE))
  }

  obj$state <- state
  return(list(obj = obj, drift = FALSE))
}

#'@export
fit.dfr_mcdd <- function(obj, data, ...) {
  return(.fit_vector_stream(obj, data))
}

#'@export
reset_state.dfr_mcdd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_mcdd(
    target_feat = obj$target_feat,
    alpha = obj$state$alpha,
    window_size = obj$state$window_size,
    monitoring_step = obj$state$monitoring_step,
    data = obj$state$window
  )$state
  return(obj)
}
