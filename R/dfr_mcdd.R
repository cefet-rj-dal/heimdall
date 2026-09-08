#'@title Mean Comparison Distance method
#'@description MCDD is a window-based detector that compares the location of a reference and a recent sample by means of a hypothesis test on their central tendency. Because it monitors the distribution of an observed feature rather than predictive errors, it is primarily intended for **virtual concept drift**. In this package, the detector follows the statistical-testing perspective adopted by Giusti et al. (2021) for drift analysis.
#'@param target_feat Feature to be monitored
#'@param alpha Probability threshold for the comparison test
#'@param normality_alpha Significance level of the Shapiro-Wilk normality test used to decide between the parametric and the nonparametric comparison. It is deliberately separate from `alpha`, since the two thresholds answer different questions.
#'@param window_size Size of the window
#'@param window_type Window policy. `"sliding"` (default) keeps at most `window_size` observations, so both halves move with the stream. `"anchored"` keeps every observation, which pins the reference half to the beginning of the stream and makes memory grow with the stream length.
#'@param monitoring_step Number of observations between two consecutive tests. The default (`1`) tests at every observation; larger values reduce the computational cost on long streams.
#'@param data Already collected data to avoid cold start.
#'@details Exactly one comparison test is applied per evaluation. When both
#'halves pass the normality test a t-test is used, otherwise a Wilcoxon
#'rank-sum test is used. Shapiro-Wilk is only defined for samples of at most
#'5000 observations, so larger halves always take the nonparametric route.
#'
#'Missing observations are skipped instead of being imputed.
#MCDD detection: Lucas Giusti, Leonardo Carvalho, Antonio Tadeu Gomes, Rafaelli Coutinho, Jorge Soares, Eduardo Ogasawara, Analysing flight delay under concept drift, Evolving Systems, 2021, DOI:/10.1007/s12530-021-09415-z.
#'@references Giusti, L., Carvalho, L., Gomes, A. T., Coutinho, R., Soares, J., and Ogasawara, E. (2021). Analysing flight delay under concept drift. *Evolving Systems*. <doi:10.1007/s12530-021-09415-z>
#'@return `dfr_mcdd` object
#'@importFrom stats shapiro.test t.test wilcox.test
#'@importFrom utils head tail
#'@example examples/1_detection/r/dfr_mcdd.R
#'@export
dfr_mcdd <- function(target_feat=NULL, alpha=1e-08, normality_alpha=0.05, window_size=1500, window_type="sliding", monitoring_step=1, data=NULL) {
  .check_probability(alpha, "alpha")
  .check_probability(normality_alpha, "normality_alpha")
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  window_type <- .check_window_type(window_type)

  obj <- dist_based(target_feat = target_feat)

  state <- list()
  
  state$window_size <- window_size
  state$alpha <- alpha
  state$normality_alpha <- normality_alpha
  state$window_type <- window_type
  state$monitoring_step <- monitoring_step
  state$n <- 0
  state$p_value <- NA_real_

  if (is.null(data)) {
    state$window <- numeric(0)
  } else {
    state$window <- as.numeric(data)
  }

  obj$state <- state

  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL

  class(obj) <- append("dfr_mcdd", class(obj))
  return(obj)
}

#'@export
update_state.dfr_mcdd <- function(obj, value, ...) {
  state <- obj$state
  obj$last_drifter_output <- cbind(old_shapiro_p = NA_real_, new_shapiro_p = NA_real_, comparison_p = NA_real_)

  state$n <- state$n + 1
  value <- .as_scalar(value)
  if (is.na(value)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  state$window <- .trim_window(c(state$window, value), state$window_size, state$window_type)

  if (length(state$window) < state$window_size) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

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

  # Normality test, used only to pick the comparison test.
  new_p <- NA_real_
  old_p <- NA_real_
  use_ttest <- FALSE
  testable <- (length(unique(new_window)) >= 3) && (length(unique(old_window)) >= 3) &&
    (length(new_window) <= 5000) && (length(old_window) <= 5000)
  if (testable) {
    new_p <- stats::shapiro.test(new_window)$p.value
    old_p <- stats::shapiro.test(old_window)$p.value
    use_ttest <- (new_p > state$normality_alpha) && (old_p > state$normality_alpha)
  }

  # Exactly one comparison test per evaluation.
  if (use_ttest) {
    state$p_value <- stats::t.test(new_window, old_window)$p.value
  } else {
    state$p_value <- suppressWarnings(stats::wilcox.test(new_window, old_window)$p.value)
  }

  obj$last_drifter_output <- cbind(old_shapiro_p = old_p, new_shapiro_p = new_p, comparison_p = state$p_value)

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
  return(.fit_vector_stream(obj, data, output_names = c("Old Shapiro p", "New Shapiro p", "Comparison p")))
}

#'@export
reset_state.dfr_mcdd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_mcdd(
    target_feat = obj$target_feat,
    alpha = obj$state$alpha,
    normality_alpha = obj$state$normality_alpha,
    window_size = obj$state$window_size,
    window_type = obj$state$window_type,
    monitoring_step = obj$state$monitoring_step
  )$state
  return(obj)
}
