#'@title Levene Based Drift Detection Method method
#'@description LBDD is a window-based detector that compares the variability of a reference and a recent sample using Levene's test. Because it monitors changes in the distribution of an observed feature rather than model performance, it is primarily aimed at **virtual concept drift**. In this package, the detector follows the statistical-testing approach discussed by Giusti et al. (2021) for drift analysis, using Levene's variance test as its core mechanism.
#'@param target_feat Feature to be monitored
#'@param alpha Probability threshold for the test statistic
#'@param window_size Size of the window
#'@param window_type Window policy. `"sliding"` (default) keeps at most `window_size` observations, so both halves move with the stream. `"anchored"` keeps every observation, which pins the reference half to the beginning of the stream and makes memory grow with the stream length.
#'@param monitoring_step Number of observations between two consecutive tests. The default (`1`) tests at every observation; larger values reduce the computational cost on long streams.
#'@param data Already collected data to avoid cold start.
#'@details Levene's test is computed with the group spread centred on the
#'median, the Brown-Forsythe variant. Missing observations are skipped instead
#'of being imputed.
#LBDD detection: Lucas Giusti, Leonardo Carvalho, Antonio Tadeu Gomes, Rafaelli Coutinho, Jorge Soares, Eduardo Ogasawara, Analysing flight delay under concept drift, Evolving Systems, 2021, DOI:/10.1007/s12530-021-09415-z.
#'@references Giusti, L., Carvalho, L., Gomes, A. T., Coutinho, R., Soares, J., and Ogasawara, E. (2021). Analysing flight delay under concept drift. *Evolving Systems*. <doi:10.1007/s12530-021-09415-z>
#'@return `dfr_lbdd` object
#'@importFrom utils head tail
#'@example examples/1_detection/r/dfr_lbdd.R
#'@export
dfr_lbdd <- function(target_feat=NULL, alpha=0.05, window_size=1500, window_type="sliding", monitoring_step=1, data=NULL) {
  .check_probability(alpha, "alpha")
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  window_type <- .check_window_type(window_type)

  obj <- dist_based(target_feat = target_feat)

  state <- list()
  state$window_size <- window_size
  state$alpha <- alpha
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
  obj$drifted <- FALSE
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL

  class(obj) <- append("dfr_lbdd", class(obj))
  return(obj)
}

#'@export
update_state.dfr_lbdd <- function(obj, value, ...) {
  state <- obj$state
  obj$last_drifter_output <- NA_real_

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

  state$p_value <- .levene_pvalue(
    values = c(old_window, new_window),
    group = rep(c('History', 'Recent'), c(length(old_window), length(new_window)))
  )
  obj$last_drifter_output <- state$p_value

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
fit.dfr_lbdd <- function(obj, data, ...) {
  return(.fit_vector_stream(obj, data, output_names = "levene_p"))
}

#'@export
reset_state.dfr_lbdd <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_lbdd(
    target_feat = obj$target_feat,
    alpha = obj$state$alpha,
    window_size = obj$state$window_size,
    window_type = obj$state$window_type,
    monitoring_step = obj$state$monitoring_step,
    data = obj$state$window
  )$state
  return(obj)
}
