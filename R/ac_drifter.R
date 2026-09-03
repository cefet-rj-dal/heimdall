#'@title Drifter
#'@description Ancestor class for drift detection.
#'
#'Every detector in the package follows the same contract:
#'\itemize{
#'\item `update_state(obj, value)` returns a list with the updated object
#'(`obj`) and a logical flag (`drift`) telling whether a drift was detected
#'**in that call**;
#'\item `obj$drifted` is sticky: it stays `TRUE` after the first detection
#'until `reset_state()` is called;
#'\item `reset_state(obj)` returns the detector to its initial state.
#'}
#'@return Drifter object
#'@examples
#'model <- drifter()
#'model$drifted
#'@import daltoolbox
#'@export
drifter <- function() {
  obj <- dal_base()
  obj$drifted <- FALSE
  obj$state <- list()
  class(obj) <- append("drifter", class(obj))
  return(obj)
}


#'@title Reset State
#'@description Reset Drifter State
#'@param obj Drifter object
#'@return updated Drifter object
#'@examples
#'model <- dfr_ddm()
#'model <- reset_state(model)
#'model$drifted
#'@export
reset_state <- function(obj) {
  UseMethod("reset_state")
}

#'@title Update State
#'@description Update Drifter State with a new observation.
#'@param obj Drifter object
#'@param value a value that represents a processed batch
#'@param ... optional arguments
#'@return a list with two elements: `obj`, the updated Drifter object, and
#'`drift`, a logical flag indicating whether a drift was detected in this call
#'@examples
#'model <- dfr_ddm()
#'output <- update_state(model, 0)
#'output$drift
#'@export
update_state <- function(obj, value, ...) {
  UseMethod("update_state")
}

#'@export
reset_state.drifter <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- list()
  return(obj)
}

#'@export
update_state.drifter <- function(obj, value, ...) {
  return(list(obj = obj, drift = obj$drifted))
}

#'@title Process Batch
#'@description Process Batch
#'@param obj Drifter object
#'@param data data batch in data frame format
#'@param prediction prediction batch as vector format. Optional, and unused by
#'the default method; it is kept for compatibility with detectors that need the
#'model output.
#'@param ... optional arguments
#'@return updated Drifter object
#'@examples
#'library(daltoolbox)
#'model <- dfr_inactive()
#'model <- fit(model, data.frame(serie = 1:10))
#'model$drifted
#'@export
fit.drifter <- function(obj, data, prediction = NULL, ...) {
  return(obj)
}

# Basic dummy detectors

#'@title Inactive dummy detector
#'@description Implements a dummy detector that never reports a drift. Useful
#'as a baseline when evaluating adaptation strategies.
#'@return Drifter object
#'@examples
#'model <- dfr_inactive()
#'output <- update_state(model, 1)
#'output$drift
#'@export
dfr_inactive <- function() {
  obj <- drifter()
  obj$state <- list()

  obj$drifted <- FALSE
  class(obj) <- append('dfr_inactive', class(obj))
  return(obj)
}

#'@export
reset_state.dfr_inactive <- function(obj) {
  obj$drifted <- FALSE
  return(obj)
}

#'@title Passive dummy detector
#'@description Implements a dummy detector that always reports a drift. Useful
#'as an upper baseline, since it forces the model to be retrained at every
#'batch.
#'@return Drifter object
#'@examples
#'model <- dfr_passive()
#'output <- update_state(model, 1)
#'output$drift
#'@export
dfr_passive <- function() {
  obj <- drifter()
  obj$state <- list()

  obj$drifted <- TRUE
  class(obj) <- append('dfr_passive', class(obj))
  return(obj)
}

#'@export
reset_state.dfr_passive <- function(obj) {
  obj$drifted <- TRUE
  return(obj)
}

#'@title Error Based Drifter sub-class
#'@description Implements Error Based drift detectors. These detectors monitor
#'the residuals of a predictive model, where `1` (or `TRUE`) means a wrong
#'prediction and `0` (or `FALSE`) a correct one. Missing values are treated as
#'correct predictions.
#'@return Drifter object
#'@examples
#'obj <- error_based()
#'class(obj)
#'@export
error_based <- function() {
  obj <- drifter()
  class(obj) <- append('error_based', class(obj))
  return(obj)
}

#'@title Distribution Based Drifter sub-class
#'@description Implements univariate Distribution Based drift detectors. These
#'detectors monitor a single numeric feature; observations that are missing are
#'skipped instead of being imputed.
#'@param target_feat Feature to be monitored.
#'@return Drifter object
#'@examples
#'obj <- dist_based(target_feat = 'serie')
#'obj$target_feat
#'@export
dist_based <- function(target_feat) {
  obj <- drifter()
  obj$target_feat <- target_feat
  class(obj) <- append('dist_based', class(obj))
  return(obj)
}

#'@title Multivariate Distribution Based Drifter sub-class
#'@description Implements Multivariate Distribution Based drift detectors.
#'@return Drifter object
#'@examples
#'obj <- mv_dist_based()
#'class(obj)
#'@export
mv_dist_based <- function() {
  obj <- drifter()
  class(obj) <- append('mv_dist_based', class(obj))
  return(obj)
}

#'@title Multi Criteria Drifter sub-class
#'@description Combines the output of several drift detectors into a single
#'decision.
#'@param drifter_list Named list of drifters to combine. Every element must
#'inherit from either `dist_based` or `mv_dist_based`.
#'@param combination How the drifters will be combined. One of `'or'`
#'(default), `'and'` or `'fuzzy'`.
#'@param fuzzy_window Sets the fuzzy window size. Only used when
#'`combination = 'fuzzy'`.
#'@return Drifter object
#'@note The `'fuzzy'` combination rebuilds the whole membership matrix on every
#'update, so its cost grows quadratically with the length of the stream. Prefer
#'`'or'` or `'and'` for long streams.
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'set.seed(1)
#'data <- data.frame(serie = c(stats::rnorm(100), stats::rnorm(100, mean = 10)))
#'
#'model <- dfr_multi_criteria(
#'  drifter_list = list(
#'    ph = dfr_page_hinkley(),
#'    ph_sensitive = dfr_page_hinkley(threshold = 10)
#'  ),
#'  combination = 'or'
#')
#'
#'model <- fit(model, data)
#'model$drifted
#'@export
dfr_multi_criteria <- function(drifter_list, combination = "or", fuzzy_window = 10) {
  combination <- match.arg(combination, c('or', 'and', 'fuzzy'))

  if (!is.list(drifter_list) || (length(drifter_list) == 0L)) {
    stop("drifter_list must be a non-empty list of drifters", call. = FALSE)
  }
  if (is.null(names(drifter_list)) || any(names(drifter_list) == "")) {
    names(drifter_list) <- paste0("drifter_", seq_along(drifter_list))
  }
  supported <- vapply(
    drifter_list,
    function(d) any(c('dist_based', 'mv_dist_based') %in% class(d)),
    logical(1)
  )
  if (!all(supported)) {
    stop(
      sprintf(
        "drifter_list elements must inherit from 'dist_based' or 'mv_dist_based'; offending elements: %s",
        paste(names(drifter_list)[!supported], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  .check_positive_integer(fuzzy_window, "fuzzy_window", min_value = 1L)

  obj <- drifter()
  obj$drifter_list <- drifter_list
  obj$combination <- combination
  obj$fuzzy_window <- fuzzy_window
  obj$drifts_fuzzy <- c()
  obj$drifts <- c()

  state <- list()
  state$drifter_list <- drifter_list

  obj$state <- state

  obj$drifted <- FALSE
  class(obj) <- append('multi_criteria', class(obj))
  return(obj)
}

#'@export
update_state.multi_criteria <- function(obj, value, ...) {
  state <- obj$state

  state$row_data <- c()
  for (dft in names(state$drifter_list)) {
    detector <- state$drifter_list[[dft]]

    if ('dist_based' %in% class(detector)) {
      if (is.null(detector$target_feat)) {
        value_mean <- mean(as.numeric(unlist(value)), na.rm = TRUE)
        drifter_output <- update_state(detector, value_mean)
      } else {
        drifter_output <- update_state(detector, value[[detector$target_feat]])
      }
    } else if ('mv_dist_based' %in% class(detector)) {
      drifter_output <- update_state(detector, value)
    } else {
      stop(
        sprintf("multi_criteria: '%s' is neither dist_based nor mv_dist_based", dft),
        call. = FALSE
      )
    }

    state$drifter_list[[dft]] <- drifter_output$obj
    state$row_data <- cbind(state$row_data, drifter_output$drift)
  }
  obj$drifts <- rbind(obj$drifts, state$row_data)

  has_drift <- FALSE
  if (obj$combination == 'or') {
    has_drift <- mean(state$row_data) > 0
  } else if (obj$combination == 'and') {
    has_drift <- mean(state$row_data) == 1
  } else if (obj$combination == 'fuzzy') {
    obj$drifts_fuzzy <- c()
    for (drifter_col in seq_len(ncol(obj$drifts))) {
      drifter_fuzzy <- rep(0, nrow(obj$drifts))
      drifts_index <- which(obj$drifts[, drifter_col] == TRUE)
      for (i in drifts_index) {
        umbrella_vector <- max(1, i - obj$fuzzy_window + 1):min(i + (obj$fuzzy_window - 1), nrow(obj$drifts))
        drifter_fuzzy[min(umbrella_vector):i] <- ((obj$fuzzy_window - length(min(umbrella_vector):i)) + 1):(obj$fuzzy_window) / obj$fuzzy_window
        drifter_fuzzy[i:max(umbrella_vector)] <- obj$fuzzy_window:(obj$fuzzy_window - length(i:max(umbrella_vector)) + 1) / obj$fuzzy_window
      }
      obj$drifts_fuzzy <- cbind(obj$drifts_fuzzy, drifter_fuzzy)
    }
    has_drift <- utils::tail(rowSums(obj$drifts_fuzzy), 1) > (ncol(obj$drifts) / 2)
  }

  obj$state <- state
  if (has_drift) {
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = has_drift))
}

#'@export
fit.multi_criteria <- function(obj, data, ...) {
  return(.fit_row_stream(obj, data))
}

#'@export
reset_state.multi_criteria <- function(obj) {
  obj <- dfr_multi_criteria(
    drifter_list = obj$drifter_list,
    combination = obj$combination,
    fuzzy_window = obj$fuzzy_window
  )
  return(obj)
}
