#'@title ADWIN method
#'@description ADWIN (Adaptive Windowing) is a sequential change detector that maintains a variable-length window and tests whether the means of two subwindows differ significantly. In this package, the implementation is primarily used for **virtual concept drift** when it monitors a numeric feature stream, although the same mechanism can also detect **real concept drift** if applied to an error or loss stream. The theoretical basis follows Bifet and Gavalda (2007) <doi:10.1137/1.9781611972771.42>.
#'@param target_feat Feature to be monitored.
#'@param delta The significance parameter for the ADWIN algorithm.
#'@details The implementation is written in pure R and follows the
#'exponential-histogram version of ADWIN distributed with scikit-multiflow.
#'The window is compressed into buckets of exponentially growing size (at most
#'five buckets per size), and the cut points are checked every 32 observations,
#'once the window holds more than 10 observations. All the state lives in the
#'object itself, so a `dfr_adwin` object can be serialized with
#'[base::saveRDS()].
#ADWIN detection: Bifet, Albert, and Ricard Gavalda. "Learning from time-changing data with adaptive windowing." In Proceedings of the 2007 SIAM international conference on data mining, pp. 443-448. Society for Industrial and Applied Mathematics, 2007.
#ADWIN implementation: Scikit-Multiflow, https://github.com/scikit-multiflow/scikit-multiflow/blob/a7e316d/src/skmultiflow/drift_detection/adwin.py
#'@references Bifet, A., and Gavalda, R. (2007). Learning from time-changing data with adaptive windowing. In *Proceedings of the 2007 SIAM International Conference on Data Mining*, 443-448. <doi:10.1137/1.9781611972771.42>
#'@return `dfr_adwin` object
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'data(st_drift_examples)
#'data <- st_drift_examples$uv_virtual_drift
#'data$event <- NULL
#'
#'model <- dfr_adwin(target_feat='serie')
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
#'print(detection[detection$type == 'drift',])
#'@export
dfr_adwin <- function(target_feat = NULL, delta = 2e-05) {
  .check_probability(delta, "delta")

  obj <- dist_based(target_feat = target_feat)

  obj$drifted <- FALSE
  obj$state <- .adwin_state(delta)
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_adwin", class(obj))
  return(obj)
}

# ADWIN constants, as in the reference implementation.
.adwin_max_buckets <- 5L
.adwin_clock <- 32L
.adwin_min_window_longitude <- 10L
.adwin_min_window_length <- 5L

# Row `r` of `totals`/`variances` holds the buckets of size 2^(r - 1); within a
# row, column 1 is the oldest bucket. The last row therefore holds the oldest
# data of the window.
#' @noRd
.adwin_state <- function(delta) {
  list(
    delta = delta,
    totals = matrix(0, nrow = 1L, ncol = .adwin_max_buckets + 1L),
    variances = matrix(0, nrow = 1L, ncol = .adwin_max_buckets + 1L),
    sizes = 0L,
    total = 0,
    variance = 0,
    width = 0,
    time = 0,
    n_detections = 0
  )
}

#' @noRd
.adwin_insert_bucket <- function(state, row, total, variance) {
  pos <- state$sizes[row] + 1L
  state$totals[row, pos] <- total
  state$variances[row, pos] <- variance
  state$sizes[row] <- pos
  return(state)
}

#' @noRd
.adwin_drop_buckets <- function(state, row, n) {
  cols <- .adwin_max_buckets + 1L
  keep <- (n + 1L):cols
  state$totals[row, ] <- c(state$totals[row, keep], rep(0, n))
  state$variances[row, ] <- c(state$variances[row, keep], rep(0, n))
  state$sizes[row] <- state$sizes[row] - n
  return(state)
}

#' @noRd
.adwin_compress <- function(state) {
  row <- 1L
  while (state$sizes[row] == (.adwin_max_buckets + 1L)) {
    if (row == length(state$sizes)) {
      state$totals <- rbind(state$totals, 0)
      state$variances <- rbind(state$variances, 0)
      state$sizes <- c(state$sizes, 0L)
    }
    n <- 2^(row - 1L)
    u1 <- state$totals[row, 1L] / n
    u2 <- state$totals[row, 2L] / n
    incremental_variance <- n * n * (u1 - u2)^2 / (n + n)
    state <- .adwin_insert_bucket(
      state, row + 1L,
      state$totals[row, 1L] + state$totals[row, 2L],
      state$variances[row, 2L] + incremental_variance
    )
    state <- .adwin_drop_buckets(state, row, 2L)
    if (state$sizes[row + 1L] <= .adwin_max_buckets) {
      break
    }
    row <- row + 1L
  }
  return(state)
}

#' @noRd
.adwin_add_element <- function(state, value) {
  state$width <- state$width + 1
  state <- .adwin_insert_bucket(state, 1L, value, 0)
  if (state$width > 1) {
    previous_mean <- state$total / (state$width - 1)
    state$variance <- state$variance + (state$width - 1) * (value - previous_mean)^2 / state$width
  }
  state$total <- state$total + value
  return(.adwin_compress(state))
}

# Removes the oldest bucket and returns its size alongside the updated state.
#' @noRd
.adwin_delete_element <- function(state) {
  last <- length(state$sizes)
  n1 <- 2^(last - 1L)
  state$width <- state$width - n1
  state$total <- state$total - state$totals[last, 1L]
  u1 <- state$totals[last, 1L] / n1
  incremental_variance <- state$variances[last, 1L] +
    n1 * state$width * (u1 - state$total / state$width)^2 / (n1 + state$width)
  state$variance <- state$variance - incremental_variance
  state <- .adwin_drop_buckets(state, last, 1L)
  if ((state$sizes[last] == 0L) && (last > 1L)) {
    state$totals <- state$totals[-last, , drop = FALSE]
    state$variances <- state$variances[-last, , drop = FALSE]
    state$sizes <- state$sizes[-last]
  }
  return(list(state = state, size = n1))
}

#' @noRd
.adwin_cut <- function(state, n0, n1, abs_value) {
  dd <- log(2 * log(state$width) / state$delta)
  v <- state$variance / state$width
  m <- (1 / (n0 - .adwin_min_window_length + 1)) + (1 / (n1 - .adwin_min_window_length + 1))
  epsilon <- sqrt(2 * m * v * dd) + (2 / 3) * dd * m
  return(isTRUE(abs(abs_value) > epsilon))
}

# Scans the cut points from the oldest to the newest bucket and drops the
# oldest bucket whenever the two subwindows differ significantly.
#' @noRd
.adwin_detect <- function(state) {
  change <- FALSE
  state$time <- state$time + 1
  if (((state$time %% .adwin_clock) != 0) || (state$width <= .adwin_min_window_longitude)) {
    return(list(state = state, change = change))
  }

  reduce_width <- TRUE
  while (reduce_width) {
    reduce_width <- FALSE
    exit <- FALSE
    n0 <- 0
    n1 <- state$width
    u0 <- 0
    u1 <- state$total
    row <- length(state$sizes)

    while (!exit && (row >= 1L)) {
      n2 <- 2^(row - 1L)
      # The newest bucket of each row is never used as a cut point, as in the
      # reference implementation.
      for (k in seq_len(max(state$sizes[row] - 1L, 0L))) {
        u2 <- state$totals[row, k]
        n0 <- n0 + n2
        n1 <- n1 - n2
        u0 <- u0 + u2
        u1 <- u1 - u2

        if ((n1 >= .adwin_min_window_length) && (n0 >= .adwin_min_window_length) &&
            .adwin_cut(state, n0, n1, (u0 / n0) - (u1 / n1))) {
          reduce_width <- TRUE
          change <- TRUE
          if (state$width > 0) {
            deleted <- .adwin_delete_element(state)
            state <- deleted$state
            n0 <- n0 - deleted$size
            exit <- TRUE
            break
          }
        }
      }
      row <- row - 1L
    }
  }

  if (change) {
    state$n_detections <- state$n_detections + 1
  }
  return(list(state = state, change = change))
}

#'@export
update_state.dfr_adwin <- function(obj, value, ...) {
  value <- .as_scalar(value, obj$target_feat)
  if (is.na(value)) {
    return(list(obj = obj, drift = FALSE))
  }

  state <- .adwin_add_element(obj$state, value)
  detection <- .adwin_detect(state)

  obj$state <- detection$state
  obj$last_drifter_output <- detection$state$width
  has_drift <- detection$change
  if (has_drift) {
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = has_drift))
}

#'@export
fit.dfr_adwin <- function(obj, data, ...) {
  return(.fit_vector_stream(obj, data, output_names = "width"))
}

#'@export
reset_state.dfr_adwin <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- .adwin_state(obj$state$delta)
  return(obj)
}
