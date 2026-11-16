#' @title Quadtree spatial mapping drift detector (QTS)
#' @description Unsupervised, model-independent detector that maps the most
#' recent observations onto a height-limited `d`-dimensional quadtree and
#' monitors its *occupancy*, the number of occupied leaves, as a univariate
#' curve (Ramos et al., 2024). A drift is signalled when the occupancy moves
#' away from the reference of the current concept or when its first
#' derivative changes. Since it monitors the distribution of the observed
#' features, it is primarily used for **virtual concept drift**.
#' @param target_feat Names of the columns to be monitored. `NULL` (the default)
#' monitors every numeric column.
#' @param height Height of the quadtree: the number of times the feature space
#' is halved along every dimension.
#' @param window_size Size of the sliding window `W` of observations mapped
#' onto the quadtree.
#' @param reference_size Size of the occupancy windows: the reference window
#' `Rw`, the summarized sliding window `Sw` and the derivative window `f'(Sw)`.
#' @param recent Share of `window_size` that forms the recent part of the
#' derivative window (`RD`); the rest is the old part (`OD`).
#' @param alpha Multiplier of the standard deviation of the old derivatives in
#' the derivative strategy.
#' @param beta Multiplier of the standard deviation of the reference occupancy
#' in the occupancy strategy.
#' @param lower,upper Bounds used to min-max scale the features to `[0, 1]`
#' before they are mapped, one value for every feature or one per monitored
#' feature. The quadtree covers the unit hypercube; values outside it fall in
#' the outermost cells. The defaults assume data already scaled to `[0, 1]`.
#' @param derivative_rule Drift condition of the derivative strategy.
#' `"code"` (the default) follows the reference implementation,
#' `|mean(RD)| > mean(OD) + alpha * sd(OD)`; `"paper"` follows the paper and
#' its pseudocode, `|mean(RD) - mean(OD)| > alpha * sd(OD)`.
#' @details At each observation the window `W` is mapped onto a quadtree of
#' height `height` whose cells are split while they hold more than one
#' observation; the observations of a cell at the maximum height are
#' summarized by their mean. The occupancy is the number of leaves, so it grows
#' when the data spread over the feature space and shrinks when they
#' concentrate.
#'
#' Once the window is full, the first `reference_size` occupancy values form
#' the reference window `Rw` of the concept. Two strategies then run in
#' parallel:
#'
#' * **occupancy**: a drift when the exponential moving average (span
#'   `reference_size`) of the last `reference_size` occupancy values leaves
#'   `mean(Rw) +/- beta * sd(Rw)`, a Chebyshev-like bound. The reference is
#'   replaced by the last `reference_size` values `reference_size`
#'   observations later.
#' * **derivative**: the derivative is the sign of the change of the occupancy
#'   (-1, 0 or 1). The last `reference_size` derivatives are split into old
#'   data `OD` and the last `recent * window_size` values `RD`, and a drift is
#'   signalled by `derivative_rule`. The reference is replaced
#'   `window_size + reference_size` observations later, when both windows hold
#'   the new concept. While it waits, the occupancy strategy is paused.
#'
#' When the window is small compared with the number of cells (`2^(height d)`),
#' nearly every observation has a leaf of its own, the occupancy rarely
#' changes and the standard deviation of the derivatives is small, so the
#' derivative strategy reacts to noise. Keep the default windows or lower the
#' height in that case.
#'
#' The occupancy is computed from the cell counts of every level, without
#' building the tree, and gives the same number of leaves as the reference
#' implementation. The implementation follows the reference code
#' (<https://github.com/beraram/QTS-method>), whose default parameters are used
#' here; the paper recommends a window of 1000 observations for most cases.
#' Rows whose monitored values are all missing are skipped; missing values in
#' the other rows are placed in the lowest cell.
#' @references Ramos, B. A., Castro, C. L., Coelho, T. A. and Angelov, P. P.
#' (2024). Unsupervised drift detection using quadtree spatial mapping.
#' *ESANN 2024 proceedings*, 467-472.
#'
#' Coelho, R., Torres, L. and Castro, C. (2023). Concept drift detection with
#' quadtree-based spatial mapping of streaming data. *Information Sciences*,
#' 625, 578-592.
#' @return `dfr_qts` object. After `fit()`, `obj$detections` holds the
#' observation and the strategy (`"occupancy"` or `"derivative"`) of every
#' drift, and `drifter_output` the occupancy, its moving average and the
#' derivative statistics of every observation.
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' # Two clusters in the unit square that spread over it after 3000 rows.
#' set.seed(1)
#' n <- 3000
#' before <- data.frame(x = stats::rnorm(n, 0.3, 0.05), y = stats::rnorm(n, 0.3, 0.05))
#' after <- data.frame(x = stats::runif(n), y = stats::runif(n))
#' model <- fit(dfr_qts(window_size = 200, reference_size = 600), rbind(before, after))
#' model$detections
#' plot(model$drifter_output$occupancy, type = "l", ylab = "Occupancy")
#' abline(v = model$detections$observation, lty = 2)
#' @export
dfr_qts <- function(target_feat = NULL, height = 4, window_size = 500, reference_size = 1500, recent = 0.1,
                    alpha = 1, beta = 3, lower = 0, upper = 1, derivative_rule = c("code", "paper")) {
  .check_target_feat(target_feat, multiple = TRUE)
  .check_positive_integer(height, "height", min_value = 1L)
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(reference_size, "reference_size", min_value = 2L)
  .check_number(alpha, "alpha", min_value = 0)
  .check_number(beta, "beta", min_value = 0)
  if (!is.numeric(recent) || (length(recent) != 1L) || is.na(recent) || (recent <= 0) || (recent >= 1)) {
    stop("recent must be a single value between 0 and 1", call. = FALSE)
  }
  recent_size <- floor(recent * window_size)
  if ((recent_size < 2L) || (recent_size >= reference_size)) {
    stop("recent * window_size must be at least 2 and smaller than reference_size", call. = FALSE)
  }
  if (!is.numeric(lower) || !is.numeric(upper) || (length(lower) == 0L) || (length(upper) == 0L) ||
      anyNA(lower) || anyNA(upper) || any(!is.finite(c(lower, upper)))) {
    stop("lower and upper must be finite numeric values", call. = FALSE)
  }
  derivative_rule <- match.arg(derivative_rule)

  obj <- mv_dist_based(target_feat = target_feat)
  obj$state <- .qts_start(height, window_size, reference_size, recent, alpha, beta, lower, upper, derivative_rule)
  obj$detections <- .qts_no_detections()
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_qts", class(obj))
  return(obj)
}

#' @noRd
.qts_start <- function(height, window_size, reference_size, recent, alpha, beta, lower, upper, derivative_rule) {
  list(
    height = height, window_size = window_size, reference_size = reference_size, recent = recent,
    recent_size = floor(recent * window_size), alpha = alpha, beta = beta, lower = lower, upper = upper,
    derivative_rule = derivative_rule,
    features = NULL,
    n = 0,                       # observations mapped so far
    keys = NULL,                 # cell of every observation of W at every level (ring buffer)
    position = 0L,
    occupancy = numeric(0),      # last reference_size occupancy values (Sw)
    derivative = numeric(0),     # last reference_size derivatives (f'(Sw))
    previous = NA_real_,
    initial = TRUE,
    reference_mean = 0, reference_sd = 0,
    window_mean = 0, window_sd = 0, window_ewm = 0, smoothed = 0,
    derivative_recent = 0, derivative_threshold = 0,
    derivative_flag = FALSE, derivative_count = 0,
    occupancy_flag = FALSE, occupancy_count = 0
  )
}

#' @noRd
.qts_no_detections <- function() {
  data.frame(observation = numeric(0), strategy = character(0))
}

# Cell of an observation (already scaled) at every level of the quadtree. A
# value goes to the upper half of a cell only when it is above the middle, as
# in the reference code, so the index at level l is ceiling(x 2^l) - 1.
#' @noRd
.qts_cells <- function(x, height) {
  x[is.na(x)] <- 0
  vapply(seq_len(height), function(level) {
    cells <- 2^level
    index <- pmin(pmax(ceiling(x * cells) - 1, 0), cells - 1)
    paste(index, collapse = " ")
  }, character(1))
}

# Number of leaves of the quadtree of the observations whose cells are the
# rows of `keys`. A cell is a leaf when it holds one observation and its parent
# more than one, or when it is at the maximum height and its parent holds more
# than one observation (its observations are summarized by their mean).
#' @noRd
.qts_occupancy <- function(keys) {
  n <- nrow(keys)
  if (n <= 1L) {
    return(n)
  }
  height <- ncol(keys)
  parent <- rep(n, n)
  leaves <- 0
  for (level in seq_len(height)) {
    cell <- match(keys[, level], keys[, level])
    count <- tabulate(cell, nbins = n)[cell]
    split <- parent >= 2
    if (level < height) {
      leaves <- leaves + sum(split & (count == 1))
    } else {
      leaves <- leaves + length(unique(cell[split]))
    }
    parent <- count
  }
  return(leaves)
}

#' @noRd
.qts_population_sd <- function(x) {
  sqrt(mean((x - mean(x))^2))
}

# Last value of pandas' ewm(span = length(x)).mean() (adjust = TRUE).
#' @noRd
.qts_ewm <- function(x) {
  weights <- (1 - 2 / (length(x) + 1))^(rev(seq_along(x)) - 1)
  sum(weights * x) / sum(weights)
}

#' @noRd
.qts_scale <- function(row, lower, upper) {
  if (((length(lower) != 1L) && (length(lower) != length(row))) ||
      ((length(upper) != 1L) && (length(upper) != length(row)))) {
    stop("lower and upper must have one value or one value per monitored feature", call. = FALSE)
  }
  span <- upper - lower
  if (any(span <= 0)) {
    stop("upper must be greater than lower", call. = FALSE)
  }
  (row - lower) / span
}

#' @export
update_state.dfr_qts <- function(obj, value, ...) {
  state <- obj$state
  row <- .select_features(value, obj$target_feat)
  if (is.null(state$features)) {
    if (length(row) == 0L) {
      stop("dfr_qts: there are no numeric features to monitor", call. = FALSE)
    }
    state$features <- names(row)
  } else if (!identical(names(row), state$features)) {
    stop("dfr_qts: the monitored features changed during the stream", call. = FALSE)
  }
  if (all(is.na(row))) {
    obj$last_drifter_output <- NULL
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  # Map the window W onto the quadtree.
  cells <- .qts_cells(.qts_scale(unname(row), state$lower, state$upper), state$height)
  if (is.null(state$keys)) {
    state$keys <- matrix(NA_character_, state$window_size, state$height)
  }
  state$position <- (state$position %% state$window_size) + 1L
  state$keys[state$position, ] <- cells
  state$n <- state$n + 1
  filled <- min(state$n, state$window_size)
  occupancy <- .qts_occupancy(state$keys[seq_len(filled), , drop = FALSE])

  size <- state$reference_size
  derivative <- if (is.na(state$previous)) 0 else sign(occupancy - state$previous)
  state$previous <- occupancy
  state$occupancy <- utils::tail(c(state$occupancy, occupancy), size)
  state$derivative <- utils::tail(c(state$derivative, derivative), size + 1L)

  # Reference window Rw: the first reference_size occupancy values with a
  # full window W.
  full <- state$n >= state$window_size + size - 1
  if (full && state$initial) {
    state$reference_mean <- mean(state$occupancy)
    state$reference_sd <- .qts_population_sd(state$occupancy)
    state$initial <- FALSE
  }
  if (full) {
    state$window_ewm <- .qts_ewm(state$occupancy)
    state$window_mean <- mean(state$occupancy)
    state$window_sd <- .qts_population_sd(state$occupancy)
  }

  # Derivative window f'(Sw): old data OD and recent data RD. As in the
  # reference, OD ends with the first value of RD.
  if (length(state$derivative) > size) {
    derivatives <- utils::tail(state$derivative, size)
    recent_size <- state$recent_size
    old <- derivatives[seq_len(size - recent_size + 1L)]
    state$derivative_recent <- mean(utils::tail(derivatives, recent_size))
    if (state$derivative_rule == "code") {
      state$derivative_threshold <- mean(old) + state$alpha * .qts_population_sd(old)
      derivative_statistic <- abs(state$derivative_recent)
    } else {
      state$derivative_threshold <- state$alpha * .qts_population_sd(old)
      derivative_statistic <- abs(state$derivative_recent - mean(old))
    }
  } else {
    derivative_statistic <- 0
  }

  strategy <- NULL
  if (!state$derivative_flag && (derivative_statistic > state$derivative_threshold)) {
    strategy <- "derivative"
    state$derivative_flag <- TRUE
  }
  # Wait for the new concept to fill W and Sw, then take it as reference.
  if (state$derivative_flag) {
    if (state$derivative_count < state$window_size + size) {
      state$derivative_count <- state$derivative_count + 1
    } else {
      state$derivative_count <- 0
      state$derivative_flag <- FALSE
      state$reference_mean <- state$window_mean
      state$reference_sd <- state$window_sd
    }
  }

  if (!state$initial) {
    state$smoothed <- state$window_ewm
  }
  bound <- state$beta * state$reference_sd
  outside <- (state$smoothed > state$reference_mean + bound) || (state$smoothed < state$reference_mean - bound)
  if (outside && !state$derivative_flag && !state$occupancy_flag) {
    strategy <- "occupancy"
    state$occupancy_flag <- TRUE
  }
  if (state$occupancy_flag) {
    if (state$occupancy_count < size) {
      state$occupancy_count <- state$occupancy_count + 1
    } else {
      state$occupancy_count <- 0
      state$occupancy_flag <- FALSE
      state$reference_mean <- state$window_mean
      state$reference_sd <- state$window_sd
    }
  }

  obj$last_drifter_output <- c(
    occupancy = occupancy,
    occupancy_ewm = if (full) state$window_ewm else NA_real_,
    reference_mean = if (state$initial) NA_real_ else state$reference_mean,
    reference_sd = if (state$initial) NA_real_ else state$reference_sd,
    derivative_recent = state$derivative_recent,
    derivative_threshold = state$derivative_threshold
  )
  obj$state <- state
  if (is.null(strategy)) {
    return(list(obj = obj, drift = FALSE))
  }
  obj$detections <- rbind(obj$detections, data.frame(observation = state$n, strategy = strategy))
  obj$drifted <- TRUE
  return(list(obj = obj, drift = TRUE))
}

#' @export
fit.dfr_qts <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data))
}

#' @export
reset_state.dfr_qts <- function(obj) {
  state <- obj$state
  obj$drifted <- FALSE
  obj$state <- .qts_start(state$height, state$window_size, state$reference_size, state$recent, state$alpha,
                          state$beta, state$lower, state$upper, state$derivative_rule)
  obj$detections <- .qts_no_detections()
  return(obj)
}
