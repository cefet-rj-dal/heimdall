#' @title Average cosine similarity to a baseline of embeddings
#' @description Scores every new observation (an embedding vector, such as the
#' sentence embedding of a text) by its average cosine similarity with the
#' embeddings of a baseline, the data drift check of the custom Amazon SageMaker
#' Model Monitor for NLP models of Nigenda et al. (2022):
#' \deqn{s(x) = \frac{1}{N}\sum_{i=1}^{N} \frac{x \cdot e_i}{\|x\| \|e_i\|}.}
#' Inputs far from the baseline distribution get low scores.
#' @param reference The baseline embeddings: a numeric matrix or data frame, one
#' row per embedding.
#' @param x The embeddings to score, with the same columns (or a single
#' embedding as a numeric vector).
#' @details The average of the cosine similarities equals the inner product of
#' the normalised input with the mean of the normalised baseline embeddings, so
#' scoring costs one inner product per input instead of one per baseline
#' embedding (the reference code loops over the baseline). Baseline embeddings
#' with zero norm are left out, and an input with zero norm gets `NA` (the
#' cosine is undefined; the reference code gets `NaN`).
#' @references Nigenda, D., Karnin, Z., Zafar, M. B., Ramesha, R., Tan, A.,
#' Donini, M. and Kenthapadi, K. (2022). Amazon SageMaker Model Monitor: A system
#' for real-time insights into deployed machine learning models. In
#' *Proceedings of the 28th ACM SIGKDD Conference on Knowledge Discovery and
#' Data Mining*, 3671-3681. <doi:10.1145/3534678.3539145>
#' @return A numeric vector with the score of every row of `x`.
#' @seealso [dfr_cosine()] to monitor a stream.
#' @examples
#' set.seed(1)
#' baseline <- matrix(stats::rnorm(2000, mean = 1), ncol = 20)
#' near <- matrix(stats::rnorm(100, mean = 1), ncol = 20)
#' far <- matrix(stats::rnorm(100, mean = -1), ncol = 20)
#' cosine_score(baseline, near)
#' cosine_score(baseline, far)
#' @export
cosine_score <- function(reference, x) {
  direction <- .cosine_direction(.cosine_matrix(reference, "reference"))
  x <- .cosine_matrix(x, "x", vector_as_row = TRUE)
  if (ncol(x) != length(direction)) {
    stop("cosine_score: reference and x must have the same number of columns", call. = FALSE)
  }
  .cosine_apply(direction, x)
}

# Mean of the normalised baseline embeddings.
#' @noRd
.cosine_direction <- function(reference) {
  norms <- sqrt(rowSums(reference^2))
  keep <- norms > 0
  if (!any(keep)) {
    stop("cosine_score: the reference has no embedding with a positive norm", call. = FALSE)
  }
  colMeans(reference[keep, , drop = FALSE] / norms[keep])
}

#' @noRd
.cosine_apply <- function(direction, x) {
  norms <- sqrt(rowSums(x^2))
  score <- as.numeric(x %*% direction) / norms
  score[norms == 0] <- NA_real_
  score
}

#' @noRd
.cosine_matrix <- function(x, name, vector_as_row = FALSE) {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  if (is.null(dim(x))) {
    x <- if (vector_as_row) matrix(x, nrow = 1L) else matrix(x, ncol = 1L)
  }
  if (!is.numeric(x)) {
    stop(sprintf("cosine_score: %s must be numeric", name), call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x))) {
    stop(sprintf("cosine_score: %s has missing or infinite values", name), call. = FALSE)
  }
  unname(x)
}

# Stream detector ---------------------------------------------------------------------------------

#' @title Cosine similarity drift detector for embeddings
#' @description Monitors a stream of embeddings (for instance BERT sentence
#' embeddings of the texts sent to an NLP model) with the custom Model Monitor
#' of Nigenda et al. (2022): every observation is scored by its average cosine
#' similarity with the baseline embeddings ([cosine_score()]), the scores are
#' averaged over monitoring windows of `window_size` observations, and a drift
#' is reported when the average falls below `threshold`. Since it monitors the
#' distribution of the inputs, it is used for **virtual concept drift**.
#' @param target_feat Names of the columns holding the embedding. `NULL` (the
#' default) uses every numeric column.
#' @param reference Optional baseline embeddings (a matrix or data frame with
#' the monitored columns), such as the embeddings of the training texts. When
#' `NULL`, the first `n_reference` observations of the stream form the
#' baseline.
#' @param n_reference Size of the baseline taken from the stream when
#' `reference` is `NULL`.
#' @param window_size Number of observations whose scores are averaged before
#' the comparison with `threshold` (the monitoring window, an hour of traffic
#' in the paper). With `1`, every observation is checked on its own, as the
#' reference code does.
#' @param threshold A window whose average score is below it is a drift (0.5 in
#' the reference code).
#' @details The windows do not overlap; the drift is reported on the last
#' observation of the window. Observations with missing values or zero norm are
#' skipped. `reset_state()` keeps a baseline given in `reference` and empties
#' the window; a baseline taken from the stream is forgotten, and the next
#' `n_reference` observations form a new one.
#' @inherit cosine_score references
#' @return `dfr_cosine` object. After `fit()`, `drifter_output` holds the score
#' of every observation (`score`) and the average of the window on its last
#' observation (`window_score`).
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' baseline <- matrix(stats::rnorm(2000, mean = 2), ncol = 20)
#' # From row 21 half of the coordinates change sign.
#' flipped <- matrix(stats::rnorm(400, mean = 2), ncol = 20) %*% diag(rep(c(1, -1), each = 10))
#' stream <- as.data.frame(rbind(matrix(stats::rnorm(400, mean = 2), ncol = 20), flipped))
#' detector <- dfr_cosine(reference = baseline, window_size = 10, threshold = 0.5)
#' model <- fit(detector, stream)
#' model$drifter_output[!is.na(model$drifter_output$window_score), ]
#' @export
dfr_cosine <- function(target_feat = NULL, reference = NULL, n_reference = 1000, window_size = 1, threshold = 0.5) {
  .check_target_feat(target_feat, multiple = TRUE)
  .check_positive_integer(n_reference, "n_reference")
  .check_positive_integer(window_size, "window_size")
  .check_number(threshold, "threshold", min_value = -1)
  obj <- mv_dist_based(target_feat = target_feat)
  direction <- NULL
  if (!is.null(reference)) {
    if (!is.null(target_feat) && is.data.frame(reference) && all(target_feat %in% names(reference))) {
      reference <- reference[target_feat]
    }
    direction <- .cosine_direction(.cosine_matrix(reference, "reference"))
  }
  obj$settings <- list(n_reference = n_reference, window_size = window_size, threshold = threshold,
                       fixed_reference = !is.null(direction))
  obj$direction <- direction
  obj$state <- list(features = NULL, buffer = NULL, filled = 0L, scores = numeric(0))
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_cosine", class(obj))
  return(obj)
}

#' @export
update_state.dfr_cosine <- function(obj, value, ...) {
  state <- obj$state
  settings <- obj$settings
  row <- .select_features(value, obj$target_feat)
  if (is.null(state$features)) {
    state$features <- names(row)
  } else if (!identical(names(row), state$features)) {
    stop("dfr_cosine: the monitored features changed during the stream", call. = FALSE)
  }
  if (anyNA(row)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  if (is.null(obj$direction)) {
    if (is.null(state$buffer)) {
      state$buffer <- matrix(NA_real_, settings$n_reference, length(row))
      state$filled <- 0L
    }
    state$filled <- state$filled + 1L
    state$buffer[state$filled, ] <- unname(row)
    if (state$filled == settings$n_reference) {
      obj$direction <- .cosine_direction(state$buffer)
      state$buffer <- NULL
    }
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  if (length(row) != length(obj$direction)) {
    stop("dfr_cosine: the embedding and the reference have different dimensions", call. = FALSE)
  }
  score <- .cosine_apply(obj$direction, matrix(unname(row), nrow = 1L))
  if (is.na(score)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  state$scores <- c(state$scores, score)
  output <- c(score = score, window_score = NA_real_)
  drift <- FALSE
  if (length(state$scores) == settings$window_size) {
    output[["window_score"]] <- mean(state$scores)
    state$scores <- numeric(0)
    drift <- output[["window_score"]] < settings$threshold
  }
  obj$state <- state
  obj$last_drifter_output <- output
  if (drift) {
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = drift))
}

#' @export
fit.dfr_cosine <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data))
}

#' @export
reset_state.dfr_cosine <- function(obj) {
  obj$drifted <- FALSE
  obj$state$buffer <- NULL
  obj$state$filled <- 0L
  obj$state$scores <- numeric(0)
  if (!obj$settings$fixed_reference) {
    obj$direction <- NULL
  }
  return(obj)
}
