#' @title Parallel Activations Drift Detector (PADD)
#' @description Unsupervised batch drift detector of Komorniczak and
#' Ksieniewicz (2024). The data stream is processed in chunks of
#' `chunk_size` observations, each passed through a randomly initialised,
#' never trained neural network (one hidden ReLU layer of `hidden` units,
#' `outputs` ReLU outputs and a softmax, weights drawn from N(0, 0.1)). For
#' every output, `replications` Student t-tests compare a sample of
#' `sample_size` activations of the current chunk with a sample of the
#' activations stored since the last drift (both drawn with replacement). A
#' drift is reported when more than `threshold * outputs * replications` tests
#' are significant at level `alpha`; the stored activations are then cleared.
#' Since it monitors the inputs only, it is used for **virtual concept drift**.
#' @param target_feat Names of the columns to be monitored. `NULL` (the default)
#' monitors every numeric column.
#' @param chunk_size Number of observations of a chunk (the batches of the
#' paper, 200 in its experiments).
#' @param alpha Significance level of the t-tests.
#' @param threshold Fraction of significant tests above which a drift is
#' reported.
#' @param outputs Number of outputs of the network (`e`).
#' @param replications Number of t-tests per output (`r`).
#' @param sample_size Size of the samples drawn for every test (`s`).
#' @param hidden Number of units of the hidden layer.
#' @param seed Seed of the network weights and of the samples. `NULL` draws it
#' from the global generator when the detector is created; the global random
#' state is left unchanged.
#' @param weights Optional fixed network: a list with the hidden layer
#' (`(n_features + 1) x hidden`, the last row is the bias) and the output layer
#' (`hidden x outputs`). By default it is drawn on the first chunk.
#' @details The defaults are those of the reference implementation
#' (<https://github.com/w4k2/padd>, `methods.py`). The paper's experiments use
#' `outputs = 12`, `replications = 12`, `sample_size = 50`, and
#' `alpha = 0.07, threshold = 0.19` for sudden drifts or `alpha = 0.13,
#' threshold = 0.26` for gradual ones.
#'
#' As in the reference, the first chunk is only stored; after a drift the
#' stored activations are replaced by those of the drifting chunk; the t-test
#' is the pooled-variance (Student) test, and a test whose two samples are
#' constant (undefined p-value) is not significant. The drift is reported on
#' the last observation of the chunk. Rows with missing values are dropped.
#' `reset_state()` clears the stored activations and keeps the network.
#' @references Komorniczak, J. and Ksieniewicz, P. (2024). Unsupervised concept
#' drift detection based on parallel activations of neural network.
#' arXiv:2404.07776.
#' @return `dfr_padd` object. After `fit()`, `drifter_output` holds, on the last
#' observation of every chunk, the fraction of significant tests
#' (`significant`), and `obj$weights` the network.
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' # 12 chunks of 200 observations; the mean of 3 of the 10 features moves at chunk 7.
#' x <- matrix(stats::rnorm(2400 * 10), ncol = 10)
#' x[1201:2400, 1:3] <- x[1201:2400, 1:3] + 1
#' detector <- dfr_padd(outputs = 12, replications = 12, sample_size = 50,
#'                      alpha = 0.07, threshold = 0.19, seed = 1)
#' model <- fit(detector, as.data.frame(x))
#' which(!is.na(model$drifter_output$significant) & model$drifter_output$significant > 0.19)
#' @export
dfr_padd <- function(target_feat = NULL, chunk_size = 200, alpha = 0.05, threshold = 0.17, outputs = 30,
                     replications = 35, sample_size = 75, hidden = 10, seed = NULL, weights = NULL) {
  .check_target_feat(target_feat, multiple = TRUE)
  .check_positive_integer(chunk_size, "chunk_size")
  .check_probability(alpha, "alpha")
  .check_probability(threshold, "threshold")
  .check_positive_integer(outputs, "outputs")
  .check_positive_integer(replications, "replications")
  .check_positive_integer(sample_size, "sample_size", min_value = 2L)
  .check_positive_integer(hidden, "hidden")
  if (!is.null(weights)) {
    if (!is.list(weights) || length(weights) != 2L || !all(vapply(weights, is.matrix, logical(1))) ||
        nrow(weights[[2]]) != ncol(weights[[1]])) {
      stop("dfr_padd: weights must be a list of two conformable matrices", call. = FALSE)
    }
    hidden <- ncol(weights[[1]])
    outputs <- ncol(weights[[2]])
  }
  obj <- mv_dist_based(target_feat = target_feat)
  obj$settings <- list(chunk_size = chunk_size, alpha = alpha, threshold = threshold, outputs = outputs,
                       replications = replications, sample_size = sample_size, hidden = hidden)
  obj$weights <- weights
  obj$state <- list(features = NULL, buffer = NULL, filled = 0L, past = NULL, rng = .rng_state(seed))
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_padd", class(obj))
  return(obj)
}

# Forward pass: ReLU hidden and output layers, then softmax (methods.py, _predict_proba).
#' @noRd
.padd_activations <- function(weights, x) {
  value <- cbind(x, 1)
  for (layer in weights) {
    value <- value %*% layer
    value <- value * (value > 0)
  }
  value <- exp(value - apply(value, 1, max))
  value / rowSums(value)
}

# Two-sided pooled-variance t-test p-values for the rows of two matrices of samples.
#' @noRd
.padd_ttest <- function(a, b) {
  n <- ncol(a)
  mean_a <- rowMeans(a)
  mean_b <- rowMeans(b)
  pooled <- (rowSums((a - mean_a)^2) + rowSums((b - mean_b)^2)) / (2 * n - 2)
  statistic <- (mean_a - mean_b) / sqrt(pooled * 2 / n)
  2 * stats::pt(-abs(statistic), df = 2 * n - 2)
}

# One chunk: the fraction of significant tests against the stored activations.
# `draw(output, n_past, n_current)` returns the 1-based indices of the samples
# (list(past = r x s, current = r x s)).
#' @noRd
.padd_test <- function(past, current, settings, draw) {
  significant <- 0
  for (output in seq_len(ncol(current))) {
    index <- draw(output, nrow(past), nrow(current))
    a <- matrix(past[index$past, output], nrow = nrow(index$past))
    b <- matrix(current[index$current, output], nrow = nrow(index$current))
    p <- .padd_ttest(a, b)
    significant <- significant + sum(!is.na(p) & p < settings$alpha)
  }
  significant
}

#' @noRd
.padd_chunk <- function(obj, x, draw = NULL) {
  settings <- obj$settings
  state <- obj$state
  if (is.null(obj$weights)) {
    drawn <- .with_rng(state$rng, function() {
      list(matrix(stats::rnorm((ncol(x) + 1) * settings$hidden, 0, 0.1), ncol(x) + 1),
           matrix(stats::rnorm(settings$hidden * settings$outputs, 0, 0.1), settings$hidden))
    })
    obj$weights <- drawn$value
    state$rng <- drawn$rng
  }
  if (nrow(obj$weights[[1]]) != ncol(x) + 1L) {
    stop("dfr_padd: the number of features does not match the network", call. = FALSE)
  }
  current <- .padd_activations(obj$weights, x)
  drift <- FALSE
  fraction <- NA_real_
  if (!is.null(state$past)) {
    total <- ncol(current) * settings$replications
    if (is.null(draw)) {
      counted <- .with_rng(state$rng, function() {
        .padd_test(state$past, current, settings, function(output, n_past, n_current) {
          size <- settings$replications * settings$sample_size
          list(past = matrix(sample.int(n_past, size, replace = TRUE), settings$replications),
               current = matrix(sample.int(n_current, size, replace = TRUE), settings$replications))
        })
      })
      significant <- counted$value
      state$rng <- counted$rng
    } else {
      significant <- .padd_test(state$past, current, settings, draw)
    }
    fraction <- significant / total
    if (significant > settings$threshold * total) {
      drift <- TRUE
      state$past <- NULL
    }
  }
  state$past <- rbind(state$past, current)
  obj$state <- state
  list(obj = obj, drift = drift, fraction = fraction)
}

#' @export
update_state.dfr_padd <- function(obj, value, ...) {
  state <- obj$state
  row <- .select_features(value, obj$target_feat)
  if (is.null(state$features)) {
    state$features <- names(row)
  } else if (!identical(names(row), state$features)) {
    stop("dfr_padd: the monitored features changed during the stream", call. = FALSE)
  }
  if (anyNA(row)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  if (is.null(state$buffer)) {
    state$buffer <- matrix(NA_real_, obj$settings$chunk_size, length(row))
    state$filled <- 0L
  }
  state$filled <- state$filled + 1L
  state$buffer[state$filled, ] <- unname(row)
  if (state$filled < obj$settings$chunk_size) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  chunk <- state$buffer
  state$buffer <- NULL
  obj$state <- state
  result <- .padd_chunk(obj, chunk)
  obj <- result$obj
  obj$last_drifter_output <- c(significant = result$fraction)
  if (result$drift) {
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = result$drift))
}

#' @export
fit.dfr_padd <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data))
}

#' @export
reset_state.dfr_padd <- function(obj) {
  obj$drifted <- FALSE
  obj$state$buffer <- NULL
  obj$state$filled <- 0L
  obj$state$past <- NULL
  return(obj)
}
