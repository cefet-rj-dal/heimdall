#' @title Domino drift effect (DDE) detector
#' @description Feature drift detector that monitors only a subset of the
#' features and estimates the probability that each of the other features
#' drifts, from the correlation between the features (Szucs and Nemeth, 2025).
#' The method relies on the *domino drift effect*: when features drift
#' together, the neighbours (strongly correlated features) of a drifting
#' feature are more likely to drift as well.
#' @param target_feat Names of the features. `NULL` (the default) uses every
#' numeric column.
#' @param monitored Names of the monitored features, \eqn{S_{check}}.
#' @param n_monitored Number of monitored features, drawn at random when
#' `monitored` is `NULL`. When both are `NULL` every feature is monitored and
#' no score is predicted.
#' @param test Univariate drift test applied to each monitored feature: one of
#' `"js"` (Jensen-Shannon distance), `"ks"` (Kolmogorov-Smirnov),
#' `"wasserstein"` (normalized Wasserstein distance) or `"adwin"`. See
#' [dde_feature_drift()].
#' @param threshold Drift threshold of the test. `NULL` uses the default of
#' [dde_feature_drift()].
#' @param window_size Size of the reference and of the analysis windows
#' (\eqn{r = q}).
#' @param monitoring_step Number of observations between two consecutive tests
#' of the window-based tests.
#' @param confirmation_delay Number of observations between the first drift
#' signal and the decision on which features drift. `NULL` (the default) uses
#' half a window. See Details.
#' @param thresholds Correlation thresholds \eqn{z} at which the DDE curve is
#' evaluated.
#' @param score_threshold Score from which an unmonitored feature is predicted
#' to drift.
#' @param update_curve Whether to refine the DDE curve with the drifts
#' detected after the initialization, by averaging it with the curves of the
#' monitored features (Algorithm 1, line 29).
#' @param delta Confidence of ADWIN, used when `test = "adwin"`.
#' @param seed Seed of the random draw of the monitored features.
#' @details
#' The detector compares, for each feature, a reference window with the
#' following analysis window, both of `window_size` observations (Algorithm 1).
#' It starts in an initialization phase in which every feature is monitored.
#' The first drift with at least two drifting features fixes the DDE curve,
#' computed with [dde_curve()] from the Pearson correlations in the reference
#' window. From then on only the monitored features are tested. When some of
#' them drift, every unmonitored feature gets a drift score with
#' [dde_score()], the mean of the DDE curve over its correlations with the
#' drifting monitored features (Eq. 25).
#'
#' A drift is reported when at least one monitored feature drifts. The
#' features do not cross their thresholds at the same observation, so the
#' first signal opens a confirmation period of `confirmation_delay`
#' observations, after which the windows are tested again: the drifting
#' features are those of this confirmation test (with ADWIN, every feature
#' signalled during the period), and no drift is reported if none remains.
#' The correlations are measured in the reference window of the first signal. After a drift the
#' analysis window becomes the new reference window.
#'
#' The fitted object exposes:
#' \itemize{
#' \item `curve`: the current DDE curve (see [dde_curve()]);
#' \item `feature_drift`: per feature, the last decision for the monitored
#' features (0 or 1) and the predicted score for the others;
#' \item `predicted_drift`: per feature, whether it is detected or predicted
#' to drift in the last drift;
#' \item `detections`: one entry per reported drift, with the observation,
#' the drifting monitored features and the predicted scores.
#' }
#' The paper's code (<https://github.com/marcellnemeth/DominoDriftEffect>)
#' is not public, so the implementation follows the paper. The window tests
#' use the definitions and default thresholds of the Evidently library.
#' @references Szucs, G. and Nemeth, M. (2025). Domino drift effect approach
#' for probability estimation of feature drift in high-dimensional data.
#' *Knowledge and Information Systems*, 67, 4597-4621.
#' <doi:10.1007/s10115-025-02362-0>
#' @return `dfr_dde` object
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#'
#' # Six features in two correlated groups; the first group drifts.
#' set.seed(1)
#' n <- 600
#' make <- function(n, shift) {
#'   a <- stats::rnorm(n, shift)
#'   b <- stats::rnorm(n)
#'   data.frame(a1 = a + stats::rnorm(n, sd = 0.3), a2 = a + stats::rnorm(n, sd = 0.3),
#'              a3 = a + stats::rnorm(n, sd = 0.3), b1 = b + stats::rnorm(n, sd = 0.3),
#'              b2 = b + stats::rnorm(n, sd = 0.3), b3 = b + stats::rnorm(n, sd = 0.3))
#' }
#' stream <- rbind(make(n, 0), make(n, 3), make(n, 3), make(n, 6))
#'
#' model <- dfr_dde(monitored = c("a1", "b1"), test = "ks", threshold = 1e-3,
#'                  window_size = 300, monitoring_step = 50)
#' model <- fit(model, stream)
#' model$curve
#' model$feature_drift
#' @export
dfr_dde <- function(target_feat = NULL, monitored = NULL, n_monitored = NULL, test = "js", threshold = NULL,
                    window_size = 500, monitoring_step = 1, confirmation_delay = NULL,
                    thresholds = seq(0, 0.9, by = 0.1), score_threshold = 0.5, update_curve = TRUE,
                    delta = 2e-05, seed = NULL) {
  test <- match.arg(test, c("js", "ks", "wasserstein", "adwin"))
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(monitoring_step, "monitoring_step", min_value = 1L)
  if (is.null(confirmation_delay)) confirmation_delay <- window_size %/% 2L
  .check_positive_integer(confirmation_delay, "confirmation_delay", min_value = 0L)
  .check_probability(score_threshold, "score_threshold")
  .check_probability(delta, "delta")
  .dde_check_thresholds(thresholds)
  if (!is.null(threshold)) .check_number(threshold, "threshold", min_value = 0)
  if (!is.null(monitored) && (!is.character(monitored) || (length(monitored) == 0L) || anyNA(monitored))) {
    stop("monitored must be NULL or a vector of feature names", call. = FALSE)
  }
  if (!is.null(n_monitored)) .check_positive_integer(n_monitored, "n_monitored", min_value = 1L)
  if (!is.logical(update_curve) || (length(update_curve) != 1L) || is.na(update_curve)) {
    stop("update_curve must be TRUE or FALSE", call. = FALSE)
  }

  obj <- mv_dist_based(target_feat = target_feat)
  obj$curve <- NULL
  obj$feature_drift <- NULL
  obj$predicted_drift <- NULL
  obj$detections <- list()

  state <- list()
  state$params <- list(
    monitored = monitored, n_monitored = n_monitored, test = test, threshold = threshold,
    window_size = window_size, monitoring_step = monitoring_step,
    confirmation_delay = as.integer(confirmation_delay), thresholds = thresholds,
    score_threshold = score_threshold, update_curve = update_curve, delta = delta
  )
  state$rng <- .rng_state(seed)
  state$features <- NULL
  state$monitored <- NULL
  state$buffer <- NULL
  state$pending <- NULL
  state$curves <- list()
  state$adwin <- list()
  state$n <- 0L
  obj$state <- state

  class(obj) <- append("dfr_dde", class(obj))
  return(obj)
}

#' @export
update_state.dfr_dde <- function(obj, value, ...) {
  state <- obj$state
  params <- state$params
  row <- .select_features(value, obj$target_feat)
  obj$last_drifter_output <- cbind(drifting = NA_real_)

  if (is.null(state$features)) {
    state <- .dde_start(state, names(row))
  } else if (!identical(names(row), state$features)) {
    stop("update_state: the features changed during the stream", call. = FALSE)
  }
  state$n <- state$n + 1L
  window <- params$window_size
  state$buffer <- .trim_window(rbind(state$buffer, row), 2L * window, "sliding")

  initializing <- length(state$curves) == 0L
  checked <- if (initializing) state$features else state$monitored
  pending <- !is.null(state$pending)
  full <- nrow(state$buffer) == 2L * window

  drifting <- NULL
  if (params$test == "adwin") {
    signals <- stats::setNames(logical(length(checked)), checked)
    for (feature in checked) {
      if (is.na(row[[feature]])) next
      output <- update_state(state$adwin[[feature]], row[[feature]])
      state$adwin[[feature]] <- output$obj
      signals[[feature]] <- output$drift
    }
    drifting <- checked[signals]
    obj$last_drifter_output <- cbind(drifting = length(drifting))
  } else if (full && (((state$n %% params$monitoring_step) == 0L) ||
                      (pending && (state$n - state$pending$at >= params$confirmation_delay)))) {
    tested <- dde_feature_drift(state$buffer[seq_len(window), checked, drop = FALSE],
                                state$buffer[window + seq_len(window), checked, drop = FALSE],
                                test = params$test, threshold = params$threshold)
    drifting <- tested$feature[tested$drift]
    obj$last_drifter_output <- cbind(drifting = length(drifting))
  }

  # A drift is confirmed confirmation_delay observations after the first
  # signal, when the change is well inside the analysis window. The drifting
  # features are those of the confirmation test (window tests) or every
  # feature signalled in between (ADWIN, whose signals are one-off).
  if (!pending) {
    if (length(drifting) == 0L) {
      obj$state <- state
      return(list(obj = obj, drift = FALSE))
    }
    state$pending <- list(at = state$n, drifting = drifting,
                          reference = state$buffer[seq_len(max(2L, nrow(state$buffer) - window)), , drop = FALSE])
  } else if (params$test == "adwin") {
    state$pending$drifting <- union(state$pending$drifting, drifting)
  }
  if ((state$n - state$pending$at) < params$confirmation_delay) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  if (params$test != "adwin") {
    state$pending$drifting <- drifting
  }
  drifting <- checked[checked %in% state$pending$drifting]
  reference <- state$pending$reference
  state$pending <- NULL
  if (length(drifting) == 0L) {
    # The change faded before it was confirmed.
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  # Curve fitting: the first drift with two drifting features fixes the
  # curve; later drifts refine it with the pairs of monitored features.
  if (initializing) {
    if (length(drifting) >= 2L) {
      state$curves <- list(dde_curve(reference, drifting, thresholds = params$thresholds))
    }
  } else if (params$update_curve && (length(drifting) >= 2L) && (length(state$monitored) > 2L)) {
    curve <- dde_curve(reference[, state$monitored, drop = FALSE], drifting, thresholds = params$thresholds)
    state$curves[[length(state$curves) + 1L]] <- curve
  }
  obj$curve <- .dde_average_curves(state$curves)

  # Drift score prediction for the unmonitored features (Eq. 25).
  decisions <- stats::setNames(as.numeric(checked %in% drifting), checked)
  scores <- NULL
  if (!initializing) {
    unmonitored <- setdiff(state$features, state$monitored)
    if (length(unmonitored) > 0L) {
      scores <- dde_score(obj$curve, reference, drifting, unmonitored)
    }
  }
  order <- state$features[state$features %in% c(checked, names(scores))]
  obj$feature_drift <- c(decisions, scores)[order]
  obj$predicted_drift <- c(decisions == 1, scores >= params$score_threshold)[order]
  obj$detections[[length(obj$detections) + 1L]] <- list(
    observation = state$n, drifting = drifting, scores = scores, initialization = initializing
  )

  # The analysis window becomes the new reference window.
  state$buffer <- utils::tail(state$buffer, window)
  obj$drifted <- TRUE
  obj$state <- state
  return(list(obj = obj, drift = TRUE))
}

#' @export
fit.dfr_dde <- function(obj, data, ...) {
  return(.fit_row_stream(obj, data, output_names = "drifting"))
}

#' @export
reset_state.dfr_dde <- function(obj) {
  params <- obj$state$params
  rng <- obj$state$rng
  reset <- dfr_dde(
    target_feat = obj$target_feat, monitored = params$monitored, n_monitored = params$n_monitored,
    test = params$test, threshold = params$threshold, window_size = params$window_size,
    monitoring_step = params$monitoring_step, confirmation_delay = params$confirmation_delay,
    thresholds = params$thresholds,
    score_threshold = params$score_threshold, update_curve = params$update_curve, delta = params$delta,
    seed = 0L
  )
  reset$state$rng <- rng
  return(reset)
}

#' Resolve the features and the monitored subset on the first observation
#' @noRd
.dde_start <- function(state, features) {
  params <- state$params
  if (length(features) < 2L) {
    stop("dfr_dde needs at least two features", call. = FALSE)
  }
  state$features <- features
  if (!is.null(params$monitored)) {
    unknown <- setdiff(params$monitored, features)
    if (length(unknown) > 0L) {
      stop(sprintf("monitored features are not in the data: %s", paste(unknown, collapse = ", ")), call. = FALSE)
    }
    state$monitored <- features[features %in% params$monitored]
  } else if (!is.null(params$n_monitored)) {
    if (params$n_monitored > length(features)) {
      stop("n_monitored is larger than the number of features", call. = FALSE)
    }
    sampled <- .sample_local(state$rng, features, params$n_monitored)
    state$rng <- sampled$rng
    state$monitored <- features[features %in% sampled$sample]
  } else {
    state$monitored <- features
  }
  if (params$test == "adwin") {
    state$adwin <- stats::setNames(lapply(features, function(f) dfr_adwin(delta = params$delta)), features)
  }
  return(state)
}

#' Average the probability of several DDE curves, threshold by threshold
#' @noRd
.dde_average_curves <- function(curves) {
  if (length(curves) == 0L) {
    return(NULL)
  }
  if (length(curves) == 1L) {
    return(curves[[1L]])
  }
  curve <- curves[[1L]]
  probabilities <- vapply(curves, function(x) x$probability, numeric(nrow(curve)))
  lifts <- vapply(curves, function(x) x$lift, numeric(nrow(curve)))
  curve$pairs <- rowSums(vapply(curves, function(x) x$pairs, numeric(nrow(curve))))
  curve$drifting_pairs <- rowSums(vapply(curves, function(x) x$drifting_pairs, numeric(nrow(curve))))
  curve$probability <- .dde_row_means(probabilities)
  curve$lift <- .dde_row_means(lifts)
  return(curve)
}

#' @noRd
.dde_row_means <- function(x) {
  means <- rowMeans(x, na.rm = TRUE)
  means[is.nan(means)] <- NA_real_
  return(means)
}

#' @noRd
.dde_check_thresholds <- function(thresholds) {
  if (!is.numeric(thresholds) || (length(thresholds) == 0L) || anyNA(thresholds) ||
      any(thresholds < 0) || any(thresholds > 1) || is.unsorted(thresholds, strictly = TRUE)) {
    stop("thresholds must be increasing values between 0 and 1", call. = FALSE)
  }
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# DDE curve and scores
# ---------------------------------------------------------------------------

#' @title DDE probability curve
#' @description Computes the DDE probability curve and the DDE probability
#' lift curve of a drift (Szucs and Nemeth, 2025, Section 2).
#' @param data Data frame or matrix with the features in the reference window,
#' where the Pearson correlations are measured.
#' @param drifting Names (or a logical vector over the columns of `data`) of
#' the features that drift.
#' @param thresholds Correlation thresholds \eqn{z}.
#' @details For each threshold \eqn{z}, \eqn{B(z)} is the number of feature
#' pairs with \eqn{|\rho_{ij}| \ge z} (Eq. 14) and \eqn{b(z)} the number of
#' those pairs in which both features drift (Eq. 15). With \eqn{m} features
#' of which \eqn{k} drift, the DDE probability is
#' \deqn{r_{n/N}(z) = \frac{m}{k} \frac{b(z)}{B(z)}}
#' (Eqs. 16 and 20), the probability that a neighbour of a drifting feature
#' drifts too, and the lift divides it by its value at \eqn{z = 0},
#' \eqn{(k - 1)/(m - 1)} (Eqs. 21 to 24). A flat lift curve means that the
#' features drift independently of their correlation; an increasing one, that
#' the drift affects groups of correlated features (the domino drift effect).
#' Features without variance have no correlation with the others and are
#' treated as uncorrelated.
#' @return data frame with one row per threshold and the columns `threshold`,
#' `pairs` (\eqn{B(z)}), `drifting_pairs` (\eqn{b(z)}), `probability`
#' (\eqn{r_{n/N}(z)}, `NA` when no pair reaches the threshold) and `lift`.
#' @examples
#' set.seed(1)
#' a <- stats::rnorm(200)
#' b <- stats::rnorm(200)
#' data <- data.frame(a1 = a, a2 = a + stats::rnorm(200, sd = 0.2),
#'                    a3 = a + stats::rnorm(200, sd = 0.5),
#'                    b1 = b, b2 = b + stats::rnorm(200, sd = 0.2),
#'                    b3 = b + stats::rnorm(200, sd = 0.5))
#' # The correlated group "a" drifts: the lift grows with the correlation.
#' dde_curve(data, c("a1", "a2", "a3"))
#' @export
dde_curve <- function(data, drifting, thresholds = seq(0, 0.9, by = 0.1)) {
  .dde_check_thresholds(thresholds)
  correlation <- .dde_correlation(data)
  features <- colnames(correlation)
  drifting <- .dde_drifting(drifting, features)
  m <- length(features)
  k <- sum(drifting)
  if (k < 2L) {
    stop("dde_curve needs at least two drifting features", call. = FALSE)
  }
  upper <- upper.tri(correlation)
  strength <- abs(correlation[upper])
  both <- outer(drifting, drifting, "&")[upper]
  pairs <- vapply(thresholds, function(z) sum(strength >= z), numeric(1))
  drifting_pairs <- vapply(thresholds, function(z) sum(both & (strength >= z)), numeric(1))
  probability <- ifelse(pairs > 0, (m / k) * drifting_pairs / pairs, NA_real_)
  baseline <- (k - 1) / (m - 1)
  return(data.frame(
    threshold = thresholds, pairs = pairs, drifting_pairs = drifting_pairs,
    probability = probability, lift = probability / baseline
  ))
}

#' @title DDE drift scores
#' @description Predicts the drift score of unmonitored features from the
#' drifting monitored features (Szucs and Nemeth, 2025, Eq. 25).
#' @param curve DDE curve, as returned by [dde_curve()].
#' @param data Data frame or matrix with the features in the reference window,
#' where the Pearson correlations are measured.
#' @param drifting Names of the monitored features that drift.
#' @param unmonitored Names of the features to score.
#' @details The score of an unmonitored feature \eqn{f_i} is the mean, over
#' the drifting monitored features \eqn{f_j}, of the DDE probability at
#' \eqn{|\rho_{ij}|}: the probability of the largest threshold that does not
#' exceed the correlation. Thresholds without pairs take the probability of
#' the previous threshold. Scores are limited to \eqn{[0, 1]}; they are 0
#' when no monitored feature drifts.
#' @return named numeric vector of scores, one per unmonitored feature.
#' @examples
#' set.seed(1)
#' a <- stats::rnorm(200)
#' b <- stats::rnorm(200)
#' data <- data.frame(a1 = a, a2 = a + stats::rnorm(200, sd = 0.2),
#'                    a3 = a + stats::rnorm(200, sd = 0.5),
#'                    b1 = b, b2 = b + stats::rnorm(200, sd = 0.2),
#'                    b3 = b + stats::rnorm(200, sd = 0.5))
#' curve <- dde_curve(data, c("a1", "a2", "a3"))
#' # a1 drifts: its neighbours get high scores, the "b" features low ones.
#' dde_score(curve, data, drifting = "a1", unmonitored = c("a2", "a3", "b2", "b3"))
#' @export
dde_score <- function(curve, data, drifting, unmonitored) {
  if (!is.data.frame(curve) || !all(c("threshold", "probability") %in% names(curve))) {
    stop("curve must be a DDE curve, as returned by dde_curve()", call. = FALSE)
  }
  correlation <- .dde_correlation(data)
  features <- colnames(correlation)
  unknown <- setdiff(c(drifting, unmonitored), features)
  if (length(unknown) > 0L) {
    stop(sprintf("features not in the data: %s", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  scores <- stats::setNames(numeric(length(unmonitored)), unmonitored)
  if ((length(drifting) == 0L) || (length(unmonitored) == 0L)) {
    return(scores)
  }
  probability <- curve$probability
  # Carry the last known probability over thresholds without pairs.
  for (i in seq_along(probability)) {
    if (is.na(probability[i])) probability[i] <- if (i > 1L) probability[i - 1L] else 0
  }
  probability <- pmin(pmax(probability, 0), 1)
  strength <- abs(correlation[unmonitored, drifting, drop = FALSE])
  level <- findInterval(strength, curve$threshold)
  values <- ifelse(level > 0L, probability[pmax(level, 1L)], 0)
  scores[] <- rowMeans(matrix(values, nrow = length(unmonitored)))
  return(scores)
}

#' Absolute-value-ready Pearson correlation matrix with named columns
#' @noRd
.dde_correlation <- function(data) {
  data <- as.matrix(as.data.frame(data))
  storage.mode(data) <- "double"
  if (is.null(colnames(data))) colnames(data) <- paste0("V", seq_len(ncol(data)))
  if (ncol(data) < 2L) {
    stop("the data must have at least two features", call. = FALSE)
  }
  correlation <- suppressWarnings(stats::cor(data, use = "pairwise.complete.obs"))
  correlation[is.na(correlation)] <- 0
  diag(correlation) <- 1
  return(correlation)
}

#' @noRd
.dde_drifting <- function(drifting, features) {
  if (is.logical(drifting)) {
    if (length(drifting) != length(features)) {
      stop("a logical 'drifting' must have one value per feature", call. = FALSE)
    }
    return(stats::setNames(drifting & !is.na(drifting), features))
  }
  unknown <- setdiff(drifting, features)
  if (length(unknown) > 0L) {
    stop(sprintf("drifting features are not in the data: %s", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  return(stats::setNames(features %in% drifting, features))
}

# ---------------------------------------------------------------------------
# Univariate window tests
# ---------------------------------------------------------------------------

#' @title Feature-wise drift tests between two windows
#' @description Tests every feature for drift between a reference window and
#' an analysis window, with the univariate tests used by Szucs and Nemeth
#' (2025).
#' @param reference Data frame or matrix with the reference window.
#' @param current Data frame or matrix with the analysis window, with the same
#' columns.
#' @param test One of `"js"`, `"ks"` or `"wasserstein"`.
#' @param threshold Drift threshold. `NULL` uses the default of the test.
#' @details The tests follow the definitions and defaults of the Evidently
#' library:
#' \itemize{
#' \item `"js"`: Jensen-Shannon distance (natural logarithm) between the
#' histograms of the two windows, on common bins chosen with Sturges' rule
#' over both windows; empty bins get a proportion of 1e-4. Drift when the
#' distance is at least `threshold` (default 0.1).
#' \item `"ks"`: two-sample Kolmogorov-Smirnov test. Drift when the p-value
#' is below `threshold` (default 0.05).
#' \item `"wasserstein"`: Wasserstein distance divided by the standard
#' deviation of the reference window (at least 0.001). Drift when it is at
#' least `threshold` (default 0.1).
#' }
#' Missing values are dropped; features with fewer than two values in either
#' window are not tested.
#'
#' The distances of the `"js"` test do not shrink to 0 between two samples of
#' the same distribution: with a few hundred observations per window the
#' Jensen-Shannon distance of identical distributions is often close to the
#' default threshold. Use windows of about a thousand observations, or a higher
#' threshold, with small windows.
#' @return data frame with one row per feature and the columns `feature`,
#' `statistic` (the distance, or the p-value for `"ks"`) and `drift`.
#' @examples
#' set.seed(1)
#' reference <- data.frame(x = stats::rnorm(300), y = stats::rnorm(300))
#' current <- data.frame(x = stats::rnorm(300, mean = 1), y = stats::rnorm(300))
#' dde_feature_drift(reference, current, test = "ks")
#' @export
dde_feature_drift <- function(reference, current, test = "js", threshold = NULL) {
  test <- match.arg(test, c("js", "ks", "wasserstein"))
  reference <- as.data.frame(reference)
  current <- as.data.frame(current)
  if (!identical(names(reference), names(current))) {
    stop("reference and current must have the same columns", call. = FALSE)
  }
  if (is.null(threshold)) {
    threshold <- c(js = 0.1, ks = 0.05, wasserstein = 0.1)[[test]]
  }
  .check_number(threshold, "threshold", min_value = 0)
  statistic <- vapply(names(reference), function(feature) {
    x <- suppressWarnings(as.numeric(reference[[feature]]))
    y <- suppressWarnings(as.numeric(current[[feature]]))
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    if ((length(x) < 2L) || (length(y) < 2L)) {
      return(NA_real_)
    }
    switch(test,
      js = .dde_js_distance(x, y),
      ks = suppressWarnings(stats::ks.test(x, y)$p.value),
      wasserstein = .dde_wasserstein(x, y) / max(.dde_population_sd(x), 0.001)
    )
  }, numeric(1))
  drift <- if (test == "ks") statistic < threshold else statistic >= threshold
  drift[is.na(drift)] <- FALSE
  return(data.frame(feature = names(reference), statistic = unname(statistic), drift = unname(drift)))
}

#' Jensen-Shannon distance between the histograms of two samples
#' @noRd
.dde_js_distance <- function(x, y) {
  values <- c(x, y)
  bins <- ceiling(log2(length(values))) + 1L
  low <- min(values)
  high <- max(values)
  if (low == high) {
    return(0)
  }
  breaks <- seq(low, high, length.out = bins + 1L)
  # Bins are closed on the left, the last one on both sides (numpy.histogram).
  p <- tabulate(findInterval(x, breaks, rightmost.closed = TRUE, all.inside = TRUE), bins) / length(x)
  q <- tabulate(findInterval(y, breaks, rightmost.closed = TRUE, all.inside = TRUE), bins) / length(y)
  p[p == 0] <- 1e-4
  q[q == 0] <- 1e-4
  p <- p / sum(p)
  q <- q / sum(q)
  mixture <- (p + q) / 2
  divergence <- (sum(p * log(p / mixture)) + sum(q * log(q / mixture))) / 2
  return(sqrt(max(divergence, 0)))
}

#' One-dimensional Wasserstein distance between two empirical distributions
#' @noRd
.dde_wasserstein <- function(x, y) {
  values <- sort(c(x, y))
  cdf_x <- stats::ecdf(x)(values)
  cdf_y <- stats::ecdf(y)(values)
  return(sum(abs(cdf_x - cdf_y)[-length(values)] * diff(values)))
}

#' @noRd
.dde_population_sd <- function(x) {
  sqrt(mean((x - mean(x))^2))
}
