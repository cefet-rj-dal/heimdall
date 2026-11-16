#' @title Population Stability Index (PSI)
#' @description Compares the distribution of a new sample with a baseline
#' sample, feature by feature, through the Population Stability Index, the
#' symmetrised Kullback-Leibler divergence of their binned distributions
#' (Kurian and Allali, 2024):
#' \deqn{PSI = \sum_{i=1}^{B} (P_i - Q_i) \ln(P_i / Q_i),}
#' where \eqn{Q_i} and \eqn{P_i} are the shares of the baseline and of the new
#' sample in bin \eqn{i}. The bins are quantile groups of the baseline
#' (deciles for `bins = 10`, demi-deciles for `bins = 20`).
#' @param expected The baseline sample: a numeric vector, or a data frame or
#' matrix with one column per feature.
#' @param actual The new sample, with the same features as `expected`.
#' @param bins Number of quantile groups of the baseline.
#' @param thresholds Limits between the drift levels: PSI below the first is
#' "little" drift, between the two "moderate" drift (to be reviewed), and above
#' the second "significant" drift.
#' @details The computation follows the SAS macro of the reference code
#' (<https://github.com/FrancisKurian/Kullback>, `PSI_computation.sas`):
#'
#' 1. The baseline is ranked (ties get their mean rank) and split into `bins`
#'    groups as `PROC RANK GROUPS=`: group `floor(rank * bins / (n + 1))`.
#'    Tied values always share a group, so heavily tied data may give fewer
#'    bins. Bin `i` covers the baseline values from its smallest to its largest.
#' 2. New values below the smallest baseline value or above the largest are
#'    moved to it (they fall in the first or the last bin).
#' 3. The shares are the bin counts over the sample sizes. Missing new values
#'    count in the size of the new sample but in no bin, so a stream that starts
#'    sending missing values lowers every share. Missing baseline values are
#'    left out. A new sample whose values are all missing has every share 0,
#'    and so a PSI of 0 (step 4): missing data must be checked separately.
#' 4. A bin without new values contributes 0, as in the reference (where the
#'    logarithm of 0 is missing and the missing part is set to 0).
#'
#' The reference assigns a value through a SAS format built from the bin
#' ranges; a new value that falls strictly between the largest value of a bin
#' and the smallest of the next one matches no range and is dropped from the
#' counts (but kept in the sample size). Here such a value goes to the lower
#' bin. The paper's thresholds are 0.1 and 0.25 (Section 5; Section 3.1 uses
#' 0.2 for the second). The PSI of two samples from the same distribution is
#' not 0: it is about `(bins - 1) (1 / n_expected + 1 / n_actual)`, which
#' matters for small samples.
#' @references Kurian, J. F. and Allali, M. (2024). Detecting drifts in data
#' streams using Kullback-Leibler (KL) divergence measure for data engineering
#' applications. *Journal of Data, Information and Management*, 6, 207-216.
#' <doi:10.1007/s42488-024-00119-y>
#' @return A `psi` object: a list with `summary`, a data frame with one row per
#' feature (`feature`, `psi`, `level`, number of `bins`), and `bins`, a data
#' frame with one row per feature and bin (`feature`, `bin`, `lower` and
#' `upper` baseline values, `expected` and `actual` shares, `part`, the
#' contribution of the bin).
#' @seealso [dfr_psi()] to monitor a stream, and [dfr_kldist()] for the same
#' divergence over a sliding window with equal-width bins.
#' @examples
#' set.seed(1)
#' base <- stats::rnorm(10000, 610, 50)
#' new <- base
#' top <- new > 643 & stats::runif(10000) < 0.5
#' new[top] <- new[top] - 200
#' result <- psi(base, new)
#' result$summary
#' head(result$bins)
#' psi(base, new, bins = 20)$summary
#' @export
psi <- function(expected, actual, bins = 10, thresholds = c(0.1, 0.25)) {
  .check_positive_integer(bins, "bins", min_value = 2L)
  if (!is.numeric(thresholds) || (length(thresholds) != 2L) || anyNA(thresholds) || (thresholds[1] > thresholds[2])) {
    stop("psi: thresholds must be two increasing numbers", call. = FALSE)
  }
  expected <- .psi_frame(expected, "expected")
  actual <- .psi_frame(actual, "actual")
  missing_features <- setdiff(names(expected), names(actual))
  if (length(missing_features) > 0L) {
    stop(sprintf("psi: actual lacks the features %s", paste(missing_features, collapse = ", ")), call. = FALSE)
  }
  parts <- lapply(names(expected), function(feature) {
    table <- .psi_feature(expected[[feature]], actual[[feature]], bins)
    cbind(feature = feature, table)
  })
  table <- do.call(rbind, parts)
  totals <- vapply(parts, function(part) sum(part$part), numeric(1))
  levels <- c("little", "moderate", "significant")
  summary <- data.frame(feature = names(expected), psi = totals,
                        level = levels[findInterval(totals, thresholds, left.open = TRUE) + 1L],
                        bins = vapply(parts, nrow, integer(1)))
  result <- list(summary = summary, bins = table, thresholds = thresholds)
  class(result) <- "psi"
  return(result)
}

#' @export
print.psi <- function(x, ...) {
  cat("Population Stability Index\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

# Bins of one feature: quantile groups of the baseline (PROC RANK GROUPS=),
# shares of both samples and the contribution of each bin.
#' @noRd
.psi_feature <- function(expected, actual, bins) {
  expected <- expected[!is.na(expected)]
  if (length(expected) == 0L) {
    stop("psi: the baseline has no values", call. = FALSE)
  }
  n <- length(expected)
  group <- floor(rank(expected, ties.method = "average") * bins / (n + 1))
  lower <- as.numeric(tapply(expected, group, min))
  upper <- as.numeric(tapply(expected, group, max))
  base_counts <- as.numeric(table(group))
  values <- pmin(pmax(actual[!is.na(actual)], lower[1]), upper[length(upper)])
  actual_counts <- tabulate(findInterval(values, lower), nbins = length(lower))
  expected_share <- base_counts / n
  actual_share <- if (length(actual) > 0L) actual_counts / length(actual) else rep(0, length(lower))
  part <- ifelse(actual_share > 0, (actual_share - expected_share) * log(actual_share / expected_share), 0)
  data.frame(bin = seq_along(lower), lower = lower, upper = upper, expected = expected_share,
             actual = actual_share, part = part)
}

#' @noRd
.psi_frame <- function(data, name) {
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }
  if (!is.data.frame(data)) {
    if (!is.numeric(data)) {
      stop(sprintf("psi: %s must be numeric", name), call. = FALSE)
    }
    data <- data.frame(value = as.numeric(data))
  }
  if (!all(vapply(data, is.numeric, logical(1)))) {
    stop(sprintf("psi: every feature of %s must be numeric", name), call. = FALSE)
  }
  if (is.null(names(data)) || any(names(data) == "")) {
    names(data) <- paste0("V", seq_along(data))
  }
  return(data)
}

# Stream detector ----------------------------------------------------------------------

#' @title PSI stream detector
#' @description Monitors a stream with the Population Stability Index
#' ([psi()]): the first `n_reference` observations form the baseline, and every
#' later window of `window_size` observations is compared with it, feature by
#' feature. Since it monitors the distribution of the observed features, it is
#' primarily used for **virtual concept drift**, and fits the paper's setting
#' of validating the fields of incoming data files against a baseline file.
#' @param target_feat Names of the columns to be monitored. `NULL` (the default)
#' monitors every numeric column.
#' @param n_reference Size of the baseline.
#' @param window_size Size of the compared windows (non-overlapping).
#' @param bins Number of quantile groups of the baseline (10 for deciles, 20 for
#' demi-deciles).
#' @param threshold A drift is reported when the PSI of some feature exceeds
#' it. The default, 0.25, is the paper's limit of a significant drift; 0.1 also
#' flags the moderate ones. See [psi()] for the PSI of samples without drift,
#' which grows as the windows get smaller.
#' @details A drift is reported on the last observation of every window whose
#' largest PSI exceeds `threshold`. `reset_state()` forgets the baseline, and the
#' next `n_reference` observations form a new one. Missing values are kept: in
#' the windows they count as observations in no bin (see [psi()]).
#' @return `dfr_psi` object. After `fit()`, `drifter_output` holds the PSI of
#' every feature (`psi_<feature>`) on the last observation of every window, and
#' `obj$reference` the baseline.
#' @inherit psi references
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' # Scores around 610; from row 3001 half of those above 643 drop by 200.
#' set.seed(1)
#' score <- stats::rnorm(5000, 610, 50)
#' late <- 3001:5000
#' drop <- late[score[late] > 643 & stats::runif(2000) < 0.5]
#' score[drop] <- score[drop] - 200
#' # The PSI reaches about 0.2: a moderate drift, flagged with threshold 0.1.
#' detector <- dfr_psi(n_reference = 2000, window_size = 500, threshold = 0.1)
#' model <- fit(detector, data.frame(cbm = score))
#' model$drifter_output[!is.na(model$drifter_output$psi_cbm), , drop = FALSE]
#' model$drifted
#' @export
dfr_psi <- function(target_feat = NULL, n_reference = 1000, window_size = 1000, bins = 10, threshold = 0.25) {
  .check_target_feat(target_feat, multiple = TRUE)
  .check_positive_integer(n_reference, "n_reference", min_value = 2L)
  .check_positive_integer(window_size, "window_size", min_value = 1L)
  .check_positive_integer(bins, "bins", min_value = 2L)
  .check_number(threshold, "threshold", min_value = 0)
  obj <- mv_dist_based(target_feat = target_feat)
  obj$state <- list(n_reference = n_reference, window_size = window_size, bins = bins, threshold = threshold,
                    features = NULL, buffer = NULL)
  obj$reference <- NULL
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_psi", class(obj))
  return(obj)
}

#' @export
update_state.dfr_psi <- function(obj, value, ...) {
  state <- obj$state
  row <- .select_features(value, obj$target_feat)
  if (is.null(state$features)) {
    state$features <- names(row)
  } else if (!identical(names(row), state$features)) {
    stop("dfr_psi: the monitored features changed during the stream", call. = FALSE)
  }
  # The buffer is allocated for the whole baseline or window and filled row by
  # row, so that collecting a sample costs linear time.
  needed <- if (is.null(obj$reference)) state$n_reference else state$window_size
  if (is.null(state$buffer)) {
    state$buffer <- matrix(NA_real_, needed, length(row))
    state$filled <- 0L
  }
  state$filled <- state$filled + 1L
  state$buffer[state$filled, ] <- unname(row)
  if (state$filled < needed) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  sample <- as.data.frame(state$buffer)
  names(sample) <- state$features
  state$buffer <- NULL
  obj$state <- state
  if (is.null(obj$reference)) {
    obj$reference <- sample
    return(list(obj = obj, drift = FALSE))
  }
  result <- psi(obj$reference, sample, bins = state$bins)$summary
  obj$last_drifter_output <- stats::setNames(result$psi, paste0("psi_", result$feature))
  if (any(result$psi > state$threshold)) {
    obj$drifted <- TRUE
    return(list(obj = obj, drift = TRUE))
  }
  return(list(obj = obj, drift = FALSE))
}

#' @export
fit.dfr_psi <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data))
}

#' @export
reset_state.dfr_psi <- function(obj) {
  obj$drifted <- FALSE
  obj$state$buffer <- NULL
  obj$reference <- NULL
  return(obj)
}
