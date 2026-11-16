#' @title PROTON: prototype-based test-time online OOD detection
#' @description Online out-of-distribution (OOD) detector for the outputs of a
#' frozen classifier with an embedding space, such as a medical
#' vision-language model (Das et al., 2026). Each observation is one input
#' scored by the model: its embedding and its class probabilities. PROTON
#' keeps a bank of the most recent confidently classified embeddings of each
#' class, scores each input by its cosine distance to the nearest class
#' prototype, and fuses that distance with the Maximum Concept Matching (MCM)
#' score, `-max(p)`, using a weight driven by the running variance of the MCM
#' score. It needs no training data, labels or model changes.
#' @param embedding_cols Names of the columns holding the embedding.
#' @param prob_cols Names of the columns holding the class probabilities (one
#' per in-distribution class), in the same order for every observation.
#' @param score_col Optional column with another base OOD score (higher means
#' more OOD), used instead of the MCM score `-max(p)`. The probabilities are
#' still used by the confidence gate.
#' @param gamma Confidence gate: an embedding enters the bank of its predicted
#' class only when `max(p) >= gamma`.
#' @param bank_size Maximum number of embeddings kept per class (`M`); the
#' oldest one is dropped first.
#' @param k_min Number of embeddings every class needs before the prototype
#' distance is used. Until then the score is the base score alone.
#' @param alpha_min,alpha_max Range of the weight of the base score in the
#' fusion.
#' @param sigma2_0 Reference variance of the base score on in-distribution
#' data.
#' @param threshold Optional threshold on the fused score. When given, an
#' observation with a score above it is reported as a drift (an OOD input);
#' when `NULL` (default) no drift is reported and the scores are only exposed
#' through `drifter_output`.
#' @details For each observation \eqn{x_t}, with \eqn{\ell_2}-normalized
#' embedding \eqn{e_t} and probabilities \eqn{p_t} (Algorithm 1 of the paper):
#' \enumerate{
#' \item the base score \eqn{S_{MCM} = -\max_c p_{t,c}} updates its running
#' variance \eqn{\sigma^2_t} (Welford's algorithm);
#' \item once every class bank holds `k_min` embeddings, the prototype distance
#' is \eqn{S_{proto} = 1 - \max_c e_t^\top \mu_c}, where \eqn{\mu_c} is the
#' normalized mean of the bank of class \eqn{c};
#' \item the weight is \eqn{\alpha_t = \alpha_{max} - \mathrm{sigmoid}(100
#' (\sigma^2_t - \sigma^2_0)) (\alpha_{max} - \alpha_{min})} and the score is
#' \eqn{S_t = \alpha_t S_{MCM} + (1 - \alpha_t) S_{proto}}; before
#' calibration \eqn{S_t = S_{MCM}};
#' \item finally, when \eqn{\max_c p_{t,c} \ge \gamma}, \eqn{e_t} enters the
#' bank of the predicted class.
#' }
#' A variance that rises above the in-distribution reference (for instance
#' when confidently misclassified, covariate-shifted inputs arrive) moves the
#' weight towards the prototype distance.
#'
#' Rows with a missing embedding or probability value are skipped.
#' `obj$drifter_output` holds, for the rows of the last `fit()`, the fused
#' `score`, the `base_score`, the `proto_score`, the weight `alpha`, the
#' running `variance`, whether the detector was `calibrated`, the predicted
#' `class` and whether the embedding was `enqueued`.
#' @references Das, A., Wasalathilaka, N., Lu, Y., Dukre, A., Mahapatra, D.,
#' Khan, S., and Razzak, I. (2026). PROTON: Prototype-Based Test-Time Online
#' OOD Detection for Medical VLMs. arXiv:2606.20913.
#'
#' Ming, Y., Cai, Z., Gu, J., Sun, Y., Li, W., and Li, Y. (2022). Delving into
#' out-of-distribution detection with vision-language representations.
#' *Advances in Neural Information Processing Systems*, 35, 35087-35102.
#' @return `dfr_proton` object
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#'
#' # Two in-distribution classes in a 3-dimensional embedding space, followed
#' # by covariate-shifted inputs: they are classified as confidently as the
#' # in-distribution ones, but their embeddings lie elsewhere.
#' set.seed(1)
#' make_rows <- function(n, center, p1) {
#'   e <- sweep(matrix(stats::rnorm(3 * n, sd = 0.05), n), 2, center, "+")
#'   data.frame(e1 = e[, 1], e2 = e[, 2], e3 = e[, 3], p1 = p1, p2 = 1 - p1)
#' }
#' stream <- rbind(
#'   make_rows(50, c(1, 0, 0), 0.9),
#'   make_rows(50, c(0, 1, 0), 0.1),
#'   make_rows(20, c(0, 0, 1), 0.9)
#' )
#'
#' detector <- dfr_proton(embedding_cols = c("e1", "e2", "e3"), prob_cols = c("p1", "p2"))
#' detector <- fit(detector, stream)
#' tail(detector$drifter_output[, c("score", "base_score", "proto_score", "alpha")])
#' @export
dfr_proton <- function(embedding_cols, prob_cols, score_col = NULL, gamma = 0.7, bank_size = 100,
                       k_min = 5, alpha_min = 0.3, alpha_max = 0.7, sigma2_0 = 0.02, threshold = NULL) {
  if (!is.character(embedding_cols) || (length(embedding_cols) == 0L) || anyNA(embedding_cols)) {
    stop("embedding_cols must be a vector of column names", call. = FALSE)
  }
  if (!is.character(prob_cols) || (length(prob_cols) < 2L) || anyNA(prob_cols)) {
    stop("prob_cols must name at least two probability columns", call. = FALSE)
  }
  if (!is.null(score_col)) {
    .check_target_feat(score_col)
  }
  columns <- c(embedding_cols, prob_cols, score_col)
  if (anyDuplicated(columns)) {
    stop("embedding_cols, prob_cols and score_col must not share columns", call. = FALSE)
  }
  .check_probability(gamma, "gamma")
  .check_positive_integer(bank_size, "bank_size", min_value = 1L)
  .check_positive_integer(k_min, "k_min", min_value = 1L)
  if (k_min > bank_size) {
    stop("k_min must not be greater than bank_size", call. = FALSE)
  }
  .check_probability(alpha_min, "alpha_min")
  .check_probability(alpha_max, "alpha_max")
  if (alpha_min > alpha_max) {
    stop("alpha_min must not be greater than alpha_max", call. = FALSE)
  }
  .check_number(sigma2_0, "sigma2_0", min_value = 0)
  if (!is.null(threshold)) {
    .check_number(threshold, "threshold")
  }

  obj <- mv_dist_based(target_feat = columns)
  obj$embedding_cols <- embedding_cols
  obj$prob_cols <- prob_cols
  obj$score_col <- score_col
  obj$gamma <- gamma
  obj$bank_size <- bank_size
  obj$k_min <- k_min
  obj$alpha_min <- alpha_min
  obj$alpha_max <- alpha_max
  obj$sigma2_0 <- sigma2_0
  obj$threshold <- threshold
  obj$state <- .proton_state(length(prob_cols), length(embedding_cols))
  obj$drifted <- FALSE
  class(obj) <- append("dfr_proton", class(obj))
  return(obj)
}

#' @noRd
.proton_state <- function(n_classes, dimension) {
  list(
    # Welford accumulators of the base score.
    n = 0,
    mean = 0,
    m2 = 0,
    # FIFO bank of each class, oldest embedding in the first row, together with
    # the running sum of its rows.
    banks = rep(list(matrix(numeric(0), nrow = 0L, ncol = dimension)), n_classes),
    sums = matrix(0, nrow = n_classes, ncol = dimension),
    prototypes = matrix(NA_real_, nrow = n_classes, ncol = dimension)
  )
}

#' @noRd
.proton_normalize <- function(x) {
  norm <- sqrt(sum(x^2))
  if (norm == 0) x else x / norm
}

# One step of Algorithm 1. `e` is the embedding, `p` the probabilities and
# `base` the base score of the observation.
#' @noRd
.proton_step <- function(obj, e, p, base) {
  state <- obj$state
  e <- .proton_normalize(e)

  # Welford update of the running variance of the base score.
  state$n <- state$n + 1
  delta <- base - state$mean
  state$mean <- state$mean + delta / state$n
  state$m2 <- state$m2 + delta * (base - state$mean)
  variance <- if (state$n > 1) state$m2 / (state$n - 1) else 0

  counts <- vapply(state$banks, nrow, integer(1))
  calibrated <- all(counts >= obj$k_min)
  if (calibrated) {
    proto_score <- 1 - max(state$prototypes %*% e)
    alpha <- obj$alpha_max - stats::plogis(100 * (variance - obj$sigma2_0)) * (obj$alpha_max - obj$alpha_min)
    score <- alpha * base + (1 - alpha) * proto_score
  } else {
    proto_score <- NA_real_
    alpha <- 1
    score <- base
  }

  # Confidence gate: only confident predictions update the prototypes.
  predicted <- which.max(p)
  enqueued <- max(p) >= obj$gamma
  if (enqueued) {
    bank <- rbind(state$banks[[predicted]], e, deparse.level = 0)
    state$sums[predicted, ] <- state$sums[predicted, ] + e
    if (nrow(bank) > obj$bank_size) {
      state$sums[predicted, ] <- state$sums[predicted, ] - bank[1L, ]
      bank <- bank[-1L, , drop = FALSE]
    }
    state$banks[[predicted]] <- bank
    state$prototypes[predicted, ] <- .proton_normalize(state$sums[predicted, ] / nrow(bank))
  }
  obj$state <- state

  has_drift <- !is.null(obj$threshold) && (score > obj$threshold)
  if (has_drift) {
    obj$drifted <- TRUE
  }
  obj$last_drifter_output <- list(
    score = score, base_score = base, proto_score = proto_score, alpha = alpha,
    variance = variance, calibrated = calibrated, class = predicted, enqueued = enqueued
  )
  return(list(obj = obj, drift = has_drift))
}

# Splits a numeric row (or the rows of a matrix) into embedding, probabilities
# and base score; returns NULL for an incomplete observation.
#' @noRd
.proton_parts <- function(obj, row) {
  e <- row[obj$embedding_cols]
  p <- row[obj$prob_cols]
  base <- if (is.null(obj$score_col)) -max(p) else row[[obj$score_col]]
  if (anyNA(e) || anyNA(p) || is.na(base)) {
    return(NULL)
  }
  list(e = unname(e), p = unname(p), base = unname(base))
}

#' @export
update_state.dfr_proton <- function(obj, value, ...) {
  row <- .select_features(value, obj$target_feat)
  parts <- .proton_parts(obj, row)
  if (is.null(parts)) {
    obj$last_drifter_output <- NULL
    return(list(obj = obj, drift = FALSE))
  }
  return(.proton_step(obj, parts$e, parts$p, parts$base))
}

#' @export
fit.dfr_proton <- function(obj, data, ...) {
  data <- as.data.frame(data)
  if (nrow(data) == 0L) {
    stop("fit: 'data' must contain at least one row", call. = FALSE)
  }
  missing_columns <- setdiff(obj$target_feat, names(data))
  if (length(missing_columns) > 0L) {
    stop(sprintf("fit: the monitored features are not in the data: %s", paste(missing_columns, collapse = ", ")), call. = FALSE)
  }
  # Converting once is much faster than selecting the columns row by row.
  values <- vapply(data[obj$target_feat], function(column) suppressWarnings(as.numeric(column)), numeric(nrow(data)))
  values <- matrix(values, nrow = nrow(data), dimnames = list(NULL, obj$target_feat))
  values[is.infinite(values)] <- NA_real_

  collected <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    parts <- .proton_parts(obj, values[i, ])
    if (is.null(parts)) {
      next
    }
    obj <- .proton_step(obj, parts$e, parts$p, parts$base)$obj
    collected[i] <- list(obj$last_drifter_output)
  }
  obj$drifter_output <- .as_drifter_output(collected)
  return(obj)
}

#' @export
reset_state.dfr_proton <- function(obj) {
  obj$state <- .proton_state(length(obj$prob_cols), length(obj$embedding_cols))
  obj$drifted <- FALSE
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  return(obj)
}
