# WWAggr: window Wasserstein-based aggregation of ensembles of change point
# detectors (Stepikin, Romanenkova and Zaytsev, 2025), with the calibration of
# the members and the naive aggregations it is compared with.

# Calibration ------------------------------------------------------------------------------------

#' @title Calibration of change point models
#' @description Post-hoc calibration of the scores of a change point model
#' ([indid()], [tscp2()], [ts2vec()]) or of every member of an ensemble
#' ([cpd_ensemble()]) on labelled sequences, as the WWAggr pipeline does before
#' aggregating: beta calibration (Kull et al., 2017, the method of the paper)
#' or temperature scaling (Guo et al., 2017).
#' @param model A change point model or a `cpd_ensemble`.
#' @param x Calibration sequences, in the formats accepted by [indid()].
#' @param labels Their labels, a 0/1 matrix `[sequence, time]` (1 from the
#' change on).
#' @param method `"beta"` (default) or `"temperature"`.
#' @param parameters Parameters of the beta calibration: `"abm"` (default),
#' `"am"`, `"ab"` or `"a"`.
#' @details Every score of every step (the steps without score of the
#' representation models excluded) is a sample, labelled by the label of its
#' step. The beta calibration follows the `betacal` package used by the
#' reference (<https://github.com/betacal/python>): a logistic regression of
#' the label on `log(s)` and `-log(1 - s)` (`"abm"`; when one coefficient is
#' negative, the regression is refitted on the other feature alone), on the
#' logit of `s` (`"am"`), on `log(2 s)` and `log(2 (1 - s))` without intercept
#' (`"ab"`), or on the logit without intercept (`"a"`). Scores are clipped to
#' `[eps, 1 - eps]` with the machine epsilon of single precision (the reference
#' scores are float32). The regressions are unpenalised (`glm`; the reference
#' uses scikit-learn's `LogisticRegression` with `C = 1e11`).
#'
#' Temperature scaling divides the logit of the score by the temperature that
#' minimises the log-loss on the calibration data. In the reference code the
#' fitted temperature is stored on the model wrapper and does not change its
#' predictions, so this option follows Guo et al. (2017) rather than the code.
#' @references Kull, M., Silva Filho, T. M. and Flach, P. (2017). Beyond
#' sigmoids: How to obtain well-calibrated probabilities from binary
#' classifiers with beta calibration. *Electronic Journal of Statistics*,
#' 11(2), 5052-5080. <doi:10.1214/17-EJS1338SI>
#'
#' Guo, C., Pleiss, G., Sun, Y. and Weinberger, K. Q. (2017). On calibration of
#' modern neural networks. *Proceedings of the 34th International Conference
#' on Machine Learning*, 1321-1330.
#' @inherit dfr_wwaggr references
#' @return A `cpd_calibrated` model (the model and its calibration map), or a
#' `cpd_ensemble` of calibrated members.
#' @examples
#' set.seed(1)
#' make <- function(n) {
#'   change <- sample(5:24, n, replace = TRUE)
#'   x <- t(vapply(change, function(c) stats::rnorm(24, ifelse(seq_len(24) < c, 0, 3)), numeric(24)))
#'   list(x = x, labels = t(vapply(change, function(c) (seq_len(24) >= c) * 1, numeric(24))))
#' }
#' train <- make(64)
#' model <- indid(train$x, train$labels, loss = "bce", epochs = 5, seed = 1)
#' calibrated <- cpd_calibrate(model, train$x, train$labels)
#' calibrated$calibrator$map
#' @export
cpd_calibrate <- function(model, x, labels, method = c("beta", "temperature"),
                          parameters = c("abm", "am", "ab", "a")) {
  method <- match.arg(method)
  parameters <- match.arg(parameters)
  if (inherits(model, "cpd_ensemble")) {
    return(cpd_ensemble(lapply(model$members, cpd_calibrate, x = x, labels = labels, method = method,
                               parameters = parameters)))
  }
  .check_cpd_model(model, "cpd_calibrate")
  x <- .indid_array(x)
  labels <- .indid_labels(labels, dim(x))
  scores <- cpd_scores(model, x)
  observed <- !is.na(scores)
  calibrator <- .calibration_fit(scores[observed], labels[observed], method, parameters)
  out <- list(model = model, calibrator = calibrator, dim = model$dim, features = model$features)
  class(out) <- c("cpd_calibrated", "cpd_model")
  out
}

#' @export
cpd_scores.cpd_calibrated <- function(model, x, ...) {
  scores <- cpd_scores(model$model, x)
  observed <- !is.na(scores)
  scores[observed] <- .calibration_apply(model$calibrator, scores[observed])
  scores
}

#' @export
print.cpd_calibrated <- function(x, ...) {
  cat(sprintf("Calibrated (%s) ", x$calibrator$method))
  print(x$model)
  invisible(x)
}

#' @noRd
.calibration_eps <- 1.1920928955078125e-07

#' @noRd
.calibration_features <- function(s, parameters) {
  s <- pmin(pmax(s, .calibration_eps), 1 - .calibration_eps)
  switch(parameters,
         abm = cbind(x1 = log(s), x2 = -log(1 - s)),
         ab = cbind(x1 = log(2 * s), x2 = log(2 * (1 - s))),
         cbind(x1 = log(s / (1 - s))))
}

#' @noRd
.logistic_fit <- function(design, y, intercept) {
  if (intercept) design <- cbind(`(Intercept)` = 1, design)
  fit <- suppressWarnings(stats::glm.fit(design, y, family = stats::binomial()))
  coefficients <- fit$coefficients
  coefficients[is.na(coefficients)] <- 0
  if (!intercept) coefficients <- c(`(Intercept)` = 0, coefficients)
  coefficients
}

#' @noRd
.calibration_fit <- function(s, y, method, parameters) {
  if (length(s) == 0L || length(unique(y)) < 2L) {
    stop("cpd_calibrate: the calibration data need scores of both labels", call. = FALSE)
  }
  if (method == "temperature") {
    logit <- stats::qlogis(pmin(pmax(s, .calibration_eps), 1 - .calibration_eps))
    loss <- function(log_t) {
      p <- pmin(pmax(stats::plogis(logit / exp(log_t)), 1e-15), 1 - 1e-15)
      -mean(y * log(p) + (1 - y) * log(1 - p))
    }
    temperature <- exp(stats::optimize(loss, c(-10, 10))$minimum)
    return(list(method = "temperature", temperature = temperature))
  }
  design <- .calibration_features(s, parameters)
  used <- colnames(design)
  coefficients <- .logistic_fit(design, y, parameters %in% c("abm", "am"))
  if (parameters == "abm") {
    if (coefficients["x1"] < 0) {
      used <- "x2"
      coefficients <- .logistic_fit(design[, used, drop = FALSE], y, TRUE)
    } else if (coefficients["x2"] < 0) {
      used <- "x1"
      coefficients <- .logistic_fit(design[, used, drop = FALSE], y, TRUE)
    }
  }
  slope <- function(name) if (name %in% names(coefficients)) unname(coefficients[name]) else 0
  intercept <- unname(coefficients["(Intercept)"])
  map <- switch(parameters,
                abm = {
                  a <- slope("x1")
                  b <- slope("x2")
                  m <- stats::optimize(function(m) abs(b * log(1 - m) - a * log(m) - intercept), c(0, 1))$minimum
                  c(a = a, b = b, m = m)
                },
                am = c(a = slope("x1"), b = slope("x1"), m = 1 / (1 + exp(intercept / slope("x1")))),
                ab = c(a = slope("x1"), b = -slope("x2"), m = 0.5),
                a = c(a = slope("x1"), b = slope("x1"), m = 0.5))
  list(method = "beta", parameters = parameters, features = used, coefficients = coefficients, map = map)
}

#' @noRd
.calibration_apply <- function(calibrator, s) {
  if (calibrator$method == "temperature") {
    logit <- stats::qlogis(pmin(pmax(s, .calibration_eps), 1 - .calibration_eps))
    return(stats::plogis(logit / calibrator$temperature))
  }
  design <- .calibration_features(s, calibrator$parameters)[, calibrator$features, drop = FALSE]
  eta <- calibrator$coefficients["(Intercept)"] + design %*% calibrator$coefficients[calibrator$features]
  as.vector(stats::plogis(eta))
}

# Ensembles --------------------------------------------------------------------------------------

#' @noRd
.check_cpd_model <- function(model, where) {
  if (!inherits(model, c("indid", "tscp2", "ts2vec", "cpd_calibrated"))) {
    stop(sprintf("%s: the model must be an indid, tscp2, ts2vec or cpd_calibrated object", where), call. = FALSE)
  }
}

#' @title Ensembles of change point models
#' @description `cpd_ensemble()` groups trained change point models ([indid()]
#' with BCE or InDiD loss, [tscp2()], [ts2vec()], or calibrated ones) into an
#' ensemble, possibly mixing types; `cpd_ensemble_fit()` trains the members of
#' an ensemble of one type. The scores of the members ([cpd_scores()]) are
#' aggregated by [wwaggr()] or monitored on a stream by [dfr_wwaggr()].
#' @param members A list of trained change point models with the same input
#' features.
#' @param x Training sequences, in the formats accepted by [indid()].
#' @param labels Their labels (0/1 matrix `[sequence, time]`), for the
#' supervised models.
#' @param model Type of the members: `"bce"` and `"indid"` ([indid()] with that
#' loss), `"tscp2"` or `"ts2vec"`.
#' @param n_models Number of members.
#' @param boot_sample_size `NULL` (default): every member is trained on all the
#' sequences and the members differ only by their seeds (the "naive"
#' ensembles of the paper). Otherwise every member is trained on a bootstrap
#' sample of `boot_sample_size` sequences.
#' @param seed Seed of the ensemble: member `i` is trained with seed
#' `seed + i - 1`, and the bootstrap samples are drawn with `seed`. `NULL`
#' draws one from the session.
#' @param ... Settings of the members, passed to [indid()], [tscp2()] or
#' [ts2vec()].
#' @details The paper also compares ensembles trained with stochastic gradient
#' Langevin dynamics; the reference code has no implementation of them and
#' they are not provided.
#' @inherit dfr_wwaggr references
#' @return A `cpd_ensemble` object (`members`).
#' @examples
#' set.seed(1)
#' make <- function(n) {
#'   change <- sample(5:24, n, replace = TRUE)
#'   x <- t(vapply(change, function(c) stats::rnorm(24, ifelse(seq_len(24) < c, 0, 3)), numeric(24)))
#'   list(x = x, labels = t(vapply(change, function(c) (seq_len(24) >= c) * 1, numeric(24))))
#' }
#' train <- make(64)
#' ensemble <- cpd_ensemble_fit(train$x, train$labels, model = "bce", n_models = 3,
#'                              epochs = 5, seed = 1)
#' dim(cpd_scores(ensemble, train$x[1:2, ]))
#' @export
cpd_ensemble <- function(members) {
  if (inherits(members, "cpd_ensemble")) return(members)
  if (!is.list(members) || length(members) < 1L) {
    stop("cpd_ensemble: members must be a non-empty list of change point models", call. = FALSE)
  }
  for (m in members) .check_cpd_model(m, "cpd_ensemble")
  dims <- vapply(members, function(m) as.integer(m$dim), integer(1))
  if (length(unique(dims)) != 1L) {
    stop("cpd_ensemble: the members have different numbers of features", call. = FALSE)
  }
  out <- list(members = members, dim = dims[1], features = members[[1]]$features)
  class(out) <- "cpd_ensemble"
  out
}

#' @rdname cpd_ensemble
#' @export
cpd_ensemble_fit <- function(x, labels = NULL, model = c("bce", "indid", "tscp2", "ts2vec"), n_models = 10,
                             boot_sample_size = NULL, seed = NULL, ...) {
  model <- match.arg(model)
  .check_positive_integer(n_models, "n_models", min_value = 1L)
  x <- .indid_array(x)
  n <- dim(x)[1]
  if (model %in% c("bce", "indid") && is.null(labels)) {
    stop("cpd_ensemble_fit: the supervised models need labels", call. = FALSE)
  }
  if (!is.null(labels)) labels <- .indid_labels(labels, dim(x))
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max - n_models, 1L)
  samples <- if (is.null(boot_sample_size)) {
    rep(list(seq_len(n)), n_models)
  } else {
    .check_positive_integer(boot_sample_size, "boot_sample_size", min_value = 1L)
    .with_rng(.rng_state(seed), function() lapply(seq_len(n_models), function(i) {
      sample.int(n, boot_sample_size, replace = TRUE)
    }))$value
  }
  members <- lapply(seq_len(n_models), function(i) {
    rows <- samples[[i]]
    xi <- x[rows, , , drop = FALSE]
    switch(model,
           bce = indid(xi, labels[rows, , drop = FALSE], loss = "bce", seed = seed + i - 1L, ...),
           indid = indid(xi, labels[rows, , drop = FALSE], loss = "indid", seed = seed + i - 1L, ...),
           tscp2 = tscp2(xi, seed = seed + i - 1L, ...),
           ts2vec = ts2vec(xi, seed = seed + i - 1L, ...))
  })
  cpd_ensemble(members)
}

#' @export
cpd_scores.cpd_ensemble <- function(model, x, ...) {
  x <- .indid_array(x)
  scores <- lapply(model$members, cpd_scores, x = x)
  aperm(array(unlist(scores), c(dim(scores[[1]]), length(scores))), c(3, 1, 2))
}

#' @export
print.cpd_ensemble <- function(x, ...) {
  types <- vapply(x$members, function(m) {
    inner <- if (inherits(m, "cpd_calibrated")) m$model else m
    label <- if (inherits(inner, "indid")) toupper(inner$settings$loss) else class(inner)[1]
    if (inherits(m, "cpd_calibrated")) paste0(label, " (calibrated)") else label
  }, character(1))
  counts <- table(types)
  cat(sprintf("Ensemble of %d change point models: %s\n", length(x$members),
              paste(sprintf("%d %s", as.integer(counts), names(counts)), collapse = ", ")))
  invisible(x)
}

# Aggregation ------------------------------------------------------------------------------------

#' @title WWAggr and naive aggregations of ensemble scores
#' @description Aggregates the change point scores of the members of an
#' ensemble ([cpd_scores()] of a [cpd_ensemble()]) into one score per time
#' step. WWAggr (Stepikin et al., 2025) compares, at every step, the scores of
#' all members in a "future" window of the last `window_size` steps with those
#' in an anchor ("history") window, by the Wasserstein distance of the two
#' samples (the window flattened over steps and members) or by the maximum
#' mean discrepancy; the naive aggregations take the mean, minimum, maximum,
#' median or a quantile of the members at every step.
#' @param scores Scores of the members: an array `[member, sequence, time]` or
#' a matrix `[member, time]` (one sequence).
#' @param method `"wasserstein"` (WWAggr, default), `"mmd"`, `"mean"`,
#' `"min"`, `"max"`, `"median"` or `"quantile"`.
#' @param window_size Size of the windows (distance methods).
#' @param anchor Anchor window: `"prev"` (default, the `window_size` steps
#' before the future window, as Algorithm 1 of the paper), `"start"` (the
#' first `window_size` steps of the sequence) or `"combined"` (the first
#' `window_size %/% 2` steps followed by the `window_size %/% 2` steps before
#' the last ones; the future window then has `2 * (window_size %/% 2)` steps).
#' @param p Order of the Wasserstein distance.
#' @param kernel Kernel of the MMD: `"rbf"` or `"multiscale"`.
#' @param bandwidth Bandwidths of the MMD kernel. `NULL` uses those of the
#' reference: 10, 15, 20 and 50 (rbf) or 0.2, 0.5, 0.9 and 1.3 (multiscale).
#' @param q Quantile of the `"quantile"` method (the median is 0.5, the
#' minimum 0 and the maximum 1; linear interpolation as `torch.quantile`).
#' @details The steps where some member has no score (the first steps of the
#' representation models) are left out, as in the reference code, and get
#' `NA`. For the distance methods, the score of the `i`-th remaining step
#' (`i > window_size`) compares the future window of steps `i - window_size`
#' to `i - 1` with the anchor window, and the first `window_size` scores are
#' 0 (`anchor_window_detector_batch` of the reference, `src/ensembles/
#' distances.py`). For `"prev"`, the anchor starts at `max(1, i - 2 *
#' window_size)`.
#'
#' The 1-Wasserstein distance of two samples of the same size is the mean
#' absolute difference of their order statistics, and the p-Wasserstein
#' distance the p-th root of the mean of their p-th powers. The MMD treats the
#' steps of a window as samples and the members as dimensions; it is the mean
#' of `k(x, x') + k(y, y') - 2 k(x, y')` over all pairs, with the sum of the
#' kernels of all bandwidths: `exp(-d / (2 a))` (rbf) or `a^2 / (a^2 + d)`
#' (multiscale), `d` the squared distance. With calibrated members, the
#' scores are in `[0, 1]` and so is the 1-Wasserstein distance, so that a
#' threshold around 0.5 works without tuning (Section IV of the paper).
#' @inherit dfr_wwaggr references
#' @return A matrix `[sequence, time]` of aggregated scores, or a vector for
#' one sequence.
#' @examples
#' set.seed(1)
#' # 10 members, 2 sequences of 30 steps; the change is at step 16.
#' scores <- array(stats::runif(10 * 2 * 30, 0, 0.2), c(10, 2, 30))
#' scores[, , 16:30] <- scores[, , 16:30] + 0.7
#' round(wwaggr(scores, window_size = 3)[1, 12:22], 2)
#' round(wwaggr(scores, method = "mean")[1, 12:22], 2)
#' @export
wwaggr <- function(scores, method = c("wasserstein", "mmd", "mean", "min", "max", "median", "quantile"),
                   window_size = 3, anchor = c("prev", "start", "combined"), p = 1,
                   kernel = c("rbf", "multiscale"), bandwidth = NULL, q = 0.5) {
  method <- match.arg(method)
  anchor <- match.arg(anchor)
  kernel <- match.arg(kernel)
  settings <- .wwaggr_settings(method, window_size, anchor, p, kernel, bandwidth, q)
  single <- is.matrix(scores)
  if (single) scores <- array(scores, c(nrow(scores), 1L, ncol(scores)))
  if (!is.array(scores) || length(dim(scores)) != 3L || !is.numeric(scores)) {
    stop("wwaggr: scores must be an array [member, sequence, time] or a matrix [member, time]", call. = FALSE)
  }
  len <- dim(scores)[3]
  missing <- apply(is.na(scores), 3, any)
  first <- if (all(missing)) len + 1L else which(!missing)[1]
  if (any(missing[first:len][first <= len])) {
    stop("wwaggr: the scores have missing values after the first complete step", call. = FALSE)
  }
  out <- matrix(NA_real_, dim(scores)[2], len)
  if (first <= len) {
    cols <- first:len
    for (s in seq_len(dim(scores)[2])) {
      out[s, cols] <- .wwaggr_sequence(matrix(scores[, s, cols], dim(scores)[1]), settings)
    }
  }
  if (single) out[1, ] else out
}

#' @noRd
.wwaggr_settings <- function(method, window_size, anchor, p, kernel, bandwidth, q) {
  .check_positive_integer(window_size, "window_size", min_value = 1L)
  .check_number(p, "p", min_value = 1)
  .check_probability(q, "q")
  if (anchor == "combined" && window_size < 2) {
    stop("wwaggr: the combined anchor needs window_size >= 2", call. = FALSE)
  }
  if (is.null(bandwidth)) {
    bandwidth <- if (kernel == "rbf") c(10, 15, 20, 50) else c(0.2, 0.5, 0.9, 1.3)
  }
  if (!is.numeric(bandwidth) || any(bandwidth <= 0)) {
    stop("wwaggr: bandwidth must be positive numbers", call. = FALSE)
  }
  list(method = method, window_size = as.integer(window_size), anchor = anchor, p = p, kernel = kernel,
       bandwidth = bandwidth, q = q)
}

# Aggregated scores of one sequence, P [member, time].
#' @noRd
.wwaggr_sequence <- function(P, settings) {
  len <- ncol(P)
  if (!(settings$method %in% c("wasserstein", "mmd"))) {
    return(vapply(seq_len(len), function(t) .wwaggr_naive(P[, t], settings), numeric(1)))
  }
  out <- numeric(len)
  w <- settings$window_size
  if (len > w) {
    for (t in (w + 1L):len) out[t] <- .wwaggr_distance_at(P, t, settings)
  }
  out
}

#' @noRd
.wwaggr_naive <- function(values, settings) {
  switch(settings$method,
         mean = mean(values),
         min = min(values),
         max = max(values),
         median = stats::quantile(values, 0.5, names = FALSE, type = 7),
         quantile = stats::quantile(values, settings$q, names = FALSE, type = 7))
}

# Windows of the score of position t (1-based) of P [member, time]: the future
# window ends at t - 1 (the reference scores position future_idx with the
# window [future_idx - w, future_idx)).
#' @noRd
.wwaggr_windows <- function(t, settings) {
  w <- settings$window_size
  idx <- t - 1L
  switch(settings$anchor,
         start = list(anchor = seq_len(w), future = (idx - w) + seq_len(w)),
         prev = {
           start <- max(0L, idx - 2L * w)
           list(anchor = start + seq_len(w), future = (idx - w) + seq_len(w))
         },
         combined = {
           h <- w %/% 2L
           list(anchor = c(seq_len(h), (idx - 2L * h) + seq_len(h)), future = (idx - 2L * h) + seq_len(2L * h))
         })
}

#' @noRd
.wwaggr_distance_at <- function(P, t, settings) {
  windows <- .wwaggr_windows(t, settings)
  a <- P[, windows$anchor, drop = FALSE]
  f <- P[, windows$future, drop = FALSE]
  if (settings$method == "wasserstein") {
    diff <- abs(sort(as.vector(a)) - sort(as.vector(f)))
    return(if (settings$p == 1) mean(diff) else mean(diff^settings$p)^(1 / settings$p))
  }
  .wwaggr_mmd(t(a), t(f), settings$kernel, settings$bandwidth)
}

# MMD_batch of the reference for samples x [n, d] and y [n, d].
#' @noRd
.wwaggr_mmd <- function(x, y, kernel, bandwidth) {
  sq <- function(u, v) outer(rowSums(u^2), rowSums(v^2), "+") - 2 * tcrossprod(u, v)
  dxx <- sq(x, x)
  dyy <- sq(y, y)
  dxy <- sq(x, y)
  k <- function(d) {
    total <- 0
    for (a in bandwidth) total <- total + if (kernel == "rbf") exp(-0.5 * d / a) else a^2 / (a^2 + d)
    total
  }
  mean(k(dxx) + k(dyy) - 2 * k(dxy))
}

# Stream detector ----------------------------------------------------------------------------------

#' @title WWAggr ensemble drift detector
#' @description Monitors a stream with an ensemble of change point models
#' ([cpd_ensemble()], preferably calibrated with [cpd_calibrate()]): every
#' member scores the observations as they arrive, the scores are aggregated
#' by WWAggr (Stepikin et al., 2025) or by a naive aggregation ([wwaggr()]),
#' and a drift is reported when the aggregated score exceeds `threshold`.
#' Since the members are trained on sequences of the monitored features, it
#' detects changes of their distribution (**virtual concept drift**).
#' @param ensemble A `cpd_ensemble`.
#' @param threshold Alarm threshold on the aggregated score. With calibrated
#' members and the 1-Wasserstein distance, the paper finds that a threshold
#' near 0.5 is close to the best one.
#' @param method,window_size,anchor,p,kernel,bandwidth,q Aggregation, as in
#' [wwaggr()].
#' @param target_feat Names of the columns fed to the members, in the order of
#' training. `NULL` (default) uses the feature names of the ensemble, or every
#' numeric column when it has none.
#' @param horizon If set, the members and the aggregation restart every
#' `horizon` observations, so that the stream is read as consecutive sequences
#' of the training length. `NULL` (default) restarts them only after a drift.
#' @details The members score each observation online, as on whole
#' sequences: the LSTM models ([indid()]) update their state; the
#' representation models ([tscp2()], [ts2vec()]) compare the embeddings of the
#' last two windows of `window` observations (every `step` observations, with
#' the exponential smoothing `alpha`), so they have no score for the first
#' `2 * window - 1` observations of a sequence. The aggregation starts when all
#' members have a score, and its score at an observation compares the window of
#' the `window_size` previous scores with the anchor (see [wwaggr()]). After a
#' drift, the members and the aggregation restart. Rows with missing values are
#' skipped.
#' @references Stepikin, A., Romanenkova, E. and Zaytsev, A. (2025). WWAggr: A
#' window Wasserstein-based aggregation for ensemble change point detection.
#' *2025 IEEE International Conference on Data Mining (ICDM)*, 1535-1544.
#' <doi:10.1109/ICDM65498.2025.00163>
#' @return `dfr_wwaggr` object. After `fit()`, `drifter_output` holds, for
#' every observation, the aggregated score (`score`, `NA` when there is none)
#' and whether a drift was reported (`drift`).
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' make <- function(n) {
#'   change <- sample(5:24, n, replace = TRUE)
#'   change[seq_len(n / 2)] <- 25
#'   x <- t(vapply(change, function(c) stats::rnorm(24, ifelse(seq_len(24) < c, 0, 3)), numeric(24)))
#'   list(x = x, labels = t(vapply(change, function(c) (seq_len(24) >= c) * 1, numeric(24))))
#' }
#' train <- make(96)
#' ensemble <- cpd_ensemble_fit(train$x, train$labels, model = "bce", n_models = 3, epochs = 10,
#'                              learning_rate = 0.01, seed = 1)
#' ensemble <- cpd_calibrate(ensemble, train$x, train$labels)
#' stream <- data.frame(value = c(stats::rnorm(15), stats::rnorm(15, 3)))
#' detector <- fit(dfr_wwaggr(ensemble, window_size = 2), stream)
#' which(detector$drifter_output$drift == 1)
#' @export
dfr_wwaggr <- function(ensemble, threshold = 0.5,
                       method = c("wasserstein", "mmd", "mean", "min", "max", "median", "quantile"),
                       window_size = 3, anchor = c("prev", "start", "combined"), p = 1,
                       kernel = c("rbf", "multiscale"), bandwidth = NULL, q = 0.5, target_feat = NULL,
                       horizon = NULL) {
  if (!inherits(ensemble, "cpd_ensemble")) {
    stop("dfr_wwaggr: ensemble must be a cpd_ensemble", call. = FALSE)
  }
  method <- match.arg(method)
  anchor <- match.arg(anchor)
  kernel <- match.arg(kernel)
  .check_number(threshold, "threshold")
  if (!is.null(horizon)) .check_positive_integer(horizon, "horizon", min_value = 1L)
  if (is.null(target_feat)) target_feat <- ensemble$features
  obj <- mv_dist_based(target_feat = target_feat)
  obj$ensemble <- ensemble
  obj$settings <- .wwaggr_settings(method, window_size, anchor, p, kernel, bandwidth, q)
  obj$state <- list(threshold = threshold, horizon = horizon, steps = 0L, sequence = .wwaggr_sequence_state(ensemble))
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_wwaggr", class(obj))
  obj
}

# State of the members and of the aggregation at the start of a sequence.
#' @noRd
.wwaggr_sequence_state <- function(ensemble) {
  list(members = vector("list", length(ensemble$members)), head = NULL, tail = NULL, count = 0L)
}

#' @export
update_state.dfr_wwaggr <- function(obj, value, ...) {
  state <- obj$state
  row <- .select_features(value, obj$target_feat)
  if (length(row) != obj$ensemble$dim) {
    stop(sprintf("dfr_wwaggr: the ensemble has %d feature(s) and the stream %d", obj$ensemble$dim, length(row)),
         call. = FALSE)
  }
  obj$last_drifter_output <- NA_real_
  if (anyNA(row)) {
    return(list(obj = obj, drift = FALSE))
  }
  if (!is.null(state$horizon) && state$steps >= state$horizon) {
    state$sequence <- .wwaggr_sequence_state(obj$ensemble)
    state$steps <- 0L
  }
  sequence <- state$sequence
  scores <- numeric(length(obj$ensemble$members))
  for (k in seq_along(obj$ensemble$members)) {
    step <- .member_stream_step(obj$ensemble$members[[k]], unname(row), sequence$members[[k]])
    sequence$members[k] <- list(step$state)
    scores[k] <- step$score
  }
  state$steps <- state$steps + 1L
  score <- NA_real_
  if (!anyNA(scores)) {
    sequence <- .wwaggr_push(sequence, scores, obj$settings)
    score <- .wwaggr_stream_score(sequence, obj$settings)
  }
  drift <- !is.na(score) && (score > state$threshold)
  if (drift) {
    sequence <- .wwaggr_sequence_state(obj$ensemble)
    state$steps <- 0L
    obj$drifted <- TRUE
  }
  state$sequence <- sequence
  obj$state <- state
  obj$last_drifter_output <- c(score = score, drift = as.numeric(drift))
  list(obj = obj, drift = drift)
}

# Keeps the first window_size score vectors of the sequence (anchor "start"
# and "combined") and the last 2 * window_size + 1 ones.
#' @noRd
.wwaggr_push <- function(sequence, scores, settings) {
  w <- settings$window_size
  sequence$count <- sequence$count + 1L
  if (is.null(sequence$head) || ncol(sequence$head) < w) sequence$head <- cbind(sequence$head, scores)
  tail <- cbind(sequence$tail, scores)
  keep <- 2L * w + 1L
  if (ncol(tail) > keep) tail <- tail[, (ncol(tail) - keep + 1L):ncol(tail), drop = FALSE]
  sequence$tail <- tail
  sequence
}

#' @noRd
.wwaggr_stream_score <- function(sequence, settings) {
  latest <- sequence$tail[, ncol(sequence$tail)]
  if (!(settings$method %in% c("wasserstein", "mmd"))) return(.wwaggr_naive(latest, settings))
  t <- sequence$count
  w <- settings$window_size
  if (t <= w) return(0)
  windows <- .wwaggr_windows(t, settings)
  # Positions are absolute in the sequence; map them to the stored columns.
  first_tail <- t - ncol(sequence$tail) + 1L
  column <- function(pos) {
    if (pos <= ncol(sequence$head)) sequence$head[, pos] else sequence$tail[, pos - first_tail + 1L]
  }
  gather <- function(positions) matrix(vapply(positions, column, numeric(nrow(sequence$tail))), ncol = length(positions))
  a <- gather(windows$anchor)
  f <- gather(windows$future)
  if (settings$method == "wasserstein") {
    diff <- abs(sort(as.vector(a)) - sort(as.vector(f)))
    return(if (settings$p == 1) mean(diff) else mean(diff^settings$p)^(1 / settings$p))
  }
  .wwaggr_mmd(t(a), t(f), settings$kernel, settings$bandwidth)
}

# One observation for a member: list(score, state); state NULL at the start of
# a sequence.
#' @noRd
.member_stream_step <- function(member, row, state) {
  if (inherits(member, "cpd_calibrated")) {
    step <- .member_stream_step(member$model, row, state)
    if (!is.na(step$score)) step$score <- .calibration_apply(member$calibrator, step$score)
    return(step)
  }
  if (inherits(member, "indid")) {
    step <- .indid_stream_step(member, row, state)
    return(list(score = step$probability, state = step$state))
  }
  settings <- member$settings
  w <- settings$window
  if (is.null(state)) state <- list(buffer = NULL, count = 0L, raw = NA_real_, ema = NA_real_)
  state$buffer <- rbind(state$buffer, row)
  if (nrow(state$buffer) > 2L * w) state$buffer <- state$buffer[-1, , drop = FALSE]
  state$count <- state$count + 1L
  k <- state$count - 2L * w
  if (k < 0L) return(list(score = NA_real_, state = state))
  if (k %% settings$step == 0L) {
    windows <- array(0, c(2L, w, ncol(state$buffer)))
    windows[1, , ] <- state$buffer[seq_len(w), ]
    windows[2, , ] <- state$buffer[w + seq_len(w), ]
    z <- if (inherits(member, "tscp2")) {
      .tscp_encode(member$params, member$buffers, settings, windows, FALSE)$z
    } else {
      e <- .ts2vec_encode(member$params, member$buffers, settings, windows, FALSE)$z
      rbind(apply(e[, seq_len(w), drop = FALSE], 1, max), apply(e[, w + seq_len(w), drop = FALSE], 1, max))
    }
    sim <- sum(z[1, ] * z[2, ]) / (max(sqrt(sum(z[1, ]^2)), 1e-6) * max(sqrt(sum(z[2, ]^2)), 1e-6))
    state$raw <- 1 - max(sim, 0)
  }
  state$ema <- if (is.na(state$ema)) state$raw else settings$alpha * state$raw + (1 - settings$alpha) * state$ema
  list(score = state$ema, state = state)
}

#' @export
fit.dfr_wwaggr <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  .fit_row_stream(obj, data, output_names = c("score", "drift"))
}

#' @export
reset_state.dfr_wwaggr <- function(obj) {
  obj$drifted <- FALSE
  obj$state$sequence <- .wwaggr_sequence_state(obj$ensemble)
  obj$state$steps <- 0L
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  obj
}
