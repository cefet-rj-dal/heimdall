#' @title Maximum Concept Discrepancy drift detector (MCD-DD)
#' @description Unsupervised online drift detector of Wan, Liang and Yoon
#' (2024). A sliding window of `window_size` observations is split into
#' `n_sub` sub-windows. Sets of `m` points sampled from a sub-window are
#' embedded by a small neural network as the mean of the point encodings (the
#' concept representation), and the maximum concept discrepancy (MCD) between
#' two sub-windows is the distance between their representations. Every time a
#' new sub-window arrives, its MCD to the previous sub-window is compared with
#' a threshold; then the encoder is trained on the window by contrastive
#' learning: sets from the same sub-window are pulled together, and sets with
#' noise added (weak negatives, same sub-window; strong negatives, first and
#' last sub-windows) pushed apart. The threshold is the `percentile` quantile
#' of the MCD between sets of the same sub-window. Since it monitors the
#' distribution of the observed features, it is used for **virtual concept
#' drift**.
#' @param target_feat Names of the columns to be monitored. `NULL` (the default)
#' monitors every numeric column.
#' @param window_size Size of the sliding window (a multiple of `n_sub`).
#' @param n_sub Number of sub-windows in a window; the window slides by one
#' sub-window (`window_size / n_sub` observations).
#' @param m Number of points in a sample set.
#' @param k Number of sample sets drawn from a sub-window (even).
#' @param hidden_size,output_size Sizes of the hidden and output layers of the
#' encoder.
#' @param eps_small,eps_big Standard deviations of the Gaussian noise of the
#' weak and strong negative sets.
#' @param learning_rate Learning rate of the Adam optimiser.
#' @param epochs Optimisation steps per window.
#' @param temperature Temperature of the InfoNCE-like loss.
#' @param lambda Weight of the gradient penalty that keeps the encoder close to
#' 1-Lipschitz.
#' @param percentile Quantile of the within-sub-window MCD used as threshold.
#' @param seed Seed of the private random stream (initial weights, sampling and
#' noise). `NULL` draws one from the session.
#' @details The detector follows the reference code
#' (<https://github.com/LiangYiAnita/mcd-dd>, `mcd.py`, `encoder.py` and
#' `main.py`), which differs from the paper in some points:
#'
#' * The encoder is `Linear - ReLU - Linear - Sigmoid`.
#' * Sample sets are drawn with replacement. The positive term of a sub-window
#'   is the mean MCD over all pairs of its `k` sets.
#' * The weak negative term compares the first `k / 2` sets with the other
#'   `k / 2` plus noise, and the strong one the sets of the first sub-window
#'   with those of the last one plus noise. Each is the mean MCD over the pairs
#'   of different indices.
#' * The loss is `log(sum(exp(pos / T)) / (sum(exp(pos / T)) + sum(exp(neg / T))))`
#'   plus `lambda` times the gradient penalty.
#' * The gradient penalty is computed on the sets of the last sub-window. The
#'   norm of the input gradient of the summed encodings is taken over the points
#'   of a set, for every set and feature, as in the reference.
#' * The MCD of two sub-windows in the test is the mean distance between the
#'   representations of their `k` sets, over pairs of different indices.
#' * The first window only trains. From then on every window is tested with
#'   the encoder and threshold of the previous one, and then trained.
#'
#' The data are not scaled, as in the reference; the noise levels refer to the
#' units of the data. The network, its gradients (including the second
#' derivatives of the gradient penalty) and Adam are written in R.
#' @references Wan, K., Liang, Y. and Yoon, S. (2024). Online drift detection
#' with maximum concept discrepancy. *Proceedings of the 30th ACM SIGKDD
#' Conference on Knowledge Discovery and Data Mining*, 2924-2935.
#' <doi:10.1145/3637528.3672016>
#' @return `dfr_mcd` object. After `fit()`, `drifter_output` holds, on the last
#' observation of every tested sub-window, its MCD to the previous sub-window
#' (`mcd`) and the threshold (`threshold`); `obj$distances` has one row per
#' tested sub-window with its MCD to each earlier sub-window of the window
#' (the heatmaps of the paper).
#' @seealso [dfr_mcdd()] is a different detector (mean comparison).
#' @examples
#' \donttest{
#' library(daltoolbox)
#' library(heimdall)
#' # Mixture of two Gaussians whose weight changes at row 4001.
#' set.seed(1)
#' mixture <- function(n, p) {
#'   wide <- stats::runif(n) > p
#'   matrix(stats::rnorm(n * 3, 20, ifelse(wide, 50, 10)), n)
#' }
#' stream <- as.data.frame(rbind(mixture(4000, 0.2), mixture(2000, 0.8)))
#' detector <- dfr_mcd(window_size = 1000, m = 30, seed = 1)
#' model <- fit(detector, stream)
#' output <- model$drifter_output
#' which(output$mcd > output$threshold)
#' }
#' @export
dfr_mcd <- function(target_feat = NULL, window_size = 3000, n_sub = 10, m = 30, k = 10, hidden_size = 100,
                    output_size = 100, eps_small = 1, eps_big = 10, learning_rate = 0.005, epochs = 1,
                    temperature = 0.1, lambda = 1, percentile = 0.95, seed = NULL) {
  .check_target_feat(target_feat, multiple = TRUE)
  .check_positive_integer(n_sub, "n_sub", min_value = 2L)
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  if ((window_size %% n_sub) != 0) {
    stop("dfr_mcd: window_size must be a multiple of n_sub", call. = FALSE)
  }
  .check_positive_integer(m, "m", min_value = 1L)
  .check_positive_integer(k, "k", min_value = 2L)
  if ((k %% 2) != 0) {
    stop("dfr_mcd: k must be even", call. = FALSE)
  }
  .check_positive_integer(hidden_size, "hidden_size", min_value = 1L)
  .check_positive_integer(output_size, "output_size", min_value = 1L)
  .check_number(eps_small, "eps_small", min_value = 0)
  .check_number(eps_big, "eps_big", min_value = 0)
  .check_number(learning_rate, "learning_rate", min_value = 0)
  .check_positive_integer(epochs, "epochs", min_value = 1L)
  .check_number(temperature, "temperature", min_value = 0)
  if (temperature == 0) {
    stop("dfr_mcd: temperature must be positive", call. = FALSE)
  }
  .check_number(lambda, "lambda", min_value = 0)
  .check_probability(percentile, "percentile")
  obj <- mv_dist_based(target_feat = target_feat)
  obj$state <- list(settings = list(window_size = window_size, n_sub = n_sub, m = m, k = k, hidden_size = hidden_size,
                                    output_size = output_size, eps_small = eps_small, eps_big = eps_big,
                                    learning_rate = learning_rate, epochs = epochs, temperature = temperature,
                                    lambda = lambda, percentile = percentile),
                    seed = seed, rng = .rng_state(seed), features = NULL, window = NULL, pending = NULL,
                    filled = 0L, n = 0L)
  obj$net <- NULL
  obj$threshold <- NULL
  obj$distances <- NULL
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_mcd", class(obj))
  return(obj)
}

#' @export
update_state.dfr_mcd <- function(obj, value, ...) {
  state <- obj$state
  settings <- state$settings
  row <- .select_features(value, obj$target_feat)
  if (is.null(state$features)) {
    state$features <- names(row)
  } else if (!identical(names(row), state$features)) {
    stop("dfr_mcd: the monitored features changed during the stream", call. = FALSE)
  }
  state$n <- state$n + 1L
  if (anyNA(row)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  slide <- settings$window_size %/% settings$n_sub
  needed <- if (is.null(state$window)) settings$window_size else slide
  if (is.null(state$pending)) {
    state$pending <- matrix(NA_real_, needed, length(row))
    state$filled <- 0L
  }
  state$filled <- state$filled + 1L
  state$pending[state$filled, ] <- unname(row)
  if (state$filled < needed) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  if (is.null(state$window)) {
    state$window <- state$pending
  } else {
    state$window <- rbind(state$window[-seq_len(slide), , drop = FALSE], state$pending)
  }
  state$pending <- NULL
  window <- state$window
  drift <- FALSE
  step <- .with_rng(state$rng, function() {
    tested <- NULL
    if (!is.null(obj$net)) {
      tested <- .mcd_test(obj$net, window, settings)
    }
    trained <- .mcd_train(if (is.null(obj$net)) .mcd_net(ncol(window), settings) else obj$net, window, settings)
    list(tested = tested, trained = trained)
  })
  state$rng <- step$rng
  if (!is.null(step$value$tested)) {
    distances <- step$value$tested
    mcd <- distances[length(distances)]
    obj$last_drifter_output <- c(mcd = mcd, threshold = obj$threshold)
    obj$distances <- rbind(obj$distances, c(observation = state$n, distances), deparse.level = 0)
    drift <- mcd > obj$threshold
  }
  obj$net <- step$value$trained$net
  obj$threshold <- step$value$trained$threshold
  obj$state <- state
  if (drift) {
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = drift))
}

#' @export
fit.dfr_mcd <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  obj <- .fit_row_stream(obj, data)
  if (!is.null(obj$distances)) {
    colnames(obj$distances) <- c("observation", paste0("sub", seq_len(ncol(obj$distances) - 1L)))
  }
  return(obj)
}

#' @export
reset_state.dfr_mcd <- function(obj) {
  obj$drifted <- FALSE
  obj$state$window <- NULL
  obj$state$pending <- NULL
  obj$state$rng <- .rng_state(obj$state$seed)
  obj$net <- NULL
  obj$threshold <- NULL
  obj$distances <- NULL
  return(obj)
}

# Network ---------------------------------------------------------------------------------

# Encoder Linear(d, h) - ReLU - Linear(h, o) - Sigmoid with the default
# initialisation of torch.nn.Linear (uniform within +-1/sqrt(fan_in)), and the
# state of Adam.
#' @noRd
.mcd_net <- function(d, settings) {
  h <- settings$hidden_size
  o <- settings$output_size
  uniform <- function(n, fan_in) stats::runif(n, -1 / sqrt(fan_in), 1 / sqrt(fan_in))
  params <- list(W1 = matrix(uniform(h * d, d), h, d, byrow = TRUE), b1 = uniform(h, d),
                 W2 = matrix(uniform(o * h, h), o, h, byrow = TRUE), b2 = uniform(o, h))
  zeros <- lapply(params, function(p) p * 0)
  list(params = params, adam = list(m = zeros, v = zeros, t = 0L))
}

#' @noRd
.mcd_forward <- function(params, x) {
  a1 <- x %*% t(params$W1) + rep(params$b1, each = nrow(x))
  z1 <- pmax(a1, 0)
  a2 <- z1 %*% t(params$W2) + rep(params$b2, each = nrow(x))
  list(a1 = a1, z1 = z1, y = 1 / (1 + exp(-a2)))
}

# Concept representations: mean encoding of each set (rows grouped by set).
#' @noRd
.mcd_embed <- function(y, k) {
  rowsum(y, rep(seq_len(k), each = nrow(y) / k), reorder = FALSE) / (nrow(y) / k)
}

# Gradients of the parameters given the gradient of the representations.
#' @noRd
.mcd_backward <- function(params, x, cache, grad_e, k) {
  m <- nrow(x) / k
  grad_y <- grad_e[rep(seq_len(k), each = m), , drop = FALSE] / m
  grad_a2 <- grad_y * cache$y * (1 - cache$y)
  grad_a1 <- (grad_a2 %*% params$W2) * (cache$a1 > 0)
  list(W1 = t(grad_a1) %*% x, b1 = colSums(grad_a1), W2 = t(grad_a2) %*% cache$z1, b2 = colSums(grad_a2))
}

#' @noRd
.mcd_add <- function(a, b) {
  for (name in names(b)) {
    a[[name]] <- a[[name]] + b[[name]]
  }
  a
}

# Mean distance over the pairs i < l of the rows of e, and its gradient.
#' @noRd
.mcd_within <- function(e) {
  k <- nrow(e)
  diff <- e[rep(seq_len(k), times = k), , drop = FALSE] - e[rep(seq_len(k), each = k), , drop = FALSE]
  dist <- matrix(sqrt(rowSums(diff^2)), k, k)
  upper <- upper.tri(dist)
  count <- sum(upper)
  # d dist[i, l] / d e_i = (e_i - e_l) / dist[i, l]; both halves of the pair.
  weight <- ifelse(upper & (dist > 0), 1 / (dist * count), 0)
  weight <- weight + t(weight)
  grad <- e * rowSums(weight) - weight %*% e
  list(value = mean(dist[upper]), pairs = dist[upper], grad = grad)
}

# Mean distance between rows of e1 and e2 over pairs of different indices.
#' @noRd
.mcd_between <- function(e1, e2) {
  k1 <- nrow(e1)
  k2 <- nrow(e2)
  diff <- e1[rep(seq_len(k1), times = k2), , drop = FALSE] - e2[rep(seq_len(k2), each = k1), , drop = FALSE]
  dist <- matrix(sqrt(rowSums(diff^2)), k1, k2)
  keep <- matrix(TRUE, k1, k2)
  diag(keep) <- FALSE
  count <- sum(keep)
  weight <- ifelse(keep & (dist > 0), 1 / (dist * count), 0)
  list(value = mean(dist[keep]), grad1 = e1 * rowSums(weight) - weight %*% e2,
       grad2 = e2 * colSums(weight) - t(weight) %*% e1)
}

# Gradient penalty on the sets of x (rows grouped by set) and its gradient.
# G = d sum(encodings) / d x per point; for every set and feature the norm of
# G over the points of the set, n; penalty mean((n - 1)^2).
#' @noRd
.mcd_penalty <- function(params, x, cache, k) {
  m <- nrow(x) / k
  relu <- (cache$a1 > 0) * 1
  s <- cache$y * (1 - cache$y)
  u <- s %*% params$W2
  g <- (relu * u) %*% params$W1
  set <- rep(seq_len(k), each = m)
  norms <- sqrt(rowsum(g^2, set, reorder = FALSE) + 1e-12)
  value <- mean((norms - 1)^2)
  v <- (2 * (norms - 1) / (length(norms) * norms))[set, , drop = FALSE] * g
  q <- v %*% t(params$W1)
  qr <- q * relu
  grad_a2 <- (qr %*% t(params$W2)) * cache$y * (1 - cache$y) * (1 - 2 * cache$y)
  grad_a1 <- (grad_a2 %*% params$W2) * relu
  list(value = value,
       grad = list(W1 = t(relu * u) %*% v + t(grad_a1) %*% x, b1 = colSums(grad_a1),
                   W2 = t(s) %*% qr + t(grad_a2) %*% cache$z1, b2 = colSums(grad_a2)))
}

# Random draws ------------------------------------------------------------------------------

# Rows of a sub-window for k sets of m points, drawn with replacement.
#' @noRd
.mcd_sets <- function(size, settings) {
  sample.int(size, settings$m * settings$k, replace = TRUE)
}

#' @noRd
.mcd_noise <- function(sets, d, sd) {
  matrix(stats::rnorm(sets * d, 0, sd), ncol = d, byrow = TRUE)
}

#' @noRd
.mcd_split <- function(window, n_sub) {
  size <- nrow(window) %/% n_sub
  lapply(seq_len(n_sub), function(j) window[(j - 1L) * size + seq_len(size), , drop = FALSE])
}

# Training and test -------------------------------------------------------------------------

# One window of training (mcd.py, MCD.train). `draws` replaces the random
# draws (for the comparison with the reference): sets (list per sub-window),
# weak (list of noise matrices per sub-window), strong (noise matrix) and
# threshold_sets (list per sub-window).
#' @noRd
.mcd_train <- function(net, window, settings, draws = NULL) {
  k <- settings$k
  m <- settings$m
  half <- k %/% 2
  subs <- .mcd_split(window, settings$n_sub)
  n_sub <- length(subs)
  d <- ncol(window)
  sets <- if (is.null(draws)) lapply(subs, function(s) .mcd_sets(nrow(s), settings)) else draws$sets
  samples <- lapply(seq_len(n_sub), function(j) subs[[j]][sets[[j]], , drop = FALSE])
  loss <- NA_real_
  for (epoch in seq_len(settings$epochs)) {
    params <- net$params
    weak_noise <- if (is.null(draws)) {
      lapply(seq_len(n_sub), function(j) .mcd_noise((k - half) * m, d, settings$eps_small))
    } else {
      draws$weak
    }
    strong_noise <- if (is.null(draws)) .mcd_noise(k * m, d, settings$eps_big) else draws$strong
    rows_altered <- half * m + seq_len((k - half) * m)
    caches <- lapply(samples, function(x) .mcd_forward(params, x))
    embeddings <- lapply(caches, function(cache) .mcd_embed(cache$y, k))
    grad_e <- lapply(embeddings, function(e) e * 0)
    positive <- lapply(embeddings, .mcd_within)
    altered <- lapply(seq_len(n_sub), function(j) {
      x <- samples[[j]][rows_altered, , drop = FALSE] + weak_noise[[j]]
      cache <- .mcd_forward(params, x)
      list(x = x, cache = cache, e = .mcd_embed(cache$y, k - half))
    })
    weak <- lapply(seq_len(n_sub), function(j) .mcd_between(embeddings[[j]][seq_len(half), , drop = FALSE],
                                                            altered[[j]]$e))
    strong_x <- samples[[n_sub]] + strong_noise
    strong_cache <- .mcd_forward(params, strong_x)
    strong <- .mcd_between(embeddings[[1]], .mcd_embed(strong_cache$y, k))
    # InfoNCE-like loss on the distances.
    temperature <- settings$temperature
    exp_pos <- exp(vapply(positive, `[[`, numeric(1), "value") / temperature)
    exp_neg <- exp(c(vapply(weak, `[[`, numeric(1), "value"), strong$value) / temperature)
    total_pos <- sum(exp_pos)
    total <- total_pos + sum(exp_neg)
    d_pos <- exp_pos / temperature * (1 / total_pos - 1 / total)
    d_neg <- -exp_neg / temperature / total
    grads <- lapply(params, function(p) p * 0)
    for (j in seq_len(n_sub)) {
      grad_e[[j]] <- grad_e[[j]] + d_pos[j] * positive[[j]]$grad
      grad_e[[j]][seq_len(half), ] <- grad_e[[j]][seq_len(half), ] + d_neg[j] * weak[[j]]$grad1
      grads <- .mcd_add(grads, .mcd_backward(params, altered[[j]]$x, altered[[j]]$cache, d_neg[j] * weak[[j]]$grad2,
                                             k - half))
    }
    grad_e[[1]] <- grad_e[[1]] + d_neg[n_sub + 1L] * strong$grad1
    grads <- .mcd_add(grads, .mcd_backward(params, strong_x, strong_cache, d_neg[n_sub + 1L] * strong$grad2, k))
    for (j in seq_len(n_sub)) {
      grads <- .mcd_add(grads, .mcd_backward(params, samples[[j]], caches[[j]], grad_e[[j]], k))
    }
    penalty <- .mcd_penalty(params, samples[[n_sub]], caches[[n_sub]], k)
    grads <- .mcd_add(grads, lapply(penalty$grad, function(g) settings$lambda * g))
    loss <- log(total_pos / total) + settings$lambda * penalty$value
    net <- .adam_step(net, grads, settings$learning_rate)
  }
  threshold_sets <- if (is.null(draws)) lapply(subs, function(s) .mcd_sets(nrow(s), settings)) else draws$threshold_sets
  pairs <- unlist(lapply(seq_len(n_sub), function(j) {
    cache <- .mcd_forward(net$params, subs[[j]][threshold_sets[[j]], , drop = FALSE])
    .mcd_within(.mcd_embed(cache$y, k))$pairs
  }))
  threshold <- stats::quantile(pairs, settings$percentile, names = FALSE, type = 7)
  list(net = net, threshold = threshold, loss = loss)
}

# MCD of every sub-window of the window to the last one (MCD.test).
#' @noRd
.mcd_test <- function(net, window, settings, sets = NULL) {
  subs <- .mcd_split(window, settings$n_sub)
  if (is.null(sets)) {
    sets <- lapply(subs, function(s) .mcd_sets(nrow(s), settings))
  }
  embeddings <- lapply(seq_along(subs), function(j) {
    .mcd_embed(.mcd_forward(net$params, subs[[j]][sets[[j]], , drop = FALSE])$y, settings$k)
  })
  last <- embeddings[[length(embeddings)]]
  vapply(embeddings[-length(embeddings)], function(e) .mcd_between(e, last)$value, numeric(1))
}
