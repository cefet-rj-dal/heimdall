#' @title InDiD change point detection model
#' @description Trains the sequence-to-sequence change point detector of
#' Romanenkova et al. (2022): a recurrent network (an LSTM layer and a linear
#' layer with sigmoid output) reads a sequence and gives, at every time step,
#' the probability that a change has already happened. The model is trained on
#' labelled sequences with the InDiD loss, a differentiable approximation of
#' the detection delay after a change and of the time to a false alarm before
#' it; with the binary cross-entropy (BCE); or with both in turn (`combined`).
#' @param x Training sequences: an array `[sequence, time, feature]`, a matrix
#' `[sequence, time]` (one feature) or a list of matrices `[time, feature]` of
#' the same size.
#' @param labels Labels of the training sequences, a matrix `[sequence, time]`
#' that is 0 before the change and 1 from the change on (all 0 when the sequence
#' has no change).
#' @param loss `"indid"` (default), `"bce"` or `"combined"` (BCE for half of
#' the epochs, then InDiD for the other half).
#' @param hidden_size Size of the LSTM hidden state.
#' @param T Length of the segment after the change used by the delay part of
#' the InDiD loss.
#' @param learning_rate Learning rate of Adam.
#' @param batch_size Number of sequences per batch.
#' @param epochs Largest number of epochs (for `"combined"`, split in two).
#' @param patience Epochs without a lower validation loss before training stops
#' (early stopping, only with validation data).
#' @param validation Optional validation data, `list(x = , labels = )`.
#' @param seed Seed of the private random stream (initial weights and order of
#' the batches). `NULL` draws one from the session.
#' @details The model and the training follow the reference code
#' (<https://github.com/romanenkova95/InDiD>: `utils/loss.py`,
#' `utils/core_models.py` (`BaseRnn`), `utils/cpd_models.py`, `train.py` and
#' the `configs/`):
#'
#' * The InDiD loss of a sequence with its first change at position `c` is
#'   `alpha * D(p[c:(c + T - 1)]) - D(p[1:(c - 1)])`, and `-D(p)` without a
#'   change, where `D(p) = sum_t t p_t prod_{i < t} (1 - p_i) + (n + 1)
#'   prod_i (1 - p_i)` is the expected stopping time on a segment of `n` steps.
#'   The loss of a batch is the mean over its sequences. `alpha` is
#'   `2 * B / T`, where `B` is the number of sequences in the batch: the
#'   reference uses `len(prob)` of the batch matrix, so `alpha` depends on the
#'   batch size, not on the sequence length.
#' * BCE is `torch.nn.BCELoss` (mean over all time steps, logarithms clamped at
#'   -100).
#' * The LSTM has one layer (the dropout of the reference configs has no effect
#'   with one layer). The weights are initialised as in torch: uniform within
#'   `+-1/sqrt(hidden_size)`.
#' * Adam with the defaults of torch, shuffled batches, and early stopping on the
#'   validation loss (mean over the validation batches weighted by their size,
#'   min_delta 0) as Lightning; the model of the last epoch is kept, as
#'   `train.py`. The reference validates on its test set.
#'
#' The network, the gradients (backpropagation through time) and Adam are
#' written in R. The defaults are those of the 1-D SyntheticNormal experiment
#' (`configs/synthetic_1D_seq2seq.yaml`).
#' @references Romanenkova, E., Stepikin, A., Morozov, M. and Zaytsev, A.
#' (2022). InDiD: Instant disorder detection via a principled neural network.
#' *Proceedings of the 30th ACM International Conference on Multimedia*,
#' 3152-3162. <doi:10.1145/3503161.3548182>
#' @return An `indid` object with the network (`params`), the settings and the
#' training and validation losses of every epoch (`history`).
#' @seealso [indid_predict()] for the probabilities of change of new sequences
#' and [dfr_indid()] to monitor a stream.
#' @examples
#' # Sequences of 32 steps around 1; half of them move to 6 at a random step.
#' set.seed(1)
#' make <- function(n) {
#'   change <- sample(2:32, n, replace = TRUE)
#'   change[seq_len(n / 2)] <- 33
#'   x <- t(vapply(change, function(c) stats::rnorm(32, ifelse(seq_len(32) < c, 1, 6)), numeric(32)))
#'   labels <- t(vapply(change, function(c) (seq_len(32) >= c) * 1, numeric(32)))
#'   list(x = x, labels = labels)
#' }
#' train <- make(200)
#' model <- indid(train$x, train$labels, T = 8, learning_rate = 0.02, epochs = 30, seed = 1)
#' test <- make(4)
#' round(indid_predict(model, test$x)[, 25:32], 2)
#' test$labels[, 25:32]
#' @export
indid <- function(x, labels, loss = c("indid", "bce", "combined"), hidden_size = 4, T = 32, learning_rate = 0.001,
                  batch_size = 64, epochs = 200, patience = 10, validation = NULL, seed = NULL) {
  loss <- match.arg(loss)
  .check_positive_integer(hidden_size, "hidden_size", min_value = 1L)
  .check_positive_integer(T, "T", min_value = 1L)
  .check_number(learning_rate, "learning_rate", min_value = 0)
  .check_positive_integer(batch_size, "batch_size", min_value = 1L)
  .check_positive_integer(epochs, "epochs", min_value = 1L)
  .check_positive_integer(patience, "patience", min_value = 1L)
  x <- .indid_array(x)
  labels <- .indid_labels(labels, dim(x))
  if (!is.null(validation)) {
    if (!is.list(validation) || is.null(validation$x) || is.null(validation$labels)) {
      stop("indid: validation must be list(x = , labels = )", call. = FALSE)
    }
    validation$x <- .indid_array(validation$x)
    validation$labels <- .indid_labels(validation$labels, dim(validation$x))
    if (dim(validation$x)[3] != dim(x)[3]) {
      stop("indid: the validation sequences have a different number of features", call. = FALSE)
    }
  }
  settings <- list(loss = loss, hidden_size = hidden_size, T = T, learning_rate = learning_rate,
                   batch_size = batch_size, epochs = epochs, patience = patience)
  result <- .with_rng(.rng_state(seed), function() {
    net <- .indid_net(dim(x)[3], hidden_size)
    if (loss == "combined") {
      first <- .indid_fit(net, x, labels, validation, utils::modifyList(settings, list(loss = "bce",
                                                                                         epochs = epochs %/% 2)))
      net <- .indid_reset_adam(first$net)
      second <- .indid_fit(net, x, labels, validation, utils::modifyList(settings, list(loss = "indid",
                                                                                          epochs = epochs %/% 2)))
      list(net = second$net, history = rbind(cbind(stage = "bce", first$history),
                                             cbind(stage = "indid", second$history)))
    } else {
      fitted <- .indid_fit(net, x, labels, validation, settings)
      list(net = fitted$net, history = cbind(stage = loss, fitted$history))
    }
  })$value
  features <- dimnames(x)[[3]]
  model <- list(params = result$net$params, settings = settings, history = result$history, features = features,
                dim = dim(x)[3], seq_len = dim(x)[2])
  class(model) <- "indid"
  return(model)
}

#' @title Probabilities of change of an InDiD model
#' @description Runs an [indid()] model on sequences and returns, for every
#' sequence and time step, the probability that a change has happened.
#' @param model An `indid` object.
#' @param x Sequences, in the formats accepted by [indid()].
#' @return A matrix `[sequence, time]` of probabilities.
#' @examples
#' set.seed(1)
#' x <- matrix(stats::rnorm(20 * 16, 1), 20)
#' labels <- matrix(0, 20, 16)
#' model <- indid(x, labels, T = 4, epochs = 2, seed = 1)
#' dim(indid_predict(model, x))
#' @export
indid_predict <- function(model, x) {
  if (!inherits(model, "indid")) {
    stop("indid_predict: model must be an indid object", call. = FALSE)
  }
  x <- .indid_array(x)
  if (dim(x)[3] != model$dim) {
    stop("indid_predict: the sequences have a different number of features from the training ones", call. = FALSE)
  }
  .indid_forward(model$params, x)$p
}

#' @export
print.indid <- function(x, ...) {
  epochs <- nrow(x$history)
  cat(sprintf("InDiD model (%s loss): LSTM with %d hidden units on %d feature(s), %d epoch(s)\n", x$settings$loss,
              x$settings$hidden_size, x$dim, epochs))
  invisible(x)
}

# Data -----------------------------------------------------------------------------------------

#' @noRd
.indid_array <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    sizes <- vapply(x, function(s) paste(dim(as.matrix(s)), collapse = "x"), character(1))
    if (length(unique(sizes)) != 1L) {
      stop("indid: the sequences must have the same length and number of features", call. = FALSE)
    }
    first <- as.matrix(x[[1]])
    x <- aperm(array(unlist(lapply(x, as.matrix)), c(dim(first), length(x)),
                     dimnames = list(NULL, colnames(first), NULL)), c(3, 1, 2))
  } else if (is.matrix(x) || is.data.frame(x)) {
    x <- as.matrix(x)
    x <- array(x, c(dim(x), 1L))
  }
  if (!is.array(x) || (length(dim(x)) != 3L) || !is.numeric(x)) {
    stop("indid: sequences must be a numeric array [sequence, time, feature], a matrix or a list of matrices",
         call. = FALSE)
  }
  if (anyNA(x)) {
    stop("indid: the sequences contain missing values", call. = FALSE)
  }
  storage.mode(x) <- "double"
  x
}

#' @noRd
.indid_labels <- function(labels, dims) {
  labels <- as.matrix(labels)
  if (!identical(as.integer(dim(labels)), as.integer(dims[1:2])) || anyNA(labels) || !all(labels %in% c(0, 1))) {
    stop("indid: labels must be a 0/1 matrix [sequence, time] matching the sequences", call. = FALSE)
  }
  storage.mode(labels) <- "double"
  labels
}

# Network ------------------------------------------------------------------------------------------

# LSTM (gates in the order of torch: input, forget, cell, output) and linear
# layer, initialised as torch.nn.LSTM and torch.nn.Linear(hidden, 1).
#' @noRd
.indid_net <- function(d, h) {
  bound <- 1 / sqrt(h)
  uniform <- function(n) stats::runif(n, -bound, bound)
  params <- list(W_ih = matrix(uniform(4 * h * d), 4 * h, d, byrow = TRUE),
                 W_hh = matrix(uniform(4 * h * h), 4 * h, h, byrow = TRUE),
                 b_ih = uniform(4 * h), b_hh = uniform(4 * h),
                 W_out = matrix(uniform(h), 1, h, byrow = TRUE), b_out = uniform(1))
  .indid_reset_adam(list(params = params))
}

#' @noRd
.indid_reset_adam <- function(net) {
  zeros <- lapply(net$params, function(p) p * 0)
  net$adam <- list(m = zeros, v = zeros, t = 0L)
  net
}

#' @noRd
.indid_sigmoid <- function(z) 1 / (1 + exp(-z))

# One LSTM step for a batch: x [B, d], h and c [B, h].
#' @noRd
.indid_cell <- function(params, x, h, c) {
  size <- ncol(h)
  z <- x %*% t(params$W_ih) + h %*% t(params$W_hh) + rep(params$b_ih + params$b_hh, each = nrow(x))
  i <- .indid_sigmoid(z[, seq_len(size), drop = FALSE])
  f <- .indid_sigmoid(z[, size + seq_len(size), drop = FALSE])
  g <- tanh(z[, 2 * size + seq_len(size), drop = FALSE])
  o <- .indid_sigmoid(z[, 3 * size + seq_len(size), drop = FALSE])
  c_new <- f * c + i * g
  tanh_c <- tanh(c_new)
  list(i = i, f = f, g = g, o = o, c = c_new, tanh_c = tanh_c, h = o * tanh_c)
}

# Forward pass over a batch x [B, L, d]; keeps what the backward pass needs.
#' @noRd
.indid_forward <- function(params, x, keep = FALSE) {
  n <- dim(x)[1]
  len <- dim(x)[2]
  size <- ncol(params$W_hh)
  h <- matrix(0, n, size)
  c <- matrix(0, n, size)
  p <- matrix(0, n, len)
  steps <- if (keep) vector("list", len) else NULL
  for (t in seq_len(len)) {
    step <- .indid_cell(params, matrix(x[, t, ], n), h, c)
    if (keep) steps[[t]] <- c(step, list(h_prev = h, c_prev = c))
    h <- step$h
    c <- step$c
    p[, t] <- .indid_sigmoid(h %*% t(params$W_out) + params$b_out)
  }
  list(p = p, steps = steps)
}

# Gradients of the parameters given the gradient of the loss with respect to
# the probabilities (backpropagation through time).
#' @noRd
.indid_backward <- function(params, x, forward, grad_p) {
  n <- dim(x)[1]
  len <- dim(x)[2]
  size <- ncol(params$W_hh)
  p <- forward$p
  grads <- lapply(params, function(q) q * 0)
  grad_a <- grad_p * p * (1 - p)
  dh_next <- matrix(0, n, size)
  dc_next <- matrix(0, n, size)
  for (t in rev(seq_len(len))) {
    step <- forward$steps[[t]]
    grads$W_out <- grads$W_out + t(grad_a[, t, drop = FALSE]) %*% step$h
    grads$b_out <- grads$b_out + sum(grad_a[, t])
    dh <- grad_a[, t] %*% params$W_out + dh_next
    dc <- dh * step$o * (1 - step$tanh_c^2) + dc_next
    dz <- cbind(dc * step$g * step$i * (1 - step$i), dc * step$c_prev * step$f * (1 - step$f),
                dc * step$i * (1 - step$g^2), dh * step$tanh_c * step$o * (1 - step$o))
    grads$W_ih <- grads$W_ih + t(dz) %*% matrix(x[, t, ], n)
    grads$W_hh <- grads$W_hh + t(dz) %*% step$h_prev
    grads$b_ih <- grads$b_ih + colSums(dz)
    dh_next <- dz %*% params$W_hh
    dc_next <- dc * step$f
  }
  grads$b_hh <- grads$b_ih
  grads
}

# Losses -------------------------------------------------------------------------------------------

# Expected stopping time D(p) of a segment and its gradient: D = 1 + sum_t S_t
# with S_t = prod_{i <= t} (1 - p_i), dD/dp_j = -S_{j-1} R_j with
# R_j = 1 + (1 - p_{j+1}) R_{j+1}, R_n = 1. The value is computed with the
# formula of loss.py (calculate_delays_).
#' @noRd
.indid_delay <- function(p) {
  n <- length(p)
  if (n == 0L) {
    return(list(value = 1, grad = numeric(0)))
  }
  survival <- cumprod(1 - p)
  before <- c(1, survival[-n])
  value <- sum(seq_len(n) * p * before) + (n + 1) * survival[n]
  rest <- numeric(n)
  rest[n] <- 1
  if (n > 1L) {
    for (j in rev(seq_len(n - 1L))) rest[j] <- 1 + (1 - p[j + 1L]) * rest[j + 1L]
  }
  list(value = value, grad = -before * rest)
}

# InDiD loss of a batch (loss.py, CPDLoss.forward) and its gradient.
#' @noRd
.indid_loss <- function(p, labels, T) {
  n <- nrow(p)
  len <- ncol(p)
  alpha <- 2 * n / T
  value <- 0
  grad <- matrix(0, n, len)
  for (s in seq_len(n)) {
    change <- which(labels[s, ] != labels[s, 1])
    if (length(change) == 0L) {
      fa <- .indid_delay(p[s, ])
      value <- value - fa$value
      grad[s, ] <- -fa$grad
    } else {
      c <- change[1]
      segment <- c:min(len, c + T - 1L)
      delay <- .indid_delay(p[s, segment])
      fa <- .indid_delay(p[s, seq_len(c - 1L)])
      value <- value + alpha * delay$value - fa$value
      grad[s, segment] <- alpha * delay$grad
      grad[s, seq_len(c - 1L)] <- -fa$grad
    }
  }
  list(value = value / n, grad = grad / n)
}

# torch.nn.BCELoss (mean) and its gradient as computed by torch.
#' @noRd
.indid_bce <- function(p, labels) {
  count <- length(p)
  value <- -mean(labels * pmax(log(p), -100) + (1 - labels) * pmax(log(1 - p), -100))
  grad <- (p - labels) / pmax((1 - p) * p, 1e-12) / count
  list(value = value, grad = grad)
}

#' @noRd
.indid_objective <- function(p, labels, settings) {
  if (settings$loss == "bce") .indid_bce(p, labels) else .indid_loss(p, labels, settings$T)
}

# Training -----------------------------------------------------------------------------------------

# One optimisation step on a batch.
#' @noRd
.indid_step <- function(net, x, labels, settings) {
  forward <- .indid_forward(net$params, x, keep = TRUE)
  objective <- .indid_objective(forward$p, labels, settings)
  grads <- .indid_backward(net$params, x, forward, objective$grad)
  list(net = .adam_step(net, grads, settings$learning_rate), loss = objective$value, grads = grads)
}

#' @noRd
.indid_batches <- function(n, size, shuffle) {
  order <- if (shuffle) sample.int(n) else seq_len(n)
  split(order, (seq_len(n) - 1L) %/% size)
}

#' @noRd
.indid_validation_loss <- function(params, validation, settings) {
  n <- dim(validation$x)[1]
  total <- 0
  for (rows in .indid_batches(n, settings$batch_size, FALSE)) {
    p <- .indid_forward(params, validation$x[rows, , , drop = FALSE])$p
    total <- total + .indid_objective(p, validation$labels[rows, , drop = FALSE], settings)$value * length(rows)
  }
  total / n
}

#' @noRd
.indid_fit <- function(net, x, labels, validation, settings) {
  best <- Inf
  wait <- 0L
  history <- NULL
  for (epoch in seq_len(settings$epochs)) {
    losses <- numeric(0)
    for (rows in .indid_batches(dim(x)[1], settings$batch_size, TRUE)) {
      step <- .indid_step(net, x[rows, , , drop = FALSE], labels[rows, , drop = FALSE], settings)
      net <- step$net
      losses <- c(losses, step$loss)
    }
    val_loss <- if (is.null(validation)) NA_real_ else .indid_validation_loss(net$params, validation, settings)
    history <- rbind(history, data.frame(epoch = epoch, train_loss = mean(losses), val_loss = val_loss))
    if (!is.null(validation)) {
      if (val_loss < best) {
        best <- val_loss
        wait <- 0L
      } else {
        wait <- wait + 1L
        if (wait >= settings$patience) break
      }
    }
  }
  list(net = net, history = history)
}

# Stream detector ----------------------------------------------------------------------------------

#' @title InDiD stream detector
#' @description Monitors a stream with a trained [indid()] model: the
#' observations are fed one by one to the recurrent network, and a drift is
#' reported when the probability of change exceeds `threshold`. Since the model
#' is trained on labelled sequences of the monitored features, it detects the
#' kinds of change it was trained on (**virtual concept drift** of those
#' features).
#' @param model A trained `indid` object.
#' @param threshold Alarm threshold on the probability of change.
#' @param target_feat Names of the columns fed to the model, in the order of
#' training. `NULL` (the default) uses the model's feature names, or every
#' numeric column when the model has none.
#' @param horizon If set, the state of the network is reset every `horizon`
#' observations, so that the stream is read as consecutive sequences of the
#' training length (`model$seq_len` is a natural choice). `NULL` (default) never
#' resets it except after a drift.
#' @details After a drift the state of the network is reset, so the data after
#' the change are read as the start of a new sequence. The model has learned
#' what the beginning of a sequence without change looks like, so it keeps
#' raising alarms if the new regime does not resemble it. Rows with missing
#' values are skipped.
#' @return `dfr_indid` object. After `fit()`, `drifter_output` holds the
#' probability of change of every observation (`probability`).
#' @inherit indid references
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' make <- function(n) {
#'   change <- sample(2:32, n, replace = TRUE)
#'   change[seq_len(n / 2)] <- 33
#'   x <- t(vapply(change, function(c) stats::rnorm(32, ifelse(seq_len(32) < c, 1, 6)), numeric(32)))
#'   labels <- t(vapply(change, function(c) (seq_len(32) >= c) * 1, numeric(32)))
#'   list(x = x, labels = labels)
#' }
#' train <- make(200)
#' model <- indid(train$x, train$labels, T = 8, learning_rate = 0.02, epochs = 30, seed = 1)
#' stream <- data.frame(value = c(stats::rnorm(20, 1), stats::rnorm(10, 6)))
#' detector <- fit(dfr_indid(model), stream)
#' which(detector$drifter_output$probability > 0.5)[1]
#' @export
dfr_indid <- function(model, threshold = 0.5, target_feat = NULL, horizon = NULL) {
  if (!inherits(model, "indid")) {
    stop("dfr_indid: model must be an indid object", call. = FALSE)
  }
  .check_probability(threshold, "threshold")
  if (!is.null(horizon)) {
    .check_positive_integer(horizon, "horizon", min_value = 1L)
  }
  if (is.null(target_feat)) {
    target_feat <- model$features
  }
  obj <- mv_dist_based(target_feat = target_feat)
  obj$model <- model
  # The network state (hidden and cell) is `fresh` at the start and after a drift.
  obj$state <- list(threshold = threshold, horizon = horizon, hidden = NULL, cell = NULL, fresh = TRUE, steps = 0L)
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_indid", class(obj))
  return(obj)
}

#' @export
update_state.dfr_indid <- function(obj, value, ...) {
  state <- obj$state
  row <- .select_features(value, obj$target_feat)
  if (length(row) != obj$model$dim) {
    stop(sprintf("dfr_indid: the model has %d feature(s) and the stream %d", obj$model$dim, length(row)),
         call. = FALSE)
  }
  obj$last_drifter_output <- NA_real_
  if (anyNA(row)) {
    return(list(obj = obj, drift = FALSE))
  }
  size <- ncol(obj$model$params$W_hh)
  if (state[["fresh"]] || (!is.null(state[["horizon"]]) && (state[["steps"]] >= state[["horizon"]]))) {
    state[["hidden"]] <- matrix(0, 1, size)
    state[["cell"]] <- matrix(0, 1, size)
    state[["fresh"]] <- FALSE
    state$steps <- 0L
  }
  step <- .indid_cell(obj$model$params, matrix(unname(row), 1), state[["hidden"]], state[["cell"]])
  probability <- .indid_sigmoid(sum(step$h * obj$model$params$W_out) + obj$model$params$b_out)
  state[["hidden"]] <- step$h
  state[["cell"]] <- step$c
  state$steps <- state$steps + 1L
  obj$last_drifter_output <- c(probability = probability)
  drift <- probability > state$threshold
  if (drift) {
    state[["fresh"]] <- TRUE
    obj$drifted <- TRUE
  }
  obj$state <- state
  return(list(obj = obj, drift = drift))
}

#' @export
fit.dfr_indid <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data, output_names = "probability"))
}

#' @export
reset_state.dfr_indid <- function(obj) {
  obj$drifted <- FALSE
  obj$state[["fresh"]] <- TRUE
  obj$state$steps <- 0L
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  return(obj)
}
