# Base change point detectors of the WWAggr ensembles (Stepikin, Romanenkova
# and Zaytsev, 2025): the generalised LSTM of indid() (BCE and InDiD models),
# TS-CP2 and SN-TS2Vec, and the scores they give to sequences.

#' @title Change point scores of a sequence model
#' @description Scores in `[0, 1]` of every time step of every sequence: the
#' probability of change of an [indid()] model, or the processed similarity of
#' consecutive windows of a [tscp2()] or [ts2vec()] model. For calibrated
#' models ([cpd_calibrate()]) the calibrated scores, and for ensembles
#' ([cpd_ensemble()]) the scores of every member.
#' @param model A model: `indid`, `tscp2`, `ts2vec`, `cpd_calibrated` or
#' `cpd_ensemble`.
#' @param x Sequences, in the formats accepted by [indid()].
#' @param ... Unused.
#' @return A matrix `[sequence, time]`; for an ensemble an array
#' `[member, sequence, time]`. The representation models (TS-CP2 and
#' SN-TS2Vec) have no score for the first `2 * window - 1` steps (`NA`).
#' @examples
#' set.seed(1)
#' # 8 sequences of 12 steps with 2 features; the mean moves at step 7.
#' x <- array(stats::rnorm(8 * 12 * 2), c(8, 12, 2))
#' x[, 7:12, ] <- x[, 7:12, ] + 3
#' model <- ts2vec(x, window = 3, output_dim = 4, hidden_dim = 8, depth = 2, epochs = 5, seed = 1)
#' round(cpd_scores(model, x[1:2, , ]), 2)
#' @export
cpd_scores <- function(model, x, ...) {
  UseMethod("cpd_scores")
}

#' @export
cpd_scores.indid <- function(model, x, ...) {
  indid_predict(model, x)
}

#' @export
cpd_scores.tscp2 <- function(model, x, ...) {
  x <- .cpd_input(model, x)
  .repr_scores(model$settings, x, function(windows) {
    .tscp_encode(model$params, model$buffers, model$settings, windows, FALSE)$z
  })
}

#' @export
cpd_scores.ts2vec <- function(model, x, ...) {
  x <- .cpd_input(model, x)
  .repr_scores(model$settings, x, function(windows) {
    w <- dim(windows)[2]
    z <- .ts2vec_encode(model$params, model$buffers, model$settings, windows, FALSE)$z
    m <- ncol(z) %/% w
    t(vapply(seq_len(m), function(j) apply(z[, (j - 1L) * w + seq_len(w), drop = FALSE], 1, max),
             numeric(nrow(z))))
  })
}

#' @noRd
.cpd_input <- function(model, x) {
  x <- .indid_array(x)
  if (dim(x)[3] != model$dim) {
    stop(sprintf("cpd_scores: the model has %d feature(s) and the sequences %d", model$dim, dim(x)[3]),
         call. = FALSE)
  }
  x
}

# Scores of a representation model (prediction_utils.get_repr_learning_output
# and post_process_output of the reference): the cosine similarity of the
# embeddings of a history window and of the following future window, both of
# `window` steps, for window starts 0, step, 2 step, ...; each similarity is
# repeated `step` times from the last step of its first future window, the
# score is 1 - max(similarity, 0), smoothed by an exponential moving average
# with factor alpha.
#' @noRd
.repr_scores <- function(settings, x, embed, chunk = 512L) {
  w <- settings$window
  step <- settings$step
  n <- dim(x)[1]
  len <- dim(x)[2]
  if (len < 2L * w) {
    stop(sprintf("cpd_scores: the sequences must have at least %d steps (2 * window)", 2L * w), call. = FALSE)
  }
  starts <- seq(0L, (len - 2L * w) %/% step) * step
  pairs <- expand.grid(window = seq_along(starts), sequence = seq_len(n))
  sim <- numeric(nrow(pairs))
  for (rows in split(seq_len(nrow(pairs)), (seq_len(nrow(pairs)) - 1L) %/% chunk)) {
    take <- function(offset) {
      out <- array(0, c(length(rows), w, dim(x)[3]))
      for (k in seq_along(rows)) {
        s <- starts[pairs$window[rows[k]]]
        out[k, , ] <- x[pairs$sequence[rows[k]], s + offset + seq_len(w), ]
      }
      out
    }
    zh <- embed(take(0L))
    zf <- embed(take(w))
    sim[rows] <- rowSums(zh * zf) / (pmax(sqrt(rowSums(zh^2)), 1e-6) * pmax(sqrt(rowSums(zf^2)), 1e-6))
  }
  sim <- matrix(sim, n, length(starts), byrow = TRUE)
  crop <- len - (2L * w - 1L)
  repeated <- sim[, rep(seq_along(starts), each = step)[seq_len(crop)], drop = FALSE]
  p <- 1 - pmax(repeated, 0)
  if (settings$alpha < 1) {
    for (t in seq_len(crop)[-1]) p[, t] <- settings$alpha * p[, t] + (1 - settings$alpha) * p[, t - 1L]
  }
  out <- matrix(NA_real_, n, len)
  out[, (2L * w):len] <- p
  out
}

# Generalised LSTM of indid() ------------------------------------------------------------------

# Parameters of BaseRnn of the reference (src/models/core_models.py): a
# torch.nn.LSTM of n_layers layers, or the LayerNormLSTM (one layer, a linear
# map per gate for the input and for the hidden state, each with its own
# layer normalisation before or after it), then Linear(hidden, 1).
#' @noRd
.s2s_init <- function(d, settings) {
  h <- settings$hidden_size
  params <- list()
  if (settings$layer_norm == "none") {
    for (l in seq_len(settings$n_layers)) {
      n_in <- if (l == 1L) d else h
      params[[paste0("W_ih", l)]] <- .nn_uniform(4 * h, n_in, h)
      params[[paste0("W_hh", l)]] <- .nn_uniform(4 * h, h, h)
      params[[paste0("b_ih", l)]] <- as.vector(.nn_uniform(1, 4 * h, h))
      params[[paste0("b_hh", l)]] <- as.vector(.nn_uniform(1, 4 * h, h))
    }
  } else {
    size_x <- if (settings$layer_norm == "before") d else h
    for (g in c("f", "i", "o", "c")) {
      params <- .nn_linear_init(params, paste0("W", g), d, h)
      params[[paste0("lnW", g, "_gamma")]] <- rep(1, size_x)
      params[[paste0("lnW", g, "_beta")]] <- rep(0, size_x)
    }
    for (g in c("f", "i", "o", "c")) {
      params <- .nn_linear_init(params, paste0("U", g), h, h)
      params[[paste0("lnU", g, "_gamma")]] <- rep(1, h)
      params[[paste0("lnU", g, "_beta")]] <- rep(0, h)
    }
  }
  .nn_linear_init(params, "out", h, 1)
}

# One step of layer l of torch.nn.LSTM (gates i, f, g, o).
#' @noRd
.s2s_lstm_cell <- function(P, l, x, h, c) {
  size <- ncol(.adv(h))
  z <- .ad_add(.ad_mm(x, .ad_t(P[[paste0("W_ih", l)]])), .ad_mm(h, .ad_t(P[[paste0("W_hh", l)]])))
  z <- .ad_add_row(.ad_add_row(z, P[[paste0("b_ih", l)]]), P[[paste0("b_hh", l)]])
  gate <- function(k) .ad_cols(z, (k - 1L) * size + seq_len(size))
  i <- .ad_sigmoid(gate(1L))
  f <- .ad_sigmoid(gate(2L))
  g <- .ad_tanh(gate(3L))
  o <- .ad_sigmoid(gate(4L))
  c_new <- .ad_add(.ad_mul(f, c), .ad_mul(i, g))
  list(h = .ad_mul(o, .ad_tanh(c_new)), c = c_new)
}

# One step of the LayerNormLSTM.
#' @noRd
.s2s_ln_cell <- function(P, mode, x, h, c) {
  branch <- function(prefix, g, input) {
    name <- paste0(prefix, g)
    ln <- function(v) .ad_norm_rows(v, P[[paste0("ln", name, "_gamma")]], P[[paste0("ln", name, "_beta")]], 1e-5, "col")
    if (mode == "before") .nn_linear(P, name, ln(input)) else ln(.nn_linear(P, name, input))
  }
  pre <- lapply(c(f = "f", i = "i", o = "o", c = "c"), function(g) .ad_add(branch("W", g, x), branch("U", g, h)))
  f <- .ad_sigmoid(pre$f)
  i <- .ad_sigmoid(pre$i)
  o <- .ad_sigmoid(pre$o)
  c_hat <- .ad_tanh(pre$c)
  c_new <- .ad_add(.ad_mul(f, c), .ad_mul(i, c_hat))
  list(h = .ad_mul(o, .ad_tanh(c_new)), c = c_new)
}

#' @noRd
.s2s_layers <- function(settings) if (settings$layer_norm == "none") settings$n_layers else 1L

#' @noRd
.s2s_cell <- function(P, settings, l, x, h, c) {
  if (settings$layer_norm == "none") .s2s_lstm_cell(P, l, x, h, c) else .s2s_ln_cell(P, settings$layer_norm, x, h, c)
}

# Probabilities of change [B, L] of a batch x [B, L, d]. In training, the
# outputs of every layer but the last go through dropout (torch.nn.LSTM).
#' @noRd
.s2s_forward <- function(P, settings, x, training = FALSE) {
  n <- dim(x)[1]
  len <- dim(x)[2]
  size <- settings$hidden_size
  inputs <- lapply(seq_len(len), function(t) matrix(x[, t, ], n))
  layers <- .s2s_layers(settings)
  for (l in seq_len(layers)) {
    h <- matrix(0, n, size)
    c <- matrix(0, n, size)
    outs <- vector("list", len)
    for (t in seq_len(len)) {
      step <- .s2s_cell(P, settings, l, inputs[[t]], h, c)
      h <- step$h
      c <- step$c
      outs[[t]] <- h
    }
    if (training && settings$dropout > 0 && l < layers) {
      outs <- lapply(outs, function(o) .ad_mul(o, .nn_dropout_mask(c(n, size), settings$dropout)))
    }
    inputs <- outs
  }
  .ad_cbind(lapply(inputs, function(o) .ad_sigmoid(.nn_linear(P, "out", o))))
}

# One observation of a stream: state is a list of (h, c) per layer.
#' @noRd
.s2s_stream_step <- function(params, settings, row, state) {
  layers <- .s2s_layers(settings)
  size <- settings$hidden_size
  if (is.null(state)) {
    state <- rep(list(list(h = matrix(0, 1, size), c = matrix(0, 1, size))), layers)
  }
  input <- matrix(row, 1)
  for (l in seq_len(layers)) {
    step <- .s2s_cell(params, settings, l, input, state[[l]]$h, state[[l]]$c)
    state[[l]] <- step
    input <- step$h
  }
  list(probability = as.numeric(1 / (1 + exp(-(input %*% t(params$out_w) + params$out_b)))), state = state)
}

#' @noRd
.s2s_validation_loss <- function(params, validation, settings) {
  n <- dim(validation$x)[1]
  total <- 0
  for (rows in .nn_batches(n, settings$batch_size, FALSE)) {
    p <- .s2s_forward(params, settings, validation$x[rows, , , drop = FALSE])
    total <- total + .indid_objective(p, validation$labels[rows, , drop = FALSE], settings)$value * length(rows)
  }
  total / n
}

#' @noRd
.s2s_fit <- function(net, x, labels, validation, settings) {
  step <- function(net, rows) {
    xb <- x[rows, , , drop = FALSE]
    lb <- labels[rows, , drop = FALSE]
    .ad_record(function() {
      P <- .ad_leaves(net$params)
      p <- .s2s_forward(P, settings, xb, training = TRUE)
      objective <- .indid_objective(p$value, lb, settings)
      grads <- .ad_grad(p, P, objective$grad)
      list(net = .nn_optimizer_step(net, grads, net$learning_rate), loss = objective$value)
    })
  }
  validate <- if (is.null(validation)) NULL else function(net) .s2s_validation_loss(net$params, validation, settings)
  train <- list(epochs = settings$epochs, batch_size = settings$batch_size, learning_rate = settings$learning_rate,
                schedule = list(type = "constant"), monitor = "val", patience = settings$patience, min_delta = 0)
  fitted <- .nn_train(net, dim(x)[1], step, train, validate)
  fitted$history <- fitted$history[, c("epoch", "train_loss", "val_loss")]
  fitted
}

# TS-CP2 -------------------------------------------------------------------------------------------

#' @title TS-CP2 change point model
#' @description Trains the unsupervised TS-CP2 model of Deldari et al. (2021),
#' as re-implemented in the WWAggr code: a temporal convolutional network
#' (TCN) embeds windows of `window` steps; it is trained with a contrastive
#' (InfoNCE-like) loss so that each history window is close to the window that
#' follows it and far from the other windows of the batch. The change score of
#' a step is 1 minus the cosine similarity of the embeddings of the two
#' consecutive windows that end at it (see [cpd_scores()]).
#' @param x Training sequences, in the formats accepted by [indid()]. Only the
#' first `2 * window` steps of every sequence are used (a history window and
#' the future window that follows it).
#' @param window Length of the history and future windows.
#' @param nb_filters Number of filters of the convolutions.
#' @param kernel_size Kernel size of the convolutions.
#' @param nb_stacks,dilations The TCN has `nb_stacks` stacks of residual
#' blocks, one per dilation.
#' @param use_skip_connections Whether the skip connection of the last block
#' is added to the output of the TCN.
#' @param batch_norm Whether the convolutions are followed by batch
#' normalisation.
#' @param n_steps,code_size Sizes of the dense layers: `2 * n_steps`,
#' `n_steps` and the embedding (`code_size`).
#' @param temperature Temperature of the contrastive loss.
#' @param learning_rate Learning rate of Adam, annealed by a cosine schedule
#' of period `decay_steps` epochs.
#' @param decay_steps Period of the cosine annealing (`T_max`, in epochs).
#' @param batch_size Number of sequences per batch.
#' @param epochs Largest number of epochs.
#' @param patience,min_delta Early stopping on the training loss: training
#' stops after `patience` epochs whose loss is not below the best one minus
#' `min_delta`.
#' @param step,alpha Prediction settings: the similarity is computed every
#' `step` steps, and the scores are smoothed by an exponential moving average
#' with factor `alpha` (1: none).
#' @param seed Seed of the private random stream (initial weights and order of
#' the batches). `NULL` draws one from the session.
#' @details The model follows the reference code
#' (<https://github.com/stalex2902/wwagr-ensemble-cpd>, `src/baselines/tscp.py`
#' and `prediction_utils.py`):
#'
#' * Every residual block is a causal dilated convolution, ReLU, a second one,
#'   ReLU, plus the input (through a 1x1 convolution when the number of
#'   channels changes), then ReLU. Convolution weights are initialised from
#'   N(0, 0.05) with zero biases.
#' * The window `[window, channels]` is reshaped (not transposed) to
#'   `[channels, window]` before the TCN, as `x.reshape(batch, -1, seq_len)`
#'   of the reference.
#' * The loss of a batch of N pairs is the binary cross-entropy (target 1) of
#'   the logits `sum_i exp(s_ii / temperature) / sum_j exp(s_ij /
#'   temperature)`, where `s_ij` is the cosine similarity of history i and
#'   future j.
#' * The scores are `1 - max(s, 0)` (Appendix B of the WWAggr paper).
#'
#' The dropout of the reference (rate 0 in its configurations) and its layer
#' and weight normalisation options (not functional there) are not
#' implemented. The network, its gradients and Adam are written in R. The
#' defaults are those of the Human Activity configuration of the reference
#' (`configs/human_activity_tscp.yaml`).
#' @references Deldari, S., Smith, D. V., Xue, H. and Salim, F. D. (2021).
#' Time series change point detection with self-supervised contrastive
#' predictive coding. *Proceedings of The Web Conference 2021*, 3124-3135.
#' <doi:10.1145/3442381.3449903>
#'
#' Stepikin, A., Romanenkova, E. and Zaytsev, A. (2025). WWAggr: A window
#' Wasserstein-based aggregation for ensemble change point detection. *2025
#' IEEE International Conference on Data Mining (ICDM)*, 1535-1544.
#' <doi:10.1109/ICDM65498.2025.00163>
#' @return A `tscp2` object with the parameters, the batch normalisation
#' statistics, the settings and the training loss of every epoch.
#' @seealso [cpd_scores()], [cpd_ensemble()], [dfr_wwaggr()].
#' @examples
#' set.seed(1)
#' x <- array(stats::rnorm(16 * 12 * 2), c(16, 12, 2))
#' model <- tscp2(x, window = 4, nb_filters = 4, dilations = c(1, 2), n_steps = 4, code_size = 3,
#'                epochs = 3, seed = 1)
#' model
#' @export
tscp2 <- function(x, window = 4, nb_filters = 64, kernel_size = 5, nb_stacks = 2, dilations = c(1, 2, 4, 8),
                  use_skip_connections = TRUE, batch_norm = FALSE, n_steps = 8, code_size = 4, temperature = 0.1,
                  learning_rate = 0.001, decay_steps = 1000, batch_size = 8, epochs = 200, patience = 5,
                  min_delta = 1e-4, step = 1, alpha = 1, seed = NULL) {
  for (name in c("window", "nb_filters", "kernel_size", "nb_stacks", "n_steps", "code_size", "decay_steps",
                 "batch_size", "epochs", "patience", "step")) {
    .check_positive_integer(get(name), name, min_value = 1L)
  }
  if (!is.numeric(dilations) || length(dilations) < 1L || any(dilations < 1) || any(dilations != round(dilations))) {
    stop("tscp2: dilations must be positive integers", call. = FALSE)
  }
  .check_number(temperature, "temperature", min_value = 0)
  .check_number(learning_rate, "learning_rate", min_value = 0)
  .check_number(min_delta, "min_delta", min_value = 0)
  .check_probability(alpha, "alpha")
  x <- .indid_array(x)
  if (dim(x)[2] < 2L * window) {
    stop("tscp2: the training sequences must have at least 2 * window steps", call. = FALSE)
  }
  settings <- list(window = as.integer(window), nb_filters = as.integer(nb_filters),
                   kernel_size = as.integer(kernel_size), nb_stacks = as.integer(nb_stacks),
                   dilations = as.integer(dilations), use_skip_connections = isTRUE(use_skip_connections),
                   batch_norm = isTRUE(batch_norm), n_steps = as.integer(n_steps), code_size = as.integer(code_size),
                   temperature = temperature, learning_rate = learning_rate, decay_steps = decay_steps,
                   batch_size = as.integer(batch_size), epochs = as.integer(epochs), patience = as.integer(patience),
                   min_delta = min_delta, step = as.integer(step), alpha = alpha)
  result <- .with_rng(.rng_state(seed), function() {
    init <- .tscp_init(dim(x)[3], settings)
    net <- .indid_reset_adam(list(params = init$params))
    net$buffers <- init$buffers
    step_fun <- function(net, rows) {
      history <- x[rows, seq_len(window), , drop = FALSE]
      future <- x[rows, window + seq_len(window), , drop = FALSE]
      .ad_record(function() {
        P <- .ad_leaves(net$params)
        h <- .tscp_encode(P, net$buffers, settings, history, TRUE)
        f <- .tscp_encode(P, h$buffers, settings, future, TRUE)
        loss <- .tscp_loss(h$z, f$z, temperature)
        grads <- .ad_grad(loss, P)
        net <- .nn_optimizer_step(net, grads, net$learning_rate)
        net$buffers <- f$buffers
        list(net = net, loss = loss$value[1])
      })
    }
    train <- list(epochs = settings$epochs, batch_size = settings$batch_size, learning_rate = learning_rate,
                  schedule = list(type = "cosine", t_max = decay_steps), monitor = "train",
                  patience = settings$patience, min_delta = min_delta)
    .nn_train(net, dim(x)[1], step_fun, train)
  })$value
  model <- list(params = result$net$params, buffers = result$net$buffers, settings = settings,
                history = result$history, dim = dim(x)[3], features = dimnames(x)[[3]])
  class(model) <- c("tscp2", "cpd_model")
  model
}

#' @export
print.tscp2 <- function(x, ...) {
  cat(sprintf("TS-CP2 model: windows of %d steps, %d feature(s), embedding of size %d, %d epoch(s)\n",
              x$settings$window, x$dim, x$settings$code_size, nrow(x$history)))
  invisible(x)
}

#' @noRd
.tscp_blocks <- function(settings) {
  dilations <- rep(settings$dilations, settings$nb_stacks)
  data.frame(name = paste0("block", seq_along(dilations)), dilation = dilations)
}

#' @noRd
.tscp_init <- function(d, settings) {
  params <- list()
  buffers <- list()
  filters <- settings$nb_filters
  blocks <- .tscp_blocks(settings)
  for (k in seq_len(nrow(blocks))) {
    name <- blocks$name[k]
    n_in <- if (k == 1L) d else filters
    params <- .nn_conv_init(params, paste0(name, "_conv1"), n_in, filters, settings$kernel_size, normal_sd = 0.05)
    params <- .nn_conv_init(params, paste0(name, "_conv2"), filters, filters, settings$kernel_size, normal_sd = 0.05)
    if (n_in != filters) {
      params <- .nn_conv_init(params, paste0(name, "_down"), n_in, filters, 1L, normal_sd = 0.05)
    }
    if (settings$batch_norm) {
      for (bn in paste0(name, c("_bn1", "_bn2"))) {
        params[[paste0(bn, "_gamma")]] <- rep(1, filters)
        params[[paste0(bn, "_beta")]] <- rep(0, filters)
        buffers[[paste0(bn, "_mean")]] <- rep(0, filters)
        buffers[[paste0(bn, "_var")]] <- rep(1, filters)
      }
    }
  }
  params <- .nn_linear_init(params, "fc1", filters * settings$window, 2L * settings$n_steps)
  params <- .nn_linear_init(params, "fc2", 2L * settings$n_steps, settings$n_steps)
  params <- .nn_linear_init(params, "output", settings$n_steps, settings$code_size)
  list(params = params, buffers = buffers)
}

# Embeddings [B, code_size] of windows x [B, window, d].
#' @noRd
.tscp_encode <- function(P, buffers, settings, x, training) {
  batch <- dim(x)[1]
  len <- dim(x)[2]
  d <- dim(x)[3]
  # torch: x.reshape(batch, -1, seq_len) of the contiguous [batch, len, d] tensor.
  input <- matrix(0, d, batch * len)
  for (b in seq_len(batch)) {
    input[, (b - 1L) * len + seq_len(len)] <- matrix(as.vector(t(matrix(x[b, , ], len, d))), d, len, byrow = TRUE)
  }
  out <- input
  skip <- NULL
  k <- settings$kernel_size
  blocks <- .tscp_blocks(settings)
  for (j in seq_len(nrow(blocks))) {
    name <- blocks$name[j]
    dilation <- blocks$dilation[j]
    h <- .nn_conv1d(P, paste0(name, "_conv1"), out, len, k, dilation, (k - 1L) * dilation)
    if (settings$batch_norm) {
      bn <- .nn_batchnorm(P, paste0(name, "_bn1"), h, buffers, training)
      h <- bn$out
      buffers <- bn$buffers
    }
    h <- .ad_relu(h)
    h <- .nn_conv1d(P, paste0(name, "_conv2"), h, len, k, dilation, (k - 1L) * dilation)
    if (settings$batch_norm) {
      bn <- .nn_batchnorm(P, paste0(name, "_bn2"), h, buffers, training)
      h <- bn$out
      buffers <- bn$buffers
    }
    h <- .ad_relu(h)
    skip <- if (is.null(P[[paste0(name, "_down_w")]])) out else .nn_pointwise(P, paste0(name, "_down"), out)
    out <- .ad_relu(.ad_add(h, skip))
  }
  if (settings$use_skip_connections) out <- .ad_add(out, skip)
  filters <- settings$nb_filters
  # Flatten [batch, filters, len] to [batch, filters * len] (column f * len + t).
  index <- outer(seq_len(batch), seq_len(filters * len), function(b, j) {
    f <- (j - 1L) %/% len
    t <- (j - 1L) %% len
    ((b - 1L) * len + t) * filters + f + 1L
  })
  flat <- .ad_gather(out, index, dim(index))
  z <- .ad_relu(.nn_linear(P, "fc1", flat))
  z <- .ad_relu(.nn_linear(P, "fc2", z))
  list(z = .nn_linear(P, "output", z), buffers = buffers)
}

# Contrastive loss of TS-CP2 (nce_loss_fn of the reference) for embeddings of
# history and future windows.
#' @noRd
.tscp_loss <- function(zh, zf, temperature) {
  h <- .ad_l2_rows(.ad_l2_rows(zh), 1e-6)
  f <- .ad_l2_rows(.ad_l2_rows(zf), 1e-6)
  sim <- .ad_mm(h, .ad_t(f))
  n <- nrow(.adv(sim))
  e <- .ad_exp(.ad_scale(sim, 1 / temperature))
  positive <- .ad_sum(.ad_gather(e, (seq_len(n) - 1L) * n + seq_len(n), c(n, 1L)))
  logits <- .ad_mul_scalar(.ad_recip(.ad_rowsums(e)), positive)
  .ad_mean(.ad_softplus(.ad_scale(logits, -1)))
}

# SN-TS2Vec ----------------------------------------------------------------------------------------

#' @title SN-TS2Vec change point model
#' @description Trains the unsupervised SN-TS2Vec model of Bazarova et al.
#' (2024), as used in the WWAggr code: the TS2Vec encoder of Yue et al. (2022)
#' (a linear input layer and residual blocks of dilated convolutions with GELU)
#' with spectral normalisation of the convolutions, trained with the
#' hierarchical contrastive loss of TS2Vec on two overlapping random crops of
#' every sequence. The change score of a step is 1 minus the cosine similarity
#' of the max-pooled embeddings of the two consecutive windows that end at it
#' (see [cpd_scores()]).
#' @param x Training sequences, in the formats accepted by [indid()].
#' @param window Length of the history and future windows of the scores.
#' @param output_dim,hidden_dim Sizes of the embedding and of the hidden
#' channels.
#' @param depth Number of hidden residual blocks (one more maps to
#' `output_dim`).
#' @param spectral_norm Whether the convolutions are spectrally normalised
#' (largest singular value at most 1).
#' @param mask Masking of the input during training: `"binomial"` (each step
#' kept with probability 0.5), `"continuous"` (5 masked segments of 10 % of
#' the length) or `"all_true"` (no masking).
#' @param temporal_unit Smallest level of the hierarchy with a temporal loss.
#' @param max_train_length If set, longer training sequences are cut at a
#' random offset to this length.
#' @param learning_rate,weight_decay Settings of AdamW; the learning rate
#' follows a cosine schedule with warm restarts (periods of 10 epochs, then
#' 1000).
#' @param batch_size Number of sequences per batch.
#' @param epochs Largest number of epochs.
#' @param patience Early stopping on the validation loss (only with
#' validation data).
#' @param validation Optional validation sequences.
#' @param step,alpha Prediction settings, as in [tscp2()].
#' @param seed Seed of the private random stream (weights, crops, masks,
#' dropout, order of the batches). `NULL` draws one from the session.
#' @details The model follows the reference code
#' (<https://github.com/stalex2902/wwagr-ensemble-cpd>,
#' `src/baselines/ts2vec_sn.py` and `prediction_utils.py`):
#'
#' * Spectral normalisation divides a weight by its largest singular value
#'   when it exceeds 1. The singular value is estimated by one power
#'   iteration at every forward pass in training (15 at initialisation), as
#'   `torch.nn.utils.parametrizations.spectral_norm`.
#' * Training masks the hidden representation of the input steps, and applies
#'   dropout (0.1) to the output.
#' * The window embedding is the maximum over its steps.
#'
#' The network, its gradients and AdamW are written in R. The defaults are
#' those of the Human Activity configuration of the reference
#' (`configs/human_activity_ts2vec.yaml`).
#' @references Bazarova, A., Romanenkova, E. and Zaytsev, A. (2024).
#' Normalizing self-supervised learning for provably reliable change point
#' detection. *2024 IEEE International Conference on Data Mining (ICDM)*,
#' 21-30.
#'
#' Yue, Z., Wang, Y., Duan, J., Yang, T., Huang, C., Tong, Y. and Xu, B.
#' (2022). TS2Vec: Towards universal representation of time series.
#' *Proceedings of the AAAI Conference on Artificial Intelligence*, 36(8),
#' 8980-8987.
#'
#' Stepikin, A., Romanenkova, E. and Zaytsev, A. (2025). WWAggr: A window
#' Wasserstein-based aggregation for ensemble change point detection. *2025
#' IEEE International Conference on Data Mining (ICDM)*, 1535-1544.
#' <doi:10.1109/ICDM65498.2025.00163>
#' @return A `ts2vec` object with the parameters, the power iteration vectors,
#' the settings and the losses of every epoch.
#' @seealso [cpd_scores()], [cpd_ensemble()], [dfr_wwaggr()].
#' @examples
#' set.seed(1)
#' x <- array(stats::rnorm(8 * 12 * 2), c(8, 12, 2))
#' model <- ts2vec(x, window = 3, output_dim = 4, hidden_dim = 6, depth = 2, epochs = 2, seed = 1)
#' model
#' @export
ts2vec <- function(x, window = 4, output_dim = 32, hidden_dim = 128, depth = 8, spectral_norm = TRUE,
                   mask = c("binomial", "continuous", "all_true"), temporal_unit = 0, max_train_length = NULL,
                   learning_rate = 0.001, weight_decay = 0.2, batch_size = 128, epochs = 40, patience = 40,
                   validation = NULL, step = 1, alpha = 1, seed = NULL) {
  mask <- match.arg(mask)
  for (name in c("window", "output_dim", "hidden_dim", "batch_size", "epochs", "patience", "step")) {
    .check_positive_integer(get(name), name, min_value = 1L)
  }
  .check_positive_integer(depth, "depth", min_value = 0L)
  .check_positive_integer(temporal_unit, "temporal_unit", min_value = 0L)
  if (!is.null(max_train_length)) .check_positive_integer(max_train_length, "max_train_length", min_value = 2L)
  .check_number(learning_rate, "learning_rate", min_value = 0)
  .check_number(weight_decay, "weight_decay", min_value = 0)
  .check_probability(alpha, "alpha")
  x <- .indid_array(x)
  if (dim(x)[2] < 2L^(temporal_unit + 1)) {
    stop("ts2vec: the training sequences are too short for temporal_unit", call. = FALSE)
  }
  if (!is.null(validation)) {
    validation <- .indid_array(validation)
    if (dim(validation)[3] != dim(x)[3]) {
      stop("ts2vec: the validation sequences have a different number of features", call. = FALSE)
    }
  }
  settings <- list(window = as.integer(window), output_dim = as.integer(output_dim),
                   hidden_dim = as.integer(hidden_dim), depth = as.integer(depth),
                   spectral_norm = isTRUE(spectral_norm), mask = mask, temporal_unit = as.integer(temporal_unit),
                   max_train_length = max_train_length, learning_rate = learning_rate, weight_decay = weight_decay,
                   batch_size = as.integer(batch_size), epochs = as.integer(epochs), patience = as.integer(patience),
                   step = as.integer(step), alpha = alpha, dropout = 0.1)
  result <- .with_rng(.rng_state(seed), function() {
    init <- .ts2vec_init(dim(x)[3], settings)
    net <- .indid_reset_adam(list(params = init$params))
    net$buffers <- init$buffers
    step_fun <- function(net, rows) {
      .ad_record(function() {
        P <- .ad_leaves(net$params)
        out <- .ts2vec_step_loss(P, net$buffers, settings, x[rows, , , drop = FALSE], TRUE)
        grads <- .ad_grad(out$loss, P)
        net <- .nn_optimizer_step(net, grads, net$learning_rate, weight_decay)
        net$buffers <- out$buffers
        list(net = net, loss = out$loss$value[1])
      })
    }
    validate <- if (is.null(validation)) NULL else function(net) {
      total <- 0
      for (rows in .nn_batches(dim(validation)[1], settings$batch_size, FALSE)) {
        out <- .ts2vec_step_loss(net$params, net$buffers, settings, validation[rows, , , drop = FALSE], FALSE)
        total <- total + out$loss[1] * length(rows)
      }
      total / dim(validation)[1]
    }
    train <- list(epochs = settings$epochs, batch_size = settings$batch_size, learning_rate = learning_rate,
                  schedule = list(type = "warm_restarts", t_0 = 10, t_mult = 100), monitor = "val",
                  patience = settings$patience, min_delta = 0)
    .nn_train(net, dim(x)[1], step_fun, train, validate)
  })$value
  model <- list(params = result$net$params, buffers = result$net$buffers, settings = settings,
                history = result$history, dim = dim(x)[3], features = dimnames(x)[[3]])
  class(model) <- c("ts2vec", "cpd_model")
  model
}

#' @export
print.ts2vec <- function(x, ...) {
  cat(sprintf("SN-TS2Vec model: windows of %d steps, %d feature(s), embedding of size %d, %d epoch(s)\n",
              x$settings$window, x$dim, x$settings$output_dim, nrow(x$history)))
  invisible(x)
}

#' @noRd
.ts2vec_blocks <- function(settings) {
  channels <- c(rep(settings$hidden_dim, settings$depth), settings$output_dim)
  n <- length(channels)
  n_in <- c(settings$hidden_dim, channels[-n])
  data.frame(name = paste0("block", seq_len(n)), n_in = n_in, n_out = channels, dilation = 2L^(seq_len(n) - 1L),
             projector = (n_in != channels) | (seq_len(n) == n))
}

#' @noRd
.ts2vec_init <- function(d, settings) {
  params <- .nn_linear_init(list(), "input_fc", d, settings$hidden_dim)
  buffers <- list()
  blocks <- .ts2vec_blocks(settings)
  for (k in seq_len(nrow(blocks))) {
    name <- blocks$name[k]
    convs <- paste0(name, c("_conv1", "_conv2"))
    params <- .nn_conv_init(params, convs[1], blocks$n_in[k], blocks$n_out[k], 3L)
    params <- .nn_conv_init(params, convs[2], blocks$n_out[k], blocks$n_out[k], 3L)
    if (blocks$projector[k]) {
      convs <- c(convs, paste0(name, "_proj"))
      params <- .nn_conv_init(params, convs[3], blocks$n_in[k], blocks$n_out[k], 1L)
    }
    if (settings$spectral_norm) {
      for (conv in convs) buffers[[conv]] <- .nn_spectral_init(params[[paste0(conv, "_w")]])
    }
  }
  list(params = params, buffers = buffers)
}

#' @noRd
.ts2vec_mask <- function(batch, len, mode) {
  keep <- matrix(TRUE, batch, len)
  if (mode == "binomial") {
    keep[] <- stats::runif(batch * len) < 0.5
  } else if (mode == "continuous") {
    n <- max(min(5L, len %/% 2L), 1L)
    l <- max(as.integer(0.1 * len), 1L)
    for (b in seq_len(batch)) {
      for (j in seq_len(n)) {
        t <- sample.int(len - l + 1L, 1L)
        keep[b, t:(t + l - 1L)] <- FALSE
      }
    }
  }
  keep
}

# Representations [output_dim, B * L] (conv layout) of x [B, L, d]. In
# training, the steps are masked, every spectrally normalised weight takes
# one power iteration and the output goes through dropout.
# `mask` [B, L] and `drop` [output_dim, B * L] replace the random draws
# (validation against recorded draws of the reference).
#' @noRd
.ts2vec_encode <- function(P, buffers, settings, x, training, mask = NULL, drop = NULL) {
  batch <- dim(x)[1]
  len <- dim(x)[2]
  d <- dim(x)[3]
  input <- matrix(aperm(x, c(3, 2, 1)), d, batch * len)
  observed <- colSums(is.na(input)) == 0
  input[, !observed] <- 0
  h <- .nn_pointwise(P, "input_fc", input)
  if (training && is.null(mask)) mask <- .ts2vec_mask(batch, len, settings$mask)
  keep <- if (training) as.vector(t(mask)) else rep(TRUE, batch * len)
  h <- .ad_mul_row(h, as.numeric(keep & observed))
  blocks <- .ts2vec_blocks(settings)
  weight <- function(conv) {
    if (!settings$spectral_norm) return(P[[paste0(conv, "_w")]])
    sn <- .nn_spectral_weight(P, conv, buffers, training)
    buffers <<- sn$buffers
    sn$weight
  }
  for (k in seq_len(nrow(blocks))) {
    name <- blocks$name[k]
    dilation <- blocks$dilation[k]
    residual <- if (blocks$projector[k]) {
      .nn_pointwise(P, paste0(name, "_proj"), h, weight = weight(paste0(name, "_proj")))
    } else {
      h
    }
    a <- .nn_conv1d(P, paste0(name, "_conv1"), .ad_gelu(h), len, 3L, dilation, dilation,
                    weight = weight(paste0(name, "_conv1")))
    a <- .nn_conv1d(P, paste0(name, "_conv2"), .ad_gelu(a), len, 3L, dilation, dilation,
                    weight = weight(paste0(name, "_conv2")))
    h <- .ad_add(a, residual)
  }
  if (training && settings$dropout > 0) {
    if (is.null(drop)) drop <- .nn_dropout_mask(dim(.adv(h)), settings$dropout)
    h <- .ad_mul(h, drop)
  }
  list(z = h, buffers = buffers)
}

# Instance or temporal contrastive loss of TS2Vec for z1, z2 [C, B * L]
# (conv layout), with its gradient computed directly: for every group (a time
# step for the instance loss, a sequence for the temporal one) the similarity
# matrix of the 2G representations without its diagonal goes through
# log-softmax, and the loss is the mean negative log-probability of the other
# view of every representation.
#' @noRd
.ts2vec_contrastive <- function(z1, z2, len, batch, type) {
  v1 <- .adv(z1)
  v2 <- .adv(z2)
  groups <- if (type == "instance") len else batch
  size <- if (type == "instance") batch else len
  if (size == 1L) return(matrix(0))
  columns <- function(g) if (type == "instance") (seq_len(batch) - 1L) * len + g else (g - 1L) * len + seq_len(len)
  total <- 0
  d1 <- array(0, dim(v1))
  d2 <- array(0, dim(v2))
  target <- c(size + seq_len(size), seq_len(size))
  weight <- 1 / (2 * size * groups)
  for (g in seq_len(groups)) {
    cols <- columns(g)
    z <- cbind(v1[, cols, drop = FALSE], v2[, cols, drop = FALSE])
    s <- crossprod(z)
    diag(s) <- -Inf
    shifted <- s - apply(s, 1, max)
    lse <- log(rowSums(exp(shifted)))
    picked <- shifted[cbind(seq_len(2L * size), target)] - lse
    total <- total - sum(picked) * weight
    soft <- exp(shifted - lse)
    soft[cbind(seq_len(2L * size), target)] <- soft[cbind(seq_len(2L * size), target)] - 1
    ds <- soft * weight
    dz <- z %*% (ds + t(ds))
    d1[, cols] <- d1[, cols] + dz[, seq_len(size)]
    d2[, cols] <- d2[, cols] + dz[, size + seq_len(size)]
  }
  .ad_out(matrix(total), list(z1, z2), function(g) list(g[1] * d1, g[1] * d2))
}

# Max pooling of kernel 2 over time on the conv layout (first maximum on ties).
#' @noRd
.ts2vec_maxpool <- function(z, len, batch) {
  zv <- .adv(z)
  half <- len %/% 2L
  first <- as.vector(outer(seq_len(half) * 2L - 1L, (seq_len(batch) - 1L) * len, "+"))
  channels <- nrow(zv)
  a <- outer(seq_len(channels), (first - 1L) * channels, "+")
  b <- a + channels
  index <- ifelse(zv[b] > zv[a], b, a)
  .ad_gather(z, index, dim(a))
}

# Hierarchical contrastive loss of TS2Vec (alpha = 0.5).
#' @noRd
.ts2vec_hierarchical <- function(z1, z2, len, batch, temporal_unit, alpha = 0.5) {
  loss <- matrix(0)
  d <- 0L
  while (len > 1L) {
    loss <- .ad_add(loss, .ad_scale(.ts2vec_contrastive(z1, z2, len, batch, "instance"), alpha))
    if (d >= temporal_unit) {
      loss <- .ad_add(loss, .ad_scale(.ts2vec_contrastive(z1, z2, len, batch, "temporal"), 1 - alpha))
    }
    d <- d + 1L
    z1 <- .ts2vec_maxpool(z1, len, batch)
    z2 <- .ts2vec_maxpool(z2, len, batch)
    len <- len %/% 2L
  }
  if (len == 1L) {
    loss <- .ad_add(loss, .ad_scale(.ts2vec_contrastive(z1, z2, len, batch, "instance"), alpha))
    d <- d + 1L
  }
  .ad_scale(loss, 1 / d)
}

#' @noRd
.randint <- function(low, high) low + sample.int(high - low + 1L, 1L) - 1L

# Loss of a batch (shared_step of the reference): two overlapping random crops
# of every sequence, encoded separately; the loss compares their common part.
# `draws` replaces the random crops, masks and dropout (validation).
#' @noRd
.ts2vec_step_loss <- function(P, buffers, settings, x, training, draws = NULL) {
  batch <- dim(x)[1]
  if (!is.null(settings$max_train_length) && dim(x)[2] > settings$max_train_length) {
    offset <- .randint(0L, dim(x)[2] - settings$max_train_length)
    x <- x[, offset + seq_len(settings$max_train_length), , drop = FALSE]
  }
  ts_l <- dim(x)[2]
  if (is.null(draws)) {
    crop_l <- .randint(2L^(settings$temporal_unit + 1L), ts_l)
    crop_left <- .randint(0L, ts_l - crop_l)
    crop_eleft <- .randint(0L, crop_left)
    crop_eright <- .randint(crop_left + crop_l, ts_l)
    crop_offset <- vapply(seq_len(batch), function(i) .randint(-crop_eleft, ts_l - crop_eright), integer(1))
  } else {
    crop_l <- draws$crop_l
    crop_left <- draws$crop_left
    crop_eleft <- draws$crop_eleft
    crop_eright <- draws$crop_eright
    crop_offset <- draws$crop_offset
  }
  crop_right <- crop_left + crop_l
  take <- function(start, size) {
    out <- array(0, c(batch, size, dim(x)[3]))
    for (b in seq_len(batch)) out[b, , ] <- x[b, start[b] + seq_len(size), ]
    out
  }
  len1 <- crop_right - crop_eleft
  len2 <- crop_eright - crop_left
  e1 <- .ts2vec_encode(P, buffers, settings, take(crop_offset + crop_eleft, len1), training,
                       mask = draws$mask1, drop = draws$drop1)
  e2 <- .ts2vec_encode(P, e1$buffers, settings, take(crop_offset + crop_left, len2), training,
                       mask = draws$mask2, drop = draws$drop2)
  z1 <- .ad_cols(e1$z, as.vector(outer((len1 - crop_l) + seq_len(crop_l), (seq_len(batch) - 1L) * len1, "+")))
  z2 <- .ad_cols(e2$z, as.vector(outer(seq_len(crop_l), (seq_len(batch) - 1L) * len2, "+")))
  list(loss = .ts2vec_hierarchical(z1, z2, crop_l, batch, settings$temporal_unit), buffers = e2$buffers)
}
