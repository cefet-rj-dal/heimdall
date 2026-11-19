# Layers, initialisation and training loop shared by the networks of the
# WWAggr ensemble members, on top of the automatic differentiation of
# nn_autodiff.R.
#
# Layouts: a batch of B sequences of length L with C channels is a matrix
# [C, B * L] whose column (b - 1) * L + t holds time t of sequence b
# ("conv layout"); rows of a [B, F] matrix are samples ("row layout").

# Initialisation --------------------------------------------------------------------------------

# torch.nn.Linear / Conv1d defaults (kaiming uniform with a = sqrt(5)):
# weights and biases uniform within +-1/sqrt(fan_in).
#' @noRd
.nn_uniform <- function(rows, cols, fan_in) {
  bound <- 1 / sqrt(fan_in)
  matrix(stats::runif(rows * cols, -bound, bound), rows, cols, byrow = TRUE)
}

#' @noRd
.nn_linear_init <- function(params, name, n_in, n_out) {
  params[[paste0(name, "_w")]] <- .nn_uniform(n_out, n_in, n_in)
  params[[paste0(name, "_b")]] <- as.vector(.nn_uniform(1, n_out, n_in))
  params
}

# Conv1d weight [out, in * K] in the memory order of torch (column i * K + k).
#' @noRd
.nn_conv_init <- function(params, name, n_in, n_out, kernel, normal_sd = NULL) {
  if (is.null(normal_sd)) {
    params[[paste0(name, "_w")]] <- .nn_uniform(n_out, n_in * kernel, n_in * kernel)
    params[[paste0(name, "_b")]] <- as.vector(.nn_uniform(1, n_out, n_in * kernel))
  } else {
    params[[paste0(name, "_w")]] <- matrix(stats::rnorm(n_out * n_in * kernel, 0, normal_sd), n_out, n_in * kernel,
                                           byrow = TRUE)
    params[[paste0(name, "_b")]] <- numeric(n_out)
  }
  params
}

# Layers ----------------------------------------------------------------------------------------

# Linear layer on rows: x [B, in] -> [B, out].
#' @noRd
.nn_linear <- function(P, name, x) {
  .ad_add_row(.ad_mm(x, .ad_t(P[[paste0(name, "_w")]])), P[[paste0(name, "_b")]])
}

# Linear layer (or 1x1 convolution) on the conv layout: x [in, N] -> [out, N].
#' @noRd
.nn_pointwise <- function(P, name, x, weight = P[[paste0(name, "_w")]]) {
  .ad_add_col(.ad_mm(weight, x), P[[paste0(name, "_b")]])
}

# Index of the im2col matrix [K * C, B * L] of x [C, B * L]: block k holds x
# shifted by `shifts[k]` steps (time t reads t - shifts[k]; zero outside the
# sequence), as a dilated convolution with zero padding.
#' @noRd
.nn_cache <- new.env(parent = emptyenv())

#' @noRd
.nn_im2col_index <- function(channels, len, batch, shifts) {
  key <- paste(channels, len, batch, paste(shifts, collapse = ","), sep = ":")
  cached <- .nn_cache[[key]]
  if (!is.null(cached)) return(cached)
  if (length(ls(.nn_cache)) > 64L) rm(list = ls(.nn_cache), envir = .nn_cache)
  index <- .nn_im2col_build(channels, len, batch, shifts)
  assign(key, index, envir = .nn_cache)
  index
}

#' @noRd
.nn_im2col_build <- function(channels, len, batch, shifts) {
  cols <- batch * len
  t <- rep(seq_len(len), batch)
  blocks <- lapply(shifts, function(s) {
    source <- t - s
    valid <- (source >= 1L) & (source <= len)
    col <- seq_len(cols) - s
    index <- outer(seq_len(channels), (col - 1L) * channels, "+")
    index[, !valid] <- 0L
    index
  })
  do.call(rbind, blocks)
}

# 1-D convolution on the conv layout with kernel K and dilation d. `pad_left`
# is the number of zeros before the sequence: (K - 1) d for a causal
# convolution, d for the "same" convolution of TS2Vec (K = 3). The weight
# [out, in * K] is in the order of torch.
# One operation of the tape, which keeps only the input and recomputes the
# im2col matrix (kernel times larger) for the gradients.
#' @noRd
.nn_conv1d <- function(P, name, x, len, kernel, dilation, pad_left, weight = P[[paste0(name, "_w")]]) {
  bias <- P[[paste0(name, "_b")]]
  xv <- .adv(x)
  wv <- .adv(weight)
  channels <- nrow(xv)
  batch <- ncol(xv) %/% len
  shifts <- pad_left - (seq_len(kernel) - 1L) * dilation
  index <- .nn_im2col_index(channels, len, batch, shifts)
  order <- as.vector(outer((seq_len(channels) - 1L) * kernel, seq_len(kernel), "+"))
  im2col <- function() {
    ok <- index > 0L
    stacked <- numeric(length(index))
    stacked[ok] <- xv[index[ok]]
    array(stacked, dim(index))
  }
  wr <- wv[, order, drop = FALSE]
  value <- wr %*% im2col() + as.vector(.adv(bias))
  .ad_out(value, list(x, weight, bias), function(g) {
    gx <- NULL
    gw <- NULL
    if (.ad_is(x)) {
      gs <- crossprod(wr, g)
      gx <- numeric(length(xv))
      for (k in seq_len(kernel)) {
        r <- (k - 1L) * channels + seq_len(channels)
        ik <- index[r, , drop = FALSE]
        keep <- ik > 0L
        gx[ik[keep]] <- gx[ik[keep]] + gs[r, , drop = FALSE][keep]
      }
      gx <- array(gx, dim(xv))
    }
    if (.ad_is(weight)) {
      gw <- array(0, dim(wv))
      gw[, order] <- tcrossprod(g, im2col())
    }
    list(gx, gw, if (.ad_is(bias)) .ad_shape(rowSums(g), .adv(bias)))
  })
}

# BatchNorm1d on the conv layout (statistics per channel over all positions).
# Training uses the batch statistics and updates the running ones (momentum
# 0.1, unbiased variance), as torch; evaluation uses the running ones.
#' @noRd
.nn_batchnorm <- function(P, name, x, buffers, training, eps = 1e-5, momentum = 0.1) {
  gamma <- P[[paste0(name, "_gamma")]]
  beta <- P[[paste0(name, "_beta")]]
  mean_name <- paste0(name, "_mean")
  var_name <- paste0(name, "_var")
  if (training) {
    xv <- .adv(x)
    n <- ncol(xv)
    m <- rowMeans(xv)
    v <- rowMeans((xv - m)^2)
    buffers[[mean_name]] <- (1 - momentum) * buffers[[mean_name]] + momentum * m
    buffers[[var_name]] <- (1 - momentum) * buffers[[var_name]] + momentum * v * n / max(n - 1, 1)
    out <- .ad_norm_rows(x, gamma, beta, eps, by = "row")
  } else {
    normalised <- .ad_mul_col(.ad_add_col(x, -buffers[[mean_name]]), 1 / sqrt(buffers[[var_name]] + eps))
    out <- .ad_add_col(.ad_mul_col(normalised, gamma), beta)
  }
  list(out = out, buffers = buffers)
}

# Dropout mask (torch.nn.functional.dropout): keep with probability 1 - p,
# scaled by 1 / (1 - p).
#' @noRd
.nn_dropout_mask <- function(dims, p) {
  if (p <= 0) return(NULL)
  array((stats::runif(prod(dims)) >= p) / (1 - p), dims)
}

# Spectral normalisation ----------------------------------------------------------------------

#' @noRd
.nn_normalize <- function(v, eps = 1e-12) v / max(sqrt(sum(v^2)), eps)

# Buffers u and v of torch's _SpectralNorm: random normal, normalised, then 15
# power iterations at registration.
#' @noRd
.nn_spectral_init <- function(w) {
  u <- .nn_normalize(stats::rnorm(nrow(w)))
  v <- .nn_normalize(stats::rnorm(ncol(w)))
  .nn_power_iteration(w, list(u = u, v = v), 15L)
}

# _SpectralNorm._power_method of torch: u = normalize(W v), then
# v = normalize(W' u).
#' @noRd
.nn_power_iteration <- function(w, uv, iterations = 1L) {
  for (i in seq_len(iterations)) {
    uv$u <- .nn_normalize(as.vector(w %*% uv$v))
    uv$v <- .nn_normalize(as.vector(crossprod(w, uv$u)))
  }
  uv
}

# Weight of a spectrally normalised layer: in training, one power iteration
# first (as every access of the parametrised weight in torch).
#' @noRd
.nn_spectral_weight <- function(P, name, buffers, training, max_norm = 1) {
  w <- P[[paste0(name, "_w")]]
  uv <- buffers[[name]]
  if (training) {
    uv <- .nn_power_iteration(.adv(w), uv, 1L)
    buffers[[name]] <- uv
  }
  list(weight = .ad_spectral(w, uv$u, uv$v, max_norm), buffers = buffers)
}

# Optimisation ---------------------------------------------------------------------------------

# Adam (weight_decay = 0) or AdamW (decoupled weight decay) with the defaults
# of torch.
#' @noRd
.nn_optimizer_step <- function(net, grads, learning_rate, weight_decay = 0) {
  if (weight_decay > 0) {
    net$params <- lapply(net$params, function(p) p * (1 - learning_rate * weight_decay))
  }
  .adam_step(net, grads, learning_rate)
}

# Learning rate of an epoch (0-based) for the schedulers of the reference,
# stepped once per epoch (the default interval of Lightning).
#' @noRd
.nn_learning_rate <- function(schedule, base, epoch) {
  switch(schedule$type,
         constant = base,
         cosine = base * (1 + cos(pi * epoch / schedule$t_max)) / 2,
         warm_restarts = {
           t_i <- schedule$t_0
           t_cur <- epoch
           while (t_cur >= t_i) {
             t_cur <- t_cur - t_i
             t_i <- t_i * schedule$t_mult
           }
           base * (1 + cos(pi * t_cur / t_i)) / 2
         })
}

#' @noRd
.nn_batches <- function(n, size, shuffle) {
  order <- if (shuffle) sample.int(n) else seq_len(n)
  split(order, (seq_len(n) - 1L) %/% size)
}

# Training loop. `step(net, rows)` returns list(net, loss) after one optimiser
# step on the training rows; `validate(net)` returns the validation loss.
# Early stopping as Lightning's EarlyStopping (mode min): an epoch improves
# when its monitored loss is below the best one minus `min_delta`; training
# stops after `patience` epochs without improvement. The model of the last
# epoch is kept. `monitor` is "val" or "train" (mean of the batch losses
# weighted by the batch sizes, Lightning's train_loss_epoch).
#' @noRd
.nn_train <- function(net, n, step, settings, validate = NULL) {
  best <- Inf
  wait <- 0L
  history <- NULL
  monitor <- settings$monitor
  use_validation <- !is.null(validate) && (monitor == "val")
  for (epoch in seq_len(settings$epochs)) {
    net$learning_rate <- .nn_learning_rate(settings$schedule, settings$learning_rate, epoch - 1L)
    total <- 0
    for (rows in .nn_batches(n, settings$batch_size, TRUE)) {
      result <- step(net, rows)
      net <- result$net
      total <- total + result$loss * length(rows)
    }
    train_loss <- total / n
    val_loss <- if (use_validation) validate(net) else NA_real_
    history <- rbind(history, data.frame(epoch = epoch, learning_rate = net$learning_rate, train_loss = train_loss,
                                         val_loss = val_loss))
    current <- if (monitor == "train") train_loss else val_loss
    if (!is.na(current)) {
      if (current < best - settings$min_delta) {
        best <- current
        wait <- 0L
      } else {
        wait <- wait + 1L
        if (wait >= settings$patience) break
      }
    }
  }
  list(net = net, history = history)
}
