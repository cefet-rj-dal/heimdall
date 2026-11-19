# Finite-difference checks of the gradients of the networks of the WWAggr
# members (automatic differentiation of the package).
suppressMessages(devtools::load_all(quiet = TRUE))
set.seed(2)
fd_check <- function(label, params, loss_fun, n_check = 6) {
  grads <- .ad_record(function() {
    P <- .ad_leaves(params)
    .ad_grad(loss_fun(P, TRUE), P)
  })
  worst <- 0
  for (name in names(params)) {
    idx <- sample(length(params[[name]]), min(n_check, length(params[[name]])))
    for (i in idx) {
      up <- params; up[[name]][i] <- up[[name]][i] + 1e-6
      dn <- params; dn[[name]][i] <- dn[[name]][i] - 1e-6
      num <- (loss_fun(up, FALSE)[1] - loss_fun(dn, FALSE)[1]) / 2e-6
      err <- abs(num - grads[[name]][i]) / max(1e-4, abs(num) + abs(grads[[name]][i]))
      worst <- max(worst, err)
    }
  }
  cat(sprintf("%-28s max relative error %.2e\n", label, worst))
}
x <- array(rnorm(5 * 7 * 3), c(5, 7, 3))
labels <- t(sapply(c(3, 8, 5, 8, 2), function(c) (seq_len(7) >= c) * 1))
for (cfg in list(list(n_layers = 2L, layer_norm = "none"), list(n_layers = 1L, layer_norm = "before"),
                 list(n_layers = 1L, layer_norm = "after"))) {
  settings <- list(hidden_size = 4L, n_layers = cfg$n_layers, layer_norm = cfg$layer_norm, dropout = 0, loss = "bce", T = 3)
  params <- .s2s_init(3, settings)
  for (loss in c("bce", "indid")) {
    settings$loss <- loss
    f <- function(P, rec) {
      p <- .s2s_forward(P, settings, x, training = rec)
      obj <- .indid_objective(.adv(p), labels, settings)
      if (rec) {
        # surrogate scalar with the same gradient: sum(p * dL/dp)
        .ad_sum(.ad_mul(p, obj$grad))
      } else obj$value
    }
    fd_check(sprintf("s2s %s %s", cfg$layer_norm, loss), params, f)
  }
}
# TS-CP2
for (bn in c(FALSE, TRUE)) {
  st <- list(window = 4L, nb_filters = 5L, kernel_size = 3L, nb_stacks = 2L, dilations = c(1L, 2L),
             use_skip_connections = TRUE, batch_norm = bn, n_steps = 3L, code_size = 3L)
  init <- .tscp_init(3, st)
  # Non-zero biases: with zero biases the padded positions sit exactly on the ReLU kink.
  for (nm in grep("_b$", names(init$params), value = TRUE)) init$params[[nm]] <- init$params[[nm]] + rnorm(length(init$params[[nm]]), 0, 0.1)
  xb <- array(rnorm(6 * 8 * 3), c(6, 8, 3))
  f <- function(P, rec) {
    h <- .tscp_encode(P, init$buffers, st, xb[, 1:4, , drop = FALSE], TRUE)
    fu <- .tscp_encode(P, h$buffers, st, xb[, 5:8, , drop = FALSE], TRUE)
    l <- .tscp_loss(h$z, fu$z, 0.5)
    if (rec) l else .adv(l)
  }
  fd_check(sprintf("tscp2 batch_norm=%s", bn), init$params, f)
}
# TS2Vec (masks and dropout fixed through the seed; spectral norm in eval mode
# so that the power iteration does not change between evaluations)
st <- list(window = 3L, output_dim = 4L, hidden_dim = 5L, depth = 2L, spectral_norm = TRUE, mask = "all_true",
           temporal_unit = 0L, dropout = 0)
init <- .ts2vec_init(3, st)
init$params <- lapply(init$params, function(p) p * 3)  # make sigma > 1 so the normalisation is active
xb <- array(rnorm(4 * 9 * 3), c(4, 9, 3))
f <- function(P, rec) {
  set.seed(5)
  out <- .ts2vec_step_loss(P, init$buffers, st, xb, FALSE)
  if (rec) out$loss else .adv(out$loss)
}
fd_check("ts2vec spectral", init$params, f)
st$spectral_norm <- FALSE
init2 <- .ts2vec_init(3, st)
f2 <- function(P, rec) {
  set.seed(7)
  out <- .ts2vec_step_loss(P, init2$buffers, st, xb, FALSE)
  if (rec) out$loss else .adv(out$loss)
}
fd_check("ts2vec plain", init2$params, f2)
