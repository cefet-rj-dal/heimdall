# Step-by-step equivalence of the WWAggr implementation of heimdall with the
# reference code, on the traces of wwaggr_reference.py (mode units).
#   Rscript development/validation/wwaggr/check_units.R <units.json> <out.csv>
suppressMessages(devtools::load_all(quiet = TRUE))
source("development/validation/wwaggr/metrics.R")
args <- commandArgs(trailingOnly = TRUE)
units <- jsonlite::fromJSON(args[1], simplifyVector = TRUE, simplifyDataFrame = FALSE)
results <- NULL
# maxdiff() records the largest absolute reference value it saw, so that each
# row also reports the difference relative to the scale of the compared values.
scale <- 0
record <- function(component, case, difference, note = "") {
  force(difference)
  results <<- rbind(results, data.frame(component = component, case = case, max_abs_difference = difference,
                                        reference_scale = scale, relative = difference / max(scale, 1e-300),
                                        note = note))
  scale <<- 0
}
maxdiff <- function(a, b) {
  scale <<- max(scale, abs(as.numeric(b)))
  max(abs(as.numeric(a) - as.numeric(b)))
}
mat <- function(v) if (is.null(dim(v))) matrix(v, 1) else v

# Aggregation ----------------------------------------------------------------------------------------
agg <- units$aggregation
preds <- agg$preds
cases <- agg$cases
for (case in cases) {
  method <- if (case$distance == "mmd") "mmd" else "wasserstein"
  ours <- wwaggr(preds, method = method, window_size = case$window, anchor = case$anchor, p = case$p,
                 kernel = case$kernel)
  label <- sprintf("%s p=%d %s %s w=%d", method, case$p, if (method == "mmd") case$kernel else "",
                   case$anchor, case$window)
  record("aggregation", label, maxdiff(ours, case$scores))
}
for (name in c("mean", "min", "max", "median")) {
  record("aggregation", name, maxdiff(wwaggr(preds, method = name), agg[[name]]))
}
record("aggregation", "quantile 0.3", maxdiff(wwaggr(preds, method = "quantile", q = 0.3), agg$q30))

# Calibration ----------------------------------------------------------------------------------------
cal <- units$calibration
for (case in cal$cases) {
  parameters <- case$parameters
  fitted <- heimdall:::.calibration_fit(cal$scores, cal$labels, "beta", parameters)
  record("beta calibration", paste(parameters, "map (a, b, m)"), maxdiff(fitted$map, case$map),
         "scikit-learn lbfgs (tol 1e-4) on float32 against glm")
  record("beta calibration", paste(parameters, "predictions"),
         maxdiff(heimdall:::.calibration_apply(fitted, cal$test), case$predictions))
}
for (case in cal$tight) {
  fitted <- heimdall:::.calibration_fit(cal$scores, cal$labels, "beta", case$parameters)
  record("beta calibration", paste(case$parameters, "map, tight tolerance (tol 1e-12, float64)"),
         maxdiff(fitted$map, case$map))
  record("beta calibration", paste(case$parameters, "predictions, tight tolerance"),
         maxdiff(heimdall:::.calibration_apply(fitted, cal$test), case$predictions))
}
fitted <- heimdall:::.calibration_fit(1 - cal$scores, cal$labels, "beta", "abm")
record("beta calibration", "abm, negative coefficient: map", maxdiff(fitted$map, cal$negative$map))
record("beta calibration", "abm, negative coefficient: predictions",
       maxdiff(heimdall:::.calibration_apply(fitted, cal$test), cal$negative$predictions))

# Metrics --------------------------------------------------------------------------------------------
met <- units$metrics
ours <- wwaggr_metrics(met$prob, met$labels, met$thresholds, margins = c(1, 2, 4))$summary
reference <- c(met$audc, met$time_to_fa, met$delay, met$f1, met$cover, met$max_cover, met$best_threshold,
               met$f1_margins)
record("metrics", "AUDC, time to FA, delay, F1, cover, max cover, threshold, F1 margins 1/2/4",
       maxdiff(unlist(ours[1, ]), reference))

# Seq2seq --------------------------------------------------------------------------------------------
s2s <- units$seq2seq
gates <- c("f", "i", "o", "c")
s2s_params <- function(state, layer_norm) {
  p <- list(out_w = mat(state[["linear.weight"]]), out_b = state[["linear.bias"]])
  if (layer_norm == "none") {
    for (l in 0:1) {
      p[[paste0("W_ih", l + 1)]] <- state[[sprintf("lstm.weight_ih_l%d", l)]]
      p[[paste0("W_hh", l + 1)]] <- state[[sprintf("lstm.weight_hh_l%d", l)]]
      p[[paste0("b_ih", l + 1)]] <- state[[sprintf("lstm.bias_ih_l%d", l)]]
      p[[paste0("b_hh", l + 1)]] <- state[[sprintf("lstm.bias_hh_l%d", l)]]
    }
  } else {
    ln <- if (layer_norm == "before") 0 else 1
    lin <- 1 - ln
    for (j in 0:3) {
      for (part in c("w", "u")) {
        prefix <- toupper(part)
        key <- function(k, what) sprintf("lstm.linear_%s.%d.%d.%s", part, j, k, what)
        p[[paste0(prefix, gates[j + 1], "_w")]] <- mat(state[[key(lin, "weight")]])
        p[[paste0(prefix, gates[j + 1], "_b")]] <- state[[key(lin, "bias")]]
        p[[paste0("ln", prefix, gates[j + 1], "_gamma")]] <- state[[key(ln, "weight")]]
        p[[paste0("ln", prefix, gates[j + 1], "_beta")]] <- state[[key(ln, "bias")]]
      }
    }
  }
  p
}
s2s_name <- function(name, layer_norm) {
  # torch parameter name of an R parameter (inverse of s2s_params).
  if (name == "out_w") return("linear.weight")
  if (name == "out_b") return("linear.bias")
  if (layer_norm == "none") {
    l <- as.integer(sub(".*([0-9])$", "\\1", name)) - 1L
    base <- sub("[0-9]$", "", name)
    return(sprintf("lstm.%s_l%d", c(W_ih = "weight_ih", W_hh = "weight_hh", b_ih = "bias_ih", b_hh = "bias_hh")[[base]], l))
  }
  ln <- if (layer_norm == "before") 0 else 1
  lin <- 1 - ln
  is_ln <- startsWith(name, "ln")
  core <- if (is_ln) sub("^ln", "", name) else name
  part <- tolower(substr(core, 1, 1))
  j <- match(substr(core, 2, 2), gates) - 1L
  what <- if (grepl("_(w|gamma)$", core)) "weight" else "bias"
  sprintf("lstm.linear_%s.%d.%d.%s", part, j, if (is_ln) ln else lin, what)
}
for (case in s2s$cases) {
  layer_norm <- if (case$name == "lstm2") "none" else case$name
  settings <- list(hidden_size = 4L, n_layers = if (layer_norm == "none") 2L else 1L, layer_norm = layer_norm,
                   dropout = 0, loss = case$loss_type, T = 4)
  params <- s2s_params(case$params, layer_norm)
  out <- .ad_record(function() {
    P <- .ad_leaves(params)
    p <- .s2s_forward(P, settings, s2s$x, training = TRUE)
    objective <- .indid_objective(p$value, s2s$labels, settings)
    list(p = p$value, loss = objective$value, grads = .ad_grad(p, P, objective$grad))
  })
  label <- paste(case$name, case$loss_type)
  record("seq2seq", paste(label, "probabilities"), maxdiff(out$p, case$p))
  record("seq2seq", paste(label, "loss"), maxdiff(out$loss, case$loss))
  record("seq2seq", paste(label, "gradients"),
         max(vapply(names(out$grads), function(n) maxdiff(out$grads[[n]], case$grads[[s2s_name(n, layer_norm)]]),
                    numeric(1))))
}

# TS-CP2 ---------------------------------------------------------------------------------------------
tscp_names <- function(state) {
  params <- list()
  buffers <- list()
  for (key in names(state)) {
    value <- state[[key]]
    m <- regmatches(key, regexec("^tcn_layer\\.residual_blocks\\.([0-9]+)\\.([a-z_0-9]+)\\.([a-z_]+)$", key))[[1]]
    if (length(m)) {
      block <- paste0("block", as.integer(m[2]) + 1L)
      layer <- c(conv_1 = "conv1", conv_2 = "conv2", downsample = "down", bn_1 = "bn1", bn_2 = "bn2", ln_1 = "",
                 ln_2 = "")[[m[3]]]
      if (layer == "") next
      what <- m[4]
      if (startsWith(layer, "conv") || layer == "down") {
        params[[sprintf("%s_%s_%s", block, layer, if (what == "weight") "w" else "b")]] <- if (what == "weight") mat(value) else value
      } else if (what %in% c("weight", "bias")) {
        params[[sprintf("%s_%s_%s", block, layer, if (what == "weight") "gamma" else "beta")]] <- value
      } else if (what %in% c("running_mean", "running_var")) {
        buffers[[sprintf("%s_%s_%s", block, layer, if (what == "running_mean") "mean" else "var")]] <- value
      }
      next
    }
    m <- regmatches(key, regexec("^(fc1|fc2|output_layer)\\.(weight|bias)$", key))[[1]]
    if (length(m)) {
      layer <- if (m[2] == "output_layer") "output" else m[2]
      params[[paste0(layer, if (m[3] == "weight") "_w" else "_b")]] <- if (m[3] == "weight") mat(value) else value
    }
  }
  list(params = params, buffers = buffers)
}
tscp_torch_name <- function(name) {
  m <- regmatches(name, regexec("^block([0-9]+)_(conv1|conv2|down|bn1|bn2)_(w|b|gamma|beta)$", name))[[1]]
  if (length(m)) {
    layer <- c(conv1 = "conv_1", conv2 = "conv_2", down = "downsample", bn1 = "bn_1", bn2 = "bn_2")[[m[3]]]
    what <- if (m[4] %in% c("w", "gamma")) "weight" else "bias"
    return(sprintf("tcn_layer.residual_blocks.%d.%s.%s", as.integer(m[2]) - 1L, layer, what))
  }
  m <- regmatches(name, regexec("^(fc1|fc2|output)_(w|b)$", name))[[1]]
  sprintf("%s.%s", if (m[2] == "output") "output_layer" else m[2], if (m[3] == "w") "weight" else "bias")
}
tscp_settings <- function(batch_norm) {
  list(window = 4L, nb_filters = 6L, kernel_size = 3L, nb_stacks = 2L, dilations = c(1L, 2L),
       use_skip_connections = TRUE, batch_norm = batch_norm, n_steps = 3L, code_size = 4L)
}
xb <- units$tscp$x
for (case in units$tscp$cases) {
  st <- tscp_settings(case$batch_norm)
  model <- tscp_names(case$params)
  out <- .ad_record(function() {
    P <- .ad_leaves(model$params)
    h <- .tscp_encode(P, model$buffers, st, xb[, 1:4, , drop = FALSE], TRUE)
    f <- .tscp_encode(P, h$buffers, st, xb[, 5:8, , drop = FALSE], TRUE)
    loss <- .tscp_loss(h$z, f$z, 0.5)
    list(loss = loss$value[1], grads = .ad_grad(loss, P), buffers = f$buffers)
  })
  label <- if (case$batch_norm) "batch norm" else "no batch norm"
  record("TS-CP2", paste(label, "loss"), maxdiff(out$loss, case$loss))
  record("TS-CP2", paste(label, "gradients"),
         max(vapply(names(out$grads), function(n) maxdiff(out$grads[[n]], case$grads[[tscp_torch_name(n)]]),
                    numeric(1))))
  if (case$batch_norm) {
    after <- case$after
    diffs <- vapply(names(out$buffers), function(n) {
      m <- regmatches(n, regexec("^block([0-9]+)_(bn1|bn2)_(mean|var)$", n))[[1]]
      key <- sprintf("tcn_layer.residual_blocks.%d.%s.running_%s", as.integer(m[2]) - 1L,
                     c(bn1 = "bn_1", bn2 = "bn_2")[[m[3]]], m[4])
      maxdiff(out$buffers[[n]], after[[key]])
    }, numeric(1))
    record("TS-CP2", "batch norm running statistics", max(diffs))
  }
}

# TS2Vec ---------------------------------------------------------------------------------------------
ts2vec_names <- function(state) {
  params <- list()
  buffers <- list()
  for (key in names(state)) {
    value <- state[[key]]
    if (key == "input_fc.weight") params$input_fc_w <- mat(value)
    if (key == "input_fc.bias") params$input_fc_b <- value
    m <- regmatches(key, regexec("^feature_extractor\\.net\\.([0-9]+)\\.(conv1\\.conv|conv2\\.conv|projector)\\.(.*)$", key))[[1]]
    if (!length(m)) next
    name <- sprintf("block%d_%s", as.integer(m[2]) + 1L, c(conv1.conv = "conv1", conv2.conv = "conv2",
                                                            projector = "proj")[[m[3]]])
    what <- m[4]
    if (what == "bias") params[[paste0(name, "_b")]] <- value
    if (what %in% c("weight", "parametrizations.weight.original")) params[[paste0(name, "_w")]] <- mat(value)
    if (what == "parametrizations.weight.0._u") buffers[[name]]$u <- value
    if (what == "parametrizations.weight.0._v") buffers[[name]]$v <- value
  }
  list(params = params, buffers = buffers)
}
ts2vec_torch_name <- function(name) {
  if (name == "input_fc_w") return("input_fc.weight")
  if (name == "input_fc_b") return("input_fc.bias")
  m <- regmatches(name, regexec("^block([0-9]+)_(conv1|conv2|proj)_(w|b)$", name))[[1]]
  layer <- c(conv1 = "conv1.conv", conv2 = "conv2.conv", proj = "projector")[[m[3]]]
  sprintf("feature_extractor.net.%d.%s.%s", as.integer(m[2]) - 1L, layer,
          if (m[4] == "w") "parametrizations.weight.original" else "bias")
}
tv <- units$ts2vec
model <- ts2vec_names(tv$params)
settings <- list(window = 3L, output_dim = 4L, hidden_dim = 5L, depth = 2L, spectral_norm = TRUE, mask = "binomial",
                 temporal_unit = 0L, dropout = 0.1, max_train_length = NULL)
ri <- tv$randint
to_conv <- function(d) {
  d <- array(d, dim(d))
  matrix(aperm(d, c(2, 3, 1)), dim(d)[2])
}
# The two calls of the encoder may have crops of different lengths: lists, or arrays when equal.
piece <- function(x, k) {
  if (is.list(x)) return(x[[k]])
  d <- dim(x)
  array(x[k, , , drop = FALSE][seq_len(prod(d[-1]))], d[-1])
}
piece4 <- function(x, k) {
  if (is.list(x)) return(x[[k]])
  d <- dim(x)
  array(x[k, , , , drop = FALSE], d[-1])
}
draws <- list(crop_l = ri[[1]], crop_left = ri[[2]], crop_eleft = ri[[3]], crop_eright = ri[[4]],
              crop_offset = as.integer(ri[[5]]), mask1 = piece(tv$masks, 1) == 1, mask2 = piece(tv$masks, 2) == 1,
              drop1 = to_conv(piece4(tv$drops, 1)), drop2 = to_conv(piece4(tv$drops, 2)))
out <- .ad_record(function() {
  P <- .ad_leaves(model$params)
  res <- .ts2vec_step_loss(P, model$buffers, settings, tv$x, TRUE, draws = draws)
  list(loss = res$loss$value[1], grads = .ad_grad(res$loss, P), buffers = res$buffers)
})
record("SN-TS2Vec", "loss", maxdiff(out$loss, tv$loss))
record("SN-TS2Vec", "gradients",
       max(vapply(names(out$grads), function(n) maxdiff(out$grads[[n]], tv$grads[[ts2vec_torch_name(n)]]),
                  numeric(1))))
uv <- vapply(names(out$buffers), function(n) {
  layer <- sub("^block[0-9]+_", "", n)
  block <- as.integer(sub("^block([0-9]+)_.*$", "\\1", n)) - 1L
  key <- sprintf("feature_extractor.net.%d.%s.parametrizations.weight.0", block,
                 c(conv1 = "conv1.conv", conv2 = "conv2.conv", proj = "projector")[[layer]])
  max(maxdiff(out$buffers[[n]]$u, tv$after[[paste0(key, "._u")]]), maxdiff(out$buffers[[n]]$v, tv$after[[paste0(key, "._v")]]))
}, numeric(1))
record("SN-TS2Vec", "power iteration vectors", max(uv))

# Optimisers -----------------------------------------------------------------------------------------
opt <- units$optimizers
for (name in names(opt$paths)) {
  net <- .indid_reset_adam(list(params = list(w = opt$start)))
  schedule <- if (name == "adam_cosine") list(type = "cosine", t_max = 5) else
    list(type = "warm_restarts", t_0 = 3, t_mult = 2)
  wd <- if (name == "adam_cosine") 0 else 0.2
  path <- NULL
  for (epoch in 0:3) {
    lr <- .nn_learning_rate(schedule, 0.01, epoch)
    for (k in 1:3) net <- .nn_optimizer_step(net, list(w = opt$grads[epoch * 3 + k, ]), lr, wd)
    path <- rbind(path, net$params$w)
  }
  record("optimisers", name, maxdiff(path, opt$paths[[name]]))
}

# Predictions of the representation models ---------------------------------------------------------------
pr <- units$predictions
for (case in pr$cases) {
  name <- case$model
  if (startsWith(name, "tscp")) {
    m <- tscp_names(pr$models[[name]])
    st <- c(tscp_settings(name == "tscp_bn"), list(step = case$step, alpha = case$alpha))
    model <- structure(list(params = m$params, buffers = m$buffers, settings = st, dim = 3L), class = c("tscp2", "cpd_model"))
  } else {
    m <- ts2vec_names(pr$models[[name]])
    st <- c(settings, list(step = case$step, alpha = case$alpha))
    st$window <- case$window
    model <- structure(list(params = m$params, buffers = m$buffers, settings = st, dim = 3L), class = c("ts2vec", "cpd_model"))
  }
  ours <- cpd_scores(model, pr$x)
  ours <- ours[, colSums(is.na(ours)) == 0, drop = FALSE]
  record("representation scores", sprintf("%s step=%d alpha=%.1f", name, case$step, case$alpha),
         maxdiff(ours, case$scores))
}

results$max_abs_difference <- signif(results$max_abs_difference, 3)
utils::write.csv(results, args[2], row.names = FALSE)
print(results, right = FALSE)
