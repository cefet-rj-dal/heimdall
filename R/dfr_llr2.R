#' @title Semiparametric Bayesian network
#' @description Fits a semiparametric Bayesian network (Atienza et al., 2022)
#' to continuous data: every node has either a linear Gaussian conditional
#' density or a conditional kernel density estimate (CKDE) given its parents.
#' It is the density model of [dfr_llr2()].
#' @param data A data frame or matrix of numeric variables (no missing values).
#' @param structure Optional structure: a named list giving the parents of
#' every variable (`list(B = "A", C = c("A", "B"), A = character(0))`) or a
#' two-column matrix of arcs (`from`, `to`). `NULL` (default) learns it by
#' greedy hill climbing.
#' @param node_types Optional named character vector of node types (`"lg"` for
#' linear Gaussian, `"ckde"` for conditional KDE). `NULL` (default) chooses each
#' node's type by cross-validated log-likelihood.
#' @param max_parents Largest number of parents of a node in the learned
#' structure.
#' @param folds Number of folds of the cross-validation used to choose the
#' node types.
#' @details Structure learning is a greedy hill climbing (arc addition, removal
#' and reversal) with the BIC score of a Gaussian Bayesian network. The type of
#' each node is then chosen given its parents: the type with the larger
#' log-likelihood over `folds` contiguous folds (each fold scored by the CPD
#' fitted on the other folds).
#'
#' A linear Gaussian CPD is the least-squares regression of the node on its
#' parents with the maximum likelihood variance. A CKDE CPD is
#' `f(x, pa) / f(pa)`, the ratio of Gaussian kernel density estimates with the
#' bandwidth matrix of the normal reference rule,
#' `H = (4 / (d + 2))^(2 / (d + 4)) S N^(-2 / (d + 4))` (`S` the sample covariance
#' of the node and its parents, `d` their number, `N` the sample size); the
#' density of the parents uses the corresponding block of `H`, so the
#' conditional density integrates to one.
#'
#' The paper uses the binned version (B-SPBN), which speeds up the kernel sums
#' by binning the data on a grid, and learns the structure and the node types
#' together by hill climbing with a cross-validated score. Here the kernel sums
#' are exact and the node types are chosen after the structure.
#' @references Atienza, D., Bielza, C. and Larrañaga, P. (2022). Semiparametric
#' Bayesian networks. *Information Sciences*, 584, 564-582.
#' <doi:10.1016/j.ins.2021.10.074>
#' @return An `spbn` object: the variables, their parents, node types and the
#' fitted CPDs.
#' @seealso [spbn_loglik()], [dfr_llr2()].
#' @examples
#' set.seed(1)
#' a <- stats::rnorm(500)
#' b <- 2 * a + stats::rnorm(500, sd = 0.5)
#' c <- ifelse(stats::runif(500) < 0.5, b - 3, b + 3) + stats::rnorm(500)
#' model <- spbn(data.frame(a, b, c))
#' model
#' colMeans(spbn_loglik(model, data.frame(a, b, c)))
#' @export
spbn <- function(data, structure = NULL, node_types = NULL, max_parents = 4, folds = 5) {
  .check_positive_integer(max_parents, "max_parents", min_value = 0L)
  .check_positive_integer(folds, "folds", min_value = 2L)
  data <- .spbn_matrix(data)
  variables <- colnames(data)
  parents <- if (is.null(structure)) .spbn_hill_climbing(data, max_parents) else .spbn_structure(structure, variables)
  if (is.null(node_types)) {
    node_types <- vapply(variables, function(v) .spbn_choose_type(data, v, parents[[v]], folds), character(1))
  } else {
    node_types <- .spbn_types(node_types, variables)
  }
  .spbn_fit(data, parents, node_types)
}

#' @title Log-likelihood of each node of a semiparametric Bayesian network
#' @description Evaluates `log f(x_i | pa_i)` for every observation and node of
#' an [spbn()] model.
#' @param model An `spbn` object.
#' @param data A data frame or matrix with the model's variables.
#' @return A matrix with one row per observation and one column per node.
#' @examples
#' set.seed(1)
#' x <- data.frame(a = stats::rnorm(200), b = stats::rnorm(200))
#' head(spbn_loglik(spbn(x), x))
#' @export
spbn_loglik <- function(model, data) {
  if (!inherits(model, "spbn")) {
    stop("spbn_loglik: model must be an spbn object", call. = FALSE)
  }
  data <- .spbn_matrix(data, model$variables)
  out <- vapply(model$variables, function(v) .spbn_cpd_loglik(model$cpds[[v]], data), numeric(nrow(data)))
  matrix(out, nrow(data), dimnames = list(NULL, model$variables))
}

#' @export
print.spbn <- function(x, ...) {
  arcs <- sum(lengths(x$parents))
  cat(sprintf("Semiparametric Bayesian network: %d nodes (%d linear Gaussian, %d CKDE), %d arcs\n",
              length(x$variables), sum(x$node_types == "lg"), sum(x$node_types == "ckde"), arcs))
  for (v in x$variables) {
    cat(sprintf("  %s [%s] <- %s\n", v, x$node_types[[v]],
                if (length(x$parents[[v]])) paste(x$parents[[v]], collapse = ", ") else "(none)"))
  }
  invisible(x)
}

# Data and structures --------------------------------------------------------------------------

#' @noRd
.spbn_matrix <- function(data, variables = NULL) {
  if (is.matrix(data) && is.null(colnames(data))) {
    colnames(data) <- paste0("V", seq_len(ncol(data)))
  }
  data <- as.data.frame(data)
  if (!is.null(variables)) {
    missing_vars <- setdiff(variables, names(data))
    if (length(missing_vars) > 0L) {
      stop(sprintf("spbn: the data lack the variables %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
    }
    data <- data[variables]
  }
  if (ncol(data) == 0L || !all(vapply(data, is.numeric, logical(1)))) {
    stop("spbn: the data must have numeric variables", call. = FALSE)
  }
  data <- as.matrix(data)
  if (anyNA(data)) {
    stop("spbn: the data contain missing values", call. = FALSE)
  }
  storage.mode(data) <- "double"
  data
}

#' @noRd
.spbn_structure <- function(structure, variables) {
  if (is.matrix(structure) || is.data.frame(structure)) {
    arcs <- as.matrix(structure)
    parents <- lapply(stats::setNames(variables, variables), function(v) as.character(arcs[arcs[, 2] == v, 1]))
  } else if (is.list(structure)) {
    parents <- lapply(stats::setNames(variables, variables), function(v) as.character(structure[[v]]))
  } else {
    stop("spbn: structure must be a list of parents or a matrix of arcs", call. = FALSE)
  }
  unknown <- setdiff(unlist(parents), variables)
  if (length(unknown) > 0L) {
    stop(sprintf("spbn: unknown variables in the structure: %s", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  if (.spbn_cyclic(parents)) {
    stop("spbn: the structure has a cycle", call. = FALSE)
  }
  parents
}

#' @noRd
.spbn_types <- function(node_types, variables) {
  if (is.null(names(node_types)) || !all(variables %in% names(node_types)) || !all(node_types %in% c("lg", "ckde"))) {
    stop("spbn: node_types must name every variable with 'lg' or 'ckde'", call. = FALSE)
  }
  node_types[variables]
}

#' @noRd
.spbn_cyclic <- function(parents) {
  remaining <- names(parents)
  while (length(remaining) > 0L) {
    roots <- remaining[vapply(remaining, function(v) !any(parents[[v]] %in% remaining), logical(1))]
    if (length(roots) == 0L) return(TRUE)
    remaining <- setdiff(remaining, roots)
  }
  FALSE
}

# Is there a directed path from `from` to `to`?
#' @noRd
.spbn_path <- function(parents, from, to) {
  children <- function(v) names(parents)[vapply(parents, function(p) v %in% p, logical(1))]
  seen <- character(0)
  frontier <- from
  while (length(frontier) > 0L) {
    if (to %in% frontier) return(TRUE)
    seen <- c(seen, frontier)
    frontier <- setdiff(unique(unlist(lapply(frontier, children))), seen)
  }
  FALSE
}

# Structure learning: hill climbing with the Gaussian BIC ---------------------------------------

#' @noRd
.spbn_bic <- function(data, node, parents) {
  y <- data[, node]
  n <- length(y)
  x <- cbind(1, data[, parents, drop = FALSE])
  fit <- stats::lm.fit(x, y)
  variance <- max(sum(fit$residuals^2) / n, 1e-300)
  -n / 2 * (log(2 * pi * variance) + 1) - (length(parents) + 2) / 2 * log(n)
}

#' @noRd
.spbn_hill_climbing <- function(data, max_parents) {
  variables <- colnames(data)
  parents <- lapply(stats::setNames(variables, variables), function(v) character(0))
  cache <- new.env(hash = TRUE)
  score <- function(node, pa) {
    key <- paste(node, paste(sort(pa), collapse = ","), sep = "|")
    if (is.null(cache[[key]])) cache[[key]] <- .spbn_bic(data, node, pa)
    cache[[key]]
  }
  repeat {
    best <- list(delta = 1e-8)
    for (from in variables) {
      for (to in variables) {
        if (from == to) next
        current_to <- parents[[to]]
        current_from <- parents[[from]]
        if (from %in% current_to) {
          # Removal of from -> to.
          delta <- score(to, setdiff(current_to, from)) - score(to, current_to)
          if (delta > best$delta) best <- list(delta = delta, op = "remove", from = from, to = to)
          # Reversal to to -> from.
          if (length(current_from) < max_parents) {
            trial <- parents
            trial[[to]] <- setdiff(current_to, from)
            if (!.spbn_path(trial, from, to)) {
              delta <- score(to, setdiff(current_to, from)) - score(to, current_to) +
                score(from, c(current_from, to)) - score(from, current_from)
              if (delta > best$delta) best <- list(delta = delta, op = "reverse", from = from, to = to)
            }
          }
        } else if (!(to %in% current_from) && length(current_to) < max_parents) {
          # Addition of from -> to, unless to already reaches from.
          if (!.spbn_path(parents, to, from)) {
            delta <- score(to, c(current_to, from)) - score(to, current_to)
            if (delta > best$delta) best <- list(delta = delta, op = "add", from = from, to = to)
          }
        }
      }
    }
    if (is.null(best$op)) break
    if (best$op == "add") {
      parents[[best$to]] <- c(parents[[best$to]], best$from)
    } else {
      parents[[best$to]] <- setdiff(parents[[best$to]], best$from)
      if (best$op == "reverse") parents[[best$from]] <- c(parents[[best$from]], best$to)
    }
  }
  parents
}

# Conditional densities -----------------------------------------------------------------------

#' @noRd
.spbn_cpd <- function(data, node, parents, type) {
  if (type == "lg") {
    x <- cbind(1, data[, parents, drop = FALSE])
    fit <- stats::lm.fit(x, data[, node])
    coefficients <- fit$coefficients
    coefficients[is.na(coefficients)] <- 0
    variance <- max(sum(fit$residuals^2) / nrow(data), 1e-12)
    return(list(type = "lg", node = node, parents = parents, coefficients = unname(coefficients), sd = sqrt(variance)))
  }
  z <- data[, c(node, parents), drop = FALSE]
  d <- ncol(z)
  n <- nrow(z)
  covariance <- if (n > 1L) stats::cov(z) else diag(d)
  diag(covariance) <- pmax(diag(covariance), 1e-10)
  bandwidth <- (4 / (d + 2))^(2 / (d + 4)) * covariance * n^(-2 / (d + 4))
  list(type = "ckde", node = node, parents = parents, sample = z, joint = .spbn_kernel(bandwidth),
       marginal = if (d > 1L) .spbn_kernel(bandwidth[-1, -1, drop = FALSE]) else NULL)
}

# Gaussian kernel with bandwidth matrix H: the whitening matrix and the log
# normalising constant.
#' @noRd
.spbn_kernel <- function(bandwidth) {
  root <- tryCatch(chol(bandwidth), error = function(e) chol(bandwidth + diag(1e-8 * max(diag(bandwidth)),
                                                                                nrow(bandwidth))))
  list(whiten = backsolve(root, diag(nrow(root))), log_constant = -nrow(root) / 2 * log(2 * pi) -
         sum(log(diag(root))))
}

# log of the kernel density estimate at the rows of x from the sample.
#' @noRd
.spbn_kde_log <- function(x, sample, kernel) {
  a <- x %*% kernel$whiten
  b <- sample %*% kernel$whiten
  squared <- outer(rowSums(a^2), rowSums(b^2), "+") - 2 * a %*% t(b)
  exponent <- -0.5 * pmax(squared, 0)
  top <- apply(exponent, 1, max)
  top + log(rowSums(exp(exponent - top))) - log(nrow(sample)) + kernel$log_constant
}

#' @noRd
.spbn_component_loglik <- function(cpd, data) {
  if (cpd$type == "lg") {
    mean <- cbind(1, data[, cpd$parents, drop = FALSE]) %*% cpd$coefficients
    return(stats::dnorm(data[, cpd$node], mean, cpd$sd, log = TRUE))
  }
  z <- data[, c(cpd$node, cpd$parents), drop = FALSE]
  value <- .spbn_kde_log(z, cpd$sample, cpd$joint)
  if (!is.null(cpd$marginal)) {
    value <- value - .spbn_kde_log(z[, -1, drop = FALSE], cpd$sample[, -1, drop = FALSE], cpd$marginal)
  }
  value
}

# A CPD is a list of components with weights: log f = sum(w * log f_k) (the
# weighted geometric mean of the transfer update, Eq. 13).
#' @noRd
.spbn_cpd_loglik <- function(cpd, data) {
  total <- 0
  for (k in seq_along(cpd$components)) {
    total <- total + cpd$weights[k] * .spbn_component_loglik(cpd$components[[k]], data)
  }
  as.numeric(total)
}

#' @noRd
.spbn_fit <- function(data, parents, node_types) {
  variables <- colnames(data)
  cpds <- lapply(stats::setNames(variables, variables), function(v) {
    list(components = list(.spbn_cpd(data, v, parents[[v]], node_types[[v]])), weights = 1)
  })
  model <- list(variables = variables, parents = parents, node_types = node_types, cpds = cpds, n = nrow(data))
  class(model) <- "spbn"
  model
}

#' @noRd
.spbn_folds <- function(n, folds) {
  split(seq_len(n), cut(seq_len(n), folds, labels = FALSE))
}

# Cross-validated log-likelihood of a node for a given type.
#' @noRd
.spbn_cv <- function(data, node, parents, type, folds) {
  sum(vapply(.spbn_folds(nrow(data), folds), function(test) {
    cpd <- .spbn_cpd(data[-test, , drop = FALSE], node, parents, type)
    sum(.spbn_component_loglik(cpd, data[test, , drop = FALSE]))
  }, numeric(1)))
}

#' @noRd
.spbn_choose_type <- function(data, node, parents, folds) {
  if (.spbn_cv(data, node, parents, "ckde", folds) > .spbn_cv(data, node, parents, "lg", folds)) "ckde" else "lg"
}

# Stream detector ------------------------------------------------------------------------------

#' @title LLR2 drift detector
#' @description Fully unsupervised drift detector of Sojo, Bielza and Larrañaga
#' (2027). A semiparametric Bayesian network ([spbn()]) is learned on a
#' baseline of the stream. For every node `i`, the statistic
#' `Lambda_i = mean log f(x_i | pa_i)` over the most recent sliding window minus
#' the same mean over a fixed reference window is compared with the chi-square
#' distribution with one degree of freedom, `p_i = 1 - F(2 |Lambda_i|)`; a drift
#' is reported when some `p_i < alpha` (or, with `fdr = TRUE`, when the
#' Benjamini-Hochberg procedure rejects some node at level `alpha`: LLR2-BH).
#' The nodes that drift are reported, and the network is updated.
#' @param target_feat Names of the columns to be monitored. `NULL` (the default)
#' monitors every numeric column.
#' @param n_baseline Size of the baseline: the first `n_baseline - window_size`
#' observations learn the network (`W_l`) and the last `window_size` form the
#' reference window (`W_r`).
#' @param window_size Size of the sliding and reference windows (`n_samples` in
#' the paper).
#' @param step Stride of the sliding window: the test is run on observations
#' whose position in the stream is a multiple of `step`.
#' @param alpha Significance level.
#' @param fdr `TRUE` for the Benjamini-Hochberg correction across nodes
#' (LLR2-BH).
#' @param update Update after a drift: `"parameter"` (default; the CPDs become
#' the weighted geometric mean of CPDs fitted on the sliding window and on the
#' learning window, Eq. 11-13), `"parameter_sliding"` (CPDs refitted on the
#' sliding window), `"sliding"` (structure and CPDs learned again on the
#' sliding window) or `"none"`.
#' @param structure,node_types,max_parents,folds Passed to [spbn()] for the
#' baseline network.
#' @details After a drift the reference window is emptied; the first full
#' sliding window after the drift becomes the new reference window (its mean
#' log-likelihood under the updated network) and tests resume at the next
#' stride. This reproduces the spacing of the detections of the reference
#' results (at least `window_size + step` observations between two drifts).
#'
#' The `"parameter"` update follows Section 4.3: for every node, the CPD
#' fitted on the sliding window (target, weight `eta = window_size / |W_l|`)
#' is combined with the CPD fitted on the learning window (source, weight
#' `1 - eta`); if the risk `|L_T| - |L_S|` (cross-validated log-likelihood of
#' the sliding window under the target CPD against its log-likelihood under the
#' source) is not positive, the source is dropped (`eta = 1`). The structure is
#' kept; the paper also relearns the structure around the drifted nodes by hill
#' climbing with transfer learning (HC-TL), which is not implemented. Its
#' ablation (Fig. 12) shows that detection is driven by the statistic, with
#' equivalent results for the parameter and sliding updates.
#' @references Sojo, R., Bielza, C. and Larrañaga, P. (2027). A fully
#' unsupervised concept drift detector via Bayesian networks for machine
#' condition monitoring. *Expert Systems with Applications*, 332, 133634.
#' <doi:10.1016/j.eswa.2026.133634>
#' @return `dfr_llr2` object. After `fit()`, `drifter_output` holds, for every
#' observation, its log-likelihood under the network in use (`loglik`) and, on
#' tested observations, the p-value of every node (`p_<node>`); `obj$drifts`
#' lists the drifts with their nodes, and `obj$network` the current network.
#' @seealso [spbn()]
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' n <- 3000
#' a <- stats::rnorm(n)
#' b <- a + stats::rnorm(n, sd = 0.5)
#' b[2001:n] <- b[2001:n] + 2
#' detector <- dfr_llr2(n_baseline = 1000, window_size = 200, step = 50)
#' model <- fit(detector, data.frame(a, b))
#' model$drifts
#' @export
dfr_llr2 <- function(target_feat = NULL, n_baseline = 4096, window_size = 250, step = 50, alpha = 0.05, fdr = FALSE,
                     update = c("parameter", "parameter_sliding", "sliding", "none"), structure = NULL,
                     node_types = NULL, max_parents = 4, folds = 5) {
  update <- match.arg(update)
  .check_target_feat(target_feat, multiple = TRUE)
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_positive_integer(n_baseline, "n_baseline", min_value = 3L)
  if (n_baseline <= window_size + 1L) {
    stop("dfr_llr2: n_baseline must exceed window_size + 1", call. = FALSE)
  }
  .check_positive_integer(step, "step", min_value = 1L)
  .check_probability(alpha, "alpha")
  if (!is.logical(fdr) || length(fdr) != 1L || is.na(fdr)) {
    stop("dfr_llr2: fdr must be TRUE or FALSE", call. = FALSE)
  }
  obj <- mv_dist_based(target_feat = target_feat)
  obj$settings <- list(n_baseline = n_baseline, window_size = window_size, step = step, alpha = alpha, fdr = fdr,
                       update = update, structure = structure, node_types = node_types, max_parents = max_parents,
                       folds = folds)
  obj$state <- list(features = NULL, n = 0L, baseline = NULL, filled = 0L, window = NULL, loglik = NULL,
                    reference = NULL, waiting = FALSE)
  obj$network <- NULL
  obj$source <- NULL
  obj$drifts <- list()
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_llr2", class(obj))
  return(obj)
}

#' @export
update_state.dfr_llr2 <- function(obj, value, ...) {
  state <- obj$state
  settings <- obj$settings
  row <- .select_features(value, obj$target_feat)
  if (is.null(state$features)) {
    state$features <- names(row)
  } else if (!identical(names(row), state$features)) {
    stop("dfr_llr2: the monitored features changed during the stream", call. = FALSE)
  }
  state$n <- state$n + 1L
  row <- matrix(unname(row), 1, dimnames = list(NULL, state$features))
  nodes <- state$features
  empty <- c(loglik = NA_real_, stats::setNames(rep(NA_real_, length(nodes)), paste0("p_", nodes)))
  obj$last_drifter_output <- empty
  if (anyNA(row)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  # Baseline: collected, then the network is learned on W_l and W_r is the last window.
  if (is.null(obj$network)) {
    if (is.null(state$baseline)) state$baseline <- matrix(NA_real_, settings$n_baseline, length(nodes),
                                                           dimnames = list(NULL, nodes))
    state$filled <- state$filled + 1L
    state$baseline[state$filled, ] <- row
    if (state$filled == settings$n_baseline) {
      learning <- state$baseline[seq_len(settings$n_baseline - settings$window_size), , drop = FALSE]
      reference <- state$baseline[settings$n_baseline - settings$window_size + seq_len(settings$window_size), ,
                                  drop = FALSE]
      obj$network <- spbn(learning, structure = settings$structure, node_types = settings$node_types,
                          max_parents = settings$max_parents, folds = settings$folds)
      obj$source <- learning
      ref_loglik <- spbn_loglik(obj$network, reference)
      state$reference <- colMeans(ref_loglik)
      state$window <- reference
      state$loglik <- ref_loglik
      state$baseline <- NULL
    }
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  # Sliding window and the log-likelihood of every observation under the current network.
  loglik <- spbn_loglik(obj$network, row)
  state$window <- utils::tail(rbind(state$window, row), settings$window_size)
  state$loglik <- utils::tail(rbind(state$loglik, loglik), settings$window_size)
  output <- empty
  output[["loglik"]] <- sum(loglik)
  drift <- FALSE
  if (nrow(state$window) == settings$window_size && (state$n %% settings$step) == 0L) {
    if (state$waiting) {
      # The first full window after a drift becomes the reference window.
      state$reference <- colMeans(state$loglik)
      state$waiting <- FALSE
    } else {
      lambda <- colMeans(state$loglik) - state$reference
      p <- stats::pchisq(2 * abs(lambda), df = 1, lower.tail = FALSE)
      output[paste0("p_", nodes)] <- p
      flagged <- if (settings$fdr) stats::p.adjust(p, "BH") <= settings$alpha else p < settings$alpha
      if (any(flagged)) {
        drift <- TRUE
        obj$drifts[[length(obj$drifts) + 1L]] <- list(observation = state$n, nodes = nodes[flagged],
                                                      lambda = lambda[flagged], p_value = p[flagged])
        obj$network <- .llr2_update(obj$network, obj$source, state$window, settings)
        # W_s is emptied: the windows after the drift start from the next observation.
        state$window <- state$window[0, , drop = FALSE]
        state$loglik <- state$loglik[0, , drop = FALSE]
        state$waiting <- TRUE
        obj$drifted <- TRUE
      }
    }
  }
  obj$last_drifter_output <- output
  obj$state <- state
  return(list(obj = obj, drift = drift))
}

#' @noRd
.llr2_update <- function(network, source, window, settings) {
  switch(settings$update,
    none = network,
    sliding = spbn(window, max_parents = settings$max_parents, folds = settings$folds),
    parameter_sliding = .spbn_fit(window, network$parents, network$node_types),
    parameter = {
      eta <- min(1, nrow(window) / nrow(source))
      for (v in network$variables) {
        parents <- network$parents[[v]]
        type <- network$node_types[[v]]
        target <- .spbn_cpd(window, v, parents, type)
        origin <- .spbn_cpd(source, v, parents, type)
        target_cv <- .spbn_cv(window, v, parents, type, min(settings$folds, nrow(window)))
        source_ll <- sum(.spbn_component_loglik(origin, window))
        risk <- abs(target_cv) - abs(source_ll)
        weight <- if (risk > 0) eta else 1
        network$cpds[[v]] <- if (weight >= 1) {
          list(components = list(target), weights = 1)
        } else {
          list(components = list(target, origin), weights = c(weight, 1 - weight))
        }
      }
      network
    }
  )
}

#' @export
fit.dfr_llr2 <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data))
}

#' @export
reset_state.dfr_llr2 <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- list(features = NULL, n = 0L, baseline = NULL, filled = 0L, window = NULL, loglik = NULL,
                    reference = NULL, waiting = FALSE)
  obj$network <- NULL
  obj$source <- NULL
  obj$drifts <- list()
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  return(obj)
}
