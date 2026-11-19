# Reverse-mode automatic differentiation on matrices, used to train the neural
# networks of the WWAggr ensemble members (generalised LSTM of indid(), TS-CP2
# and SN-TS2Vec). Values are plain numeric matrices. While a tape is recording,
# an operation whose inputs include a node returns a node (`adn`: value and
# id) and records the function that maps the gradient of its output to the
# gradients of its inputs; otherwise it returns the plain value, so the same
# forward code is used for training and for prediction.
#' @noRd
.ad_env <- new.env(parent = emptyenv())
.ad_env$recording <- FALSE

#' @noRd
.ad_is <- function(x) inherits(x, "adn")

#' @noRd
.adv <- function(x) if (inherits(x, "adn")) x$value else x

# Runs `code` with a fresh tape; returns its value. Nodes created inside are
# only valid until the next call.
#' @noRd
.ad_record <- function(code) {
  old <- .ad_env$recording
  .ad_env$back <- vector("list", 4096L)
  .ad_env$parents <- vector("list", 4096L)
  .ad_env$n <- 0L
  .ad_env$recording <- TRUE
  on.exit(.ad_env$recording <- old)
  code()
}

#' @noRd
.ad_new <- function(value, parents, backward) {
  n <- .ad_env$n + 1L
  if (n > length(.ad_env$back)) {
    length(.ad_env$back) <- 2L * n
    length(.ad_env$parents) <- 2L * n
  }
  .ad_env$n <- n
  if (!is.null(backward)) .ad_env$back[[n]] <- backward
  .ad_env$parents[[n]] <- parents
  structure(list(value = value, id = n), class = "adn")
}

# A leaf (parameter) of the tape.
#' @noRd
.ad_leaf <- function(value) {
  if (!.ad_env$recording) return(value)
  .ad_new(value, integer(0), NULL)
}

#' @noRd
.ad_leaves <- function(params) lapply(params, .ad_leaf)

# Output of an operation: `inputs` are its arguments (nodes or values) and
# `backward(g)` returns the list of their gradients (NULL for values).
#' @noRd
.ad_out <- function(value, inputs, backward) {
  if (!.ad_env$recording) return(value)
  ids <- vapply(inputs, function(a) if (inherits(a, "adn")) a$id else 0L, integer(1))
  if (all(ids == 0L)) return(value)
  .ad_new(value, ids, backward)
}

# Gradients of the scalar node `out` (or of a node with output gradient
# `grad`) with respect to the leaves `leaves` (a named list of nodes).
#' @noRd
.ad_grad <- function(out, leaves, grad = 1) {
  force(out)
  n <- .ad_env$n
  back <- .ad_env$back
  parents <- .ad_env$parents
  grads <- vector("list", n)
  grads[[out$id]] <- if (length(grad) == 1L) array(grad, dim(as.matrix(out$value))) else grad
  for (i in rev(seq_len(n))) {
    g <- grads[[i]]
    if (is.null(g) || is.null(back[[i]])) next
    gs <- back[[i]](g)
    p <- parents[[i]]
    for (j in seq_along(p)) {
      k <- p[j]
      if (k > 0L && !is.null(gs[[j]])) {
        grads[[k]] <- if (is.null(grads[[k]])) gs[[j]] else grads[[k]] + gs[[j]]
      }
    }
    grads[i] <- list(NULL)
  }
  lapply(leaves, function(leaf) {
    g <- grads[[leaf$id]]
    if (is.null(g)) leaf$value * 0 else array(g, dim(as.matrix(leaf$value)))
  })
}

#' @noRd
.ad_shape <- function(g, like) {
  d <- dim(like)
  if (is.null(d)) as.vector(g) else array(g, d)
}

# Linear algebra and broadcasting --------------------------------------------------------------

#' @noRd
.ad_mm <- function(a, b) {
  av <- .adv(a)
  bv <- .adv(b)
  .ad_out(av %*% bv, list(a, b), function(g) {
    list(if (.ad_is(a)) g %*% t(bv), if (.ad_is(b)) crossprod(av, g))
  })
}

#' @noRd
.ad_add <- function(a, b) {
  .ad_out(.adv(a) + .adv(b), list(a, b), function(g) list(g, g))
}

#' @noRd
.ad_sub <- function(a, b) {
  .ad_out(.adv(a) - .adv(b), list(a, b), function(g) list(g, -g))
}

#' @noRd
.ad_mul <- function(a, b) {
  av <- .adv(a)
  bv <- .adv(b)
  .ad_out(av * bv, list(a, b), function(g) list(if (.ad_is(a)) g * bv, if (.ad_is(b)) g * av))
}

#' @noRd
.ad_scale <- function(a, k) .ad_out(.adv(a) * k, list(a), function(g) list(g * k))

# a [n, m] plus a vector v of length m added to every row.
#' @noRd
.ad_add_row <- function(a, v) {
  av <- .adv(a)
  vv <- .adv(v)
  .ad_out(av + rep(as.vector(vv), each = nrow(av)), list(a, v), function(g) {
    list(g, if (.ad_is(v)) .ad_shape(colSums(g), vv))
  })
}

# a [n, m] plus a vector v of length n added to every column.
#' @noRd
.ad_add_col <- function(a, v) {
  av <- .adv(a)
  vv <- .adv(v)
  .ad_out(av + as.vector(vv), list(a, v), function(g) list(g, if (.ad_is(v)) .ad_shape(rowSums(g), vv)))
}

# a [n, m] times a vector v of length m (every row).
#' @noRd
.ad_mul_row <- function(a, v) {
  av <- .adv(a)
  vv <- .adv(v)
  scale <- rep(as.vector(vv), each = nrow(av))
  .ad_out(av * scale, list(a, v), function(g) {
    list(if (.ad_is(a)) g * scale, if (.ad_is(v)) .ad_shape(colSums(g * av), vv))
  })
}

# a [n, m] times a vector v of length n (every column).
#' @noRd
.ad_mul_col <- function(a, v) {
  av <- .adv(a)
  vv <- as.vector(.adv(v))
  .ad_out(av * vv, list(a, v), function(g) {
    list(if (.ad_is(a)) g * vv, if (.ad_is(v)) .ad_shape(rowSums(g * av), .adv(v)))
  })
}

#' @noRd
.ad_t <- function(a) .ad_out(t(.adv(a)), list(a), function(g) list(t(g)))

# Element-wise reciprocal.
#' @noRd
.ad_recip <- function(a) {
  av <- .adv(a)
  .ad_out(1 / av, list(a), function(g) list(-g / av^2))
}

# a times the scalar (1 x 1) s.
#' @noRd
.ad_mul_scalar <- function(a, s) {
  av <- .adv(a)
  sv <- .adv(s)[1]
  .ad_out(av * sv, list(a, s), function(g) list(if (.ad_is(a)) g * sv, if (.ad_is(s)) matrix(sum(g * av))))
}

# Elements `index` of a (column-major), as an array of dimension `dim`. Index
# 0 gives 0 (padding). `unique` tells whether the positive indices are
# distinct, which makes the gradient a plain assignment.
# `blocks` > 1: the index matrix is made of `blocks` row blocks, the indices
# of each being distinct (im2col), and the gradients of the blocks add up.
#' @noRd
.ad_gather <- function(a, index, dim, unique = TRUE, blocks = 1L) {
  av <- .adv(a)
  ok <- index > 0L
  value <- numeric(length(index))
  value[ok] <- av[index[ok]]
  .ad_out(array(value, dim), list(a), function(g) {
    grad <- numeric(length(av))
    if (blocks > 1L) {
      rows <- nrow(index) %/% blocks
      for (k in seq_len(blocks)) {
        r <- (k - 1L) * rows + seq_len(rows)
        ik <- index[r, , drop = FALSE]
        keep <- ik > 0L
        grad[ik[keep]] <- grad[ik[keep]] + g[r, , drop = FALSE][keep]
      }
    } else if (unique) {
      grad[index[ok]] <- g[ok]
    } else {
      sums <- rowsum(as.vector(g)[ok], index[ok], reorder = FALSE)
      grad[as.integer(rownames(sums))] <- sums[, 1]
    }
    list(array(grad, dim(as.matrix(av))))
  })
}

#' @noRd
.ad_cols <- function(a, j) {
  av <- .adv(a)
  .ad_out(av[, j, drop = FALSE], list(a), function(g) {
    grad <- array(0, dim(av))
    grad[, j] <- g
    list(grad)
  })
}

#' @noRd
.ad_rows <- function(a, i) {
  av <- .adv(a)
  .ad_out(av[i, , drop = FALSE], list(a), function(g) {
    grad <- array(0, dim(av))
    grad[i, ] <- g
    list(grad)
  })
}

#' @noRd
.ad_cbind <- function(parts) {
  values <- lapply(parts, function(p) as.matrix(.adv(p)))
  widths <- vapply(values, ncol, integer(1))
  ends <- cumsum(widths)
  .ad_out(do.call(cbind, values), parts, function(g) {
    lapply(seq_along(parts), function(k) g[, (ends[k] - widths[k] + 1L):ends[k], drop = FALSE])
  })
}

#' @noRd
.ad_rbind <- function(parts) {
  values <- lapply(parts, function(p) as.matrix(.adv(p)))
  heights <- vapply(values, nrow, integer(1))
  ends <- cumsum(heights)
  .ad_out(do.call(rbind, values), parts, function(g) {
    lapply(seq_along(parts), function(k) g[(ends[k] - heights[k] + 1L):ends[k], , drop = FALSE])
  })
}

# Reductions ------------------------------------------------------------------------------------

#' @noRd
.ad_sum <- function(a) {
  av <- .adv(a)
  .ad_out(matrix(sum(av)), list(a), function(g) list(array(g[1], dim(as.matrix(av)))))
}

#' @noRd
.ad_mean <- function(a) {
  av <- .adv(a)
  count <- length(av)
  .ad_out(matrix(sum(av) / count), list(a), function(g) list(array(g[1] / count, dim(as.matrix(av)))))
}

#' @noRd
.ad_rowsums <- function(a) {
  av <- .adv(a)
  .ad_out(matrix(rowSums(av)), list(a), function(g) list(array(as.vector(g), dim(av))))
}

#' @noRd
.ad_colsums <- function(a) {
  av <- .adv(a)
  .ad_out(matrix(colSums(av), 1), list(a), function(g) list(array(rep(as.vector(g), each = nrow(av)), dim(av))))
}

# Element-wise functions ---------------------------------------------------------------------------

#' @noRd
.ad_sigmoid <- function(a) {
  s <- 1 / (1 + exp(-.adv(a)))
  .ad_out(s, list(a), function(g) list(g * s * (1 - s)))
}

#' @noRd
.ad_tanh <- function(a) {
  s <- tanh(.adv(a))
  .ad_out(s, list(a), function(g) list(g * (1 - s^2)))
}

#' @noRd
.ad_relu <- function(a) {
  av <- .adv(a)
  .ad_out(av * (av > 0), list(a), function(g) list(g * (av > 0)))
}

# GELU with the exact (erf) form, as torch.nn.functional.gelu.
#' @noRd
.ad_gelu <- function(a) {
  av <- .adv(a)
  cdf <- stats::pnorm(av)
  .ad_out(av * cdf, list(a), function(g) list(g * (cdf + av * stats::dnorm(av))))
}

#' @noRd
.ad_exp <- function(a) {
  e <- exp(.adv(a))
  .ad_out(e, list(a), function(g) list(g * e))
}

#' @noRd
.ad_log <- function(a) {
  av <- .adv(a)
  .ad_out(log(av), list(a), function(g) list(g / av))
}

# log(1 + exp(a)), computed stably.
#' @noRd
.ad_softplus <- function(a) {
  av <- .adv(a)
  value <- pmax(av, 0) + log1p(exp(-abs(av)))
  .ad_out(value, list(a), function(g) list(g / (1 + exp(-av))))
}

# Normalisation and probabilities ------------------------------------------------------------------

# Normalises every row of a [n, m] to mean 0 and (biased) variance 1. The
# affine parameters are per column (`by = "col"`, layer normalisation of
# [batch, feature] rows) or per row (`by = "row"`, batch normalisation of a
# [channel, position] matrix).
#' @noRd
.ad_norm_rows <- function(a, gamma, beta, eps = 1e-5, by = c("col", "row")) {
  by <- match.arg(by)
  av <- .adv(a)
  m <- ncol(av)
  centred <- av - rowMeans(av)
  sd <- sqrt(rowMeans(centred^2) + eps)
  xhat <- centred / sd
  gv <- as.vector(.adv(gamma))
  scale <- if (by == "col") rep(gv, each = nrow(av)) else gv
  shift <- if (by == "col") rep(as.vector(.adv(beta)), each = nrow(av)) else as.vector(.adv(beta))
  .ad_out(xhat * scale + shift, list(a, gamma, beta), function(g) {
    dxhat <- g * scale
    da <- (dxhat - rowMeans(dxhat) - xhat * rowMeans(dxhat * xhat)) / sd
    sums <- if (by == "col") colSums else rowSums
    list(da, if (.ad_is(gamma)) .ad_shape(sums(g * xhat), .adv(gamma)),
         if (.ad_is(beta)) .ad_shape(sums(g), .adv(beta)))
  })
}

#' @noRd
.ad_log_softmax_rows <- function(a) {
  av <- .adv(a)
  shifted <- av - apply(av, 1, max)
  value <- shifted - log(rowSums(exp(shifted)))
  soft <- exp(value)
  .ad_out(value, list(a), function(g) list(g - soft * rowSums(g)))
}

# Rows divided by their Euclidean norm (torch.nn.functional.normalize, dim 1).
#' @noRd
.ad_l2_rows <- function(a, eps = 1e-12) {
  av <- .adv(a)
  norm <- sqrt(rowSums(av^2))
  d <- pmax(norm, eps)
  value <- av / d
  .ad_out(value, list(a), function(g) {
    inner <- rowSums(g * value)
    list(ifelse(norm > eps, 1, 0) * (g - value * inner) / d + ifelse(norm > eps, 0, 1) * g / d)
  })
}

# Spectral normalisation with a maximum norm (SpectralMaxNorm of the
# reference): W / sigma when sigma = u' W v exceeds max_norm, W otherwise; u
# and v are constants (power iteration buffers).
#' @noRd
.ad_spectral <- function(w, u, v, max_norm = 1) {
  wv <- .adv(w)
  sigma <- sum(u * (wv %*% v))
  if (sigma <= max_norm) return(w)
  value <- max_norm * wv / sigma
  .ad_out(value, list(w), function(g) {
    list(max_norm * (g / sigma - sum(g * wv) / sigma^2 * outer(u, v)))
  })
}
