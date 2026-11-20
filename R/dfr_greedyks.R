# GreedyKS: online, approximate one-sample Kolmogorov-Smirnov test with m
# buckets (Cardoso and Galeno, 2023), following the reference implementation
# (driftwatch, src/driftwatch/greedyks.py, commit f1cdf2e).

#' GreedyKS sketch of the one-sample Kolmogorov-Smirnov statistic
#'
#' Online approximation of the statistic `D = sup |F_n(x) - F(x)|` of the
#' one-sample Kolmogorov-Smirnov test between a stream and a reference
#' distribution with CDF `F`, in `O(log m)` time per observation and `O(m)`
#' memory, whatever the number of observations (Cardoso and Galeno, 2023).
#'
#' @param cdf Cumulative distribution function of the reference distribution
#'   (for example `function(x) pnorm(x, 1, 1)`).
#' @param m Number of buckets (the memory units of the paper).
#' @param min_instances Number of observations before the statistic is
#'   estimated (it is 0 before).
#' @details Each observation `x` is mapped to `f = F(x)` and to the bucket
#'   `b = ceiling(f m)` of `0, ..., m`, which keeps the number of observations
#'   and the smallest and largest `f` it received. The largest gaps between the
#'   ECDF and the CDF, `D+` and `D-`, are updated greedily (Algorithm 2 of the
#'   paper): with the bucket just updated and with the buckets that gave the
#'   current maxima, re-evaluated at every step. The counts before a bucket
#'   come from a Fenwick tree (binary indexed tree). The estimate is
#'   `max(D+, D-)`.
#'
#'   The reference implementation uses `ceiling(f m)` (the paper writes
#'   `floor`), so there are `m + 1` buckets; heimdall does the same.
#' @return `greedyks()` returns a `greedyks` sketch. `greedyks_update()` returns
#'   the sketch after the observations `x`, in order. `greedyks_statistic()`
#'   returns the estimate of `D`, and `greedyks_pvalue()` its p-value for the
#'   number of observations processed (see [ks_sf()]).
#' @references Cardoso, D. O. and Galeno, T. D. (2023). Online evaluation of the
#'   Kolmogorov-Smirnov test on arbitrarily large samples. *Journal of
#'   Computational Science*, 67, 101959. <doi:10.1016/j.jocs.2023.101959>
#' @examples
#' set.seed(1)
#' x <- rnorm(5000, 1.1, 1)
#' sketch <- greedyks_update(greedyks(function(q) pnorm(q, 1, 1), m = 100), x)
#' greedyks_statistic(sketch)
#' unname(ks.test(x, "pnorm", 1, 1)$statistic)   # exact value
#' greedyks_pvalue(sketch)
#' @export
greedyks <- function(cdf, m = 100, min_instances = 0) {
  if (!is.function(cdf)) stop("'cdf' must be a function", call. = FALSE)
  .check_positive_integer(m, "m", min_value = 1L)
  .check_number(min_instances, "min_instances", min_value = 0)
  sketch <- list(cdf = cdf, m = as.integer(m), min_instances = min_instances)
  class(sketch) <- "greedyks"
  .greedyks_reset(sketch)
}

#' @noRd
.greedyks_reset <- function(sketch) {
  size <- sketch$m + 1L
  sketch$n <- 0
  sketch$dp <- 0
  sketch$dn <- 0
  sketch$old_p <- 0L
  sketch$old_n <- -1L
  sketch$num <- numeric(size)
  sketch$most <- numeric(size)
  sketch$least <- rep(1, size)
  sketch$tree <- numeric(size)
  sketch
}

#' @rdname greedyks
#' @param sketch A `greedyks` sketch.
#' @param x Numeric vector of observations, processed in order. Missing values
#'   are skipped.
#' @export
greedyks_update <- function(sketch, x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(sketch)
  f <- as.numeric(sketch$cdf(x))
  if (length(f) != length(x) || any(is.na(f) | f < 0 | f > 1)) {
    stop("'cdf' must return probabilities in [0, 1]", call. = FALSE)
  }
  .greedyks_add(sketch, f)
}

# Iterations of GreedyKS (driftwatch GreedyKS.add_element) for the CDF values
# `fs`, in order. The state is held in local variables, updated in place. The
# Fenwick tree over buckets 0..m is stored at positions 1..m+1; `prefix(last)`
# counts the observations in buckets 0..last (0 if last < 0).
#' @noRd
.greedyks_add <- function(sketch, fs) {
  m <- sketch$m
  size <- m + 1L
  min_instances <- sketch$min_instances
  n <- sketch$n
  dp <- sketch$dp
  dn <- sketch$dn
  old_p <- sketch$old_p
  old_n <- sketch$old_n
  num <- sketch$num
  most <- sketch$most
  least <- sketch$least
  tree <- sketch$tree
  prefix <- function(last) {
    i <- last + 1L
    total <- 0
    while (i > 0L) {
      total <- total + tree[i]
      i <- i - bitwAnd(i, -i)
    }
    total
  }
  for (f in fs) {
    n <- n + 1
    b <- as.integer(ceiling(f * m))
    b1 <- b + 1L
    num[b1] <- num[b1] + 1
    i <- b1
    while (i <= size) {
      tree[i] <- tree[i] + 1
      i <- i + bitwAnd(i, -i)
    }
    if (f > most[b1]) most[b1] <- f
    if (f < least[b1]) least[b1] <- f

    before <- prefix(b - 1L)
    dn_alt <- least[b1] - before / n
    dn <- if (old_n != -1L) least[old_n + 1L] - prefix(old_n - 1L) / n else 0
    if (n >= min_instances && dn < dn_alt) {
      dn <- dn_alt
      old_n <- b
    }

    dp_alt <- (before + num[b1]) / n - most[b1]
    dp <- prefix(old_p) / n - most[old_p + 1L]
    if (n >= min_instances && dp < dp_alt) {
      dp <- dp_alt
      old_p <- b
    }
  }
  sketch$n <- n
  sketch$dp <- dp
  sketch$dn <- dn
  sketch$old_p <- old_p
  sketch$old_n <- old_n
  sketch$num <- num
  sketch$most <- most
  sketch$least <- least
  sketch$tree <- tree
  sketch
}

#' @rdname greedyks
#' @export
greedyks_statistic <- function(sketch) {
  if (sketch$n >= sketch$min_instances) max(sketch$dn, sketch$dp) else 0
}

#' @rdname greedyks
#' @param exact Passed to [ks_sf()]: the exact distribution of the statistic
#'   (`TRUE`, the default, as `scipy.stats.kstwo` in the reference) or the
#'   limiting Kolmogorov distribution (`FALSE`).
#' @export
greedyks_pvalue <- function(sketch, exact = TRUE) {
  if (sketch$n == 0) return(1)
  ks_sf(greedyks_statistic(sketch), sketch$n, exact = exact)
}

#' @export
print.greedyks <- function(x, ...) {
  cat(sprintf("GreedyKS sketch: %d buckets, %d observations, D = %.6g\n", x$m + 1L, as.integer(x$n),
              greedyks_statistic(x)))
  invisible(x)
}

# Detector -------------------------------------------------------------------------------------

#' GreedyKS drift detector
#'
#' Online one-sample Kolmogorov-Smirnov test of a numeric stream against a
#' reference distribution, with the GreedyKS sketch of Cardoso and Galeno
#' (2023): a drift is signalled when the p-value of the estimated statistic
#' falls to `alpha` or below. The detector is used for **virtual concept
#' drift** on a numeric feature, or on the error of a model.
#'
#' @param cdf The reference distribution: a CDF (a function of a numeric
#'   vector, kept for the whole stream), or one of `"norm"`, `"exp"` and
#'   `"unif"`, whose parameters are fitted by maximum likelihood (as
#'   `scipy.stats` `fit()`: mean and standard deviation with divisor `n`; minimum
#'   and mean minus minimum; minimum and range) to the first `reference_size`
#'   observations, and again after every drift.
#' @param m Number of buckets of the sketch (the memory units of the paper).
#' @param alpha Significance level: drift when the p-value is `<= alpha`.
#' @param grace_period Number of observations processed before the test is
#'   applied (the paper tests once more observations than memory units were
#'   seen; the default is `m`). It counts from the start of the sketch, so
#'   with a fitted distribution it includes the reference observations.
#' @param reference_size With a fitted distribution (`cdf` a name), the number
#'   of observations it is fitted to. They are also the first observations of
#'   the sketch, as in the call-center experiment of the paper.
#' @param exact Exact distribution of the statistic (`TRUE`, the default, as
#'   the reference code) or the limiting distribution (`FALSE`); see [ks_sf()].
#' @param min_instances Passed to [greedyks()].
#' @param target_feat Feature to be monitored.
#' @details After a drift, the sketch restarts empty (and, with a fitted
#'   distribution, the reference is fitted again to the next `reference_size`
#'   observations).
#'
#'   The output of each observation (`drifter_output` after [fit()]) has the
#'   estimated statistic `D` and its p-value `p_value`. For speed, when more
#'   than 140 observations were processed and `n D^2 < 2.2`, the p-value is the
#'   Pelz-Good approximation instead of the exact Durbin matrix (its p-value
#'   there is above 0.02); with `alpha >= 0.02` the exact value is used. The
#'   decisions are those of the exact test.
#' @return `dfr_greedyks` object
#' @references Cardoso, D. O. and Galeno, T. D. (2023). Online evaluation of the
#'   Kolmogorov-Smirnov test on arbitrarily large samples. *Journal of
#'   Computational Science*, 67, 101959. <doi:10.1016/j.jocs.2023.101959>
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' x <- c(rnorm(1000, 1, 1), rnorm(4000, 1.3, 1))
#' detector <- fit(dfr_greedyks(cdf = function(q) pnorm(q, 1, 1)), x)
#' which(detector$drifter_output$drift)
#' # reference fitted to the first 300 observations
#' detector <- fit(dfr_greedyks(cdf = "norm", reference_size = 300), x)
#' which(detector$drifter_output$drift)
#' @export
dfr_greedyks <- function(cdf = "norm", m = 100, alpha = 0.01, grace_period = m, reference_size = 1000,
                         exact = TRUE, min_instances = 0, target_feat = NULL) {
  .check_positive_integer(m, "m", min_value = 1L)
  .check_probability(alpha, "alpha")
  .check_number(grace_period, "grace_period", min_value = 0)
  .check_number(min_instances, "min_instances", min_value = 0)
  family <- NULL
  if (is.character(cdf)) {
    family <- match.arg(cdf, c("norm", "exp", "unif"))
    .check_positive_integer(reference_size, "reference_size", min_value = 2L)
    cdf <- NULL
  } else if (!is.function(cdf)) {
    stop("'cdf' must be a function or one of \"norm\", \"exp\" and \"unif\"", call. = FALSE)
  }

  obj <- dist_based(target_feat = target_feat)
  obj$state <- list(cdf = cdf, family = family, m = as.integer(m), alpha = alpha, grace_period = grace_period,
                    reference_size = reference_size, exact = isTRUE(exact), min_instances = min_instances)
  obj$state <- .greedyks_restart(obj$state)
  obj$drifted <- FALSE
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_greedyks", class(obj))
  obj
}

#' @noRd
.greedyks_restart <- function(state) {
  state$sketch <- if (is.null(state$family)) greedyks(state$cdf, state$m, state$min_instances) else NULL
  state$reference <- numeric(0)
  state
}

# Maximum likelihood fit of the reference family, as scipy.stats fit().
#' @noRd
.greedyks_fit_cdf <- function(family, x) {
  switch(family,
         norm = {
           mu <- mean(x)
           sigma <- sqrt(mean((x - mu)^2))
           if (sigma <= 0) return(NULL)
           function(q) stats::pnorm(q, mu, sigma)
         },
         exp = {
           loc <- min(x)
           scale <- mean(x) - loc
           if (scale <= 0) return(NULL)
           function(q) stats::pexp(q - loc, 1 / scale)
         },
         unif = {
           lo <- min(x)
           hi <- max(x)
           if (hi <= lo) return(NULL)
           function(q) stats::punif(q, lo, hi)
         })
}

# P-value for the decision: exact, except the Durbin matrix region of large n
# (p > 0.02 there), where the Pelz-Good approximation is used when alpha < 0.02.
#' @noRd
.greedyks_pvalue <- function(d, n, alpha, exact) {
  if (!exact) return(.kolmogorov_sf(sqrt(n) * d))
  if (n > 140 && n * d^2 < 2.2 && alpha < 0.02 && d > 0 && n * d > 1 && n * d < n - 1 && d < 0.5) {
    return(min(max(1 - .kolmogn_pelz_good(n, d), 0), 1))
  }
  .kolmogn_sf(n, d)
}

#' @export
update_state.dfr_greedyks <- function(obj, value, ...) {
  state <- obj$state
  value <- .as_scalar(value, obj$target_feat)
  obj$last_drifter_output <- c(D = NA_real_, p_value = NA_real_)
  if (is.na(value)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  if (is.null(state$sketch)) {
    state$reference <- c(state$reference, value)
    if (length(state$reference) < state$reference_size) {
      obj$state <- state
      return(list(obj = obj, drift = FALSE))
    }
    cdf <- .greedyks_fit_cdf(state$family, state$reference)
    if (is.null(cdf)) {
      # Degenerate reference (constant values): wait for more observations.
      state$reference <- state$reference[-1L]
      obj$state <- state
      return(list(obj = obj, drift = FALSE))
    }
    state$sketch <- greedyks_update(greedyks(cdf, state$m, state$min_instances), state$reference)
    state$reference <- numeric(0)
  } else {
    state$sketch <- greedyks_update(state$sketch, value)
  }

  sketch <- state$sketch
  d <- greedyks_statistic(sketch)
  if (sketch$n <= state$grace_period) {
    obj$last_drifter_output <- c(D = d, p_value = NA_real_)
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  p <- .greedyks_pvalue(d, sketch$n, state$alpha, state$exact)
  obj$last_drifter_output <- c(D = d, p_value = p)
  if (p <= state$alpha) {
    obj$state <- .greedyks_restart(state)
    obj$drifted <- TRUE
    return(list(obj = obj, drift = TRUE))
  }
  obj$state <- state
  list(obj = obj, drift = FALSE)
}

#' @export
fit.dfr_greedyks <- function(obj, data, ...) {
  data <- .select_stream(data, obj$target_feat)
  if (length(data) == 0L) stop("fit: 'data' must contain at least one observation", call. = FALSE)
  output <- list(obj = obj, drift = FALSE)
  rows <- vector("list", length(data))
  drift <- logical(length(data))
  for (i in seq_along(data)) {
    output <- update_state(output$obj, data[i])
    rows[[i]] <- output$obj$last_drifter_output
    drift[i] <- output$drift
  }
  out <- as.data.frame(do.call(rbind, rows))
  out$drift <- drift
  output$obj$drifter_output <- out
  output$obj
}

#' @export
reset_state.dfr_greedyks <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- .greedyks_restart(obj$state)
  obj
}
