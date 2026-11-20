# Fast likelihood-based change point detection in binary streams (Tatti, 2019/2023).
# Implemented from the paper (Algorithms 1-3); the code of the author
# (https://version.helsinki.fi/dacs/) was not reachable.

# Block sequences -----------------------------------------------------------------------------
# A block sequence is kept as the cumulative counts of ones (cu) and zeros (cv)
# at the end of each block; block j has u_j = cu[j] - cu[j - 1] ones and
# v_j = cv[j] - cv[j - 1] zeros, and the proportions of ones increase strictly.

# Appends an entry s (0 or 1) and merges the last blocks while the proportion
# of the last one is not larger than the proportion of the previous one (the
# pool adjacent violators update of Calders et al., 2008).
.fastlr_append <- function(cu, cv, s) {
  k <- length(cu)
  lu <- s
  lv <- 1 - s
  while (k >= 1L) {
    pu <- cu[k] - (if (k > 1L) cu[k - 1L] else 0)
    pv <- cv[k] - (if (k > 1L) cv[k - 1L] else 0)
    if (lu * (pu + pv) <= pu * (lu + lv)) {
      lu <- lu + pu
      lv <- lv + pv
      k <- k - 1L
    } else {
      break
    }
  }
  bu <- if (k >= 1L) cu[k] else 0
  bv <- if (k >= 1L) cv[k] else 0
  list(cu = c(cu[seq_len(k)], bu + lu), cv = c(cv[seq_len(k)], bv + lv))
}

# Block sequence of a whole binary vector.
.fastlr_blocks <- function(x) {
  cu <- numeric(0)
  cv <- numeric(0)
  for (s in x) {
    b <- .fastlr_append(cu, cv, s)
    cu <- b$cu
    cv <- b$cv
  }
  list(cu = cu, cv = cv)
}

# Log-likelihoods ---------------------------------------------------------------------------

# l(a, b) = a log p + b log(1 - p) with p = a / (a + b) and 0 log 0 = 0, vectorised.
.fastlr_llmax <- function(a, b) {
  t <- a + b
  out <- numeric(length(t))
  i <- a > 0
  out[i] <- a[i] * log(a[i] / t[i])
  j <- b > 0
  out[j] <- out[j] + b[j] * log(b[j] / t[j])
  out
}

# FindChange ------------------------------------------------------------------------------

# Best change point of ChangeBlock on the block sequence (cu, cv), with the
# (1 - eps) approximation of Algorithm 3 (eps = 0: every border index).
# Returns the block index i (the second segment starts at block i; i = 1 or
# k + 1 means no split), its score q(i; B) and the number of tested indices.
.fastlr_find_change <- function(cu, cv, eps) {
  k <- length(cu)
  if (k <= 1L) return(c(block = 1, score = 0, tests = k, cands = k))
  a <- cu[k]
  b <- cv[k]
  base <- .fastlr_llmax(a, b)
  if (eps == 0) {
    a1 <- c(0, cu[-k])
    b1 <- c(0, cv[-k])
    s <- .fastlr_llmax(a1, b1) + .fastlr_llmax(a - a1, b - b1) - base
    i <- which.max(s)
    return(c(block = i, score = s[i], tests = k, cands = k))
  }
  # counts before block i: cu0[i], cv0[i] (i = 1..k + 1)
  cu0 <- c(0, cu)
  cv0 <- c(0, cv)
  tot <- a + b

  # Algorithm 1 (FindCands): candidates for p2, with
  # f2(j) = log av(j, k) - log q increasing in j.
  lq <- log(a / tot)
  c2 <- 1L
  i <- 1L
  while (i < k) {
    rho <- (log((a - cu0[i]) / (tot - cu0[i] - cv0[i])) - lq) / (1 - eps)
    if (log((a - cu0[k]) / (tot - cu0[k] - cv0[k])) - lq > rho) {
      lo <- i + 1L
      hi <- k
      while (lo < hi) {
        mid <- (lo + hi) %/% 2L
        if (log((a - cu0[mid]) / (tot - cu0[mid] - cv0[mid])) - lq > rho) hi <- mid else lo <- mid + 1L
      }
      i <- lo
    } else {
      i <- k
    }
    c2 <- c(c2, i)
  }
  # Algorithm 2 (FindCands'): candidates for p1, with
  # f1(j) = log(1 - av(1, j - 1)) - log(1 - q) decreasing in j (j >= 2).
  l1q <- log1p(-a / tot)
  c1 <- k
  i <- k
  while (i > 1L) {
    rho <- (log1p(-cu0[i] / (cu0[i] + cv0[i])) - l1q) / (1 - eps)
    if (i >= 3L && log1p(-cu0[2L] / (cu0[2L] + cv0[2L])) - l1q > rho) {
      lo <- 2L
      hi <- i - 1L
      while (lo < hi) {
        mid <- (lo + hi + 1L) %/% 2L
        if (log1p(-cu0[mid] / (cu0[mid] + cv0[mid])) - l1q > rho) lo <- mid else hi <- mid - 1L
      }
      i <- lo
    } else {
      i <- 1L
    }
    c1 <- c(c1, i)
  }
  # Algorithm 3 (FindChange): every candidate, plus FindSegment(av(1, c_j - 1),
  # av(c_{j-1}, k)) between non-adjacent candidates.
  cands <- sort(unique(c(c1, c2)))
  tested <- cands
  for (j in seq_along(cands)[-1L]) {
    if (cands[j - 1L] + 1L < cands[j]) {
      r1 <- cu0[cands[j]] / (cu0[cands[j]] + cv0[cands[j]])
      r2 <- (a - cu0[cands[j - 1L]]) / (tot - cu0[cands[j - 1L]] - cv0[cands[j - 1L]])
      if (r1 < r2) tested <- c(tested, .fastlr_find_segment(cu0, cv0, k, r1, r2))
    }
  }
  tested <- unique(tested)
  a1 <- cu0[tested]
  b1 <- cv0[tested]
  s <- .fastlr_llmax(a1, b1) + .fastlr_llmax(a - a1, b - b1) - base
  i <- which.max(s)
  c(block = tested[i], score = s[i], tests = length(tested), cands = length(cands))
}

# FindSegment (Proposition 2): the optimal change point for fixed parameters
# p1 < p2, the smallest block j with d_j = l(u_j, v_j; p1) - l(u_j, v_j; p2) <= 0
# (k + 1 if there is none), by binary search.
# cu0, cv0: counts before each block (length k + 1, starting with 0).
.fastlr_find_segment <- function(cu0, cv0, k, p1, p2) {
  # d_j / t_j = (u_j / t_j) (X - Y) + Y is decreasing in j (proof of Proposition 2)
  x <- log(p1) - log(p2)
  y <- log1p(-p1) - log1p(-p2)
  positive <- function(j) {
    u <- cu0[j + 1L] - cu0[j]
    v <- cv0[j + 1L] - cv0[j]
    d <- (if (u > 0) u * x else 0) + (if (v > 0) v * y else 0)
    !is.nan(d) && d > 0
  }
  if (positive(k)) return(k + 1L)
  lo <- 1L
  hi <- k
  while (lo < hi) {
    mid <- (lo + hi) %/% 2L
    if (positive(mid)) lo <- mid + 1L else hi <- mid
  }
  lo
}

# Change (Problem 1): ChangeInc on the sequence and on the flipped sequence.
# inc = blocks of s, dec = blocks of 1 - s. Returns the score, the position of
# the first entry of the second segment (1..n + 1) and the number of tests.
.fastlr_change <- function(inc_u, inc_v, dec_u, dec_v, eps) {
  r1 <- .fastlr_find_change(inc_u, inc_v, eps)
  r2 <- .fastlr_find_change(dec_u, dec_v, eps)
  position <- function(cu, cv, i) if (i > 1L) cu[i - 1L] + cv[i - 1L] + 1 else 1
  if (r2[["score"]] > r1[["score"]]) {
    pos <- position(dec_u, dec_v, r2[["block"]])
    score <- r2[["score"]]
  } else {
    pos <- position(inc_u, inc_v, r1[["block"]])
    score <- r1[["score"]]
  }
  c(score = score, position = pos, tests = r1[["tests"]] + r2[["tests"]], cands = r1[["cands"]] + r2[["cands"]],
    blocks = length(inc_u) + length(dec_u))
}

#' @title Likelihood-ratio change point of a binary sequence
#' @description Finds the split of a binary sequence that maximises the
#' log-likelihood ratio between two Bernoulli segments and a single one
#' (problem *Change* of Tatti, 2019), searching only the border indices, with
#' the \eqn{(1 - \epsilon)} approximation of Algorithm 3 when `eps > 0`.
#' @param x Binary vector (0/1 or logical).
#' @param eps Approximation guarantee, in \[0, 1): the returned score is at
#' least \eqn{1 - \epsilon} times the optimum. `0` tests every border index.
#' @return A list with `score` (log-likelihood ratio, natural logarithm),
#' `index` (the first entry of the second segment; `length(x) + 1` or `1` when
#' no split improves the likelihood), `tests` (number of evaluated change points),
#' `candidates` (candidates of FindCands, without the FindSegment points)
#' and `blocks` (number of border indices of the sequence and of the flipped
#' sequence).
#' @references Tatti, N. (2019). Fast likelihood-based change point detection.
#' In *Machine Learning and Knowledge Discovery in Databases (ECML PKDD 2019)*,
#' 662-677. <doi:10.1007/978-3-030-46150-8_39>. arXiv:2301.08892.
#' @examples
#' set.seed(1)
#' x <- c(rbinom(300, 1, 0.2), rbinom(200, 1, 0.6))
#' fastlr_split(x)
#' fastlr_split(x, eps = 0.5)$score
#' @export
fastlr_split <- function(x, eps = 0) {
  x <- .fastlr_check_binary(x)
  .fastlr_check_eps(eps)
  inc <- .fastlr_blocks(x)
  dec <- .fastlr_blocks(1 - x)
  r <- .fastlr_change(inc$cu, inc$cv, dec$cu, dec$cv, eps)
  list(score = r[["score"]], index = r[["position"]], tests = r[["tests"]], candidates = r[["cands"]],
       blocks = r[["blocks"]])
}

.fastlr_check_binary <- function(x) {
  x <- as.numeric(unlist(x, use.names = FALSE))
  if (anyNA(x) || !all(x %in% c(0, 1))) stop("fastlr: the values must be 0 or 1", call. = FALSE)
  x
}

.fastlr_check_eps <- function(eps) {
  .check_number(eps, "eps", min_value = 0)
  if (eps >= 1) stop("eps must be smaller than 1", call. = FALSE)
  invisible(TRUE)
}

# Detector --------------------------------------------------------------------------------

#' @title Fast likelihood-based change point detector for binary streams
#' @description Error-based detector of Tatti (2019). The entries since the
#' last change are modelled by one Bernoulli variable (M1) or split in two
#' segments with their own Bernoulli variables (M2). After every entry the
#' split that maximises the log-likelihood ratio of M2 over M1 is found among
#' the border indices (blocks maintained by the pool adjacent violators
#' update) with a \eqn{(1 - \epsilon)} approximation in
#' \eqn{O(\epsilon^{-1} \log^2 n)} time; a change is declared when the ratio
#' exceeds \eqn{\sigma = \tau + \log n} (BIC penalty plus `tau`), and the
#' window then restarts from scratch.
#' @param tau Threshold parameter \eqn{\tau} (the paper uses 6).
#' @param eps Approximation guarantee \eqn{\epsilon} in \[0, 1); `0` tests every
#' border index (exact).
#' @details Values must be `1` (error) or `0` (correct prediction); missing
#' values are treated as `0`, as in the other error-based detectors. Unlike
#' DDM, changes in both directions are detected. `drifter_output` has the score
#' (log-likelihood ratio, natural logarithm) after every entry and, when a
#' change is declared, the stream index of the estimated change point (the
#' first entry of the second segment).
#' @return `dfr_fastlr` object
#' @inherit fastlr_split references
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' x <- c(rbinom(1000, 1, 0.25), rbinom(1000, 1, 0.75), rbinom(1000, 1, 0.25))
#' detector <- fit(dfr_fastlr(tau = 6, eps = 0.1), x)
#' out <- detector$drifter_output
#' which(!is.na(out$change_point))     # when the changes were declared
#' out$change_point[!is.na(out$change_point)]   # estimated change points
#' @export
dfr_fastlr <- function(tau = 6, eps = 0) {
  .check_number(tau, "tau", min_value = -Inf)
  .fastlr_check_eps(eps)
  obj <- error_based()
  obj$state <- .fastlr_state(tau, eps)
  obj$drifted <- FALSE
  class(obj) <- append("dfr_fastlr", class(obj))
  obj
}

.fastlr_state <- function(tau, eps, seen = 0) {
  list(tau = tau, eps = eps, n = 0, seen = seen, start = seen + 1, inc_u = numeric(0), inc_v = numeric(0),
       dec_u = numeric(0), dec_v = numeric(0), score = 0, change_point = NA_real_, tests = 0, cands = 0)
}

# One entry: appends it, solves Change and tests the threshold. With
# optimum = TRUE the exact score (eps = 0) on the same window is kept in
# state$optimum (used by the validation of the approximation).
.fastlr_step <- function(state, s, optimum = FALSE) {
  b <- .fastlr_append(state$inc_u, state$inc_v, s)
  state$inc_u <- b$cu
  state$inc_v <- b$cv
  b <- .fastlr_append(state$dec_u, state$dec_v, 1 - s)
  state$dec_u <- b$cu
  state$dec_v <- b$cv
  state$n <- state$n + 1
  state$seen <- state$seen + 1
  r <- .fastlr_change(state$inc_u, state$inc_v, state$dec_u, state$dec_v, state$eps)
  opt <- if (optimum) .fastlr_change(state$inc_u, state$inc_v, state$dec_u, state$dec_v, 0)[["score"]] else NA_real_
  state$score <- r[["score"]]
  state$tests <- r[["tests"]]
  state$cands <- r[["cands"]]
  state$change_point <- NA_real_
  drift <- state$n >= 2 && r[["score"]] > state$tau + log(state$n)
  if (drift) {
    cp <- state$start + r[["position"]] - 1
    n <- state$n
    state <- .fastlr_state(state$tau, state$eps, state$seen)
    state$score <- r[["score"]]
    state$tests <- r[["tests"]]
    state$cands <- r[["cands"]]
    state$change_point <- cp
    state$window <- n
  } else {
    state$window <- state$n
  }
  state$blocks <- r[["blocks"]]
  state$optimum <- opt
  list(state = state, drift = drift)
}

#' @export
update_state.dfr_fastlr <- function(obj, value, ...) {
  value <- .as_error(value)
  step <- .fastlr_step(obj$state, value)
  obj$state <- step$state
  obj$last_drifter_output <- c(score = step$state$score, change_point = step$state$change_point)
  if (step$drift) obj$drifted <- TRUE
  list(obj = obj, drift = step$drift)
}

#' @export
fit.dfr_fastlr <- function(obj, data, ...) {
  .fit_vector_stream(obj, data, output_names = c("score", "change_point"))
}

#' @export
reset_state.dfr_fastlr <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- .fastlr_state(obj$state$tau, obj$state$eps, obj$state$seen)
  obj
}
