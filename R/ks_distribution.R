# Distribution of the one-sample Kolmogorov-Smirnov statistic D_n, as in
# scipy.stats.kstwo (exact, finite n) and scipy.stats.kstwobign (limiting
# distribution of sqrt(n) D_n). The exact distribution follows the algorithm
# of Simard and L'Ecuyer (2011) used by scipy (scipy/stats/_ksstats.py): the
# Durbin matrix (Marsaglia, Tsang and Wang, 2003), the Pomeranz recursion, the
# Pelz-Good approximation and the one-sided Smirnov distribution
# (Birnbaum-Tingey formula), each in its region.

#' Distribution of the Kolmogorov-Smirnov statistic
#'
#' Upper tail probability `P(D_n >= d)` of the one-sample, two-sided
#' Kolmogorov-Smirnov statistic `D_n = sup |F_n(x) - F(x)|` of a sample of size
#' `n` from a continuous distribution.
#'
#' @param d Value(s) of the statistic, in `[0, 1]`.
#' @param n Sample size (a positive integer, recycled with `d`).
#' @param exact `TRUE` (the default) for the exact distribution of `D_n`, as
#'   `scipy.stats.kstwo`; `FALSE` for the limiting Kolmogorov distribution of
#'   `sqrt(n) D_n`, as `scipy.stats.kstwobign`.
#' @details The exact distribution is computed as Simard and L'Ecuyer (2011),
#'   with the same choices of method as `scipy.stats.kstwo`: Ruben-Gambino
#'   formulas at the ends of the support; for `n <= 140` the Durbin matrix
#'   (`n d^2 <= 0.754693`), the Pomeranz recursion (`n d^2 <= 4`) or twice the
#'   one-sided Smirnov tail; for larger `n` twice the one-sided tail
#'   (`n d^2 >= 2.2`), the Durbin matrix (`n d^1.5 <= 1.4`, `n <= 100000`) or the
#'   Pelz-Good approximation. The one-sided tail is the Birnbaum-Tingey sum.
#' @return The probabilities `P(D_n >= d)`.
#' @references Simard, R. and L'Ecuyer, P. (2011). Computing the two-sided
#'   Kolmogorov-Smirnov distribution. *Journal of Statistical Software*, 39(11),
#'   1-18. <doi:10.18637/jss.v039.i11>
#'
#' Marsaglia, G., Tsang, W. W. and Wang, J. (2003). Evaluating Kolmogorov's
#'   distribution. *Journal of Statistical Software*, 8(18), 1-4.
#'   <doi:10.18637/jss.v008.i18>
#' @examples
#' ks_sf(0.1, 100)
#' ks_sf(0.1, 100, exact = FALSE)
#' # same as the p-value of ks.test()
#' x <- c(0.61, 0.29, 0.06, 0.59, -1.73, -0.74, 0.51, -0.56, 0.39, 1.64)
#' ks.test(x, "pnorm")$p.value
#' ks_sf(unname(ks.test(x, "pnorm")$statistic), 10)
#' @export
ks_sf <- function(d, n, exact = TRUE) {
  d <- as.numeric(d)
  n <- as.numeric(n)
  if (any(!is.finite(n) | n < 1 | n != floor(n))) {
    stop("'n' must contain positive integers", call. = FALSE)
  }
  len <- max(length(d), length(n))
  d <- rep_len(d, len)
  n <- rep_len(n, len)
  out <- numeric(len)
  for (i in seq_len(len)) {
    out[i] <- if (is.na(d[i])) NA_real_
              else if (exact) .kolmogn_sf(n[i], d[i])
              else .kolmogorov_sf(sqrt(n[i]) * d[i])
  }
  out
}

# Limiting distribution ------------------------------------------------------------------------

# P(K >= z) of the Kolmogorov distribution (scipy.special.kolmogorov): the
# Jacobi theta series of the CDF below 0.82, the alternating series above.
#' @noRd
.kolmogorov_sf <- function(z) {
  if (is.na(z)) return(NA_real_)
  if (z <= 0) return(1)
  if (z < 0.82) {
    k <- seq_len(20)
    cdf <- sqrt(2 * pi) / z * sum(exp(-(2 * k - 1)^2 * pi^2 / (8 * z^2)))
    return(min(max(1 - cdf, 0), 1))
  }
  k <- seq_len(100)
  min(max(2 * sum((-1)^(k - 1) * exp(-2 * k^2 * z^2)), 0), 1)
}

# Exact distribution ---------------------------------------------------------------------------

.ks_e128 <- 2^128
.ks_em128 <- 2^-128

# log(n! / n^n) by Stirling's series, as scipy's _log_nfactorial_div_n_pow_n.
#' @noRd
.log_nfact_div_npown <- function(n) {
  coeffs <- c(-2.955065359477124183e-2, 6.4102564102564102564e-3, -1.9175269175269175269e-3,
              8.4175084175084175084e-4, -5.952380952380952381e-4, 7.9365079365079365079e-4,
              -2.7777777777777777778e-3, 8.3333333333333333333e-2)
  rn <- 1 / n
  x <- rn / n
  poly <- 0
  for (cf in coeffs) poly <- poly * x + cf
  log(n) / 2 - n + log(2 * pi) / 2 + rn * poly
}

# P(D_n >= d) (scipy _kolmogn with cdf = False).
#' @noRd
.kolmogn_sf <- function(n, x) {
  clip <- function(p) min(max(p, 0), 1)
  if (x >= 1) return(0)
  if (x <= 0) return(1)
  t <- n * x
  if (t <= 1) {
    if (t <= 0.5) return(1)
    prob <- if (n <= 140) prod(seq_len(n) * (1 / n) * (2 * t - 1))
            else exp(.log_nfact_div_npown(n) + n * log(2 * t - 1))
    return(clip(1 - prob))
  }
  if (t >= n - 1) return(clip(2 * (1 - x)^n))
  if (x >= 0.5) return(clip(2 * .smirnov_sf(n, x)))
  nxsquared <- t * x
  if (n <= 140) {
    if (nxsquared <= 0.754693) return(clip(1 - .kolmogn_dmtw(n, x)))
    if (nxsquared <= 4) return(clip(1 - .kolmogn_pomeranz(n, x)))
    return(clip(2 * .smirnov_sf(n, x)))
  }
  if (nxsquared >= 370) return(0)
  if (nxsquared >= 2.2) return(clip(2 * .smirnov_sf(n, x)))
  cdf <- if (n <= 100000 && n * x^1.5 <= 1.4) .kolmogn_dmtw(n, x) else .kolmogn_pelz_good(n, x)
  clip(1 - cdf)
}

# One-sided tail P(D_n^+ >= x) by the Birnbaum-Tingey formula
# x * sum_{j = 0}^{floor(n (1 - x))} C(n, j) (x + j/n)^(j - 1) (1 - x - j/n)^(n - j),
# summed in logarithms.
#' @noRd
.smirnov_sf <- function(n, x) {
  if (x <= 0) return(1)
  if (x >= 1) return(0)
  if (n == 1) return(1 - x)
  j <- 0:floor(n * (1 - x) + 1e-12)
  rest <- 1 - x - j / n
  keep <- rest > 0 | j == n
  j <- j[keep]
  rest <- rest[keep]
  logterm <- lchoose(n, j) + log(x) + (j - 1) * log(x + j / n) + ifelse(j == n, 0, (n - j) * log(rest))
  top <- max(logterm)
  min(exp(top) * sum(exp(logterm - top)), 1)
}

# P(D_n < d) by the Durbin matrix, as Marsaglia, Tsang and Wang (scipy
# _kolmogn_DMTW), with the same scaling of the powers.
#' @noRd
.kolmogn_dmtw <- function(n, d) {
  if (d >= 1) return(1)
  nd <- n * d
  if (nd <= 0.5) return(0)
  k <- ceiling(nd)
  h <- k - nd
  m <- 2 * k - 1
  intm <- seq_len(m)
  v <- 1 - h^intm
  w <- numeric(m)
  fac <- 1
  for (j in intm) {
    w[j] <- fac
    fac <- fac / j
    v[j] <- v[j] * fac
  }
  tt <- max(2 * h - 1, 0)^m - 2 * h^m
  v[m] <- (1 + tt) * fac
  H <- matrix(0, m, m)
  if (m > 1) {
    for (i in 1:(m - 1)) H[i:m, i + 1] <- w[1:(m - i + 1)]
  }
  H[, 1] <- v
  H[m, ] <- rev(v)
  Hpwr <- diag(m)
  nn <- n
  expnt <- 0
  Hexpnt <- 0
  while (nn > 0) {
    if (nn %% 2 == 1) {
      Hpwr <- Hpwr %*% H
      expnt <- expnt + Hexpnt
    }
    H <- H %*% H
    Hexpnt <- Hexpnt * 2
    if (abs(H[k, k]) > .ks_e128) {
      H <- H / .ks_e128
      Hexpnt <- Hexpnt + 128
    }
    nn <- nn %/% 2
  }
  p <- Hpwr[k, k]
  for (i in seq_len(n)) {
    p <- i * p / n
    if (abs(p) < .ks_em128) {
      p <- p * .ks_e128
      expnt <- expnt - 128
    }
  }
  if (expnt != 0) p <- p * 2^expnt
  min(max(p, 0), 1)
}

# Bounds of the non-zero entries of row i of the Pomeranz recursion (0-based,
# as scipy _pomeranz_compute_j1j2).
#' @noRd
.pomeranz_j1j2 <- function(i, n, ll, ceilf, roundf) {
  if (i == 0) {
    j1 <- -ll - ceilf - 1
    j2 <- ll + ceilf - 1
  } else {
    ip1div2 <- (i + 1) %/% 2
    ip1mod2 <- (i + 1) %% 2
    if (ip1mod2 == 0) {
      if (ip1div2 == n + 1) {
        j1 <- n - ll - ceilf - 1
        j2 <- n + ll + ceilf - 1
      } else {
        j1 <- ip1div2 - 1 - ll - roundf - 1
        j2 <- ip1div2 + ll - 1 + ceilf - 1
      }
    } else {
      j1 <- ip1div2 - 1 - ll - 1
      j2 <- ip1div2 + ll + roundf - 1
    }
  }
  c(max(j1 + 2, 0), min(j2, n))
}

# P(D_n < x) by the Pomeranz recursion (scipy _kolmogn_Pomeranz). Vectors are
# 0-based in the comments, stored 1-based.
#' @noRd
.kolmogn_pomeranz <- function(n, x) {
  t <- n * x
  ll <- floor(t)
  f <- t - ll
  g <- min(f, 1 - f)
  ceilf <- if (f > 0) 1 else 0
  roundf <- if (f > 0.5) 1 else 0
  npwrs <- 2 * (ll + 1)
  gpower <- twogpower <- onem2gpower <- numeric(npwrs)
  gpower[1] <- twogpower[1] <- onem2gpower[1] <- 1
  expnt <- 0
  for (m in seq_len(npwrs - 1)) {
    gpower[m + 1] <- gpower[m] * (g / n) / m
    twogpower[m + 1] <- twogpower[m] * (2 * g / n) / m
    onem2gpower[m + 1] <- onem2gpower[m] * ((1 - 2 * g) / n) / m
  }
  V0 <- numeric(npwrs)
  V1 <- numeric(npwrs)
  V1[1] <- 1
  V0s <- 0
  V1s <- 0
  jj <- .pomeranz_j1j2(0, n, ll, ceilf, roundf)
  j1 <- jj[1]
  for (i in 1:(2 * n + 1)) {
    k1 <- j1
    tmp <- V0; V0 <- V1; V1 <- tmp
    tmp <- V0s; V0s <- V1s; V1s <- tmp
    V1[] <- 0
    jj <- .pomeranz_j1j2(i, n, ll, ceilf, roundf)
    j1 <- jj[1]
    j2 <- jj[2]
    pwrs <- if (i == 1 || i == 2 * n + 1) gpower else if (i %% 2 == 1) twogpower else onem2gpower
    ln2 <- j2 - k1 + 1
    if (ln2 > 0) {
      start <- k1 - V0s
      a <- V0[(start + 1):(start + ln2)]
      a[is.na(a)] <- 0
      conv_start <- j1 - k1
      conv_len <- j2 - j1 + 1
      if (conv_len > 0) {
        # Entries conv_start, ..., conv_start + conv_len - 1 (0-based) of the full
        # convolution of a and pwrs[1:ln2], sum_i a[i] pwrs[c - i], as a matrix product.
        lag <- outer(conv_start + seq_len(conv_len), seq_len(ln2), "-") + 1L
        lag[lag < 1L | lag > ln2] <- NA_integer_
        toeplitz <- matrix(pwrs[lag], conv_len, ln2)
        toeplitz[is.na(toeplitz)] <- 0
        V1[1:conv_len] <- as.vector(toeplitz %*% a)
      }
      mx <- max(V1)
      if (mx > 0 && mx < .ks_em128) {
        V1 <- V1 * .ks_e128
        expnt <- expnt - 128
      }
      V1s <- V0s + j1 - k1
    }
  }
  ans <- V1[n - V1s + 1]
  for (m in seq_len(n)) {
    if (abs(ans) > .ks_e128) {
      ans <- ans * .ks_em128
      expnt <- expnt + 128
    }
    ans <- ans * m
  }
  if (expnt != 0) ans <- ans * 2^expnt
  min(max(ans, 0), 1)
}

# Pelz-Good approximation of P(D_n < x) (scipy _kolmogn_PelzGood).
#' @noRd
.kolmogn_pelz_good <- function(n, x) {
  if (x <= 0) return(0)
  if (x >= 1) return(1)
  z <- sqrt(n) * x
  zsquared <- z^2
  zthree <- z^3
  zfour <- z^4
  zsix <- z^6
  qlog <- -pi^2 / 8 / zsquared
  if (qlog < -708) return(0)
  q <- exp(qlog)
  k1a <- -zsquared
  k1b <- pi^2 / 4
  k2a <- 6 * zsix + 2 * zfour
  k2b <- (2 * zfour - 5 * zsquared) * pi^2 / 4
  k2c <- pi^4 * (1 - 2 * zsquared) / 16
  k3d <- pi^6 * (5 - 30 * zsquared) / 64
  k3c <- pi^4 * (-60 * zsquared + 212 * zfour) / 16
  k3b <- pi^2 * (135 * zfour - 96 * zsix) / 4
  k3a <- -30 * zsix - 90 * z^8
  K <- numeric(4)
  maxk <- ceiling(16 * z / pi)
  for (k in seq(maxk, 1)) {
    m <- 2 * k - 1
    m2 <- m^2
    m4 <- m^4
    m6 <- m^6
    qpower <- q^(8 * k)
    coeffs <- c(1, k1a + k1b * m2, k2a + k2b * m2 + k2c * m4, k3a + k3b * m2 + k3c * m4 + k3d * m6)
    K <- K * qpower + coeffs
  }
  K <- K * q * sqrt(2 * pi)
  K <- K / c(z, 6 * zfour, 72 * z^7, 6480 * z^10)
  q <- exp(-pi^2 / 2 / zsquared)
  ks <- seq(maxk, 1)
  ksquared <- ks^2
  sqrt3z <- sqrt(3) * z
  kspi <- pi * ks
  qpwers <- q^ksquared
  K[3] <- K[3] + sum(ksquared * qpwers) * pi^2 * sqrt(2 * pi) / (-36 * zthree)
  K[4] <- K[4] + sum((sqrt3z + kspi) * (sqrt3z - kspi) * ksquared * qpwers) * pi^2 * sqrt(2 * pi) / (216 * zsix)
  K <- K / n^((0:3) / 2)
  sum(K)
}
