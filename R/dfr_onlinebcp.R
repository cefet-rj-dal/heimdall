#' @title Online Bayesian change point detection (onlineBcp)
#' @description Online Bayesian change point detection for normal data of
#' Yigiter et al. (2015), as implemented in the R package onlineBcp (Xu,
#' Yigiter and Chen, 2022). The time since the last change (run length)
#' follows a Markov chain with geometric hazard (`1 - theta` is the
#' probability of a change at each step), and the segments are normal with a
#' normal-inverse-gamma prior on their mean and variance. At every step the
#' posterior of the last change point location is updated; when its largest
#' value exceeds `threshold`, that location becomes the last change point and
#' the search restarts from it.
#' @param x A numeric vector without missing values.
#' @param theta Probability that no change occurs at a step.
#' @param alpha,beta Hyperparameters of the inverse-gamma prior of the
#' variance.
#' @param threshold Threshold of the posterior probability (`th_cp`).
#' @param numerics `"stable"` (default) computes the weights in log scale with
#' `lgamma()`. `"reference"` computes them as onlineBcp 0.1.4 does, with
#' `gamma()` and powers that overflow for segments longer than about 340
#' observations; the overflowing weights are set to 0, which forces change
#' points at multiples of about 341 observations (see Details).
#' @details The update follows `online_cp()` of onlineBcp 0.1.4
#' (<https://github.com/ElsevierSoftwareX/SOFTX-D-21-00221>). At time `t`, for
#' every candidate location `j` from the last change point `tau` to `t`,
#' * `j < t`: `w = Gamma(u + 1/2) / Gamma(u) * sqrt((t - j + 1) / (t - j + 2)) *
#'   (A + 2 beta)^u / (B + 2 beta)^(u + 1/2) / sqrt(pi)`, `u = (t - j + 2 alpha) / 2`,
#'   with `A` and `B` the centred sums of squares of `x[(j + 1):t]` and
#'   `x[(j + 1):(t + 1)]` (with denominators `t - j + 1` and `t - j + 2`), and
#'   `P(R = j) = w theta P_prev(R = j)`;
#' * `j = t`: `w = 2^alpha Gamma(alpha + 1/2) / (sqrt(2 pi) (x[t + 1]^2 / 2 + beta)^(alpha + 1/2))`,
#'   and `P(R = t) = w (1 - theta) sum(P_prev)`.
#'
#' The code differs from Eq. 3 of the paper in the weight of a new segment,
#' `(x^2 / 2 + beta)` where the paper writes `(x^2 / 2 + 2 beta)`; the code is
#' followed. With `numerics = "reference"`, `gamma()` overflows when
#' `(t - j + 2 alpha + 1) / 2` exceeds about 171.6, and the weight, then
#' `NaN` or `Inf`, is set to 0 as in the code. The change points of the paper's
#' aCGH example at 341, 682 and 1023 come from this overflow; `"stable"`
#' avoids it.
#'
#' The segments are built as `summary.BayesCP()` does. A change point is a
#' location whose posterior exceeds `threshold` at a step where the location
#' differs from that of the previous step. Segments of one observation are
#' merged with the previous segment of more than one, or with the next one at
#' the start. The interval of each segment mean is
#' `mean +- qnorm(0.95) sd / sqrt(n)` (a 90% interval; the paper calls it 95%).
#' With `normality = TRUE`, the p-value of the Kolmogorov-Smirnov test of each
#' segment against the normal distribution with its mean and standard deviation
#' is added.
#'
#' The cost of a step grows with the time since the last change point, so a
#' long sequence without changes takes quadratic time.
#' @param normality `TRUE` adds the Kolmogorov-Smirnov normality test of each
#' segment.
#' @references Yigiter, A., Chen, J., An, L. and Danacioglu, N. (2015). An
#' on-line CNV detection method for short sequencing reads. *Journal of Applied
#' Statistics*, 42(7), 1556-1571. <doi:10.1080/02664763.2014.1001330>
#'
#' Xu, H., Yigiter, A. and Chen, J. (2022). onlineBcp: An R package for online
#' change point detection using a Bayesian approach. *SoftwareX*, 17, 100999.
#' <doi:10.1016/j.softx.2022.100999>
#' @return An `online_bcp` object: `changepoints` (location, the last
#' observation before the change, and its posterior probability), `segments`
#' (begin, end, mean, SD, interval limits and, optionally, the normality
#' p-value), and `trace`, one row per step with the largest posterior
#' probability (`max_prob`), the location (`location`, 0 when below the
#' threshold) and the run of equal locations (`run`), as `max_p` of onlineBcp.
#' @seealso [dfr_onlinebcp()] to monitor a stream.
#' @examples
#' set.seed(1)
#' x <- c(stats::rnorm(100, 0), stats::rnorm(100, 3), stats::rnorm(100, 0))
#' result <- online_bcp(x, theta = 0.95, threshold = 0.7)
#' result$changepoints
#' result$segments
#' @export
online_bcp <- function(x, theta = 0.9, alpha = 1, beta = 1, threshold = 0.5, numerics = c("stable", "reference"),
                       normality = FALSE) {
  numerics <- match.arg(numerics)
  settings <- .obcp_settings(theta, alpha, beta, threshold, numerics)
  if (!is.numeric(x) || anyNA(x) || length(x) < 2L) {
    stop("online_bcp: x must be a numeric vector of at least two values without missing values", call. = FALSE)
  }
  x <- as.numeric(x)
  n <- length(x)
  state <- .obcp_state(x[1])
  trace <- matrix(0, n, 3, dimnames = list(NULL, c("max_prob", "location", "run")))
  for (t in seq_len(n - 1L)) {
    step <- .obcp_step(state, x[t + 1L], settings)
    state <- step$state
    trace[t, ] <- c(step$max_prob, step$location, 0)
    if (t > 1L && trace[t - 1L, "location"] == trace[t, "location"]) {
      trace[t, "run"] <- trace[t - 1L, "run"] + 1
    }
  }
  segments <- .obcp_segments(x, trace, threshold, normality)
  result <- c(segments, list(trace = trace, x = x, settings = settings))
  class(result) <- "online_bcp"
  result
}

#' @export
print.online_bcp <- function(x, ...) {
  cat(sprintf("Online Bayesian change point detection: %d change point(s) in %d observations\n",
              nrow(x$changepoints), length(x$x)))
  if (nrow(x$changepoints)) print(x$changepoints, row.names = FALSE)
  invisible(x)
}

#' @noRd
.obcp_settings <- function(theta, alpha, beta, threshold, numerics) {
  .check_probability(theta, "theta")
  .check_number(alpha, "alpha", min_value = 0)
  .check_number(beta, "beta", min_value = 0)
  if (alpha <= 0 || beta <= 0) {
    stop("alpha and beta must be positive", call. = FALSE)
  }
  .check_probability(threshold, "threshold")
  list(theta = theta, alpha = alpha, beta = beta, threshold = threshold, numerics = numerics)
}

# State after the first observation: P(R = 0) = 1. The locations j = 0, ..., t
# are kept with the running sums of x and x^2 since the start (prefix sums).
#' @noRd
.obcp_state <- function(first) {
  list(t = 1L, tau = 0L, prob = 1, sum1 = c(0, first), sum2 = c(0, first^2), last = first)
}

# One step: the observation x[t + 1] arrives.
#' @noRd
.obcp_step <- function(state, value, settings) {
  t <- state$t
  tau <- state$tau
  alpha <- settings$alpha
  beta <- settings$beta
  theta <- settings$theta
  sum1 <- c(state$sum1, state$sum1[t + 1L] + value)
  sum2 <- c(state$sum2, state$sum2[t + 1L] + value^2)
  j <- if (tau < t) tau:(t - 1L) else integer(0)
  # x[(j + 1):t] and x[(j + 1):(t + 1)].
  y2 <- sum1[t + 1L] - sum1[j + 1L]
  ys2 <- sum2[t + 1L] - sum2[j + 1L]
  y1 <- sum1[t + 2L] - sum1[j + 1L]
  ys1 <- sum2[t + 2L] - sum2[j + 1L]
  a1 <- ys1 - y1^2 / (t - j + 2)
  a2 <- ys2 - y2^2 / (t - j + 1)
  us1 <- (t - j + 2 * alpha) / 2
  us2 <- (t - j + 2 * alpha + 1) / 2
  us3 <- (2 * alpha + 1) / 2
  a3 <- value^2 / 2
  prob <- numeric(t + 1L)
  if (settings$numerics == "reference") {
    w1 <- (1 / sqrt(pi)) * (gamma(us2) / gamma(us1)) * (sqrt(t - j + 1) / sqrt(t - j + 2)) * ((a2 + 2 * beta)^us1) /
      ((a1 + 2 * beta)^us2)
    w1[is.nan(w1) | is.infinite(w1)] <- 0
    prob[j + 1L] <- w1 * theta * state$prob[j + 1L]
    w2 <- ((2^alpha) * gamma(us3)) / (sqrt(2 * pi) * (a3 + beta)^us3)
    prob[t + 1L] <- w2 * (1 - theta) * sum(state$prob[seq_len(t)])
    prob <- prob / sum(prob)
  } else {
    log_w1 <- -0.5 * log(pi) + lgamma(us2) - lgamma(us1) + 0.5 * (log(t - j + 1) - log(t - j + 2)) +
      us1 * log(a2 + 2 * beta) - us2 * log(a1 + 2 * beta)
    log_w2 <- alpha * log(2) + lgamma(us3) - 0.5 * log(2 * pi) - us3 * log(a3 + beta)
    log_prob <- rep(-Inf, t + 1L)
    log_prob[j + 1L] <- log_w1 + log(theta) + log(state$prob[j + 1L])
    log_prob[t + 1L] <- log_w2 + log(1 - theta) + log(sum(state$prob[seq_len(t)]))
    top <- max(log_prob)
    prob <- exp(log_prob - top)
    prob <- prob / sum(prob)
  }
  max_prob <- max(prob)
  location <- 0
  if (!is.na(max_prob) && max_prob > settings$threshold) {
    location <- which.max(prob) - 1
    tau <- as.integer(location)
  }
  list(state = list(t = t + 1L, tau = tau, prob = prob, sum1 = sum1, sum2 = sum2, last = value),
       max_prob = max_prob, location = location)
}

# Segments and change points of summary.BayesCP() (onlineBcp 0.1.4).
#' @noRd
.obcp_segments <- function(x, trace, threshold, normality) {
  n <- length(x)
  rows <- cbind(seq_len(n), trace)
  if (rows[1, 3] != 0) {
    rows <- rbind(rows[1, ], rows)
    rows[1, 2] <- threshold + 0.01
  }
  if (rows[1, 3] == 0 && rows[1, 2] < threshold) {
    rows[1, 2] <- threshold + 0.01
  }
  selected <- rows[(rows[, 2] > threshold) & (rows[, 4] == 0), , drop = FALSE]
  z <- stats::qnorm(0.05, lower.tail = FALSE)
  describe <- function(values) {
    m <- mean(values)
    s <- stats::sd(values)
    c(mean = m, SD = s, `LL of CI` = m - sqrt(stats::var(values) / length(values)) * z,
      `UL of CI` = m + sqrt(stats::var(values) / length(values)) * z)
  }
  if (nrow(selected) < 2L) {
    segment <- data.frame(begin = 1, end = n, t(describe(x)), check.names = FALSE)
    if (normality) {
      segment$`normality p-value` <- suppressWarnings(stats::ks.test(x, "pnorm", mean(x), stats::sd(x))$p.value)
    }
    return(list(changepoints = data.frame(location = numeric(0), post.prob = numeric(0)), segments = segment))
  }
  # Segment label of every observation, as mean_[, 2] in summary.BayesCP().
  label <- numeric(n)
  k <- 1
  for (i in 2:nrow(selected)) {
    j <- selected[i - 1, 3] + 1
    if (selected[i, 3] != 0 && selected[i, 4] == 0) {
      label[j:selected[i, 3]] <- k
      k <- k + 1
    }
  }
  j <- selected[nrow(selected), 3] + 1
  label[j:n] <- k
  index <- split(seq_len(n), label)
  values <- split(x, label)
  # Segments of one observation are merged as in summary.BayesCP().
  sizes <- vapply(values, length, integer(1))
  long <- which(sizes > 1)[1]
  if (long > 1) {
    values[[long]] <- c(unlist(values[seq_len(long - 1)]), values[[long]])
    index[[long]] <- c(unlist(index[seq_len(long - 1)]), index[[long]])
  }
  for (i in long:length(values)) {
    if (length(values[[i]]) > 1) {
      long <- i
    } else {
      values[[long]] <- c(values[[long]], values[[i]])
      index[[long]] <- c(index[[long]], index[[i]])
    }
  }
  keep <- vapply(values, length, integer(1)) > 1
  values <- values[keep]
  index <- index[keep]
  segments <- do.call(rbind, lapply(seq_along(values), function(i) {
    out <- data.frame(begin = index[[i]][1], end = index[[i]][length(index[[i]])],
                      `post. prob.` = selected[i, 2], t(describe(values[[i]])), check.names = FALSE)
    if (normality) {
      out$`normality p-value` <- suppressWarnings(stats::ks.test(values[[i]], "pnorm", mean(values[[i]]),
                                                                 stats::sd(values[[i]]))$p.value)
    }
    out
  }))
  k <- nrow(segments)
  changepoints <- if (k > 1) {
    data.frame(location = segments$end[seq_len(k - 1)], post.prob = segments$`post. prob.`[2:k])
  } else {
    data.frame(location = numeric(0), post.prob = numeric(0))
  }
  segments$`post. prob.` <- NULL
  rownames(segments) <- NULL
  list(changepoints = changepoints, segments = segments)
}

# Stream detector ----------------------------------------------------------------------------------

#' @title onlineBcp stream detector
#' @description Monitors a univariate stream with the online Bayesian change
#' point detection of [online_bcp()]: every observation updates the posterior
#' of the location of the last change point, and a drift is reported when the
#' largest posterior exceeds `threshold` at a new location. Since it monitors
#' the distribution of a feature, it is used for **virtual concept drift**.
#' @param target_feat Feature to be monitored.
#' @inheritParams online_bcp
#' @param max_run If set, the posterior is kept for at most the last `max_run`
#' candidate locations (the oldest are dropped), which bounds the cost of a step
#' on long streams without changes. `NULL` (default) keeps them all, as the
#' reference.
#' @details The detector reports a drift on the observation at which a new
#' change point location is found; that location (the last observation before
#' the change) is kept in `obj$changepoints` with its posterior probability.
#' The data are assumed approximately normal within segments; transform them
#' first otherwise (for counts, `sqrt(y + 3/8)`, as in the paper). Missing
#' values are skipped.
#' @inherit online_bcp references
#' @return `dfr_onlinebcp` object. After `fit()`, `drifter_output` holds the
#' largest posterior probability (`max_prob`) and the location found
#' (`location`, 0 when below the threshold) for every observation.
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' data <- data.frame(serie = c(stats::rnorm(100, 0), stats::rnorm(100, 3)))
#' model <- fit(dfr_onlinebcp(theta = 0.95, threshold = 0.7), data)
#' model$changepoints
#' @export
dfr_onlinebcp <- function(target_feat = NULL, theta = 0.9, alpha = 1, beta = 1, threshold = 0.5,
                          numerics = c("stable", "reference"), max_run = NULL) {
  numerics <- match.arg(numerics)
  if (!is.null(max_run)) .check_positive_integer(max_run, "max_run", min_value = 2L)
  obj <- dist_based(target_feat = target_feat)
  obj$settings <- c(.obcp_settings(theta, alpha, beta, threshold, numerics), list(max_run = max_run))
  obj$state <- list(engine = NULL, offset = 0L, n = 0L)
  obj$changepoints <- data.frame(location = numeric(0), post.prob = numeric(0), detected_at = numeric(0))
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_onlinebcp", class(obj))
  return(obj)
}

#' @export
update_state.dfr_onlinebcp <- function(obj, value, ...) {
  if (is.data.frame(value) || is.list(value)) {
    value <- if (!is.null(obj$target_feat)) value[[obj$target_feat]] else unlist(value)[1]
  }
  value <- as.numeric(value)[1]
  state <- obj$state
  obj$last_drifter_output <- c(max_prob = NA_real_, location = NA_real_)
  if (is.na(value)) {
    return(list(obj = obj, drift = FALSE))
  }
  state$n <- state$n + 1L
  if (is.null(state$engine)) {
    state$engine <- .obcp_state(value)
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  previous <- state$engine$tau
  step <- .obcp_step(state$engine, value, obj$settings)
  engine <- step$state
  drift <- step$location != 0 && engine$tau != previous
  location <- if (step$location != 0) step$location + state$offset else 0
  obj$last_drifter_output <- c(max_prob = step$max_prob, location = location)
  # Keep at most max_run candidate locations: the oldest are dropped and the
  # indices shifted (state$offset maps them back to stream positions).
  max_run <- obj$settings$max_run
  if (!is.null(max_run) && (engine$t - engine$tau) > max_run) {
    drop <- engine$t - max_run
    engine$prob <- engine$prob[-seq_len(drop)]
    engine$prob <- engine$prob / sum(engine$prob)
    engine$sum1 <- engine$sum1[-seq_len(drop)] - engine$sum1[drop + 1L]
    engine$sum2 <- engine$sum2[-seq_len(drop)] - engine$sum2[drop + 1L]
    engine$t <- engine$t - drop
    engine$tau <- max(0L, engine$tau - drop)
    state$offset <- state$offset + drop
  }
  state$engine <- engine
  if (drift) {
    obj$changepoints <- rbind(obj$changepoints, data.frame(location = location, post.prob = step$max_prob,
                                                           detected_at = state$n))
    obj$drifted <- TRUE
  }
  obj$state <- state
  return(list(obj = obj, drift = drift))
}

#' @export
fit.dfr_onlinebcp <- function(obj, data, ...) {
  if (is.data.frame(data) || is.matrix(data)) {
    data <- as.data.frame(data)
    column <- if (!is.null(obj$target_feat)) obj$target_feat else names(data)[1]
    data <- data.frame(value = data[[column]])
    names(data) <- column
  } else {
    data <- data.frame(value = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data))
}

#' @export
reset_state.dfr_onlinebcp <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- list(engine = NULL, offset = 0L, n = 0L)
  obj$changepoints <- obj$changepoints[0, , drop = FALSE]
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  return(obj)
}
