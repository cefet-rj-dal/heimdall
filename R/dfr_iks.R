# Incremental Kolmogorov-Smirnov test (IKS) of dos Reis, Flach, Matwin and
# Batista (2016), following the reference implementation
# (https://github.com/denismr/incremental-ks, commit c6bc988:
# IncrementalKS/Pure Python/IKS.py, Treap.py and IKSSW.py).
#
# The sketch is a treap (randomised binary search tree) with lazy propagation,
# stored in vectors indexed by node (0 is NIL). Each operation (.iks_run) works
# on local copies of the vectors, updated in place by closures, and returns
# them, so the sketch keeps value semantics.

#' Incremental two-sample Kolmogorov-Smirnov test
#'
#' A sketch of two samples A (group 0) and B (group 1) that supports the
#' insertion and removal of observations in `O(log N)` expected time and gives
#' the two-sample Kolmogorov-Smirnov statistic `D = max |F_A(x) - F_B(x)|` in
#' `O(1)` (dos Reis et al., 2016).
#'
#' @param ratio The constant ratio r = |A| / |B| at which the statistic is
#'   computed (`1`, the default, for samples of equal sizes). Each observation
#'   of B weighs `r`.
#' @param seed Seed of the private generator (Park-Miller) of the priorities of
#'   the treap and of the tie-breaking values; `NULL` draws it from R's
#'   generator. The priorities do not change the statistic.
#' @details Observations are sorted by the key `(x, tie, group)`. The treap
#'   stores, at every observation, `G = F'_A - F'_B` (the counts of A minus `r`
#'   times the counts of B up to and including it); an insertion adds `1` or
#'   `-r` to every later observation with a lazily propagated bulk operation,
#'   and the subtree maxima and minima give `D = max(max G, -min G) / |A|`.
#'
#'   As noted by the authors, the statistic is exact only when the observations
#'   are distinct: with tied values it can be larger than the one of
#'   [stats::ks.test()]. A random `tie` value per observation (as `IKSSW` of the
#'   reference code) breaks the ties.
#' @return `iks()` returns an `iks` sketch; `iks_add()` and `iks_remove()` the
#'   updated sketch; `iks_statistic()` the statistic `D`; `iks_kuiper()` the
#'   Kuiper statistic `(max G - min G) / |A|`; `iks_test()` `TRUE` when the null
#'   hypothesis of equal distributions is rejected; `iks_ca()` the coefficient
#'   `c(alpha) = sqrt(-log(alpha / 2) / 2)`.
#' @references dos Reis, D. M., Flach, P., Matwin, S. and Batista, G. (2016).
#'   Fast unsupervised online drift detection using incremental
#'   Kolmogorov-Smirnov test. In *Proceedings of the 22nd ACM SIGKDD
#'   International Conference on Knowledge Discovery and Data Mining*,
#'   1545-1554. <doi:10.1145/2939672.2939836>
#' @examples
#' set.seed(1)
#' a <- rnorm(200)
#' b <- rnorm(200, 0.3)
#' sketch <- iks()
#' for (i in 1:200) sketch <- iks_add(iks_add(sketch, a[i], 0), b[i], 1)
#' iks_statistic(sketch)
#' unname(ks.test(a, b)$statistic)
#' # slide the window of B by one observation
#' sketch <- iks_add(iks_remove(sketch, b[1], 1), 2, 1)
#' iks_test(sketch, ca = iks_ca(0.001))
#' @export
iks <- function(ratio = 1, seed = NULL) {
  .check_number(ratio, "ratio", min_value = 0)
  if (ratio <= 0) stop("'ratio' must be positive", call. = FALSE)
  if (is.null(seed)) seed <- sample.int(2147483646L, 1L)
  seed <- as.numeric(seed) %% 2147483646
  if (seed == 0) seed <- 1
  sketch <- list(ratio = ratio, n = c(0, 0), root = 0L, used = 0L, free = integer(0), lcg = seed,
                 x = numeric(0), tie = numeric(0), group = integer(0), priority = numeric(0), value = numeric(0),
                 maxv = numeric(0), minv = numeric(0), lazy = numeric(0), left = integer(0), right = integer(0))
  class(sketch) <- "iks"
  sketch
}

# Helpers of .iks_run (Treap.py: SumAll, Unlazy, Update, SplitKeepRight, Merge,
# SplitSmallest, SplitGreatest), byte-compiled once; .iks_run sets their
# environment to its own frame, whose vectors they modify in place with <<-.
# Their environment at definition holds placeholders of those variables, so
# that the code checks of R CMD check see the targets of <<-.
.iks_template_env <- new.env()
for (name in c("x", "tie", "group", "kx", "ktie", "kgroup", "priority", "value", "maxv", "minv", "lazy", "left",
               "right", "free", "used", "lcg", "key_le", "sum_all", "unlazy", "update", "split", "merge",
               "split_smallest", "split_greatest", "new_node")) {
  assign(name, NULL, envir = .iks_template_env)
}
rm(name)
.iks_template <- function(f) {
  environment(f) <- .iks_template_env
  compiler::cmpfun(f)
}
# key_le(i): the key (x, tie, group) is <= the key of node i.
.iks_h_key_le <- .iks_template(function(i) {
  if (x != kx[i]) return(x < kx[i])
  if (tie != ktie[i]) return(tie < ktie[i])
  group <= kgroup[i]
})
.iks_h_sum_all <- .iks_template(function(i, v) {
  if (i == 0L) return(invisible(NULL))
  value[i] <<- value[i] + v
  maxv[i] <<- maxv[i] + v
  minv[i] <<- minv[i] + v
  lazy[i] <<- lazy[i] + v
  invisible(NULL)
})
.iks_h_unlazy <- .iks_template(function(i) {
  lz <- lazy[i]
  if (lz != 0) {
    sum_all(left[i], lz)
    sum_all(right[i], lz)
    lazy[i] <<- 0
  }
  invisible(NULL)
})
.iks_h_update <- .iks_template(function(i) {
  if (i == 0L) return(invisible(NULL))
  unlazy(i)
  mx <- value[i]
  mn <- mx
  l <- left[i]
  r <- right[i]
  if (l != 0L) {
    if (maxv[l] > mx) mx <- maxv[l]
    if (minv[l] < mn) mn <- minv[l]
  }
  if (r != 0L) {
    if (maxv[r] > mx) mx <- maxv[r]
    if (minv[r] < mn) mn <- minv[r]
  }
  maxv[i] <<- mx
  minv[i] <<- mn
  invisible(NULL)
})
.iks_h_split <- .iks_template(function(i) {
  if (i == 0L) return(c(0L, 0L))
  unlazy(i)
  if (key_le(i)) {
    parts <- split(left[i])
    left[i] <<- parts[2]
    out <- c(parts[1], i)
  } else {
    parts <- split(right[i])
    right[i] <<- parts[1]
    out <- c(i, parts[2])
  }
  update(out[1])
  update(out[2])
  out
})
.iks_h_merge <- .iks_template(function(l, r) {
  if (l == 0L) return(r)
  if (r == 0L) return(l)
  if (priority[l] > priority[r]) {
    unlazy(l)
    right[l] <<- merge(right[l], r)
    node <- l
  } else {
    unlazy(r)
    left[r] <<- merge(l, left[r])
    node <- r
  }
  update(node)
  node
})
.iks_h_split_smallest <- .iks_template(function(i) {
  if (i == 0L) return(c(0L, 0L))
  unlazy(i)
  if (left[i] != 0L) {
    parts <- split_smallest(left[i])
    left[i] <<- parts[2]
    out <- c(parts[1], i)
  } else {
    out <- c(i, right[i])
    right[i] <<- 0L
  }
  update(out[1])
  update(out[2])
  out
})
.iks_h_split_greatest <- .iks_template(function(i) {
  if (i == 0L) return(c(0L, 0L))
  unlazy(i)
  if (right[i] != 0L) {
    parts <- split_greatest(right[i])
    right[i] <<- parts[1]
    out <- c(i, parts[2])
  } else {
    out <- c(left[i], i)
    left[i] <<- 0L
  }
  update(out[1])
  update(out[2])
  out
})
.iks_h_new_node <- .iks_template(function(val) {
  if (length(free)) {
    i <- free[length(free)]
    free <<- free[-length(free)]
  } else {
    i <- used + 1L
    used <<- i
    if (i > length(kx)) {
      grow <- max(16L, length(kx))
      kx <<- c(kx, numeric(grow))
      ktie <<- c(ktie, numeric(grow))
      kgroup <<- c(kgroup, integer(grow))
      priority <<- c(priority, numeric(grow))
      value <<- c(value, numeric(grow))
      maxv <<- c(maxv, numeric(grow))
      minv <<- c(minv, numeric(grow))
      lazy <<- c(lazy, numeric(grow))
      left <<- c(left, integer(grow))
      right <<- c(right, integer(grow))
    }
  }
  lcg <<- (48271 * lcg) %% 2147483647
  kx[i] <<- x
  ktie[i] <<- tie
  kgroup[i] <<- group
  priority[i] <<- lcg / 2147483647
  value[i] <<- val
  maxv[i] <<- val
  minv[i] <<- val
  lazy[i] <<- 0
  left[i] <<- 0L
  right[i] <<- 0L
  i
})


# One operation ("add" or "remove") of IKS.Add / IKS.Remove on local copies
# of the vectors of the treap (0 is NIL).
#' @noRd
.iks_run <- function(sketch, op, x, tie, group) {
  ratio <- sketch$ratio
  n <- sketch$n
  root <- sketch$root
  used <- sketch$used
  free <- sketch$free
  lcg <- sketch$lcg
  kx <- sketch$x
  ktie <- sketch$tie
  kgroup <- sketch$group
  priority <- sketch$priority
  value <- sketch$value
  maxv <- sketch$maxv
  minv <- sketch$minv
  lazy <- sketch$lazy
  left <- sketch$left
  right <- sketch$right


  # The helpers, with this call as their enclosing environment.
  for (helper in c("key_le", "sum_all", "unlazy", "update", "split", "merge", "split_smallest", "split_greatest",
                   "new_node")) {
    fun <- get(paste0(".iks_h_", helper))
    environment(fun) <- environment()
    assign(helper, fun)
  }

  step <- if (group == 0L) 1 else -ratio
  parts <- split(root)
  if (op == "add") {
    n[group + 1L] <- n[group + 1L] + 1
    greatest <- split_greatest(parts[1])
    val <- if (greatest[2] == 0L) 0 else value[greatest[2]]
    lhs <- merge(greatest[1], greatest[2])
    rhs <- merge(new_node(val), parts[2])
    sum_all(rhs, step)
    root <- merge(lhs, rhs)
  } else {
    smallest <- split_smallest(parts[2])
    s <- smallest[1]
    if (s == 0L || kx[s] != x || ktie[s] != tie || kgroup[s] != group) {
      stop("iks_remove: the observation is not in the sample", call. = FALSE)
    }
    n[group + 1L] <- n[group + 1L] - 1
    sum_all(smallest[2], -step)
    free <- c(free, s)
    root <- merge(parts[1], smallest[2])
  }
  sketch <- list(ratio = ratio, n = n, root = root, used = used, free = free, lcg = lcg, x = kx, tie = ktie,
                 group = kgroup, priority = priority, value = value, maxv = maxv, minv = minv, lazy = lazy,
                 left = left, right = right)
  class(sketch) <- "iks"
  sketch
}

#' @noRd
.iks_check_args <- function(x, group, tie) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) stop("'x' must be one number", call. = FALSE)
  if (length(group) != 1L || !(group %in% c(0, 1))) stop("'group' must be 0 or 1", call. = FALSE)
  if (length(tie) != 1L || !is.numeric(tie) || is.na(tie)) stop("'tie' must be one number", call. = FALSE)
}

#' @rdname iks
#' @param sketch An `iks` sketch.
#' @param x The observation.
#' @param group `0` for sample A (the reference), `1` for sample B.
#' @param tie Tie-breaking value of the observation (`0` by default; a random
#'   value, as `stats::runif(1)`, makes keys distinct). An observation is removed
#'   with the same `x` and `tie` it was inserted with.
#' @export
iks_add <- function(sketch, x, group, tie = 0) {
  .iks_check_args(x, group, tie)
  .iks_run(sketch, "add", as.numeric(x), as.numeric(tie), as.integer(group))
}

#' @rdname iks
#' @export
iks_remove <- function(sketch, x, group, tie = 0) {
  .iks_check_args(x, group, tie)
  .iks_run(sketch, "remove", as.numeric(x), as.numeric(tie), as.integer(group))
}

#' @rdname iks
#' @export
iks_statistic <- function(sketch) {
  if (sketch$n[1] == 0 || sketch$root == 0L) return(0)
  max(sketch$maxv[sketch$root], -sketch$minv[sketch$root]) / sketch$n[1]
}

#' @rdname iks
#' @export
iks_kuiper <- function(sketch) {
  if (sketch$n[1] == 0 || sketch$root == 0L) return(0)
  (sketch$maxv[sketch$root] - sketch$minv[sketch$root]) / sketch$n[1]
}

#' @rdname iks
#' @param ca Coefficient of the critical value `ca sqrt((n + m) / (n m))`; the
#'   default `1.95` is the one of the paper and of the reference code
#'   (`alpha = 0.001`).
#' @export
iks_test <- function(sketch, ca = 1.95) {
  n <- sketch$n[1]
  m <- sketch$n[2]
  if (n == 0 || m == 0) return(FALSE)
  iks_statistic(sketch) > ca * sqrt((n + m) / (n * m))
}

#' @rdname iks
#' @param alpha Significance level.
#' @export
iks_ca <- function(alpha) {
  .check_probability(alpha, "alpha")
  sqrt(-0.5 * log(alpha / 2))
}

#' @export
print.iks <- function(x, ...) {
  cat(sprintf("Incremental KS sketch: %d + %d observations, D = %.6g\n", as.integer(x$n[1]), as.integer(x$n[2]),
              iks_statistic(x)))
  invisible(x)
}

# Detector ------------------------------------------------------------------------------------

#' Incremental Kolmogorov-Smirnov drift detector
#'
#' Unsupervised drift detection of dos Reis et al. (2016): for every monitored
#' feature, a two-sample Kolmogorov-Smirnov test between a reference sample and
#' the sliding window of the last `window_size` observations, computed
#' incrementally with [iks()]. A drift is signalled when the test rejects the
#' null hypothesis for at least one feature. The detector monitors **virtual
#' concept drift** (the feature distribution), without labels.
#'
#' @param target_feat Features to be monitored (by default every numeric column).
#' @param window_size Size `W` of the reference sample and of the sliding window.
#' @param alpha Significance level of the test.
#' @param ca Coefficient of the critical value `ca sqrt(2 / W)`; by default
#'   `iks_ca(alpha)`, `1.9495` for `alpha = 0.001` (the paper and the reference
#'   code use the table value `1.95`).
#' @param reference `"fixed"` (the default, the paper's method): the reference
#'   sample is the first `W` observations (the data the model was trained on)
#'   and the window starts as a copy of it; after a drift the reference becomes
#'   the current window. `"consecutive"`: the reference is the window of the
#'   `W` observations before the current window (the consecutive sliding
#'   windows of Zliobaite, 2010, the CD baseline of the paper).
#' @param tie_break Pair every observation with a random value to break ties
#'   (`TRUE`, the default, as `IKSSW` of the reference code); otherwise the
#'   statistic can be overestimated when values repeat, in particular while the
#'   window overlaps the reference.
#' @param seed Seed of the private generator of the sketches.
#' @details The output of each observation (`drifter_output` after [fit()]) has
#'   the statistic of every monitored feature (`D_<feature>`) and `drift`.
#'   After a drift with `reference = "consecutive"`, both windows restart.
#' @return `dfr_iks` object
#' @references dos Reis, D. M., Flach, P., Matwin, S. and Batista, G. (2016).
#'   Fast unsupervised online drift detection using incremental
#'   Kolmogorov-Smirnov test. In *Proceedings of the 22nd ACM SIGKDD
#'   International Conference on Knowledge Discovery and Data Mining*,
#'   1545-1554. <doi:10.1145/2939672.2939836>
#'
#' Zliobaite, I. (2010). Change with delayed labeling: when is it detectable?
#'   In *2010 IEEE International Conference on Data Mining Workshops*, 843-850.
#'   <doi:10.1109/ICDMW.2010.49>
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' data <- data.frame(a = c(rnorm(600), rnorm(600, 1)), b = rnorm(1200))
#' detector <- fit(dfr_iks(window_size = 100, seed = 1), data)
#' which(detector$drifter_output$drift)
#' @export
dfr_iks <- function(target_feat = NULL, window_size = 100, alpha = 0.001, ca = iks_ca(alpha),
                    reference = c("fixed", "consecutive"), tie_break = TRUE, seed = NULL) {
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  .check_probability(alpha, "alpha")
  .check_number(ca, "ca", min_value = 0)
  reference <- match.arg(reference)
  if (!is.null(target_feat)) .check_target_feat(target_feat, multiple = TRUE)
  obj <- mv_dist_based()
  obj$target_feat <- target_feat
  if (is.null(seed)) seed <- sample.int(2147483646L, 1L)
  obj$state <- list(window_size = as.integer(window_size), ca = ca, reference = reference,
                    tie_break = isTRUE(tie_break), seed = seed, features = NULL)
  obj$state <- .iks_detector_restart(obj$state)
  obj$drifted <- FALSE
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_iks", class(obj))
  obj
}

#' @noRd
.iks_detector_restart <- function(state) {
  state$sketches <- NULL
  state$reference_values <- NULL
  state$window_values <- NULL
  state$buffer <- NULL
  state
}

# Tie-breaking value of a new observation, from the private generator of the detector.
#' @noRd
.iks_tie <- function(state) {
  if (!state$tie_break) return(list(state = state, tie = 0))
  state$seed <- (48271 * as.numeric(state$seed)) %% 2147483647
  list(state = state, tie = state$seed / 2147483647)
}

# Build the sketches from a reference matrix [W, features] and a window matrix
# (by default a copy of the reference, with new tie-breaking values).
#' @noRd
.iks_detector_build <- function(state, reference, window = reference) {
  k <- ncol(reference)
  state$sketches <- vector("list", k)
  state$reference_values <- array(0, c(nrow(reference), k, 2))
  state$window_values <- array(0, c(nrow(window), k, 2))
  for (j in seq_len(k)) {
    sketch <- iks(seed = state$seed + j)
    for (i in seq_len(nrow(reference))) {
      drawn <- .iks_tie(state)
      state <- drawn$state
      sketch <- iks_add(sketch, reference[i, j], 0, drawn$tie)
      state$reference_values[i, j, ] <- c(reference[i, j], drawn$tie)
    }
    for (i in seq_len(nrow(window))) {
      drawn <- .iks_tie(state)
      state <- drawn$state
      sketch <- iks_add(sketch, window[i, j], 1, drawn$tie)
      state$window_values[i, j, ] <- c(window[i, j], drawn$tie)
    }
    state$sketches[[j]] <- sketch
  }
  state
}

#' @export
update_state.dfr_iks <- function(obj, value, ...) {
  state <- obj$state
  row <- as.data.frame(value)
  if (is.null(state$features)) {
    features <- if (is.null(obj$target_feat)) names(row)[vapply(row, is.numeric, logical(1))] else obj$target_feat
    if (!length(features)) stop("dfr_iks: no numeric feature to monitor", call. = FALSE)
    state$features <- features
  }
  missing <- setdiff(state$features, names(row))
  if (length(missing)) stop(sprintf("dfr_iks: missing features %s", paste(missing, collapse = ", ")), call. = FALSE)
  x <- as.numeric(unlist(row[1, state$features, drop = TRUE]))
  k <- length(state$features)
  empty <- stats::setNames(c(rep(NA_real_, k)), paste0("D_", state$features))
  obj$last_drifter_output <- empty
  if (anyNA(x)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  W <- state$window_size

  if (is.null(state$sketches)) {
    state$buffer <- rbind(state$buffer, x)
    needed <- if (state$reference == "fixed") W else 2L * W
    if (nrow(state$buffer) < needed) {
      obj$state <- state
      return(list(obj = obj, drift = FALSE))
    }
    buffer <- unname(state$buffer)
    state$buffer <- NULL
    state <- if (state$reference == "fixed") .iks_detector_build(state, buffer)
             else .iks_detector_build(state, buffer[seq_len(W), , drop = FALSE], buffer[W + seq_len(W), , drop = FALSE])
    obj$last_drifter_output <- stats::setNames(vapply(state$sketches, iks_statistic, numeric(1)),
                                               paste0("D_", state$features))
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  stats_now <- numeric(k)
  reject <- logical(k)
  for (j in seq_len(k)) {
    sketch <- state$sketches[[j]]
    oldest <- state$window_values[1, j, ]
    sketch <- iks_remove(sketch, oldest[1], 1, oldest[2])
    if (state$reference == "consecutive") {
      first_ref <- state$reference_values[1, j, ]
      sketch <- iks_remove(sketch, first_ref[1], 0, first_ref[2])
      sketch <- iks_add(sketch, oldest[1], 0, oldest[2])
      state$reference_values[, j, ] <- rbind(state$reference_values[-1, j, , drop = FALSE][, 1, ], oldest)
    }
    drawn <- .iks_tie(state)
    state <- drawn$state
    sketch <- iks_add(sketch, x[j], 1, drawn$tie)
    state$window_values[, j, ] <- rbind(state$window_values[-1, j, , drop = FALSE][, 1, ], c(x[j], drawn$tie))
    state$sketches[[j]] <- sketch
    stats_now[j] <- iks_statistic(sketch)
    reject[j] <- iks_test(sketch, state$ca)
  }
  obj$last_drifter_output <- stats::setNames(stats_now, paste0("D_", state$features))
  if (any(reject)) {
    window <- state$window_values[, , 1, drop = FALSE]
    window <- matrix(window, W, k)
    if (state$reference == "fixed") {
      state <- .iks_detector_build(state, window)
    } else {
      state <- .iks_detector_restart(state)
    }
    state$drifted_features <- state$features[reject]
    obj$state <- state
    obj$drifted <- TRUE
    return(list(obj = obj, drift = TRUE))
  }
  obj$state <- state
  list(obj = obj, drift = FALSE)
}

#' @export
fit.dfr_iks <- function(obj, data, ...) {
  data <- as.data.frame(data)
  if (nrow(data) == 0L) stop("fit: 'data' must contain at least one row", call. = FALSE)
  output <- list(obj = obj, drift = FALSE)
  rows <- vector("list", nrow(data))
  drift <- logical(nrow(data))
  for (i in seq_len(nrow(data))) {
    output <- update_state(output$obj, data[i, , drop = FALSE])
    rows[[i]] <- output$obj$last_drifter_output
    drift[i] <- output$drift
  }
  out <- as.data.frame(do.call(rbind, rows), check.names = FALSE)
  out$drift <- drift
  output$obj$drifter_output <- out
  output$obj
}

#' @export
reset_state.dfr_iks <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- .iks_detector_restart(obj$state)
  obj
}

# Variables of the frame of .iks_run, used by the .iks_h_ helpers.
utils::globalVariables(c("x", "tie", "group", "kx", "ktie", "kgroup", "priority", "value", "maxv", "minv", "lazy",
                         "left", "right", "free", "used", "lcg", "sum_all", "unlazy", "update", "split", "merge",
                         "split_smallest", "split_greatest", "key_le", "new_node"))
