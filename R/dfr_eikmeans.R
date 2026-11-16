#' @title Equal intensity k-means space partitioning (EI-kMeans)
#' @description Histogram-based two-sample test for drift detection (Liu et
#' al., 2021). The feature space is partitioned by k-means clusters of
#' (nearly) equal intensity, so that no bin straddles a gap between clusters
#' of the data, and a new sample is compared with the training sample by
#' Pearson's chi-square test on the counts of the two samples in every bin.
#' @param training The training sample (matrix or data frame of numeric
#' features).
#' @param n_test Size of the samples that will be tested. The number of bins
#' is at most `n_test / 5`, so that the expected count of every bin in a test
#' sample is at least 5.
#' @param k Largest number of bins.
#' @param alpha Significance level of the chi-square test.
#' @param min_points Smallest number of training points per bin (`beta`).
#' @param n_build Number of training points used to build the partitioning:
#' the first `n_build` rows. The counts of the bins are then taken from the
#' whole training sample.
#' @param thetas Values of the shape parameter `theta` of the amplify-shrink
#' coefficients `exp(theta (r - 1))`, tried in order.
#' @details The partitioning follows the reference code
#' (<https://github.com/Anjin-Liu/TCYB2019-EIkMeansDriftDetection>):
#'
#' 1. The number of bins starts at `min(k, n_test / 5, n / 50)` (`n` is the
#'    training size).
#' 2. Greedy equal intensity initialization (Algorithm 1): `K - 1` times, the
#'    point whose `n_build / K`-th nearest neighbour is farthest is taken
#'    together with its `n_build / K` nearest neighbours as a group, and the
#'    group is removed; the remaining points are the last group. The first
#'    point of every group starts k-means (Lloyd's algorithm, with the
#'    stopping rule of scikit-learn).
#' 3. Amplify-shrink (Algorithm 2): with intensity ratios `r` (points per
#'    cluster over `n_build / K`), a point goes to the cluster that minimises
#'    its distance times `exp(theta (r - 1))`, so crowded clusters shrink and
#'    sparse ones grow. The ratios are updated after every `theta`, and the
#'    search stops when every cluster has more than `min_points` points. If no
#'    `theta` is enough, the number of bins is lowered by one and the search
#'    repeated.
#' 4. A test sample is assigned to the bins in the same way, and the 2 x K
#'    table of training and test counts is tested with Pearson's chi-square
#'    test (Algorithm 3); bins empty in both samples are left out, and the
#'    Yates correction is used with one degree of freedom, as in
#'    `scipy.stats.chi2_contingency`.
#'
#' The reference code differs from the paper in a few points, and the code is
#' followed: `theta` ranges over 0, 0.05, ..., 0.95 (the paper gives up to
#' 1.5), the initialization uses the distance to the `n / K`-th neighbour (the
#' paper describes the nearest one) and starts k-means from a point of each
#' group (the paper, from the group mean), the starting number of bins depends
#' on the test size, and the partitioning is built on the first 2000 training
#' points. If the number of bins reaches 1, the reference falls back to `k`
#' plain k-means clusters; so does this implementation.
#' @references Liu, A., Lu, J. and Zhang, G. (2021). Concept drift detection
#' via equal intensity k-means space partitioning. *IEEE Transactions on
#' Cybernetics*, 51(6), 3198-3211. <doi:10.1109/TCYB.2020.2983962>
#' @return An `eikmeans` object with the centroids (`centers`), the
#' amplify-shrink coefficients (`amplify`), the training counts of every bin
#' (`counts`) and the settings.
#' @seealso [eikmeans_test()] to test a sample and [dfr_eikmeans()] to
#' monitor a stream.
#' @examples
#' # Two clusters; the right one moves.
#' set.seed(1)
#' cluster <- function(n, x) cbind(stats::rnorm(n, x), stats::rnorm(n))
#' training <- rbind(cluster(1000, 0), cluster(1000, 5))
#' model <- eikmeans(training, n_test = 200)
#' nrow(model$centers)
#' eikmeans_test(model, rbind(cluster(100, 0), cluster(100, 5)))
#' eikmeans_test(model, rbind(cluster(100, 0), cluster(100, 5.8)))
#' @export
eikmeans <- function(training, n_test, k = 40, alpha = 0.05, min_points = 50, n_build = 2000,
                     thetas = seq(0, 0.95, by = 0.05)) {
  .check_positive_integer(n_test, "n_test", min_value = 1L)
  .check_positive_integer(k, "k", min_value = 1L)
  .check_probability(alpha, "alpha")
  .check_positive_integer(min_points, "min_points", min_value = 1L)
  .check_positive_integer(n_build, "n_build", min_value = 2L)
  if (!is.numeric(thetas) || (length(thetas) == 0L) || anyNA(thetas)) {
    stop("thetas must be a numeric vector", call. = FALSE)
  }
  data <- .eik_matrix(training)
  if (nrow(data) < 2L) {
    stop("eikmeans: the training sample must have at least two rows", call. = FALSE)
  }
  built <- .eik_build(data, n_test, k, min_points, n_build, thetas)
  labels <- .eik_assign(built$centers, built$amplify, data)
  model <- list(centers = built$centers, amplify = built$amplify,
                counts = tabulate(labels, nbins = nrow(built$centers)), alpha = alpha, n_test = n_test,
                features = colnames(data), dim = ncol(data))
  class(model) <- "eikmeans"
  return(model)
}

#' @title Test a sample with an EI-kMeans partitioning
#' @description Assigns the points of a sample to the bins of an
#' [eikmeans()] model and compares the counts with those of the training
#' sample by Pearson's chi-square test.
#' @param model An `eikmeans` object.
#' @param sample The sample to test (matrix or data frame with the training
#' features).
#' @return A data frame with the chi-square `statistic`, the degrees of
#' freedom (`df`), the `p_value` and whether a drift is detected
#' (`drift`, `p_value < alpha`).
#' @examples
#' set.seed(1)
#' model <- eikmeans(matrix(stats::rnorm(4000), ncol = 2), n_test = 200)
#' eikmeans_test(model, matrix(stats::rnorm(400, 0.5), ncol = 2))
#' @export
eikmeans_test <- function(model, sample) {
  if (!inherits(model, "eikmeans")) {
    stop("eikmeans_test: model must be an eikmeans object", call. = FALSE)
  }
  data <- .eik_matrix(sample, model$features)
  if (ncol(data) != model$dim) {
    stop("eikmeans_test: the sample has a different number of features from the training sample", call. = FALSE)
  }
  labels <- .eik_assign(model$centers, model$amplify, data)
  observed <- tabulate(labels, nbins = nrow(model$centers))
  test <- .eik_chisq(rbind(model$counts, observed))
  data.frame(statistic = test$statistic, df = test$df, p_value = test$p_value,
             drift = test$p_value < model$alpha)
}

#' @export
print.eikmeans <- function(x, ...) {
  cat(sprintf("EI-kMeans partitioning: %d bins for test samples of %d, alpha = %g\n", nrow(x$centers), x$n_test,
              x$alpha))
  cat(sprintf("Training counts per bin: %d to %d\n", min(x$counts), max(x$counts)))
  invisible(x)
}

# Partitioning --------------------------------------------------------------------------

# EIkMeans.build_partition of the reference code.
#' @noRd
.eik_build <- function(data, n_test, k, min_points, n_build, thetas) {
  m <- nrow(data)
  initial <- data[seq_len(min(m, n_build)), , drop = FALSE]
  m_initial <- nrow(initial)
  k <- min(as.integer(min(n_test / 5, m / 50)), k)
  if (k < 1L) {
    stop("eikmeans: the training or test sample is too small for a single bin (n / 50 and n_test / 5 below 1)",
         call. = FALSE)
  }
  distances <- as.matrix(stats::dist(initial))
  ordering <- .eik_order(distances)
  counts <- 0
  clusters <- k + 1L
  amplify <- NULL
  centers <- NULL
  while (min(counts) < min_points) {
    clusters <- clusters - 1L
    if (clusters == 1L) {
      # As the reference: fall back to k plain k-means clusters.
      fitted <- .eik_kmeans(initial, initial[.eik_greedy(distances, k, ordering), , drop = FALSE])
      centers <- fitted$centers
      amplify <- rep(1, k)
      break
    }
    fitted <- .eik_kmeans(initial, initial[.eik_greedy(distances, clusters, ordering), , drop = FALSE])
    centers <- fitted$centers
    present <- tabulate(fitted$labels, nbins = clusters)
    counts <- present[present > 0]
    ratio <- counts / (m_initial / clusters)
    for (theta in thetas) {
      if (length(ratio) < clusters) {
        break
      }
      amplify <- exp((ratio - 1) * theta)
      labels <- .eik_assign(centers, amplify, initial)
      counts <- tabulate(labels, nbins = clusters)
      ratio <- counts / as.integer(m_initial / clusters)
      if (min(counts) > min_points) {
        break
      }
    }
    if (is.null(amplify) || (length(amplify) != clusters)) {
      amplify <- rep(1, clusters)
    }
  }
  list(centers = centers, amplify = amplify)
}

# Greedy equal intensity initialization (greed_compact_partition): returns the
# index of the first point of each group. `ordering` (from .eik_order) holds
# every point's neighbours sorted by distance, so the size-th nearest remaining
# neighbour is found by counting the remaining points along that list.
#' @noRd
.eik_greedy <- function(distances, k, ordering = .eik_order(distances)) {
  m <- nrow(distances)
  size <- as.integer(m / k)
  group <- rep(0L, m)
  alive <- rep(TRUE, m)
  # Only the first `depth` neighbours of each point are scanned; a point with
  # fewer than `size` remaining among them gets its list refreshed from the
  # full ordering (its first remaining neighbours, padded with a removed one).
  depth <- min(m, 4L * size)
  top <- ordering$index[seq_len(depth), , drop = FALSE]
  for (i in seq_len(k - 1L)) {
    remaining <- which(alive)
    r <- length(remaining)
    block <- top[, remaining, drop = FALSE]
    flags <- matrix(alive[block], depth)
    short <- which(colSums(flags) < size)
    if (length(short) > 0L) {
      full <- ordering$index[, remaining[short], drop = FALSE]
      refreshed <- matrix(full[alive[full]], r)[seq_len(min(depth, r)), , drop = FALSE]
      if (r < depth) {
        refreshed <- rbind(refreshed, matrix(which(!alive)[1], depth - r, length(short)))
      }
      top[, remaining[short]] <- refreshed
      block[, short] <- refreshed
      flags[, short] <- alive[refreshed]
    }
    # Running count of remaining points along each list; the size-th nearest
    # (the point itself included) is where the count reaches size.
    running <- cumsum(flags)
    before <- c(0L, running[depth * seq_len(r - 1L)])
    position <- colSums(matrix(running < rep(before + size, each = depth), depth)) + 1L
    kth <- distances[cbind(block[cbind(position, seq_len(r))], remaining)]
    chosen <- remaining[max(which(kth == max(kth)))]
    neighbours <- ordering$index[, chosen]
    neighbours <- neighbours[alive[neighbours]][seq_len(size)]
    group[neighbours] <- i
    alive[neighbours] <- FALSE
  }
  group[group == 0L] <- k
  vapply(seq_len(k), function(i) which(group == i)[1], integer(1))
}

# Neighbours of every point (one column per point) sorted by distance, ties by
# index.
#' @noRd
.eik_order <- function(distances) {
  list(index = apply(distances, 2, order))
}

# k-means with Lloyd's algorithm from the given centres, with the stopping
# rule of scikit-learn's KMeans (labels unchanged, or squared centre shift
# below tol times the mean variance of the features) and its relocation of
# empty clusters.
#' @noRd
.eik_kmeans <- function(x, centers, max_iter = 300L, tol = 1e-4) {
  n <- nrow(x)
  k <- nrow(centers)
  variances <- apply(x, 2, function(column) mean((column - mean(column))^2))
  tolerance <- mean(variances) * tol
  labels_old <- rep(-1L, n)
  strict <- FALSE
  for (iteration in seq_len(max_iter)) {
    labels <- .eik_nearest(centers, x)
    # rowsum() only returns the groups present, so it is placed into k rows.
    present <- rowsum(x, labels, reorder = TRUE)
    sums <- matrix(0, k, ncol(x))
    sums[as.integer(rownames(present)), ] <- present
    weights <- tabulate(labels, nbins = k)
    empty <- which(weights == 0)
    if (length(empty) > 0L) {
      far <- order(rowSums((x - centers[labels, , drop = FALSE])^2), decreasing = TRUE)[seq_along(empty)]
      for (j in seq_along(empty)) {
        old <- labels[far[j]]
        sums[old, ] <- sums[old, ] - x[far[j], ]
        weights[old] <- weights[old] - 1
        sums[empty[j], ] <- x[far[j], ]
        weights[empty[j]] <- 1
      }
    }
    updated <- sums / weights
    updated[weights == 0, ] <- centers[weights == 0, ]
    shift <- sum((updated - centers)^2)
    centers <- updated
    if (identical(labels, labels_old)) {
      strict <- TRUE
      break
    }
    if (shift <= tolerance) {
      break
    }
    labels_old <- labels
  }
  if (!strict) {
    labels <- .eik_nearest(centers, x)
  }
  dimnames(centers) <- NULL
  list(centers = centers, labels = labels)
}

#' @noRd
.eik_distances <- function(centers, x) {
  squared <- outer(rowSums(x^2), rowSums(centers^2), "+") - 2 * x %*% t(centers)
  sqrt(pmax(squared, 0))
}

#' @noRd
.eik_nearest <- function(centers, x) {
  max.col(-.eik_distances(centers, x), ties.method = "first")
}

# Bin of every point: the centroid with the smallest amplified distance.
#' @noRd
.eik_assign <- function(centers, amplify, x) {
  amplified <- sweep(.eik_distances(centers, x), 2, amplify, "*")
  max.col(-amplified, ties.method = "first")
}

# Pearson's chi-square test of a 2 x K contingency table, as
# scipy.stats.chi2_contingency (Yates correction with one degree of freedom).
#' @noRd
.eik_chisq <- function(table) {
  table <- table[, colSums(table) > 0, drop = FALSE]
  df <- ncol(table) - 1L
  if ((df < 1L) || any(rowSums(table) == 0)) {
    return(list(statistic = 0, df = max(df, 0L), p_value = 1))
  }
  expected <- outer(rowSums(table), colSums(table)) / sum(table)
  observed <- table
  if (df == 1L) {
    difference <- expected - observed
    observed <- observed + pmin(0.5, abs(difference)) * sign(difference)
  }
  statistic <- sum((observed - expected)^2 / expected)
  list(statistic = statistic, df = df, p_value = stats::pchisq(statistic, df, lower.tail = FALSE))
}

#' @noRd
.eik_matrix <- function(data, features = NULL) {
  if (is.data.frame(data)) {
    if (!is.null(features)) {
      missing_features <- setdiff(features, names(data))
      if (length(missing_features) > 0L) {
        stop(sprintf("eikmeans: the sample lacks the features %s", paste(missing_features, collapse = ", ")),
             call. = FALSE)
      }
      data <- data[features]
    }
    if (!all(vapply(data, is.numeric, logical(1)))) {
      stop("eikmeans: every feature must be numeric", call. = FALSE)
    }
    data <- as.matrix(data)
  }
  if (!is.matrix(data) || !is.numeric(data)) {
    stop("eikmeans: the sample must be a numeric matrix or data frame", call. = FALSE)
  }
  if (!is.null(features) && !is.null(colnames(data)) && all(features %in% colnames(data))) {
    data <- data[, features, drop = FALSE]
  }
  if (anyNA(data)) {
    stop("eikmeans: the sample contains missing values", call. = FALSE)
  }
  storage.mode(data) <- "double"
  return(data)
}

# Stream detector ----------------------------------------------------------------------

#' @title EI-kMeans stream detector
#' @description Monitors a stream with an EI-kMeans partitioning
#' ([eikmeans()]): the first `n_train` observations form the training sample,
#' and every later window of `window_size` observations is tested against it.
#' Since it monitors the distribution of the observed features, it is
#' primarily used for **virtual concept drift**.
#' @param target_feat Names of the columns to be monitored. `NULL` (the default)
#' monitors every numeric column.
#' @param n_train Size of the training sample.
#' @param window_size Size of the tested windows (non-overlapping).
#' @param ... Further arguments for [eikmeans()] (`k`, `alpha`, `min_points`,
#' `n_build`, `thetas`).
#' @details A drift is reported on the last observation of every window whose
#' p-value is below `alpha`. `reset_state()` forgets the partitioning, and
#' the next `n_train` observations train a new one, which is the adaptation the
#' paper recommends after a drift. Rows with missing values are skipped.
#' @return `dfr_eikmeans` object. After `fit()`, `drifter_output` holds the
#' chi-square statistic and the p-value of every tested window (on its last
#' observation), and `obj$model` the partitioning.
#' @inherit eikmeans references
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' # Two clusters; after 3000 rows the right one moves.
#' set.seed(1)
#' cluster <- function(n, x) data.frame(x1 = stats::rnorm(n, x), x2 = stats::rnorm(n))
#' stream <- rbind(cluster(1500, 0), cluster(1500, 5))[sample(3000), ]
#' drifted <- rbind(cluster(500, 0), cluster(500, 6))[sample(1000), ]
#' model <- fit(dfr_eikmeans(n_train = 2000, window_size = 200), rbind(stream, drifted))
#' which(model$drifter_output$p_value < 0.05)
#' @export
dfr_eikmeans <- function(target_feat = NULL, n_train = 2000, window_size = 200, ...) {
  .check_target_feat(target_feat, multiple = TRUE)
  .check_positive_integer(n_train, "n_train", min_value = 2L)
  .check_positive_integer(window_size, "window_size", min_value = 1L)
  settings <- list(...)
  unknown <- setdiff(names(settings), c("k", "alpha", "min_points", "n_build", "thetas"))
  if ((length(settings) > 0L) && (is.null(names(settings)) || any(names(settings) == "") || (length(unknown) > 0L))) {
    stop(sprintf("dfr_eikmeans: unknown arguments for eikmeans(): %s", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  obj <- mv_dist_based(target_feat = target_feat)
  obj$state <- list(n_train = n_train, window_size = window_size, settings = settings, features = NULL,
                    buffer = NULL, n = 0)
  obj$model <- NULL
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_eikmeans", class(obj))
  return(obj)
}

#' @export
update_state.dfr_eikmeans <- function(obj, value, ...) {
  state <- obj$state
  row <- .select_features(value, obj$target_feat)
  if (is.null(state$features)) {
    state$features <- names(row)
  } else if (!identical(names(row), state$features)) {
    stop("dfr_eikmeans: the monitored features changed during the stream", call. = FALSE)
  }
  state$n <- state$n + 1
  if (anyNA(row)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  state$buffer <- rbind(state$buffer, row, deparse.level = 0)
  needed <- if (is.null(obj$model)) state$n_train else state$window_size
  if (nrow(state$buffer) < needed) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  sample <- state$buffer
  colnames(sample) <- state$features
  state$buffer <- NULL
  if (is.null(obj$model)) {
    obj$model <- do.call(eikmeans, c(list(training = sample, n_test = state$window_size), state$settings))
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  result <- eikmeans_test(obj$model, sample)
  obj$last_drifter_output <- c(statistic = result$statistic, p_value = result$p_value)
  obj$state <- state
  if (result$drift) {
    obj$drifted <- TRUE
    return(list(obj = obj, drift = TRUE))
  }
  return(list(obj = obj, drift = FALSE))
}

#' @export
fit.dfr_eikmeans <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data))
}

#' @export
reset_state.dfr_eikmeans <- function(obj) {
  obj$drifted <- FALSE
  obj$state$buffer <- NULL
  obj$model <- NULL
  return(obj)
}
