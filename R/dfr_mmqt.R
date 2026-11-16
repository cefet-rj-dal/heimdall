#' @title Multimodal QuantTree (MMQT)
#' @description Batch-wise change detector for stationary conditions made of
#' several *modalities* (Stucchi et al., 2023). Every stationary batch is
#' drawn from one of a set of unknown distributions, for instance the
#' operating modes of a machine, and a change is any batch that comes from
#' none of them. MMQT builds a single QuantTree histogram over the training
#' batches, clusters their bin-probability vectors to find the modalities,
#' and tests each new batch against the modality closest to it with a
#' modality-specific threshold that controls the false positive rate.
#' @param training The training set: a list of batches (matrices or data
#' frames with the same columns and `nu` rows each), or a single matrix or
#' data frame that is cut into consecutive batches of `nu` rows (an incomplete
#' last batch is dropped).
#' @param nu Number of observations per batch. Taken from the batches when
#' `training` is a list.
#' @param bins Number of bins `K` of the histogram, built with uniform target
#' probabilities `1 / K`.
#' @param alpha Target false positive rate of the test.
#' @param n_modalities Number of modalities `M`. `NULL` (the default)
#' estimates it with Algorithm 1 of the paper: the smallest `M` whose
#' detector has an empirical false positive rate on the training batches not
#' above `alpha_modalities`.
#' @param alpha_modalities Target false positive rate `alpha'` used to estimate
#' the number of modalities.
#' @param max_modalities Largest number of modalities tried by the estimation.
#' @param calibration Whether to calibrate the bin probabilities of each
#' modality (Eqs. 12-13), which corrects for a histogram built on the data of
#' all modalities.
#' @param n_repeats Number of times the estimation is repeated, each with a
#' new histogram. The number of modalities is the most frequent estimate, and
#' the histogram the one whose training false positive rate is closest to
#' `alpha_modalities` among those that gave it. Ignored when `n_modalities`
#' is given.
#' @param n_sim Number of Monte Carlo simulations used to compute each
#' threshold.
#' @param partition A partitioning from another `mmqt` model
#' (`model$partition`) to reuse instead of building a new one.
#' @param seed Seed of a private random number stream, so that training does
#' not change the global one. `NULL` uses the global generator.
#' @details **Histogram.** The partitioning is a QuantTree (Boracchi et al.,
#' 2018), ported from the reference code: each bin is cut from the remaining
#' space along a random coordinate, at the sample quantile that leaves the
#' target share of the remaining training points in the bin. Its bin
#' probabilities have a distribution that does not depend on the data, so the
#' threshold of the total variation statistic
#' `T = 0.5 * sum(|p - pi|)` can be computed by simulation for any data
#' (Frittoli et al., 2022): the bin probabilities of the histogram are drawn
#' from their Dirichlet distribution and the counts of a batch from a
#' multinomial, `n_sim` times, and the threshold is the `1 - alpha` quantile
#' of the simulated statistic.
#'
#' **Modalities.** The bin-probability vectors of the training batches are
#' clustered by k-means (k-means++ initialization, ten starts). The centroid
#' of a cluster estimates the bin probabilities `pi^m` of a modality, which
#' are calibrated as in Eq. 12, and the modality gets the threshold of a
#' QuantTree with probabilities `pi^m` built on the `nu N_m` points of its
#' `N_m` batches.
#'
#' **Monitoring.** A batch is assigned to the modality whose (calibrated) bin
#' probabilities are closest in Euclidean distance (Eq. 7), and a change is
#' detected when its statistic against that modality exceeds the modality's
#' threshold (Eqs. 8-9).
#'
#' The calibration needs, for every bin, the training points of the modality
#' closest to the bin boundary inside and outside the bin. When the modality
#' has no point on one of the two sides, `g` is set to 0.
#'
#' The reference repository (<https://github.com/diegocarrera89/quantTree>)
#' implements QuantTree, but its MMQT is announced as "coming soon"; MMQT is
#' therefore implemented from the paper.
#' @references Stucchi, D., Magri, L., Carrera, D. and Boracchi, G. (2023).
#' Multimodal batch-wise change detection. *IEEE Transactions on Neural
#' Networks and Learning Systems*, 34(10), 6783-6797.
#' <doi:10.1109/TNNLS.2023.3294846>
#'
#' Boracchi, G., Carrera, D., Cervellera, C. and Maccio, D. (2018).
#' QuantTree: histograms for change detection in multivariate data streams.
#' *Proceedings of the 35th International Conference on Machine Learning*,
#' PMLR 80, 639-648.
#' @return An `mmqt` object with the partitioning (`partition`), the reference
#' bin probabilities of each modality (`reference`, one row per modality),
#' their thresholds (`thresholds`), the number of training batches of each
#' modality (`n_batches`), the false positive rate on the training batches
#' (`training_fpr`) and, when the number of modalities was estimated, the
#' estimates of every repetition (`estimation`).
#' @seealso [mmqt_test()] to test batches and [dfr_mmqt()] to monitor a
#' stream.
#' @examples
#' # Two operating modes; a change moves the first one.
#' set.seed(1)
#' mode <- function(n, center) matrix(stats::rnorm(2 * n, center), ncol = 2)
#' training <- c(lapply(1:20, function(i) mode(64, 0)), lapply(1:20, function(i) mode(64, 4)))
#' model <- mmqt(training, bins = 8, n_repeats = 2, n_sim = 2000, seed = 1)
#' model$n_modalities
#' mmqt_test(model, list(mode(64, 0), mode(64, 4), mode(64, 1.5)))
#' @export
mmqt <- function(training, nu = NULL, bins = 16, alpha = 0.05, n_modalities = NULL, alpha_modalities = 0.05,
                 max_modalities = 10, calibration = TRUE, n_repeats = 10, n_sim = 10000, partition = NULL,
                 seed = NULL) {
  .check_positive_integer(bins, "bins", min_value = 2L)
  .check_probability(alpha, "alpha")
  .check_probability(alpha_modalities, "alpha_modalities")
  .check_positive_integer(max_modalities, "max_modalities", min_value = 1L)
  .check_positive_integer(n_repeats, "n_repeats", min_value = 1L)
  .check_positive_integer(n_sim, "n_sim", min_value = 100L)
  if (!is.null(n_modalities)) {
    .check_positive_integer(n_modalities, "n_modalities", min_value = 1L)
  }
  if (!is.logical(calibration) || (length(calibration) != 1L) || is.na(calibration)) {
    stop("calibration must be TRUE or FALSE", call. = FALSE)
  }
  batches <- .mmqt_batches(training, nu)
  nu <- nrow(batches[[1]])
  if (length(batches) < 2L) {
    stop("mmqt: the training set must contain at least two batches", call. = FALSE)
  }
  if (length(batches) * nu < bins) {
    stop("mmqt: the training set must contain at least as many points as bins", call. = FALSE)
  }
  if (!is.null(partition) && !inherits(partition, "mmqt_partition")) {
    stop("partition must be the partition of an mmqt model", call. = FALSE)
  }
  if (!is.null(partition) && (partition$bins != bins)) {
    bins <- partition$bins
  }

  train <- function() {
    .mmqt_train(batches, nu, bins, alpha, n_modalities, alpha_modalities, max_modalities, calibration,
                n_repeats, n_sim, partition)
  }
  if (is.null(seed)) {
    model <- train()
  } else {
    model <- .with_rng(.rng_state(seed), train)$value
  }
  model$features <- colnames(batches[[1]])
  model$dim <- ncol(batches[[1]])
  return(model)
}

#' @title Test batches with a Multimodal QuantTree
#' @description Assigns each batch to the closest modality of an [mmqt()]
#' model (Eq. 7) and compares its total variation statistic with the
#' threshold of that modality (Eqs. 8-9).
#' @param model An `mmqt` object.
#' @param batches A batch (matrix or data frame) or a list of batches, with
#' the columns of the training set and `model$nu` rows each.
#' @return A data frame with one row per batch: the assigned `modality`, the
#' `statistic`, the `threshold` of the modality and whether a change is
#' detected (`drift`).
#' @examples
#' set.seed(1)
#' training <- lapply(1:40, function(i) matrix(stats::rnorm(128), ncol = 2))
#' model <- mmqt(training, bins = 8, n_modalities = 1, n_sim = 2000)
#' mmqt_test(model, list(matrix(stats::rnorm(128), ncol = 2), matrix(stats::rnorm(128, 1), ncol = 2)))
#' @export
mmqt_test <- function(model, batches) {
  if (!inherits(model, "mmqt")) {
    stop("mmqt_test: model must be an mmqt object", call. = FALSE)
  }
  if (is.data.frame(batches) || is.matrix(batches)) {
    batches <- list(batches)
  }
  batches <- lapply(batches, .mmqt_matrix, features = model$features)
  if (any(vapply(batches, ncol, integer(1)) != model$dim)) {
    stop("mmqt_test: the batch has a different number of features from the training set", call. = FALSE)
  }
  if (any(vapply(batches, nrow, integer(1)) != model$nu)) {
    stop(sprintf("mmqt_test: every batch must have nu = %d rows", model$nu), call. = FALSE)
  }
  vectors <- t(vapply(batches, function(batch) .mmqt_bin_vector(model$partition, batch), numeric(model$partition$bins)))
  result <- .mmqt_statistics(vectors, model$reference, model$thresholds)
  return(result)
}

# Training ------------------------------------------------------------------------

#' @noRd
.mmqt_train <- function(batches, nu, bins, alpha, n_modalities, alpha_modalities, max_modalities, calibration,
                        n_repeats, n_sim, partition) {
  points <- do.call(rbind, batches)
  batch_of <- rep(seq_along(batches), each = nu)
  repeats <- if (is.null(n_modalities) && is.null(partition)) n_repeats else 1L
  fits <- vector("list", repeats)
  for (r in seq_len(repeats)) {
    tree <- if (is.null(partition)) .qt_build(points, rep(1 / bins, bins)) else partition
    point_bins <- .qt_find_bin(tree, points)
    vectors <- .mmqt_vectors(point_bins, batch_of, length(batches), bins, nu)
    if (!is.null(n_modalities)) {
      fit <- .mmqt_fit(tree, points, point_bins, batch_of, vectors, n_modalities, nu, alpha, calibration, n_sim)
      if (is.null(fit)) {
        stop(sprintf("mmqt: the training batches cannot be split into %d modalities", n_modalities), call. = FALSE)
      }
      fits[[r]] <- fit
      next
    }
    # Algorithm 1: the smallest number of modalities whose training FPR is
    # not above alpha_modalities.
    limit <- min(max_modalities, nrow(unique(vectors)))
    fit <- NULL
    for (m in seq_len(limit)) {
      candidate <- .mmqt_fit(tree, points, point_bins, batch_of, vectors, m, nu, alpha, calibration, n_sim)
      if (is.null(candidate)) {
        break
      }
      fit <- candidate
      if (fit$training_fpr <= alpha_modalities) {
        break
      }
    }
    if (is.null(fit)) {
      stop("mmqt: the modalities could not be estimated", call. = FALSE)
    }
    fits[[r]] <- fit
  }

  estimates <- vapply(fits, function(fit) fit$n_modalities, numeric(1))
  fprs <- vapply(fits, function(fit) fit$training_fpr, numeric(1))
  counts <- table(estimates)
  chosen_m <- min(as.numeric(names(counts)[counts == max(counts)]))
  candidates <- which(estimates == chosen_m)
  chosen <- candidates[which.min(abs(fprs[candidates] - alpha_modalities))]
  model <- fits[[chosen]]
  model$nu <- nu
  model$alpha <- alpha
  model$alpha_modalities <- alpha_modalities
  model$calibration <- calibration
  model$estimation <- if (is.null(n_modalities) && is.null(partition)) {
    data.frame(repetition = seq_len(repeats), n_modalities = estimates, training_fpr = fprs)
  } else {
    NULL
  }
  if (is.null(n_modalities) && (chosen_m == max_modalities) && (model$training_fpr > alpha_modalities)) {
    warning("mmqt: the training false positive rate is above alpha_modalities with max_modalities modalities",
            call. = FALSE)
  }
  class(model) <- "mmqt"
  return(model)
}

# Fits the modalities of a partitioning: k-means on the bin-probability
# vectors, calibration, thresholds and the false positive rate on the
# training batches. NULL when the vectors cannot be split into m clusters.
#' @noRd
.mmqt_fit <- function(tree, points, point_bins, batch_of, vectors, m, nu, alpha, calibration, n_sim) {
  clusters <- .mmqt_kmeans(vectors, m)
  if (is.null(clusters)) {
    return(NULL)
  }
  bins <- tree$bins
  reference <- matrix(0, m, bins)
  thresholds <- numeric(m)
  n_batches <- numeric(m)
  for (j in seq_len(m)) {
    members <- which(clusters$cluster == j)
    n_batches[j] <- length(members)
    reference[j, ] <- colMeans(vectors[members, , drop = FALSE])
    if (calibration) {
      inside <- batch_of %in% members
      reference[j, ] <- .mmqt_calibrate(tree, points[inside, , drop = FALSE], point_bins[inside])
    }
    thresholds[j] <- .qt_threshold(reference[j, ], nu * length(members), nu, alpha, n_sim)
  }
  statistics <- .mmqt_statistics(vectors, reference, thresholds)
  list(partition = tree, reference = reference, thresholds = thresholds, n_batches = n_batches,
       n_modalities = m, training_fpr = mean(statistics$drift))
}

# Batch-to-modality mapping (Eq. 7) and total variation statistic (Eq. 8).
#' @noRd
.mmqt_statistics <- function(vectors, reference, thresholds) {
  distances <- vapply(seq_len(nrow(reference)), function(j) {
    rowSums(sweep(vectors, 2, reference[j, ])^2)
  }, numeric(nrow(vectors)))
  distances <- matrix(distances, nrow = nrow(vectors))
  modality <- max.col(-distances, ties.method = "first")
  statistic <- 0.5 * rowSums(abs(vectors - reference[modality, , drop = FALSE]))
  data.frame(modality = modality, statistic = statistic, threshold = thresholds[modality],
             drift = statistic > thresholds[modality])
}

# k-means with k-means++ initialization and the best of n_init starts, as
# sklearn's KMeans used in the paper.
#' @noRd
.mmqt_kmeans <- function(x, k, n_init = 10L) {
  if (k == 1L) {
    return(list(cluster = rep(1L, nrow(x))))
  }
  if (nrow(unique(x)) < k) {
    return(NULL)
  }
  best <- NULL
  for (start in seq_len(n_init)) {
    centers <- .mmqt_kmeanspp(x, k)
    fit <- tryCatch(suppressWarnings(stats::kmeans(x, centers, iter.max = 300L, algorithm = "Lloyd")),
                    error = function(e) NULL)
    if (is.null(fit) || (length(unique(fit$cluster)) < k)) {
      next
    }
    if (is.null(best) || (fit$tot.withinss < best$tot.withinss)) {
      best <- fit
    }
  }
  if (is.null(best)) {
    return(NULL)
  }
  list(cluster = best$cluster)
}

#' @noRd
.mmqt_kmeanspp <- function(x, k) {
  chosen <- sample.int(nrow(x), 1L)
  distance <- rowSums(sweep(x, 2, x[chosen, ])^2)
  for (j in seq_len(k - 1L)) {
    next_point <- if (sum(distance) > 0) sample.int(nrow(x), 1L, prob = distance) else sample.int(nrow(x), 1L)
    chosen <- c(chosen, next_point)
    distance <- pmin(distance, rowSums(sweep(x, 2, x[next_point, ])^2))
  }
  x[chosen, , drop = FALSE]
}

# Calibrated bin probabilities of one modality (Eqs. 12 and 13), from its
# training points and their bins.
#' @noRd
.mmqt_calibrate <- function(tree, points, point_bins) {
  bins <- tree$bins
  n <- nrow(points)
  counts <- tabulate(point_bins, nbins = bins)
  g <- numeric(bins - 1L)
  available <- rep(TRUE, n)
  for (k in seq_len(bins - 1L)) {
    values <- points[available, tree$dim[k]]
    inside <- point_bins[available] == k
    if (any(inside) && any(!inside)) {
      if (tree$side[k] < 0) {
        boundary <- tree$upper[k]
        last_inside <- max(values[inside])
        first_outside <- min(values[!inside])
        gap <- first_outside - last_inside
        g[k] <- if (gap > 0) (boundary - last_inside) / gap else 0
      } else {
        boundary <- tree$lower[k]
        last_inside <- min(values[inside])
        first_outside <- max(values[!inside])
        gap <- last_inside - first_outside
        g[k] <- if (gap > 0) (last_inside - boundary) / gap else 0
      }
    }
    available[which(available)[inside]] <- FALSE
  }
  g <- pmin(pmax(g, 0), 1)
  # q_{j+1} n_m is the number of points left after bin j.
  remaining <- n - cumsum(counts[-bins])
  factors <- 1 - g / (remaining + 1)
  calibrated <- numeric(bins)
  for (k in seq_len(bins - 1L)) {
    calibrated[k] <- (counts[k] + g[k]) / (n + 1) * prod(factors[seq_len(k - 1L)])
  }
  calibrated[bins] <- max(0, 1 - sum(calibrated[-bins]))
  return(calibrated)
}

# QuantTree ------------------------------------------------------------------------

# QuantTree partitioning (QuantTreePartitioning._build_partitioning of the
# reference): bin k is cut from the remaining space along a random coordinate,
# on a random side, at the sample quantile that leaves round(p * N) of the N
# remaining points in it.
# `dims` and `sides` (-1 lower tail, 1 upper tail) replace the random choices;
# the validation uses them to replay partitionings of the reference code.
#' @noRd
.qt_build <- function(points, pi_values, dims = NULL, sides = NULL) {
  n <- nrow(points)
  d <- ncol(points)
  bins <- length(pi_values)
  tree <- list(bins = bins, dim = integer(bins), lower = rep(-Inf, bins), upper = rep(Inf, bins),
               side = integer(bins))
  lower_limit <- rep(-Inf, d)
  upper_limit <- rep(Inf, d)
  available <- rep(TRUE, n)
  for (k in seq_len(bins - 1L)) {
    i <- if (is.null(dims)) sample.int(d, 1L) else dims[k]
    index <- which(available)
    values <- points[index, i]
    sorted <- order(values)
    values <- values[sorted]
    remaining <- length(index)
    share <- pi_values[k] / (1 - sum(pi_values[seq_len(k - 1L)]))
    size <- min(max(round(share * remaining), 1), remaining)
    tree$dim[k] <- i
    lower_tail <- if (is.null(sides)) sample(c(TRUE, FALSE), 1L) else (sides[k] < 0)
    if (lower_tail) {
      tree$lower[k] <- lower_limit[i]
      tree$upper[k] <- values[size]
      tree$side[k] <- -1L
      lower_limit[i] <- values[size]
      taken <- sorted[seq_len(size)]
    } else {
      tree$lower[k] <- values[remaining - size + 1L]
      tree$upper[k] <- upper_limit[i]
      tree$side[k] <- 1L
      upper_limit[i] <- values[remaining - size + 1L]
      taken <- sorted[(remaining - size + 1L):remaining]
    }
    available[index[taken]] <- FALSE
  }
  i <- if (is.null(dims)) sample.int(d, 1L) else dims[bins]
  tree$dim[bins] <- i
  tree$lower[bins] <- lower_limit[i]
  tree$upper[bins] <- upper_limit[i]
  class(tree) <- "mmqt_partition"
  return(tree)
}

# Bin of every point (QuantTreePartitioning._find_bin): the first bin that
# contains it, the last bin otherwise.
#' @noRd
.qt_find_bin <- function(tree, points) {
  n <- nrow(points)
  result <- rep(tree$bins, n)
  available <- seq_len(n)
  for (k in seq_len(tree$bins - 1L)) {
    if (length(available) == 0L) {
      break
    }
    values <- points[available, tree$dim[k]]
    inside <- if (tree$side[k] < 0) {
      (tree$lower[k] < values) & (values <= tree$upper[k])
    } else {
      (tree$lower[k] <= values) & (values < tree$upper[k])
    }
    inside[is.na(inside)] <- FALSE
    result[available[inside]] <- k
    available <- available[!inside]
  }
  return(result)
}

#' @noRd
.mmqt_vectors <- function(point_bins, batch_of, n_batches, bins, nu) {
  counts <- tabulate((batch_of - 1L) * bins + point_bins, nbins = n_batches * bins)
  matrix(counts, nrow = n_batches, ncol = bins, byrow = TRUE) / nu
}

#' @noRd
.mmqt_bin_vector <- function(tree, batch) {
  tabulate(.qt_find_bin(tree, batch), nbins = tree$bins) / nrow(batch)
}

# Threshold of the total variation statistic of a QuantTree with bin
# probabilities pi_values built on n_train points, for batches of nu points
# (QuantTreeThresholdStrategy.compute_threshold, in probability units). The
# univariate QuantTree of the reference simulation puts its edges at the order
# statistics cumsum(L) of n_train uniform points, so its bin probabilities
# are Dirichlet(L_1, ..., L_{K-1}, n_train + 1 - sum(L_{1..K-1})); the counts of
# a batch are multinomial.
#' @noRd
.qt_threshold <- function(pi_values, n_train, nu, alpha, n_sim) {
  bins <- length(pi_values)
  expected <- pi_values * n_train
  sizes <- floor(expected)
  extra <- n_train - sum(sizes)
  if (extra > 0) {
    order_fraction <- order(expected - sizes, decreasing = TRUE)
    sizes[order_fraction[seq_len(extra)]] <- sizes[order_fraction[seq_len(extra)]] + 1
  }
  shape <- c(sizes[-bins], n_train + 1 - sum(sizes[-bins]))
  gammas <- matrix(stats::rgamma(n_sim * bins, shape = rep(shape, each = n_sim)), nrow = n_sim)
  probabilities <- gammas / rowSums(gammas)
  counts <- matrix(0, n_sim, bins)
  left <- rep(nu, n_sim)
  mass <- rep(1, n_sim)
  for (k in seq_len(bins - 1L)) {
    share <- ifelse(mass > 0, pmin(probabilities[, k] / mass, 1), 0)
    counts[, k] <- stats::rbinom(n_sim, left, share)
    left <- left - counts[, k]
    mass <- mass - probabilities[, k]
  }
  counts[, bins] <- left
  statistics <- 0.5 * rowSums(abs(counts - matrix(nu * pi_values, n_sim, bins, byrow = TRUE)))
  values <- sort(unique(statistics))
  ecdf_values <- findInterval(values, sort(statistics)) / n_sim
  position <- min(sum(ecdf_values <= 1 - alpha) + 1L, length(values))
  return(values[position] / nu)
}

# Input ------------------------------------------------------------------------------

#' @noRd
.mmqt_matrix <- function(batch, features = NULL) {
  if (is.data.frame(batch)) {
    if (!is.null(features)) {
      missing_features <- setdiff(features, names(batch))
      if (length(missing_features) > 0L) {
        stop(sprintf("mmqt: the batch lacks the features %s", paste(missing_features, collapse = ", ")), call. = FALSE)
      }
      batch <- batch[features]
    }
    if (!all(vapply(batch, is.numeric, logical(1)))) {
      stop("mmqt: every feature must be numeric", call. = FALSE)
    }
    batch <- as.matrix(batch)
  }
  if (!is.matrix(batch) || !is.numeric(batch)) {
    stop("mmqt: a batch must be a numeric matrix or data frame", call. = FALSE)
  }
  if (!is.null(features) && !is.null(colnames(batch)) && !identical(colnames(batch), features)) {
    batch <- batch[, features, drop = FALSE]
  }
  if (!is.null(features) && (ncol(batch) != length(features))) {
    stop("mmqt: the batch has a different number of features from the training set", call. = FALSE)
  }
  return(batch)
}

#' @noRd
.mmqt_batches <- function(training, nu) {
  if (is.data.frame(training) || is.matrix(training)) {
    if (is.null(nu)) {
      stop("mmqt: nu is required when training is a single matrix or data frame", call. = FALSE)
    }
    .check_positive_integer(nu, "nu", min_value = 1L)
    data <- .mmqt_matrix(training)
    n_batches <- nrow(data) %/% nu
    if (n_batches == 0L) {
      stop("mmqt: the training set has fewer than nu rows", call. = FALSE)
    }
    return(lapply(seq_len(n_batches), function(b) data[(b - 1L) * nu + seq_len(nu), , drop = FALSE]))
  }
  if (!is.list(training) || (length(training) == 0L)) {
    stop("mmqt: training must be a list of batches, a matrix or a data frame", call. = FALSE)
  }
  batches <- lapply(training, .mmqt_matrix)
  features <- colnames(batches[[1]])
  batches <- lapply(batches, .mmqt_matrix, features = features)
  sizes <- vapply(batches, nrow, integer(1))
  if (length(unique(sizes)) != 1L) {
    stop("mmqt: every training batch must have the same number of rows", call. = FALSE)
  }
  if (!is.null(nu) && (sizes[1] != nu)) {
    stop("mmqt: the training batches do not have nu rows", call. = FALSE)
  }
  if (anyNA(unlist(batches))) {
    stop("mmqt: the training batches contain missing values", call. = FALSE)
  }
  return(batches)
}

#' @export
print.mmqt <- function(x, ...) {
  cat(sprintf("Multimodal QuantTree: %d bins, batches of %d, alpha = %g%s\n", x$partition$bins, x$nu, x$alpha,
              if (x$calibration) ", calibrated" else ""))
  cat(sprintf("%d modalities (training batches: %s), thresholds: %s\n", x$n_modalities,
              paste(x$n_batches, collapse = ", "), paste(signif(x$thresholds, 3), collapse = ", ")))
  cat(sprintf("False positive rate on the training batches: %.3f\n", x$training_fpr))
  invisible(x)
}

# Stream detector ----------------------------------------------------------------------

#' @title Multimodal QuantTree stream detector
#' @description Monitors a stream with a Multimodal QuantTree ([mmqt()]): the
#' observations are grouped in consecutive batches of `nu` rows, the first
#' `n_train_batches` batches train the model, and every later batch is
#' tested. Since it monitors the distribution of the observed features, it is
#' primarily used for **virtual concept drift**.
#' @param target_feat Names of the columns to be monitored. `NULL` (the default)
#' monitors every numeric column.
#' @param nu Number of observations per batch.
#' @param n_train_batches Number of batches used for training. Ignored when
#' `model` is given.
#' @param model A trained [mmqt()] model. When given, every batch is tested
#' and `reset_state()` keeps the model.
#' @param ... Further arguments for [mmqt()] (`bins`, `alpha`,
#' `n_modalities`, `alpha_modalities`, `max_modalities`, `calibration`,
#' `n_repeats`, `n_sim`, `seed`).
#' @details Each batch is tested on its own (MMQT is a one-shot test, with a
#' false positive rate `alpha` per batch), so the detector reports a drift on
#' the last observation of every batch whose statistic exceeds the threshold
#' of its modality. `reset_state()` forgets the model it trained and trains a
#' new one on the next `n_train_batches` batches. Rows with missing values
#' are skipped.
#' @return `dfr_mmqt` object. After `fit()`, `drifter_output` holds, on the
#' last observation of every tested batch, the assigned modality, the
#' statistic and the threshold, and `obj$model` the trained model.
#' @inherit mmqt references
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' # Batches alternate between two modes; from batch 50 the first mode moves.
#' set.seed(1)
#' batch <- function(center) data.frame(x = stats::rnorm(64, center), y = stats::rnorm(64, center))
#' centers <- c(rep(c(0, 4), 25), rep(c(1.5, 4), 10))
#' stream <- do.call(rbind, lapply(centers, batch))
#' detector <- dfr_mmqt(nu = 64, n_train_batches = 40, bins = 8, n_repeats = 2, n_sim = 2000, seed = 1)
#' model <- fit(detector, stream)
#' model$model$n_modalities
#' which(model$drifter_output$statistic > model$drifter_output$threshold) %/% 64
#' @export
dfr_mmqt <- function(target_feat = NULL, nu = 128, n_train_batches = 64, model = NULL, ...) {
  .check_target_feat(target_feat, multiple = TRUE)
  .check_positive_integer(nu, "nu", min_value = 1L)
  .check_positive_integer(n_train_batches, "n_train_batches", min_value = 2L)
  if (!is.null(model)) {
    if (!inherits(model, "mmqt")) {
      stop("model must be an mmqt object", call. = FALSE)
    }
    nu <- model$nu
  }
  settings <- list(...)
  unknown <- setdiff(names(settings), c("bins", "alpha", "n_modalities", "alpha_modalities", "max_modalities",
                                        "calibration", "n_repeats", "n_sim", "seed"))
  if ((length(settings) > 0L) && (is.null(names(settings)) || any(names(settings) == "") || (length(unknown) > 0L))) {
    stop(sprintf("dfr_mmqt: unknown arguments for mmqt(): %s", paste(unknown, collapse = ", ")), call. = FALSE)
  }

  obj <- mv_dist_based(target_feat = target_feat)
  obj$state <- list(nu = nu, n_train_batches = n_train_batches, settings = settings, given_model = model,
                    features = NULL, buffer = NULL, training = list(), n = 0)
  obj$model <- model
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_mmqt", class(obj))
  return(obj)
}

#' @export
update_state.dfr_mmqt <- function(obj, value, ...) {
  state <- obj$state
  row <- .select_features(value, obj$target_feat)
  if (is.null(state$features)) {
    state$features <- names(row)
  } else if (!identical(names(row), state$features)) {
    stop("dfr_mmqt: the monitored features changed during the stream", call. = FALSE)
  }
  state$n <- state$n + 1
  if (anyNA(row)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  state$buffer <- rbind(state$buffer, row, deparse.level = 0)
  if (nrow(state$buffer) < state$nu) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  batch <- state$buffer
  colnames(batch) <- state$features
  state$buffer <- NULL

  if (is.null(obj$model)) {
    state$training[[length(state$training) + 1L]] <- batch
    if (length(state$training) == state$n_train_batches) {
      obj$model <- do.call(mmqt, c(list(training = state$training), state$settings))
      state$training <- list()
    }
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  result <- mmqt_test(obj$model, batch)
  obj$last_drifter_output <- c(modality = result$modality, statistic = result$statistic,
                               threshold = result$threshold)
  obj$state <- state
  if (result$drift) {
    obj$drifted <- TRUE
    return(list(obj = obj, drift = TRUE))
  }
  return(list(obj = obj, drift = FALSE))
}

#' @export
fit.dfr_mmqt <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  return(.fit_row_stream(obj, data))
}

#' @export
reset_state.dfr_mmqt <- function(obj) {
  state <- obj$state
  obj$drifted <- FALSE
  obj$state$buffer <- NULL
  obj$state$training <- list()
  obj$model <- state$given_model
  return(obj)
}
