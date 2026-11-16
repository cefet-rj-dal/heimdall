#' @title Drift benchmark source
#' @description Creates a data source with controlled real and virtual drift,
#' following the pipeline of Hinder et al. (2026), "Real vs. Virtual Drift:
#' Creating Realistic Stream Learning Benchmarks". A source combines `nv`
#' feature distributions (virtual drift states, given by `samplers`) with `nr`
#' labelling rules (real drift states, given by `labelers`), so it has
#' `nv * nr` concepts. Moving between two concepts that share the labelling rule
#' is pure virtual drift; moving between two that share the feature
#' distribution is pure real drift.
#'
#' Most users will create sources with [dsg_source_data()] (from a real-world
#' stream) or with the synthetic generators [dsg_sea()], [dsg_agrawal()],
#' [dsg_sine()] and [dsg_mixed()], draw samples with [dsg_take()], induce drift
#' with [dsg_drift()], and build whole streams with [dsg_stream()].
#' @param samplers List of functions; `samplers[[v]](n)` returns a data frame
#' (or matrix) with up to `n` rows drawn from feature distribution `v`. It may
#' return fewer rows; [dsg_take()] calls it again until it has enough.
#' @param labelers List of functions; `labelers[[r]](x)` returns one label per
#' row of the data frame `x` according to labelling rule `r`.
#' @param real_drift_level Smallest share of labels that a real drift must
#' change (on `test_size` points) to be accepted.
#' @param virtual_p_value Largest p-value of the kernel two-sample test
#' ([dsg_mmd_test()]) for a virtual drift to be accepted.
#' @param test_size Number of points drawn to verify each drift.
#' @param n_perm Number of permutations of the two-sample test.
#' @param max_tries Number of candidate concepts tried by [dsg_drift()] before
#' it falls back to the strongest real drift found.
#' @return `dsg_source` object
#' @references Hinder, F., Brinkrolf, J., Lammers, K., and Hammer, B. (2026).
#' Real vs. Virtual Drift: Creating Realistic Stream Learning Benchmarks. In
#' *Proceedings of the European Symposium on Artificial Neural Networks,
#' Computational Intelligence and Machine Learning (ESANN 2026)*, 133-138.
#' @examples
#' # Two feature distributions and two labelling rules: four concepts.
#' source <- dsg_source(
#'   samplers = list(
#'     function(n) data.frame(x = stats::runif(n, 0, 1)),
#'     function(n) data.frame(x = stats::runif(n, 0.5, 1.5))
#'   ),
#'   labelers = list(
#'     function(x) as.integer(x$x > 0.5),
#'     function(x) as.integer(x$x > 1)
#'   ),
#'   test_size = 200, n_perm = 200
#' )
#' set.seed(1)
#' stream <- dsg_stream(source, "-v-r", batch_size = 100)
#' stream$segments
#' @export
dsg_source <- function(samplers, labelers, real_drift_level = 0.01, virtual_p_value = 0.001,
                       test_size = 1000, n_perm = 2500, max_tries = 100) {
  is_function_list <- function(x) is.list(x) && (length(x) > 0L) && all(vapply(x, is.function, logical(1)))
  if (!is_function_list(samplers)) {
    stop("samplers must be a non-empty list of functions", call. = FALSE)
  }
  if (!is_function_list(labelers)) {
    stop("labelers must be a non-empty list of functions", call. = FALSE)
  }
  .check_probability(real_drift_level, "real_drift_level")
  .check_probability(virtual_p_value, "virtual_p_value")
  .check_positive_integer(test_size, "test_size", min_value = 2L)
  .check_positive_integer(n_perm, "n_perm", min_value = 1L)
  .check_positive_integer(max_tries, "max_tries", min_value = 1L)

  # Concepts are numbered as in the reference implementation: for every
  # feature distribution, every labelling rule.
  states <- data.frame(
    virtual = rep(seq_along(samplers), each = length(labelers)),
    real = rep(seq_along(labelers), times = length(samplers))
  )
  obj <- list(
    samplers = samplers,
    labelers = labelers,
    states = states,
    state = sample.int(nrow(states), 1L),
    real_drift_level = real_drift_level,
    virtual_p_value = virtual_p_value,
    test_size = test_size,
    n_perm = n_perm,
    max_tries = max_tries
  )
  class(obj) <- "dsg_source"
  return(obj)
}

#' @export
print.dsg_source <- function(x, ...) {
  current <- x$states[x$state, ]
  cat(sprintf(
    "<%s> %d feature distributions x %d labelling rules; current concept: virtual %d, real %d\n",
    class(x)[1], length(x$samplers), length(x$labelers), current$virtual, current$real
  ))
  invisible(x)
}

#' @title Draw samples from a drift benchmark source
#' @description Draws `n` labelled samples from the current concept of a
#' [dsg_source()].
#' @param source `dsg_source` object
#' @param n Number of samples.
#' @return list with the features `x` (data frame) and the labels `y`
#' @examples
#' set.seed(1)
#' sample <- dsg_take(dsg_sea(), 5)
#' sample$x
#' sample$y
#' @export
dsg_take <- function(source, n = 1) {
  .check_positive_integer(n, "n", min_value = 1L)
  concept <- source$states[source$state, ]
  sampler <- source$samplers[[concept$virtual]]
  parts <- list()
  received <- 0L
  attempts <- 0L
  while (received < n) {
    part <- as.data.frame(sampler(n - received))
    attempts <- attempts + 1L
    if ((nrow(part) == 0L) && (attempts > 1000L)) {
      stop("the sampler of the current concept returns no data", call. = FALSE)
    }
    parts[[length(parts) + 1L]] <- part
    received <- received + nrow(part)
  }
  x <- do.call(rbind, parts)
  if (nrow(x) > n) {
    x <- x[sample.int(nrow(x), n), , drop = FALSE]
  }
  rownames(x) <- NULL
  y <- source$labelers[[concept$real]](x)
  if (length(y) != nrow(x)) {
    stop("a labeler must return one label per row", call. = FALSE)
  }
  return(list(x = x, y = y))
}

# Draws a new concept that differs from the current one as `type` requires
# ("v": only the feature distribution, "r": only the labelling rule, "c": both,
# "a": any change), as in DataSource.generate_drift of the reference code.
#' @noRd
.dsg_pick_state <- function(source, type) {
  old <- source$states[source$state, ]
  for (candidate in sample.int(nrow(source$states))) {
    new <- source$states[candidate, ]
    virtual <- new$virtual != old$virtual
    real <- new$real != old$real
    accepted <- switch(type,
      a = virtual || real,
      v = virtual && !real,
      r = real && !virtual,
      c = virtual && real
    )
    if (accepted) {
      return(list(state = candidate, virtual = virtual, real = real))
    }
  }
  stop(sprintf("the source has no concept for a drift of type '%s'", type), call. = FALSE)
}

#' @title Induce a drift in a benchmark source
#' @description Moves a [dsg_source()] to another concept and verifies the
#' drift: a virtual drift must make the kernel two-sample test
#' ([dsg_mmd_test()]) reject at `virtual_p_value`, and a real drift must change
#' at least `real_drift_level` of the labels of the same points. Candidates that
#' fail are discarded; after `max_tries` candidates, the strongest real drift
#' found is used with a warning.
#' @param source `dsg_source` object
#' @param type `"-"` (or `"_"`) no drift, `"v"` pure virtual drift, `"r"` pure
#' real drift, `"c"` both, `"a"` any of them.
#' @return list with the updated `source`, the logical flags `virtual` and
#' `real`, and the verification statistics `p_value` (two-sample test, `NA`
#' without virtual drift) and `label_change` (share of changed labels, `NA`
#' without real drift)
#' @examples
#' set.seed(2)
#' drift <- dsg_drift(dsg_sine(), "r")
#' drift$real
#' drift$label_change
#' @export
dsg_drift <- function(source, type = "a") {
  type <- .dsg_check_type(type)
  if (type == "-") {
    return(list(source = source, virtual = FALSE, real = FALSE, p_value = NA_real_, label_change = NA_real_))
  }
  old_state <- source$state
  old_real <- source$states$real[old_state]
  reference <- dsg_take(source, source$test_size)$x

  best_state <- NA_integer_
  best_change <- -1
  for (try in seq_len(source$max_tries)) {
    source$state <- old_state
    pick <- .dsg_pick_state(source, type)
    source$state <- pick$state
    drawn <- dsg_take(source, source$test_size)
    p_value <- NA_real_
    label_change <- NA_real_
    if (pick$virtual) {
      p_value <- dsg_mmd_test(reference, drawn$x, n_perm = source$n_perm)$p_value
      if (p_value > source$virtual_p_value) {
        next
      }
    }
    if (pick$real) {
      label_change <- mean(drawn$y != source$labelers[[old_real]](drawn$x))
      if (label_change > best_change) {
        best_state <- pick$state
        best_change <- label_change
      }
      if (label_change < source$real_drift_level) {
        next
      }
    }
    return(list(source = source, virtual = pick$virtual, real = pick$real, p_value = p_value, label_change = label_change))
  }

  if (is.na(best_state) || (best_change <= 0)) {
    source$state <- old_state
    stop(sprintf("unable to create a verified drift of type '%s'", type), call. = FALSE)
  }
  source$state <- best_state
  warning(sprintf("unable to create a noticeable drift; using the strongest real drift found (%.3f of the labels change)", best_change), call. = FALSE)
  virtual <- source$states$virtual[best_state] != source$states$virtual[old_state]
  return(list(source = source, virtual = virtual, real = TRUE, p_value = NA_real_, label_change = best_change))
}

#' @noRd
.dsg_check_type <- function(type) {
  if (!is.character(type) || (length(type) != 1L) || !(type %in% c("-", "_", "a", "v", "r", "c"))) {
    stop("type must be one of '-', '_', 'a', 'v', 'r' or 'c'", call. = FALSE)
  }
  if (type == "_") "-" else type
}

#' @title Generate a benchmark stream with controlled drift
#' @description Builds a stream segment by segment. Each character of
#' `pattern` first induces a drift with [dsg_drift()] (`"-"` or `"_"` none,
#' `"v"` virtual, `"r"` real, `"c"` both, `"a"` any) and then draws
#' `batch_size` samples from the resulting concept.
#' @param source `dsg_source` object
#' @param pattern Drift pattern, such as `"--v--r-"`.
#' @param batch_size Number of samples per segment.
#' @return list with
#' \itemize{
#' \item `data`: data frame with the features, the label `y`, the `segment`
#' and the `drift` code of the segment (0 none, 1 virtual, 2 real, 3 both, as
#' in the reference implementation);
#' \item `segments`: one row per segment with the drift type, the concept and
#' the verification statistics;
#' \item `source`: the source in its final concept.
#' }
#' @examples
#' set.seed(3)
#' stream <- dsg_stream(dsg_sea(), "--v--r", batch_size = 200)
#' head(stream$data)
#' stream$segments
#' @export
dsg_stream <- function(source, pattern, batch_size = 500) {
  if (!is.character(pattern) || (length(pattern) != 1L) || !grepl("^[-_vrca]+$", pattern)) {
    stop("pattern must be a string made of '-', '_', 'v', 'r', 'c' and 'a'", call. = FALSE)
  }
  .check_positive_integer(batch_size, "batch_size", min_value = 1L)
  types <- strsplit(gsub("_", "-", pattern), "")[[1]]
  reserved <- intersect(names(dsg_take(source, 1)$x), c("y", "segment", "drift"))
  if (length(reserved) > 0L) {
    stop(sprintf("feature names clash with the stream columns: %s", paste(reserved, collapse = ", ")), call. = FALSE)
  }
  batches <- vector("list", length(types))
  segments <- vector("list", length(types))
  for (i in seq_along(types)) {
    drift <- dsg_drift(source, types[i])
    source <- drift$source
    drawn <- dsg_take(source, batch_size)
    code <- as.integer(drift$virtual) + 2L * as.integer(drift$real)
    batches[[i]] <- cbind(drawn$x, y = drawn$y, segment = i, drift = code)
    segments[[i]] <- data.frame(
      segment = i, type = types[i], drift = code,
      virtual_state = source$states$virtual[source$state],
      real_state = source$states$real[source$state],
      p_value = drift$p_value, label_change = drift$label_change
    )
  }
  data <- do.call(rbind, batches)
  rownames(data) <- NULL
  return(list(data = data, segments = do.call(rbind, segments), source = source))
}

# ---------------------------------------------------------------------------
# Kernel two-sample test
# ---------------------------------------------------------------------------

# Leaf of every row in every totally random tree (the embedding of
# sklearn.ensemble.RandomTreesEmbedding): each split draws a feature that is
# not constant in the node and a threshold uniformly between its minimum and
# maximum there.
#' @noRd
.dsg_random_tree_leaves <- function(x, n_trees = 100L, max_depth = 5L) {
  n <- nrow(x)
  leaves <- matrix(1L, nrow = n, ncol = n_trees)
  for (tree in seq_len(n_trees)) {
    node <- rep(1L, n)
    for (depth in seq_len(max_depth)) {
      for (id in unique(node)) {
        rows <- which(node == id)
        if (length(rows) < 2L) {
          next
        }
        ranges <- apply(x[rows, , drop = FALSE], 2, range)
        candidates <- which(ranges[2, ] > ranges[1, ])
        if (length(candidates) == 0L) {
          next
        }
        feature <- candidates[sample.int(length(candidates), 1L)]
        threshold <- stats::runif(1, ranges[1, feature], ranges[2, feature])
        node[rows] <- 2L * id + as.integer(x[rows, feature] > threshold)
      }
    }
    leaves[, tree] <- node
  }
  leaves
}

#' @title Kernel two-sample test (MMD)
#' @description Permutation test of whether two samples come from the same
#' distribution, using the maximum mean discrepancy (Gretton et al., 2012) with
#' a random-trees kernel: the similarity of two points is the share of totally
#' random trees in which they fall in the same leaf. This is the test used to
#' verify virtual drift in the reference implementation of Hinder et al.
#' (2026).
#' @param x1,x2 Data frames or matrices with the same numeric columns.
#' @param n_perm Number of permutations.
#' @param n_trees,max_depth Size of the random-trees embedding.
#' @return list with the MMD `statistic` and the permutation `p_value` (share
#' of permutations with a larger statistic)
#' @references Gretton, A., Borgwardt, K. M., Rasch, M. J., Scholkopf, B., and
#' Smola, A. (2012). A kernel two-sample test. *Journal of Machine Learning
#' Research*, 13, 723-773.
#' @examples
#' set.seed(1)
#' same <- dsg_mmd_test(matrix(stats::rnorm(200), 100), matrix(stats::rnorm(200), 100), n_perm = 200)
#' shifted <- dsg_mmd_test(matrix(stats::rnorm(200), 100), matrix(stats::rnorm(200, 1), 100),
#'                         n_perm = 200)
#' c(same$p_value, shifted$p_value)
#' @export
dsg_mmd_test <- function(x1, x2, n_perm = 2500, n_trees = 100, max_depth = 5) {
  x1 <- data.matrix(as.data.frame(x1))
  x2 <- data.matrix(as.data.frame(x2))
  if (ncol(x1) != ncol(x2)) {
    stop("x1 and x2 must have the same columns", call. = FALSE)
  }
  .check_positive_integer(n_perm, "n_perm", min_value = 1L)
  n1 <- nrow(x1)
  n2 <- nrow(x2)
  x <- rbind(x1, x2)

  leaves <- .dsg_random_tree_leaves(x, n_trees, max_depth)
  kernel <- matrix(0, nrow(x), nrow(x))
  for (tree in seq_len(n_trees)) {
    for (group in split(seq_len(nrow(x)), leaves[, tree])) {
      kernel[group, group] <- kernel[group, group] + 1
    }
  }
  kernel <- kernel / n_trees

  w <- c(rep(1 / n1, n1), rep(-1 / n2, n2))
  weights <- rbind(w, t(replicate(n_perm, sample(w))))
  statistics <- rowSums((weights %*% kernel) * weights)
  list(statistic = statistics[1], p_value = sum(statistics[-1] > statistics[1]) / n_perm)
}

# ---------------------------------------------------------------------------
# Sources built from a real-world stream
# ---------------------------------------------------------------------------

# Block boundaries as in normalize_block of the reference implementation: an
# integer k gives k boundaries evenly spread (k + 1 blocks); fractions are
# relative positions; larger values are row indices.
#' @noRd
.dsg_boundaries <- function(blocks, n) {
  if ((length(blocks) == 1L) && (blocks == round(blocks)) && (blocks >= 1)) {
    blocks <- seq_len(blocks) / (blocks + 1)
  }
  blocks <- sort(as.numeric(blocks))
  if ((blocks[1] <= 0) || any(diff(blocks) <= 0)) {
    stop("blocks must be a positive count or increasing positive positions", call. = FALSE)
  }
  if (blocks[length(blocks)] < 1) {
    blocks <- floor(blocks * n)
  }
  blocks <- as.integer(blocks)
  if ((blocks[1] < 1) || (blocks[length(blocks)] >= n) || anyDuplicated(blocks)) {
    stop("blocks must split the data into non-empty parts", call. = FALSE)
  }
  blocks
}

# Index (0, 1, ...) of the block of each row, given the block boundaries.
#' @noRd
.dsg_block_ids <- function(n, boundaries) {
  findInterval(seq_len(n) - 1L, boundaries)
}

# Random permutation without fixed points.
#' @noRd
.dsg_derangement <- function(n) {
  repeat {
    permutation <- sample.int(n)
    if (!any(permutation == seq_len(n))) {
      return(permutation)
    }
  }
}

# Region-wise label flips that enhance the real drift between blocks
# (add_flip of the reference implementation): the data are split by k-means,
# and in each block the labels of a block-specific subset of the clusters are
# permuted among the classes.
#' @noRd
.dsg_add_flips <- function(x, y, block, n_clusters, n_ignore) {
  classes <- sort(unique(y))
  codes <- match(y, classes)
  km <- suppressWarnings(stats::kmeans(x, centers = n_clusters, iter.max = 50L))
  flipped <- block == ((km$cluster - 1L) %% (length(classes) + n_ignore))
  permutation <- .dsg_derangement(length(classes))
  codes[flipped] <- permutation[codes[flipped]]
  list(y = classes[codes], flipped = flipped)
}

# Default model of P(y | block, x): a random forest giving class probabilities.
#' @noRd
.dsg_default_model <- function(nodesize = 10, ntree = 100, node_budget = 2e7) {
  function(x, y) {
    if (!requireNamespace("randomForest", quietly = TRUE)) {
      stop("the default model of dsg_source_data() needs the 'randomForest' package", call. = FALSE)
    }
    # randomForest reserves 2 n / max(1, nodesize - 4) + 1 nodes per tree for
    # the whole forest before trimming it, which for streams of several
    # hundred thousand points takes gigabytes. Such forests are grown in
    # chunks of trees and combined; the quality is averaged over the chunks.
    nrnodes <- 2 * nrow(x) / max(1, nodesize - 4) + 1
    chunk <- max(1L, min(ntree, as.integer(node_budget %/% nrnodes)))
    sizes <- diff(unique(c(seq(0L, ntree, by = chunk), ntree)))
    y <- factor(y)
    forests <- lapply(sizes, function(size) {
      randomForest::randomForest(x = x, y = y, ntree = size, nodesize = nodesize, importance = TRUE)
    })
    weights <- sizes / sum(sizes)
    oob <- vapply(forests, function(f) 1 - f$err.rate[f$ntree, "OOB"], numeric(1))
    importance <- vapply(forests, function(f) unname(f$importance["block", "MeanDecreaseAccuracy"]), numeric(1))
    forest <- if (length(forests) == 1L) forests[[1L]] else do.call(randomForest::combine, forests)
    rm(forests)
    predict_fn <- function(newx) stats::predict(forest, newx, type = "prob")
    attr(predict_fn, "quality") <- list(
      oob_accuracy = sum(weights * oob),
      block_importance = sum(weights * importance)
    )
    predict_fn
  }
}

#' @title Benchmark source from a real-world stream
#' @description Turns a real-world data stream into a [dsg_source()] whose real
#' and virtual drift are controlled (Hinder et al., 2026). The stream, assumed
#' to be in time order, is split into blocks twice:
#' \itemize{
#' \item `virtual_blocks` time segments give the feature distributions, sampled
#' with replacement from the rows of each segment (virtual drift);
#' \item `real_blocks` time segments are given to a model of
#' \eqn{P(y \mid \tilde{T}, x)}, where \eqn{\tilde{T}} is the segment index; the
#' labels of every concept are sampled from that model with the index of a
#' segment fixed (real drift).
#' }
#' Labelling the data of any feature segment with the model of any label
#' segment separates the virtual drift between segments from the real drift
#' learned by the model, giving `(virtual_blocks + 1) * (real_blocks + 1)`
#' concepts with known drift between them.
#' @param data Data frame with the stream in time order. Every column but
#' `target` must be numeric.
#' @param target Name of the label column.
#' @param real_blocks,virtual_blocks Number of segment boundaries (giving one
#' more segment), or the boundaries themselves as relative positions in (0, 1)
#' or as row indices.
#' @param model Function `model(x, y)` that fits a classifier on the data frame
#' `x` (with the segment index in column `block`) and returns a function
#' giving a matrix of class probabilities (one column per class, named after
#' the classes) for new data. `NULL` uses a random forest from the
#' `randomForest` package (100 trees, `nodesize` as given).
#' @param nodesize Minimum leaf size of the default random forest.
#' @param n_clusters When greater than 0, the real drift is enhanced with
#' region-wise label flips: the data are split into
#' `(n_ignore + 1) * real_blocks * n_clusters` k-means clusters, and in each
#' segment the labels of a segment-specific subset of the clusters are
#' permuted among the classes.
#' @param n_ignore Controls the share of clusters that are never flipped.
#' @param ... Further arguments for [dsg_source()], such as `test_size`.
#' @details With the default model, `obj$quality` holds the out-of-bag
#' accuracy of the forest (how well the labelling model fits the data) and the
#' permutation importance of the segment index (how much real drift the model
#' found).
#' @return `dsg_source` object (also of class `dsg_source_data`)
#' @references Hinder, F., Brinkrolf, J., Lammers, K., and Hammer, B. (2026).
#' Real vs. Virtual Drift: Creating Realistic Stream Learning Benchmarks. In
#' *ESANN 2026*, 133-138.
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 600
#' x1 <- c(stats::rnorm(300), stats::rnorm(300, 2))
#' x2 <- stats::rnorm(n)
#' y <- as.integer(x2 > c(rep(0, 300), rep(1, 300)))
#' stream <- data.frame(x1, x2, y)
#' source <- dsg_source_data(stream, "y", real_blocks = 1, virtual_blocks = 1,
#'                           test_size = 200, n_perm = 200)
#' dsg_stream(source, "-v-r", batch_size = 100)$segments
#' }
#' @export
dsg_source_data <- function(data, target, real_blocks = 5, virtual_blocks = 5, model = NULL,
                            nodesize = 10, n_clusters = 0, n_ignore = 3, ...) {
  data <- as.data.frame(data)
  .check_target_feat(target)
  if (is.null(target) || !(target %in% names(data))) {
    stop("target must name a column of data", call. = FALSE)
  }
  features <- setdiff(names(data), target)
  numeric_columns <- vapply(data[features], function(column) is.numeric(column) || is.logical(column), logical(1))
  if (!all(numeric_columns)) {
    stop(sprintf("the features must be numeric: %s", paste(features[!numeric_columns], collapse = ", ")), call. = FALSE)
  }
  if ("block" %in% features) {
    stop("the data must not have a column named 'block'", call. = FALSE)
  }
  if (!is.null(model) && !is.function(model)) {
    stop("model must be NULL or a function", call. = FALSE)
  }
  .check_positive_integer(n_clusters, "n_clusters", min_value = 0L)
  .check_positive_integer(n_ignore, "n_ignore", min_value = 0L)
  x <- data[features]
  y <- data[[target]]
  if (anyNA(x) || anyNA(y)) {
    stop("data must not have missing values", call. = FALSE)
  }
  n <- nrow(data)

  virtual_boundaries <- .dsg_boundaries(virtual_blocks, n)
  starts <- c(1L, virtual_boundaries + 1L)
  ends <- c(virtual_boundaries, n)
  samplers <- lapply(seq_along(starts), function(i) {
    rows <- starts[i]:ends[i]
    function(size) x[rows[sample.int(length(rows), size, replace = TRUE)], , drop = FALSE]
  })

  real_boundaries <- .dsg_boundaries(real_blocks, n)
  block <- .dsg_block_ids(n, real_boundaries)
  flipped <- NULL
  if (n_clusters > 0) {
    k <- length(real_boundaries)
    flips <- .dsg_add_flips(data.matrix(x), y, block, (n_ignore + 1L) * k * n_clusters, n_ignore * k)
    y <- flips$y
    flipped <- flips$flipped
  }

  fit_model <- if (is.null(model)) .dsg_default_model(nodesize = nodesize) else model
  predict_proba <- fit_model(cbind(block = block, x), y)
  classes <- sort(unique(y))
  labelers <- lapply(seq_len(length(real_boundaries) + 1L), function(r) {
    function(newx) {
      probabilities <- as.matrix(predict_proba(cbind(block = r - 1L, as.data.frame(newx))))
      probabilities <- probabilities[, as.character(classes), drop = FALSE]
      upper <- 1 * upper.tri(diag(length(classes)), diag = TRUE)
      cumulative <- probabilities %*% upper
      draws <- stats::runif(nrow(probabilities))
      classes[pmin(rowSums(cumulative < draws) + 1L, length(classes))]
    }
  })

  obj <- dsg_source(samplers, labelers, ...)
  obj$features <- features
  obj$target <- target
  obj$virtual_boundaries <- virtual_boundaries
  obj$real_boundaries <- real_boundaries
  obj$classes <- classes
  obj$flipped <- flipped
  obj$quality <- attr(predict_proba, "quality")
  class(obj) <- c("dsg_source_data", class(obj))
  return(obj)
}

# ---------------------------------------------------------------------------
# Synthetic sources
# ---------------------------------------------------------------------------

#' @title Benchmark source from a synthetic generator
#' @description Builds a [dsg_source()] from a generator of feature vectors and
#' a list of labelling rules (the real drift states). Virtual drift states are
#' made by regional sub-sampling, as in Hinder et al. (2026): the feature space
#' is split into the Voronoi cells of a k-means clustering, the cells are
#' dealt into `n_variants` groups, and each virtual state keeps only the
#' samples falling in its group.
#' @param generator Function `generator(n)` returning a data frame with `n`
#' feature vectors.
#' @param labelers List of labelling functions, see [dsg_source()].
#' @param n_clusters Approximate number of k-means clusters; rounded up to the
#' next multiple of `n_variants` above `n_clusters`, as in the reference code.
#' @param n_variants Number of virtual drift states.
#' @param init_size Number of samples used to fit the clustering.
#' @param ... Further arguments for [dsg_source()].
#' @return `dsg_source` object
#' @examples
#' set.seed(1)
#' source <- dsg_source_regions(
#'   generator = function(n) data.frame(a = stats::runif(n), b = stats::runif(n)),
#'   labelers = list(function(x) as.integer(x$a > x$b), function(x) as.integer(x$a > 0.5)),
#'   test_size = 200, n_perm = 200
#' )
#' source
#' @export
dsg_source_regions <- function(generator, labelers, n_clusters = 20, n_variants = 2, init_size = 1000, ...) {
  if (!is.function(generator)) {
    stop("generator must be a function", call. = FALSE)
  }
  .check_positive_integer(n_variants, "n_variants", min_value = 1L)
  .check_positive_integer(n_clusters, "n_clusters", min_value = 1L)
  .check_positive_integer(init_size, "init_size", min_value = 2L)
  n_clusters <- (n_clusters %/% n_variants + 1L) * n_variants
  sample_data <- data.matrix(as.data.frame(generator(init_size)))
  centers <- suppressWarnings(stats::kmeans(sample_data, centers = n_clusters, iter.max = 50L))$centers
  nearest <- function(x) {
    x <- data.matrix(as.data.frame(x))
    distances <- outer(rowSums(x^2), rowSums(centers^2), "+") - 2 * x %*% t(centers)
    max.col(-distances, ties.method = "first")
  }
  samplers <- lapply(seq_len(n_variants), function(i) {
    function(n) {
      x <- as.data.frame(generator(max(100L, n)))
      x[((nearest(x) - 1L) %% n_variants) == (i - 1L), , drop = FALSE]
    }
  })
  obj <- dsg_source(samplers, labelers, ...)
  obj$centers <- centers
  obj$n_variants <- n_variants
  class(obj) <- c("dsg_source_regions", class(obj))
  return(obj)
}

#' @title Synthetic benchmark sources
#' @description Synthetic streams commonly used in stream learning, as defined
#' in the `river` library, turned into [dsg_source_regions()] sources: their
#' concept functions give the real drift states, and regional sub-sampling
#' gives the virtual drift states (Hinder et al., 2026).
#' \itemize{
#' \item `dsg_sea()`: three features uniform in \[0, 10\]; label
#' `x1 + x2 > theta` with `theta` in 8, 9, 7 and 9.5 (Street and Kim, 2001).
#' \item `dsg_agrawal()`: the nine features of the loan application generator
#' of Agrawal et al. (1993) and its classification functions 0 to 8.
#' \item `dsg_sine()`: two features uniform in \[0, 1\] and the four SINE
#' functions.
#' \item `dsg_mixed()`: two Boolean (`x1`, `x2`) and two numeric
#' (`x3`, `x4`) features and the two MIXED functions.
#' }
#' @param ... Arguments for [dsg_source_regions()] and [dsg_source()], such as
#' `n_clusters`, `n_variants` or `test_size`.
#' @return `dsg_source` object
#' @references Agrawal, R., Imielinski, T., and Swami, A. (1993). Database
#' mining: a performance perspective. *IEEE Transactions on Knowledge and Data
#' Engineering*, 5(6), 914-925.
#'
#' Street, W. N., and Kim, Y. (2001). A streaming ensemble algorithm (SEA) for
#' large-scale classification. In *KDD 2001*, 377-382.
#'
#' Gama, J., Medas, P., Castillo, G., and Rodrigues, P. (2004). Learning with
#' drift detection. In *SBIA 2004*, 286-295.
#'
#' Montiel, J. et al. (2021). River: machine learning for streaming data in
#' Python. *Journal of Machine Learning Research*, 22(110), 1-8.
#' @examples
#' set.seed(1)
#' dsg_take(dsg_agrawal(), 3)
#' @name dsg_synthetic
NULL

#' @rdname dsg_synthetic
#' @export
dsg_sea <- function(...) {
  generator <- function(n) {
    data.frame(x1 = stats::runif(n, 0, 10), x2 = stats::runif(n, 0, 10), x3 = stats::runif(n, 0, 10))
  }
  labelers <- lapply(c(8, 9, 7, 9.5), function(theta) function(x) as.integer(x$x1 + x$x2 > theta))
  dsg_source_regions(generator, labelers, ...)
}

#' @rdname dsg_synthetic
#' @export
dsg_agrawal <- function(...) {
  generator <- function(n) {
    salary <- 20000 + 130000 * stats::runif(n)
    commission <- ifelse(salary >= 75000, 0, 10000 + 75000 * stats::runif(n))
    zipcode <- sample(0:8, n, replace = TRUE)
    data.frame(
      salary = salary,
      commission = commission,
      age = sample(20:80, n, replace = TRUE),
      elevel = sample(0:4, n, replace = TRUE),
      car = sample(1:20, n, replace = TRUE),
      zipcode = zipcode,
      hvalue = (8 - zipcode) * 100000 * (0.5 + stats::runif(n)),
      hyears = sample(1:30, n, replace = TRUE),
      loan = stats::runif(n) * 500000
    )
  }
  between <- function(v, low, high) (low <= v) & (v <= high)
  by_age <- function(age, young, middle, old) ifelse(age < 40, young, ifelse(age < 60, middle, old))
  # Functions 0 to 8 of river's Agrawal generator, verbatim (including the
  # comparisons of function 4 that use the salary where the loan is expected).
  functions <- list(
    function(x) (x$age < 40) | (60 <= x$age),
    function(x) by_age(x$age, between(x$salary, 50000, 100000), between(x$salary, 75000, 125000), between(x$salary, 25000, 75000)),
    function(x) by_age(x$age, x$elevel %in% 0:1, x$elevel %in% 1:3, x$elevel %in% 2:4),
    function(x) {
      by_age(
        x$age,
        ifelse(x$elevel %in% 0:1, between(x$salary, 25000, 75000), between(x$salary, 50000, 100000)),
        ifelse(x$elevel %in% 1:3, between(x$salary, 50000, 100000), between(x$salary, 75000, 125000)),
        ifelse(x$elevel %in% 2:4, between(x$salary, 50000, 100000), between(x$salary, 25000, 75000))
      )
    },
    function(x) {
      by_age(
        x$age,
        ifelse(between(x$salary, 50000, 100000), between(x$loan, 100000, 300000), between(x$salary, 200000, 400000)),
        ifelse(between(x$salary, 75000, 125000), (200000 <= x$salary) & (x$loan <= 400000), between(x$salary, 300000, 500000)),
        ifelse(between(x$salary, 25000, 75000), between(x$loan, 300000, 500000), between(x$loan, 75000, 300000))
      )
    },
    function(x) {
      total <- x$salary + x$commission
      by_age(x$age, between(total, 50000, 100000), between(total, 75000, 125000), between(total, 25000, 75000))
    },
    function(x) !(2 * (x$salary + x$commission) / 3 - x$loan / 5 - 20000 > 1),
    function(x) !(2 * (x$salary + x$commission) / 3 - 5000 * x$elevel - 20000 > 1),
    function(x) !(2 * (x$salary + x$commission) / 3 - 5000 * x$elevel - x$loan / 5 - 10000 > 1)
  )
  labelers <- lapply(functions, function(f) function(x) as.integer(f(x)))
  dsg_source_regions(generator, labelers, ...)
}

#' @rdname dsg_synthetic
#' @export
dsg_sine <- function(...) {
  generator <- function(n) data.frame(x1 = stats::runif(n), x2 = stats::runif(n))
  labelers <- list(
    function(x) as.integer(!(x$x1 >= sin(x$x2))),
    function(x) as.integer(!(x$x1 < sin(x$x2))),
    function(x) as.integer(!(x$x1 >= 0.5 + 0.3 * sin(3 * pi * x$x2))),
    function(x) as.integer(!(x$x1 < 0.5 + 0.3 * sin(3 * pi * x$x2)))
  )
  dsg_source_regions(generator, labelers, ...)
}

#' @rdname dsg_synthetic
#' @export
dsg_mixed <- function(...) {
  generator <- function(n) {
    data.frame(
      x1 = as.integer(stats::runif(n) >= 0.5), x2 = as.integer(stats::runif(n) >= 0.5),
      x3 = stats::runif(n), x4 = stats::runif(n)
    )
  }
  rule <- function(x) {
    v <- x$x1 == 1
    w <- x$x2 == 1
    z <- x$x4 < 0.5 + 0.3 * sin(3 * pi * x$x3)
    (v & w) | (v & z) | (w & z)
  }
  labelers <- list(
    function(x) as.integer(!rule(x)),
    function(x) as.integer(rule(x))
  )
  dsg_source_regions(generator, labelers, ...)
}
