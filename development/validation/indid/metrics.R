# Port of the evaluation of the reference code (utils/metrics.py,
# evaluation_pipeline, and the thresholds of test.py) to R, for probabilities
# of change computed beforehand.
#
# For every threshold, the predicted change of a sequence is its first time
# step with probability > threshold (none: -1), and the true change the first
# step whose label differs from 0. A detection at or after the true change is a
# TP, before it (or in a sequence without change) a FP, no detection in a
# sequence with change a FN, and none in one without a TN. Time to a false
# alarm and detection delay follow calculate_errors; covering follows
# cover_single. AUC is the area under the detection curve (np.trapz of the
# delays over the times to false alarm, in the order of the thresholds); the
# reported F1, time to FA, delay and covering are those of the first threshold
# with the largest F1.

indid_thresholds <- function(number = 100) {
  c(-0.001, 1 / (1 + exp(-seq(-5, 5, length.out = number))), 1.001)
}

first_change <- function(mask) {
  index <- max.col(mask * 1, ties.method = "first") - 1L
  index[rowSums(mask) == 0] <- -1L
  index
}

cover_one <- function(real, pred, len) {
  parts <- function(cp) {
    if (cp <= 0 || cp >= len) list(0:(len - 1L)) else list(0:(cp - 1L), cp:(len - 1L))
  }
  truth <- parts(real)
  predicted <- parts(pred)
  total <- 0
  for (t in truth) {
    total <- total + length(t) * max(vapply(predicted, function(p) {
      length(intersect(t, p)) / length(union(t, p))
    }, numeric(1)))
  }
  total / len
}

indid_metrics <- function(prob, labels, thresholds = indid_thresholds()) {
  len <- ncol(prob)
  real <- first_change(labels != 0)
  rows <- lapply(thresholds, function(th) {
    pred <- first_change(prob > th)
    tn <- (real == pred) & (real == -1)
    fn <- (real != pred) & (pred == -1)
    tp <- (real <= pred) & (real != -1)
    fp <- ((real > pred) & (real != -1) & (pred != -1)) | ((pred != -1) & (real == -1))
    fp_delay <- numeric(length(real))
    delay <- numeric(length(real))
    fp_delay[tn | fn] <- len
    fp_delay[tp] <- real[tp]
    fp_delay[fp] <- pred[fp]
    delay[fn] <- len - real[fn]
    delay[tp] <- pred[tp] - real[tp]
    cover <- mean(mapply(cover_one, real, pred, MoreArgs = list(len = len)))
    data.frame(threshold = th, TN = sum(tn), FP = sum(fp), FN = sum(fn), TP = sum(tp), time_to_fa = mean(fp_delay),
               delay = mean(delay), cover = cover, f1 = 2 * sum(tp) / (2 * sum(tp) + sum(fn) + sum(fp)))
  })
  curve <- do.call(rbind, rows)
  x <- curve$time_to_fa
  y <- curve$delay
  auc <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
  best <- which.max(curve$f1)
  list(summary = data.frame(time_to_fa = curve$time_to_fa[best], delay = curve$delay[best], auc = auc,
                            f1 = curve$f1[best], cover = curve$cover[best], max_cover = max(curve$cover),
                            threshold = curve$threshold[best]),
       curve = curve)
}

read_synthetic <- function(dir, name, part) {
  meta <- jsonlite::fromJSON(file.path(dir, paste0(name, "_", part, ".json")))
  shape <- meta$shape
  read <- function(suffix, count) readBin(file.path(dir, paste0(name, "_", part, suffix)), "double", count, 8,
                                          endian = "little")
  # The files are in C order [sequence, time, feature]; R arrays are column-major.
  x <- aperm(array(read("_x.bin", prod(shape)), rev(shape)), 3:1)
  y <- t(matrix(read("_y.bin", prod(shape[1:2])), shape[2], shape[1]))
  list(x = x, labels = y)
}
