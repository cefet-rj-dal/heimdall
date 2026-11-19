# Port of the evaluation of the WWAggr reference code to R
# (src/metrics/metrics_utils.py and evaluation_pipeline of
# src/metrics/evaluation_pipelines.py), for scores computed beforehand.
#
# For every threshold, the predicted change of a sequence is its first step
# with score > threshold (none: -1) and the true change its first step whose
# label differs from the first label of the batch (0). A detection at or after
# the true change is a TP, before it (or in a sequence without change) a FP, no
# detection in a sequence with change a FN, and none in one without a TN. With
# a margin m, a TP needs |detection - change| <= m. Time to a false alarm,
# detection delay and covering follow calculate_errors and cover_single. AUDC
# is the area under the detection curve (np.trapz of the delays over the times
# to false alarm, in the order of the thresholds). The reported time to FA,
# delay and covering are those of the first threshold with the largest F1; the
# F1 with margin m is its maximum over the thresholds.

wwaggr_thresholds <- function(number = 300, distance = FALSE) {
  if (distance) return(seq(0, 1, length.out = number))
  c(-0.001, 1 / (1 + exp(-seq(-5, 5, length.out = number))), 1.001)
}

first_change <- function(mask) {
  index <- max.col(mask * 1, ties.method = "first") - 1L
  index[rowSums(mask) == 0] <- -1L
  index
}

cover_one <- function(real, pred, len) {
  parts <- function(cp) if (cp <= 0 || cp >= len) list(0:(len - 1L)) else list(0:(cp - 1L), cp:(len - 1L))
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

# Covering of (real, pred) pairs, cached by pair (the same pairs recur over thresholds).
cover_many <- function(real, pred, len) {
  key <- paste(real, pred)
  unique_keys <- !duplicated(key)
  values <- mapply(cover_one, real[unique_keys], pred[unique_keys], MoreArgs = list(len = len))
  values[match(key, key[unique_keys])]
}

wwaggr_metrics <- function(prob, labels, thresholds, margins = NULL) {
  # Steps without score (NA, representation models) are cropped with their labels.
  keep <- colSums(is.na(prob)) == 0
  prob <- prob[, keep, drop = FALSE]
  labels <- labels[, keep, drop = FALSE]
  len <- ncol(prob)
  real <- first_change(labels != labels[1, 1])
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
    f1m <- vapply(margins, function(m) {
      tpm <- (abs(real - pred) <= m) & (real != -1) & (pred != -1)
      fpm <- ((abs(real - pred) > m) & (real != -1) & (pred != -1)) | ((pred != -1) & (real == -1))
      2 * sum(tpm) / (2 * sum(tpm) + sum(fn) + sum(fpm))
    }, numeric(1))
    row <- data.frame(threshold = th, TN = sum(tn), FP = sum(fp), FN = sum(fn), TP = sum(tp),
                      time_to_fa = mean(fp_delay), delay = mean(delay), cover = mean(cover_many(real, pred, len)),
                      f1 = 2 * sum(tp) / (2 * sum(tp) + sum(fn) + sum(fp)))
    if (length(margins)) row[paste0("f1_m", margins)] <- as.list(f1m)
    row
  })
  curve <- do.call(rbind, rows)
  x <- curve$time_to_fa
  y <- curve$delay
  audc <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
  best <- which.max(curve$f1)
  summary <- data.frame(audc = audc, time_to_fa = curve$time_to_fa[best], delay = curve$delay[best],
                        f1 = curve$f1[best], cover = curve$cover[best], max_cover = max(curve$cover),
                        threshold = curve$threshold[best])
  for (m in margins) summary[[paste0("f1_m", m)]] <- max(curve[[paste0("f1_m", m)]])
  list(summary = summary, curve = curve)
}
