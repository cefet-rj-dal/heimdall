# Validation of dfr_dde() / dde_curve() / dde_score() against the experiments
# of Szűcs and Németh (2025), "Domino drift effect approach for probability
# estimation of feature drift in high-dimensional data" (KAIS 67:4597-4621),
# on the Covertype dataset (top 16 features, two classes, Table 2).
#
# Run from the package root, inside the devcontainer:
#   Rscript development/validation/dde/run_dde.R [window] [folds] [output dir]
#
# Experiments
#   A. Paper setting: drifts artificially induced by exchanging the values of
#      randomly chosen features (Sections 4.2 and 5.2). Expected: a flat DDE
#      lift curve (Fig. 3, Covertype) and, with 7 of the 16 features monitored,
#      recall 0.618, precision 0.522 and F1 0.549 (Table 6).
#   B. Natural drift: Covertype in its original order, whose drift comes from
#      the spatial ordering of the records. Expected: an increasing lift curve,
#      as for the natural drifts of Fig. 3, and DDE better than the baselines
#      in recall and F1 (Tables 7 and 8).
#   C. The online detector, dfr_dde(), on the natural stream with 7 of the 16
#      features monitored.
#
# Drift prediction (Section 5.3): the DDE curve of one drift predicts which
# unmonitored features drift in the next one; the monitored features are
# drawn at random in each fold. Predictions are compared with full
# monitoring (every feature tested with the same test). Baselines (Section
# 5.4): reduced monitoring (unmonitored features never drift) and reduced
# random (unmonitored features drift with the share of drifting monitored
# features).

devtools::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
window <- if (length(args) > 0) as.integer(args[1]) else 1000L
n_folds <- if (length(args) > 1) as.integer(args[2]) else 10L
out_dir <- if (length(args) > 2) args[3] else file.path("development", "validation", "results")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
prefix <- function(name) file.path(out_dir, paste0("dde_", name))
thresholds <- seq(0, 0.9, by = 0.1)
test <- "js" # Jensen-Shannon, the detector of Fig. 3 and of Table 6

# Data ------------------------------------------------------------------------------
path <- file.path("data-real", "dde", "covtype.data.gz")
if (!file.exists(path)) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz",
                       path, mode = "wb", quiet = TRUE)
}
raw <- utils::read.csv(gzfile(path), header = FALSE)
names(raw) <- c("Elevation", "Aspect", "Slope", "Horizontal_Distance_To_Hydrology", "Vertical_Distance_To_Hydrology",
                "Horizontal_Distance_To_Roadways", "Hillshade_9am", "Hillshade_Noon", "Hillshade_3pm",
                "Horizontal_Distance_To_Fire_Points", paste0("Wilderness_Area", 1:4), paste0("Soil_Type", 1:40),
                "Cover_Type")
# Two classes, as in Table 2 (class 2, the most frequent, against the rest).
label <- factor(raw$Cover_Type == 2)
features <- raw[setdiff(names(raw), "Cover_Type")]
# "Top 16 features": the paper does not say how they were ranked; here they
# are the 16 most important features of a random forest on a sample.
set.seed(1)
sample_rows <- sample.int(nrow(features), 20000)
forest <- randomForest::randomForest(features[sample_rows, ], label[sample_rows], ntree = 100, importance = TRUE)
importance <- sort(forest$importance[, "MeanDecreaseAccuracy"], decreasing = TRUE)
top <- names(importance)[1:16]
data <- features[top]
m <- length(top)
cat("Top 16 features:", paste(top, collapse = ", "), "\n")

# Helpers ---------------------------------------------------------------------------
detect <- function(reference, current) {
  result <- dde_feature_drift(reference, current, test = test)
  result$feature[result$drift]
}
metrics <- function(truth, predicted) {
  tp <- sum(truth & predicted)
  recall <- if (sum(truth) > 0) tp / sum(truth) else NA_real_
  precision <- if (sum(predicted) > 0) tp / sum(predicted) else NA_real_
  f1 <- if (sum(truth) + sum(predicted) > 0) 2 * tp / (sum(truth) + sum(predicted)) else NA_real_
  c(recall = recall, precision = precision, f1 = f1)
}
auc <- function(truth, score) {
  if (length(unique(truth)) < 2L) return(NA_real_)
  as.numeric(pROC::auc(pROC::roc(truth, score, quiet = TRUE, direction = "<")))
}

# Predicts drift event `target` from the curve of event `source`, for every
# size of the monitored set and fold.
predict_events <- function(events, pairs, sizes, setting) {
  rows <- list()
  for (p in seq_len(nrow(pairs))) {
    source <- events[[pairs$source[p]]]
    target <- events[[pairs$target[p]]]
    curve <- dde_curve(source$reference, source$drifting, thresholds = thresholds)
    truth <- stats::setNames(top %in% target$drifting, top)
    for (size in sizes) {
      for (fold in seq_len(n_folds)) {
        monitored <- sample(top, size)
        unmonitored <- setdiff(top, monitored)
        detected <- intersect(monitored, target$drifting)
        scores <- dde_score(curve, target$reference, detected, unmonitored)
        share <- length(detected) / size
        predictions <- list(
          DDE = top %in% c(detected, names(scores)[scores >= 0.5]),
          `Reduced monitoring` = top %in% detected,
          `Reduced random` = top %in% c(detected, unmonitored[stats::runif(length(unmonitored)) < share])
        )
        for (method in names(predictions)) {
          rows[[length(rows) + 1L]] <- data.frame(
            setting = setting, pair = p, monitored = size, fold = fold, method = method,
            t(metrics(truth, predictions[[method]])),
            auc = if (method == "DDE" && length(unmonitored) > 0) auc(truth[unmonitored], scores) else NA_real_
          )
        }
      }
    }
  }
  do.call(rbind, rows)
}

lift_curves <- function(events, setting) {
  do.call(rbind, lapply(seq_along(events), function(e) {
    curve <- dde_curve(events[[e]]$reference, events[[e]]$drifting, thresholds = thresholds)
    cbind(setting = setting, event = e, k = length(events[[e]]$drifting), curve)
  }))
}

# The mean lift over drift points is dominated by thresholds with a handful of
# pairs, so the summary also pools the pairs of every drift point: the pooled
# lift is the number of drifting pairs over the number expected if drift did
# not depend on correlation, sum b / sum(B k (k - 1) / (m (m - 1))).
summarise_curves <- function(curves) {
  per_event <- stats::aggregate(cbind(probability, lift) ~ setting + threshold, curves,
                                function(x) c(mean = mean(x), median = stats::median(x), min = min(x), max = max(x)))
  curves$expected <- curves$pairs * curves$k * (curves$k - 1) / (m * (m - 1))
  pooled <- stats::aggregate(cbind(pairs, drifting_pairs, expected) ~ setting + threshold, curves, sum)
  pooled$pooled_lift <- pooled$drifting_pairs / pooled$expected
  pooled$expected <- NULL
  merge(do.call(data.frame, per_event), pooled, by = c("setting", "threshold"))
}

# A. Artificially induced drifts (paper setting) -----------------------------------------
# The stream is shuffled, so it has no natural drift. Each drift exchanges the
# values of 2 to 8 randomly chosen features in the analysis window (a cyclic
# permutation of their columns); the reference window is left as it is.
set.seed(2)
shuffled <- data[sample.int(nrow(data)), ]
n_events <- 30L
events_a <- list()
for (e in seq_len(n_events)) {
  rows <- (e - 1L) * 2L * window + seq_len(2L * window)
  reference <- shuffled[rows[seq_len(window)], ]
  current <- shuffled[rows[window + seq_len(window)], ]
  exchanged <- sample(top, sample(2:8, 1))
  current[exchanged] <- current[c(exchanged[-1], exchanged[1])]
  drifting <- detect(reference, current)
  if (length(drifting) >= 2L) {
    events_a[[length(events_a) + 1L]] <- list(reference = reference, drifting = drifting, exchanged = exchanged)
  }
}
cat(sprintf("A. %d induced drifts with at least two drifting features\n", length(events_a)))

# B. Natural drift (original order) ----------------------------------------------------
# Consecutive, non-overlapping windows; a drift point is a pair of windows in
# which at least two features (and not every one) drift.
starts <- seq(1L, nrow(data) - 2L * window + 1L, by = window)
events_b <- list()
for (s in starts) {
  reference <- data[s - 1L + seq_len(window), ]
  current <- data[s - 1L + window + seq_len(window), ]
  drifting <- detect(reference, current)
  if ((length(drifting) >= 2L) && (length(drifting) < m)) {
    events_b[[length(events_b) + 1L]] <- list(reference = reference, drifting = drifting, start = s)
  }
}
cat(sprintf("B. %d natural drift points out of %d window pairs\n", length(events_b), length(starts)))

# Lift curves (Fig. 3) and prediction of the next drift (Section 5.3, Tables 6-8).
curves <- rbind(lift_curves(events_a, "A. induced drift"), lift_curves(events_b, "B. natural drift"))
utils::write.csv(curves, prefix("lift_curves.csv"), row.names = FALSE)
curve_summary <- summarise_curves(curves)
utils::write.csv(curve_summary, prefix("lift_summary.csv"), row.names = FALSE)
cat("\nDDE lift curves (pooled over drift points, and median per drift point)\n")
print(format(curve_summary[c("setting", "threshold", "pairs", "drifting_pairs", "pooled_lift", "lift.median", "lift.mean")],
             digits = 3), row.names = FALSE)

set.seed(3)
sizes <- c(1, 2, 4, 7, 10, 13, 16)
consecutive <- function(n) data.frame(source = seq_len(n - 1L), target = seq_len(n - 1L) + 1L)
pairs_b <- consecutive(length(events_b))
if (nrow(pairs_b) > 100L) pairs_b <- pairs_b[sort(sample.int(nrow(pairs_b), 100L)), ]
prediction <- rbind(
  predict_events(events_a, consecutive(length(events_a)), sizes, "A. induced drift"),
  predict_events(events_b, pairs_b, sizes, "B. natural drift")
)
utils::write.csv(prediction, prefix("prediction.csv"), row.names = FALSE)
prediction_summary <- stats::aggregate(cbind(recall, precision, f1, auc) ~ setting + method + monitored,
                                       prediction, mean, na.rm = TRUE, na.action = stats::na.pass)
prediction_summary <- prediction_summary[order(prediction_summary$setting, prediction_summary$method,
                                               -prediction_summary$monitored), ]
utils::write.csv(prediction_summary, prefix("prediction_summary.csv"), row.names = FALSE)
cat("\nPrediction of the next drift (mean over drift pairs and folds)\n")
print(format(prediction_summary, digits = 3), row.names = FALSE)

# C. Online detector ----------------------------------------------------------------------
# dfr_dde() on the first 200 000 records in their natural order, with 7 of the
# 16 features monitored. Each prediction is checked against full monitoring of
# the windows where the drift was confirmed.
set.seed(4)
online_rows <- seq_len(min(nrow(data), 200000L))
online <- dfr_dde(n_monitored = 7, test = test, window_size = window, monitoring_step = window %/% 4L, seed = 4)
online <- fit(online, data[online_rows, ])
checks <- do.call(rbind, lapply(online$detections, function(d) {
  if (d$initialization) return(NULL)
  end <- d$observation
  reference <- data[end - 2L * window + seq_len(window), ]
  current <- data[end - window + seq_len(window), ]
  truth <- stats::setNames(top %in% detect(reference, current), top)
  unmonitored <- names(d$scores)
  predicted <- top %in% c(d$drifting, unmonitored[d$scores >= 0.5])
  data.frame(observation = end, t(metrics(truth, predicted)), auc = auc(truth[unmonitored], d$scores))
}))
utils::write.csv(checks, prefix("online.csv"), row.names = FALSE)
cat(sprintf("\nC. dfr_dde on %d records: %d drifts, the first %s the initialization\n",
            length(online_rows), length(online$detections),
            if (length(online$detections) > 0 && online$detections[[1]]$initialization) "being" else "not being"))
cat("Monitored:", paste(online$state$monitored, collapse = ", "), "\n")
if (!is.null(checks)) print(round(colMeans(checks[-1], na.rm = TRUE), 3))
utils::write.csv(online$curve, prefix("online_curve.csv"), row.names = FALSE)

# Figures ------------------------------------------------------------------------------------
grDevices::png(prefix("lift_curves.png"), width = 900, height = 420)
graphics::par(mfrow = c(1, 2))
for (setting in unique(curves$setting)) {
  part <- curves[curves$setting == setting, ]
  summary <- curve_summary[curve_summary$setting == setting, ]
  graphics::plot(NA, xlim = range(thresholds), ylim = c(0, 4), xlab = "Correlation threshold", ylab = "DDE lift",
                 main = paste0(setting, " (", length(unique(part$event)), " drifts)"))
  for (event in unique(part$event)) {
    one <- part[part$event == event, ]
    graphics::lines(one$threshold, one$lift, col = grDevices::adjustcolor("grey40", 0.15))
  }
  graphics::lines(summary$threshold, summary$pooled_lift, col = "steelblue", lwd = 3)
  graphics::lines(summary$threshold, summary$lift.median, col = "darkorange", lwd = 2, lty = 2)
  graphics::abline(h = 1, lty = 3, col = "grey50")
  graphics::legend("topleft", c("pooled lift", "median per drift", "each drift"), col = c("steelblue", "darkorange", "grey70"),
                   lwd = c(3, 2, 1), lty = c(1, 2, 1), bty = "n")
}
grDevices::dev.off()

grDevices::png(prefix("prediction_f1.png"), width = 900, height = 420)
graphics::par(mfrow = c(1, 2))
colors <- c(DDE = "steelblue", `Reduced monitoring` = "darkorange", `Reduced random` = "grey40")
for (setting in unique(prediction_summary$setting)) {
  part <- prediction_summary[prediction_summary$setting == setting, ]
  graphics::plot(NA, xlim = rev(range(sizes)), ylim = c(0, 1), xlab = "Monitored features (of 16)", ylab = "F1",
                 main = setting)
  for (method in names(colors)) {
    rows <- part[part$method == method, ]
    rows <- rows[order(rows$monitored), ]
    graphics::lines(rows$monitored, rows$f1, col = colors[[method]], lwd = 2, type = "b", pch = 19)
  }
  graphics::legend("bottomleft", names(colors), col = colors, lwd = 2, bty = "n")
}
grDevices::dev.off()
