# Evaluation of the WWAggr ensembles on the Human Activity data, for the
# members trained with heimdall (har_member.R) and with the reference code
# (har_reference.py), following scripts/evaluate_ensemble.py of the reference:
#   * single models: every member alone (uncalibrated), 300 thresholds sigmoid(linspace(-5, 5)) plus -0.001 and
#     1.001;
#   * mean, min, max, median of the members: the same thresholds;
#   * WWAggr (1-Wasserstein), anchors "start" and "prev", windows 1, 2, 3 (configs/human_activity_*.yaml),
#     300 thresholds linspace(0, 1); for each anchor the window with the best F1;
#   * with and without beta calibration (abm, fitted on the calibration split of the reference).
# Other uses of the method (calibrated members): the MMD (rbf) and the 2-Wasserstein distances, the combined
# anchor, mixed ensembles (5 BCE + 5 InDiD, 5 TS-CP2 + 5 SN-TS2Vec), bootstrap BCE ensembles, few thresholds
# (Figure 2 of the paper: best F1 over linspace(0, 1, n) for n = 3, 5, 9) and the fixed threshold 0.5, and the
# expected calibration error (ECE) before and after beta and temperature calibration (heimdall members).
#   Rscript development/validation/wwaggr/evaluate_har.R <har.rds> <work dir> <out dir> [cores]
suppressMessages(devtools::load_all(quiet = TRUE))
source("development/validation/wwaggr/metrics.R")
args <- commandArgs(trailingOnly = TRUE)
har <- readRDS(args[1])
work <- args[2]
out_dir <- args[3]
cores <- if (length(args) > 3) as.integer(args[4]) else 4L
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
labels <- har$test$labels
len <- ncol(labels)
n_test <- nrow(labels)
sigmoid_thresholds <- wwaggr_thresholds(300)
distance_thresholds <- wwaggr_thresholds(300, distance = TRUE)
margins <- c(1, 2, 4)

# Scores [member, sequence, step] of an ensemble; the representation models have no score for the first steps.
member_scores <- function(source, model, ensemble, calibrated, members = 0:9) {
  arrays <- lapply(members, function(s) {
    if (source == "heimdall") {
      out <- readRDS(file.path(work, "members_r", sprintf("%s_%d_%d.rds", model, ensemble, s)))
      if (calibrated) out$test_calibrated else out$test
    } else {
      prefix <- file.path(work, "members", sprintf("%s_%d_%d", model, ensemble, s))
      shape <- jsonlite::fromJSON(paste0(prefix, ".json"))$shape
      v <- readBin(paste0(prefix, if (calibrated) "_test_calibrated.bin" else "_test.bin"), "double", prod(shape), 8,
                   endian = "little")
      scores <- matrix(NA_real_, n_test, len)
      scores[, (len - shape[2] + 1):len] <- matrix(v, shape[1], shape[2], byrow = TRUE)
      scores
    }
  })
  aperm(array(unlist(arrays), c(n_test, len, length(arrays))), c(3, 1, 2))
}

summary_row <- function(scores, thresholds) wwaggr_metrics(scores, labels, thresholds, margins)$summary

evaluate_ensemble <- function(scores) {
  rows <- list()
  for (method in c("mean", "min", "max", "median")) {
    rows[[method]] <- cbind(aggregation = method, params = "", summary_row(wwaggr(scores, method = method),
                                                                          sigmoid_thresholds))
  }
  for (anchor in c("start", "prev")) {
    candidates <- lapply(1:3, function(w) summary_row(wwaggr(scores, window_size = w, anchor = anchor),
                                                      distance_thresholds))
    best <- which.max(vapply(candidates, function(r) r$f1, numeric(1)))
    rows[[anchor]] <- cbind(aggregation = paste0("wwaggr_", anchor), params = paste0("window ", best),
                            candidates[[best]])
  }
  do.call(rbind, rows)
}

sources <- c("heimdall", "reference")
models <- c("bce", "indid", "tscp", "ts2vec", "bce_boot")
jobs <- expand.grid(source = sources, model = models, ensemble = 1:3, calibrated = c(FALSE, TRUE),
                    stringsAsFactors = FALSE)
start <- Sys.time()
main <- parallel::mclapply(seq_len(nrow(jobs)), function(j) {
  job <- jobs[j, ]
  scores <- member_scores(job$source, job$model, job$ensemble, job$calibrated)
  rows <- evaluate_ensemble(scores)
  if (!job$calibrated) {
    singles <- do.call(rbind, lapply(seq_len(dim(scores)[1]), function(k) summary_row(scores[k, , ], sigmoid_thresholds)))
    rows <- rbind(cbind(aggregation = "single", params = paste0("member ", seq_len(nrow(singles)) - 1L), singles), rows)
  }
  cbind(job[rep(1, nrow(rows)), ], rows, row.names = NULL)
}, mc.cores = cores, mc.preschedule = FALSE)
main <- do.call(rbind, main)
utils::write.csv(main, file.path(out_dir, "har_runs.csv"), row.names = FALSE)

# Other uses of the method, calibrated members.
# Mixed ensembles: members 0-4 of two ensembles of the same index.
abind_members <- function(source, models, ensemble) {
  parts <- lapply(models, function(m) member_scores(source, m, ensemble, TRUE, members = 0:4))
  scores <- array(NA_real_, c(10, n_test, len))
  scores[1:5, , ] <- parts[[1]]
  scores[6:10, , ] <- parts[[2]]
  scores
}
others <- expand.grid(source = sources, ensemble = 1:3, stringsAsFactors = FALSE)
alternatives <- parallel::mclapply(seq_len(nrow(others)), function(j) {
  job <- others[j, ]
  rows <- list()
  add <- function(model, label, params, row) {
    rows[[length(rows) + 1L]] <<- cbind(source = job$source, model = model, ensemble = job$ensemble,
                                        variant = label, params = params, row)
  }
  for (model in c("bce", "indid", "tscp", "ts2vec")) {
    scores <- member_scores(job$source, model, job$ensemble, TRUE)
    for (w in 1:3) {
      add(model, "wasserstein p=2, prev", paste0("window ", w),
          summary_row(wwaggr(scores, window_size = w, p = 2), distance_thresholds))
      mmd <- wwaggr(scores, method = "mmd", window_size = w)
      add(model, "mmd rbf, prev", paste0("window ", w),
          summary_row(mmd, seq(0, max(mmd, na.rm = TRUE), length.out = 300)))
      if (w >= 2) {
        add(model, "wasserstein, combined", paste0("window ", w),
            summary_row(wwaggr(scores, window_size = w, anchor = "combined"), distance_thresholds))
      }
      prev <- wwaggr(scores, window_size = w)
      for (n in c(3, 5, 9)) {
        add(model, sprintf("wasserstein prev, %d thresholds", n), paste0("window ", w),
            summary_row(prev, seq(0, 1, length.out = n)))
      }
      add(model, "wasserstein prev, threshold 0.5", paste0("window ", w), summary_row(prev, 0.5))
      uncal <- wwaggr(member_scores(job$source, model, job$ensemble, FALSE), window_size = w)
      add(model, "uncalibrated wasserstein prev, threshold 0.5", paste0("window ", w), summary_row(uncal, 0.5))
      for (n in c(3, 5, 9)) {
        add(model, sprintf("uncalibrated wasserstein prev, %d thresholds", n), paste0("window ", w),
            summary_row(uncal, seq(0, 1, length.out = n)))
      }
    }
  }
  for (mix in list(c("bce", "indid"), c("tscp", "ts2vec"))) {
    scores <- abind_members(job$source, mix, job$ensemble)
    label <- paste(mix, collapse = "+")
    for (method in c("mean", "min", "max", "median")) {
      add(label, method, "", summary_row(wwaggr(scores, method = method), sigmoid_thresholds))
    }
    for (anchor in c("start", "prev")) {
      for (w in 1:3) {
        add(label, paste0("wwaggr_", anchor), paste0("window ", w),
            summary_row(wwaggr(scores, window_size = w, anchor = anchor), distance_thresholds))
      }
    }
  }
  do.call(rbind, rows)
}, mc.cores = cores, mc.preschedule = FALSE)
utils::write.csv(do.call(rbind, alternatives), file.path(out_dir, "har_alternatives.csv"), row.names = FALSE)

# Expected calibration error of the heimdall members (10 uniform bins; the
# reference's ece() with the pandas column names it assumes), before and after
# beta and temperature calibration on the calibration split.
ece <- function(p, y) {
  bin <- pmin(floor(p * 10), 9)
  sum(vapply(split(seq_along(p), bin), function(i) abs(mean(y[i]) - mean(p[i])) * length(i), numeric(1))) / length(p)
}
val_idx <- readBin(file.path(work, "val_idx.bin"), "double", 10000, 8, endian = "little") + 1
ece_rows <- parallel::mclapply(c("bce", "indid", "tscp", "ts2vec"), function(model) {
  do.call(rbind, lapply(0:9, function(s) {
    out <- readRDS(file.path(work, "members_r", sprintf("%s_1_%d.rds", model, s)))
    temperature <- cpd_calibrate(out$model, har$train$x[val_idx, , , drop = FALSE],
                                 har$train$labels[val_idx, , drop = FALSE], method = "temperature")
    keep <- !is.na(out$test)
    data.frame(model = model, member = s, none = ece(out$test[keep], labels[keep]),
               temperature = ece(cpd_scores(temperature, har$test$x)[keep], labels[keep]),
               beta = ece(out$test_calibrated[keep], labels[keep]), temperature_value = temperature$calibrator$temperature)
  }))
}, mc.cores = cores)
utils::write.csv(do.call(rbind, ece_rows), file.path(out_dir, "har_ece.csv"), row.names = FALSE)

# Summary: mean and standard deviation over the three ensembles.
for (column in c("f1", "audc", "cover")) main[[column]] <- as.numeric(main[[column]])
single <- main[main$aggregation == "single", ]
single_stats <- aggregate(f1 ~ source + model, single, function(v) c(mean = mean(v), sd = stats::sd(v)))
ens <- main[main$aggregation != "single", ]
ens_stats <- aggregate(cbind(f1, audc, cover) ~ source + model + calibrated + aggregation, ens,
                       function(v) c(mean = mean(v), sd = stats::sd(v)))
utils::write.csv(do.call(data.frame, single_stats), file.path(out_dir, "har_single_summary.csv"), row.names = FALSE)
utils::write.csv(do.call(data.frame, ens_stats), file.path(out_dir, "har_summary.csv"), row.names = FALSE)
cat(sprintf("Evaluation: %.0f s\n", as.numeric(difftime(Sys.time(), start, units = "secs"))))
print(do.call(data.frame, ens_stats)[, c("source", "model", "calibrated", "aggregation", "f1.mean", "f1.sd")])
print(do.call(data.frame, single_stats))
cat("HAR_EVALUATION_DONE\n")
