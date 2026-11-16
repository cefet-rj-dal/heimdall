# Validation of mmqt() against the experiments of Stucchi et al. (2023),
# "Multimodal batch-wise change detection", IEEE TNNLS 34(10).
#
# Run from the package root, inside the devcontainer, after prepare_data.py:
#   OPENBLAS_NUM_THREADS=1 Rscript development/validation/mmqt/run_mmqt.R [cores] [parts]
# parts: comma-separated subset of synthetic,calibration,cwru (default: all).
#
# A. Synthetic Gaussian modalities (Section V-E; Figs. 5-6, Table III):
#    Nm = 64 training batches of nu = 128 per modality, K = 4d bins,
#    alpha = alpha' = 5%; MMQT (estimated M, calibrated), MMQT without
#    calibration and a single QuantTree on the same histogram.
# B. Calibration (Table IV): M = 2 modalities in R^8 at sKL 1, 2, 4 and 8,
#    K = 32, nu = 128, Nm from 4 to 64, M known; FPR with and without
#    calibration.
# C. CWRU bearing data (Section V-F; Table VI, top): STFT features (d = 8),
#    nu = 32, K = 16, target FPR 0.5%, 1%, 2% and 5%; Single Speed and
#    Multi Speed scenarios.

devtools::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args) > 0) as.integer(args[1]) else 6L
parts <- if (length(args) > 1) strsplit(args[2], ",")[[1]] else c("synthetic", "calibration", "cwru")
data_dir <- file.path("data-real", "mmqt", "prepared")
out_dir <- file.path("development", "validation", "results", "mmqt")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
n_sim <- 10000

draw <- function(gaussian, n) {
  mean <- unlist(gaussian$mean)
  cov <- matrix(unlist(gaussian$cov), length(mean), byrow = TRUE)
  z <- matrix(stats::rnorm(n * length(mean)), n)
  sweep(z %*% chol(cov), 2, mean, "+")
}
batches_of <- function(gaussian, n_batches, nu) {
  points <- draw(gaussian, n_batches * nu)
  lapply(seq_len(n_batches), function(b) points[(b - 1L) * nu + seq_len(nu), , drop = FALSE])
}
auc <- function(negative, positive) {
  ranks <- rank(c(negative, positive))
  (sum(ranks[length(negative) + seq_along(positive)]) - length(positive) * (length(positive) + 1) / 2) /
    (length(negative) * length(positive))
}
# Thresholds of a trained model for another target FPR.
with_alpha <- function(model, alpha) {
  model$thresholds <- vapply(seq_len(model$n_modalities), function(j) {
    heimdall:::.qt_threshold(model$reference[j, ], model$nu * model$n_batches[j], model$nu, alpha, n_sim)
  }, numeric(1))
  model$alpha <- alpha
  model
}
read_json <- function(file) jsonlite::fromJSON(file.path(data_dir, file), simplifyVector = FALSE)

# A. Synthetic -------------------------------------------------------------------------
if ("synthetic" %in% parts) {
  configs <- read_json("synthetic.json")
  nu <- 128
  n_train <- 64
  n_test <- 500
  run_synthetic <- function(config) {
    set.seed(config$run + 7919 * config$M + 104729 * config$dim)
    m <- config$M
    bins <- 4 * config$dim
    training <- unlist(lapply(config$stationary, batches_of, n_batches = n_train, nu = nu), recursive = FALSE)
    started <- Sys.time()
    model <- mmqt(training, bins = bins, alpha = 0.05, alpha_modalities = 0.05, n_repeats = 10, n_sim = n_sim)
    seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    uncalibrated <- mmqt(training, bins = bins, n_modalities = model$n_modalities, calibration = FALSE,
                         partition = model$partition, n_sim = n_sim)
    single <- mmqt(training, bins = bins, n_modalities = 1, calibration = FALSE, partition = model$partition,
                   n_sim = n_sim)
    stationary <- unlist(lapply(config$stationary, batches_of, n_batches = n_test, nu = nu), recursive = FALSE)
    change <- unlist(lapply(config$change, batches_of, n_batches = n_test, nu = nu), recursive = FALSE)
    source <- rep(seq_len(m), each = n_test)
    rows <- lapply(list(MMQT = model, `MMQT (w/o cal.)` = uncalibrated, QuantTree = single), function(fitted) {
      before <- mmqt_test(fitted, stationary)
      after <- mmqt_test(fitted, change)
      per_modality <- tapply(before$drift, source, mean)
      data.frame(fpr = mean(before$drift), auc = auc(before$statistic, after$statistic),
                 tpr = mean(after$drift), fpr_min = min(per_modality), fpr_max = max(per_modality),
                 fpr_modalities = paste(sprintf("%.4f", per_modality), collapse = " "))
    })
    cbind(dim = config$dim, M = m, run = config$run, method = names(rows), do.call(rbind, rows),
          M_hat = model$n_modalities, delta_M = model$n_modalities - m, seconds = seconds)
  }
  synthetic <- do.call(rbind, parallel::mclapply(configs, run_synthetic, mc.cores = cores, mc.preschedule = FALSE))
  utils::write.csv(synthetic, file.path(out_dir, "synthetic_runs.csv"), row.names = FALSE)
  summary_synthetic <- stats::aggregate(cbind(fpr, auc, tpr, delta_M, M_hat) ~ dim + M + method, synthetic, mean)
  summary_synthetic <- summary_synthetic[order(summary_synthetic$dim, summary_synthetic$method, summary_synthetic$M), ]
  utils::write.csv(summary_synthetic, file.path(out_dir, "synthetic_summary.csv"), row.names = FALSE)
  estimation <- as.data.frame(table(dim = synthetic$dim[synthetic$method == "MMQT"],
                                    M = synthetic$M[synthetic$method == "MMQT"],
                                    delta_M = synthetic$delta_M[synthetic$method == "MMQT"]))
  estimation <- estimation[estimation$Freq > 0, ]
  utils::write.csv(estimation, file.path(out_dir, "synthetic_estimation.csv"), row.names = FALSE)
  cat("A. Synthetic data: mean over runs\n")
  print(format(summary_synthetic, digits = 3), row.names = FALSE)
  cat("\nEstimated number of modalities (delta M = M_hat - M)\n")
  print(estimation, row.names = FALSE)
  table3 <- synthetic[(synthetic$dim == 8) & (synthetic$M == 4), ]
  cat("\nTable III (d = 8, M = 4): FPR per modality, first run\n")
  print(table3[table3$run == 0, c("method", "fpr_modalities", "fpr")], row.names = FALSE)
  cat("Mean over runs of the smallest and largest modality FPR\n")
  print(stats::aggregate(cbind(fpr_min, fpr_max, fpr) ~ method, table3, mean), row.names = FALSE)
}

# B. Calibration -------------------------------------------------------------------
if ("calibration" %in% parts) {
  configs <- read_json("calibration.json")
  nu <- 128
  n_test <- 1000
  sizes <- c(4, 8, 16, 32, 64)
  run_calibration <- function(config) {
    do.call(rbind, lapply(sizes, function(n_m) {
      set.seed(config$run + 31 * n_m + 1009 * config$skl)
      training <- unlist(lapply(config$stationary, batches_of, n_batches = n_m, nu = nu), recursive = FALSE)
      calibrated <- mmqt(training, bins = 32, n_modalities = 2, n_sim = n_sim)
      uncalibrated <- mmqt(training, bins = 32, n_modalities = 2, calibration = FALSE,
                           partition = calibrated$partition, n_sim = n_sim)
      test <- unlist(lapply(config$stationary, batches_of, n_batches = n_test, nu = nu), recursive = FALSE)
      data.frame(skl = config$skl, run = config$run, N_m = n_m,
                 calibrated = mean(mmqt_test(calibrated, test)$drift),
                 uncalibrated = mean(mmqt_test(uncalibrated, test)$drift),
                 change_norm = sqrt(sum((calibrated$reference - uncalibrated$reference)^2) / 2))
    }))
  }
  calibration <- do.call(rbind, parallel::mclapply(configs, run_calibration, mc.cores = cores, mc.preschedule = FALSE))
  utils::write.csv(calibration, file.path(out_dir, "calibration_runs.csv"), row.names = FALSE)
  table4 <- stats::aggregate(cbind(calibrated, uncalibrated, change_norm) ~ N_m + skl, calibration, mean)
  table4$difference <- table4$uncalibrated - table4$calibrated
  utils::write.csv(table4, file.path(out_dir, "calibration_summary.csv"), row.names = FALSE)
  cat("\nB. Table IV: FPR with and without calibration (target 5%)\n")
  print(format(table4, digits = 3), row.names = FALSE)
}

# C. CWRU ----------------------------------------------------------------------------
if ("cwru" %in% parts) {
  nu <- 32
  bins <- 16
  alphas <- c(0.005, 0.01, 0.02, 0.05)
  normal_files <- c(`1` = "97", `2` = "98", `3` = "99")
  faults <- data.frame(type = rep(c("IR", "B", "OR"), each = 9), size = rep(rep(c(7, 14, 21), each = 3), 3),
                       speed = rep(1:3, 9),
                       file = c(105:107, 169:171, 209:211, 118:120, 185:187, 222:224, 130:132, 197:199, 234:236))
  read_batches <- function(file) {
    points <- as.matrix(utils::read.csv(file.path(data_dir, "cwru", paste0(file, ".csv")), header = FALSE))
    n <- nrow(points) %/% nu
    lapply(seq_len(n), function(b) points[(b - 1L) * nu + seq_len(nu), , drop = FALSE])
  }
  normal <- lapply(normal_files, read_batches)
  faulty <- lapply(faults$file, read_batches)
  n_train <- 100
  n_test <- 150
  run_cwru <- function(run, scenario) {
    set.seed(run + if (scenario == "Multi Speed") 5000 else 0)
    speeds <- if (scenario == "Multi Speed") 1:3 else sample(1:3, 1)
    candidates <- which(faults$speed %in% speeds)
    fault <- candidates[sample.int(length(candidates), 1L)]
    fault_rows <- which((faults$type == faults$type[fault]) & (faults$size == faults$size[fault]) &
                          (faults$speed %in% speeds))
    training <- list()
    test <- list()
    for (s in speeds) {
      order_batches <- sample(length(normal[[s]]))
      training <- c(training, normal[[s]][order_batches[seq_len(n_train)]])
      test <- c(test, normal[[s]][order_batches[n_train + seq_len(n_test)]])
    }
    change <- unlist(lapply(fault_rows, function(f) faulty[[f]][sample(length(faulty[[f]]), n_test)]), recursive = FALSE)
    model <- mmqt(training, bins = bins, alpha = 0.05, alpha_modalities = 0.05, n_repeats = 10, n_sim = n_sim)
    fitted <- list(MMQT = model,
                   `MMQT (w/o cal.)` = mmqt(training, bins = bins, n_modalities = model$n_modalities,
                                            calibration = FALSE, partition = model$partition, n_sim = n_sim),
                   QuantTree = mmqt(training, bins = bins, n_modalities = 1, calibration = FALSE,
                                    partition = model$partition, n_sim = n_sim))
    do.call(rbind, lapply(names(fitted), function(method) {
      before <- mmqt_test(fitted[[method]], test)
      after <- mmqt_test(fitted[[method]], change)
      fpr <- vapply(alphas, function(alpha) mean(mmqt_test(with_alpha(fitted[[method]], alpha), test)$drift),
                    numeric(1))
      data.frame(scenario = scenario, run = run, method = method, fault = paste0(faults$type[fault], faults$size[fault]),
                 auc = auc(before$statistic, after$statistic), fpr_0.5 = fpr[1], fpr_1 = fpr[2], fpr_2 = fpr[3],
                 fpr_5 = fpr[4], M_hat = model$n_modalities, delta_M = model$n_modalities - length(speeds))
    }))
  }
  jobs <- expand.grid(run = 1:100, scenario = c("Single Speed", "Multi Speed"), stringsAsFactors = FALSE)
  cwru <- do.call(rbind, parallel::mclapply(seq_len(nrow(jobs)), function(j) run_cwru(jobs$run[j], jobs$scenario[j]),
                                            mc.cores = cores, mc.preschedule = FALSE))
  utils::write.csv(cwru, file.path(out_dir, "cwru_runs.csv"), row.names = FALSE)
  table6 <- stats::aggregate(cbind(auc, fpr_0.5, fpr_1, fpr_2, fpr_5, delta_M) ~ scenario + method, cwru, mean)
  utils::write.csv(table6, file.path(out_dir, "cwru_summary.csv"), row.names = FALSE)
  cat("\nC. CWRU (Table VI, top): mean over 100 changes\n")
  print(format(table6, digits = 3), row.names = FALSE)
}
