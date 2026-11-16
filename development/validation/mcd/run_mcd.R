# Evaluation of dfr_mcd() on the synthetic data sets of Wan, Liang and Yoon
# (2024), "Online drift detection with maximum concept discrepancy", KDD 2024
# (Table 2), and of the reference code on the same data with the same
# evaluation.
#
# Run from the package root, inside the devcontainer, after run_reference.py
# (see README.md):
#   OPENBLAS_NUM_THREADS=1 Rscript development/validation/mcd/run_mcd.R [cores] [reuse]
#
# Data: the seven synthetic streams of the reference repository (data/toy_data),
# converted to CSV in data-real/mcdd/csv. Settings of configs/para_simulated_data.yaml:
# W = 3000, 10 sub-windows, m = 30, k = 10, encoder 100-100, 20 runs.
#
# Evaluation (Section 6.1.1; the code is not in the repository): every tested
# sub-window (300 observations) is "Drift" when a drift starts in it, or, for the
# incremental ramps of GM_Inc, when it overlaps a ramp; Precision, F1 and MCC
# of the detections over the tested sub-windows, mean and sd over the runs.
# The change points are those of Appendix A.1.1, checked on the data.

suppressMessages(library(daltoolbox))
devtools::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args) > 0) as.integer(args[1]) else 6L
out_dir <- file.path("development", "validation", "results", "mcd")
read_data <- function(name) utils::read.csv(file.path("data-real", "mcdd", "csv", paste0(name, ".csv")), header = FALSE)

# First observation (1-based) of every new concept; ramps as intervals.
truth <- list(
  GM_Sud = list(points = 21001),
  GM_Rec = list(points = c(15001, 25001)),
  GM_Grad = list(points = c(10001, 11001, 12001, 15001, 18001, 21001, 24001)),
  GM_Inc = list(ramps = rbind(c(12001, 12600), c(18001, 19200), c(24001, 25200))),
  GamLog_Sud = list(points = 21001),
  LogGamWei_Sud = list(points = c(15001, 24001)),
  GamGM_SudGrad = list(points = c(11001, 12001, 15001, 18001, 21001, 24001))
)
paper <- data.frame(dataset = names(truth),
                    precision = c(1.00, 1.00, 1.00, 0.99, 1.00, 0.98, 0.98),
                    f1 = c(1.00, 0.79, 0.79, 0.44, 1.00, 0.94, 0.99),
                    mcc = c(1.00, 0.81, 0.79, 0.51, 1.00, 0.95, 0.99))
slide <- 300

# Check of the change points on the data: share of the narrow Gaussian
# component (GM data sets) or mean of the features, by blocks of 1000.
cat("Change points: profile of the data by blocks of 1000 observations\n")
for (name in names(truth)) {
  x <- as.matrix(read_data(name))
  block <- (seq_len(nrow(x)) - 1) %/% 1000
  profile <- if (startsWith(name, "GM") || startsWith(name, "GamGM")) {
    tapply(rowSums((x - 20)^2) / ncol(x) < 900, block, mean)
  } else {
    tapply(rowMeans(x), block, mean)
  }
  cat(sprintf("%-14s %s\n", name, paste(sprintf("%.2f", profile), collapse = " ")))
}

label <- function(name, observation) {
  start <- observation - slide + 1
  t <- truth[[name]]
  drift <- rep(FALSE, length(observation))
  for (p in t$points) drift <- drift | (start <= p & p <= observation)
  if (!is.null(t$ramps)) {
    for (r in seq_len(nrow(t$ramps))) drift <- drift | (start <= t$ramps[r, 2] & t$ramps[r, 1] <= observation)
  }
  drift
}
scores <- function(drift, detected) {
  tp <- sum(drift & detected)
  fp <- sum(!drift & detected)
  fn <- sum(drift & !detected)
  tn <- sum(!drift & !detected)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  recall <- tp / (tp + fn)
  f1 <- if ((precision + recall) > 0) 2 * precision * recall / (precision + recall) else 0
  denominator <- sqrt(as.numeric(tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc <- if (denominator > 0) (tp * tn - fp * fn) / denominator else 0
  c(precision = precision, f1 = f1, mcc = mcc, detections = tp + fp, true_detections = tp)
}

# dfr_mcd: 20 runs per data set (reused from dfr_mcd_runs.csv with the argument "reuse")
reuse <- (length(args) > 1) && (args[2] == "reuse") && file.exists(file.path(out_dir, "dfr_mcd_runs.csv"))
jobs <- expand.grid(run = 1:20, dataset = names(truth), stringsAsFactors = FALSE)
r_runs <- if (reuse) list(utils::read.csv(file.path(out_dir, "dfr_mcd_runs.csv"))) else parallel::mclapply(split(jobs, seq_len(nrow(jobs))), function(job) {
  data <- read_data(job$dataset)
  started <- Sys.time()
  model <- fit(dfr_mcd(window_size = 3000, m = 30, seed = job$run), data)
  output <- model$drifter_output
  tested <- which(!is.na(output$mcd))
  data.frame(dataset = job$dataset, run = job$run, observation = tested, mcd = output$mcd[tested],
             threshold = output$threshold[tested],
             seconds = as.numeric(difftime(Sys.time(), started, units = "secs")))
}, mc.cores = cores, mc.preschedule = FALSE)
r_runs <- do.call(rbind, r_runs)
if (!reuse) utils::write.csv(r_runs, file.path(out_dir, "dfr_mcd_runs.csv"), row.names = FALSE)

read_reference <- function(suffix) do.call(rbind, lapply(names(truth), function(name) {
  path <- file.path(out_dir, "reference", paste0(name, suffix, ".csv"))
  if (!file.exists(path)) return(NULL)
  cbind(dataset = name, utils::read.csv(path))
}))
reference_runs <- read_reference("")
# Reference in float64 (only for the data sets run that way, see README.md).
reference64_runs <- read_reference("_float64")

evaluate <- function(runs, implementation) {
  per_run <- do.call(rbind, lapply(split(runs, list(runs$dataset, runs$run), drop = TRUE), function(p) {
    s <- scores(label(p$dataset[1], p$observation), p$mcd > p$threshold)
    data.frame(implementation = implementation, dataset = p$dataset[1], run = p$run[1], t(s))
  }))
  per_run
}
per_run <- rbind(evaluate(r_runs, "dfr_mcd"), if (!is.null(reference_runs)) evaluate(reference_runs, "reference"),
                 if (!is.null(reference64_runs)) evaluate(reference64_runs, "reference_float64"))
utils::write.csv(per_run, file.path(out_dir, "table2_runs.csv"), row.names = FALSE)
summary <- do.call(rbind, lapply(split(per_run, list(per_run$implementation, per_run$dataset), drop = TRUE), function(p) {
  data.frame(implementation = p$implementation[1], dataset = p$dataset[1],
             precision = mean(p$precision), precision_sd = stats::sd(p$precision), f1 = mean(p$f1),
             f1_sd = stats::sd(p$f1), mcc = mean(p$mcc), mcc_sd = stats::sd(p$mcc),
             detections = mean(p$detections), true_detections = mean(p$true_detections))
}))
summary <- summary[order(match(summary$dataset, names(truth)), summary$implementation), ]
paper_rows <- cbind(implementation = "paper", paper, precision_sd = NA, f1_sd = NA, mcc_sd = NA, detections = NA,
                    true_detections = NA)[names(summary)]
table2 <- rbind(summary, paper_rows)
table2 <- table2[order(match(table2$dataset, names(truth)), match(table2$implementation,
                                                                  c("dfr_mcd", "reference", "reference_float64", "paper"))), ]
utils::write.csv(table2, file.path(out_dir, "table2.csv"), row.names = FALSE)
cat("\nTable 2 (MCD-DD): mean (sd) over 20 runs\n")
print(format(table2, digits = 2), row.names = FALSE)
labels <- vapply(names(truth), function(name) sum(label(name, seq(3300, 30000, by = slide))), numeric(1))
cat("\nDrift sub-windows per data set:", paste(names(labels), labels, sep = " = ", collapse = ", "), "\n")
cat("Seconds per run of dfr_mcd:", round(mean(tapply(r_runs$seconds, paste(r_runs$dataset, r_runs$run), max)), 1), "\n")
cat("MCD_DONE\n")
