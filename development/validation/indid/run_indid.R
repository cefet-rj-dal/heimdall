# Table 2 of Romanenkova et al. (2022) on the SyntheticNormal data sets with
# indid(), next to the reference code (run_reference.py) and the paper.
#
# Run from the package root, inside the devcontainer, after prepare_data.py and
# run_reference.py (see README.md):
#   OPENBLAS_NUM_THREADS=1 Rscript development/validation/indid/run_indid.R [cores]
#
# Settings of configs/synthetic_{1D,100D}_seq2seq.yaml: one LSTM layer with 4
# (1D) or 8 (100D) hidden units, T = 32, Adam lr 0.001, batches of 64, up to 200
# epochs (100 + 100 for BCE+InDiD) with early stopping (patience 10) on the test
# set, as the reference. Seeds 102-106 (the reference default is 102); the paper
# reports means of 5 runs without the seeds.

devtools::load_all(quiet = TRUE)
source(file.path("development", "validation", "indid", "metrics.R"))
args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args) > 0) as.integer(args[1]) else 6L
data_dir <- file.path("data-real", "indid", "synthetic")
out_dir <- file.path("development", "validation", "results", "indid")
reference_dir <- file.path(out_dir, "reference")
hidden <- c(synthetic_1D = 4, synthetic_100D = 8)
seeds <- 102:106

jobs <- expand.grid(seed = seeds, loss = c("indid", "bce", "combined"), dataset = names(hidden),
                    stringsAsFactors = FALSE)
runs <- parallel::mclapply(split(jobs, seq_len(nrow(jobs))), function(job) {
  train <- read_synthetic(data_dir, job$dataset, "train")
  test <- read_synthetic(data_dir, job$dataset, "test")
  started <- Sys.time()
  model <- indid(train$x, train$labels, loss = job$loss, hidden_size = hidden[[job$dataset]], T = 32,
                 learning_rate = 0.001, batch_size = 64, epochs = 200, patience = 10, validation = test,
                 seed = job$seed)
  seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  metrics <- indid_metrics(indid_predict(model, test$x), test$labels)$summary
  cbind(implementation = "indid()", dataset = job$dataset, loss = job$loss, seed = job$seed,
        epochs = nrow(model$history), seconds = seconds, metrics)
}, mc.cores = cores, mc.preschedule = FALSE)
ours <- do.call(rbind, runs)

# Reference runs, and the R metrics on the reference probabilities.
reference <- do.call(rbind, lapply(list.files(reference_dir, pattern = "^synthetic_.*[0-9][.]json$", full.names = TRUE),
                                   function(path) {
  r <- jsonlite::fromJSON(path)
  test <- read_synthetic(data_dir, r$dataset, "test")
  prob <- t(matrix(readBin(sub("[.]json$", "_prob.bin", path), "double", prod(dim(test$labels)), 8, endian = "little"),
                   ncol(test$labels)))
  again <- indid_metrics(prob, test$labels)$summary
  data.frame(implementation = "reference", dataset = r$dataset, loss = r$loss, seed = r$seed, epochs = sum(r$epochs),
             seconds = r$seconds, time_to_fa = r$time_to_fa, delay = r$delay, auc = r$auc, f1 = r$f1, cover = r$cover,
             max_cover = r$max_cover, threshold = r$threshold,
             r_metrics_difference = max(abs(c(again$time_to_fa - r$time_to_fa, again$delay - r$delay,
                                              again$auc - r$auc, again$f1 - r$f1, again$cover - r$cover))))
}))
runs_all <- rbind(cbind(ours, r_metrics_difference = NA), reference)
utils::write.csv(runs_all, file.path(out_dir, "table2_runs.csv"), row.names = FALSE)
cat("metrics.R against metrics.py on the probabilities of the", nrow(reference),
    "reference runs: largest difference", signif(max(reference$r_metrics_difference), 3), "\n\n")

paper <- data.frame(
  dataset = rep(c("synthetic_1D", "synthetic_100D"), each = 3), loss = rep(c("bce", "indid", "combined"), 2),
  implementation = "paper",
  time_to_fa = c(94.49, 94.41, 94.59, 94.20, 94.20, 94.20), time_to_fa_sd = c(0.00, 0.13, 0.17, 0, 0, 0),
  delay = c(0.50, 0.54, 0.61, 0.03, 0.03, 0.03), delay_sd = c(0.02, 0.04, 0.13, 0.00, 0.01, 0.00),
  auc = c(606.00, 598.70, 605.30, 573.39, 573.47, 573.29), auc_sd = c(4.91, 5.00, 5.12, 0.22, 0.31, 0.16),
  f1 = c(0.9904, 0.9898, 0.9891, 0.9968, 0.9968, 0.9968), f1_sd = c(0, 0.0010, 0.0021, 0, 0, 0),
  cover = c(0.9941, 0.9938, 0.9936, 0.9996, 0.9996, 0.9996), cover_sd = c(0.0002, 0.0004, 0.0006, 0.0001, 0.0001, 0)
)
summary <- do.call(rbind, lapply(split(runs_all, list(runs_all$implementation, runs_all$dataset, runs_all$loss),
                                       drop = TRUE), function(p) {
  m <- function(v) mean(p[[v]])
  s <- function(v) stats::sd(p[[v]])
  data.frame(dataset = p$dataset[1], loss = p$loss[1], implementation = p$implementation[1],
             time_to_fa = m("time_to_fa"), time_to_fa_sd = s("time_to_fa"), delay = m("delay"), delay_sd = s("delay"),
             auc = m("auc"), auc_sd = s("auc"), f1 = m("f1"), f1_sd = s("f1"), cover = m("cover"),
             cover_sd = s("cover"))
}))
table2 <- rbind(summary, paper)
table2 <- table2[order(match(table2$dataset, names(hidden)), match(table2$loss, c("bce", "indid", "combined")),
                       match(table2$implementation, c("indid()", "reference", "paper"))), ]
utils::write.csv(table2, file.path(out_dir, "table2.csv"), row.names = FALSE)
cat("Table 2 (SyntheticNormal): mean and sd over 5 runs\n")
print(format(table2, digits = 4), row.names = FALSE)
cat("\nEpochs and seconds per run\n")
print(stats::aggregate(cbind(epochs, seconds) ~ implementation + dataset + loss, runs_all, mean), row.names = FALSE)
cat("INDID_DONE\n")
