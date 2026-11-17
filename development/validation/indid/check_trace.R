# Step-by-step comparison of indid() with the reference code, from the trace
# of reference_trace.py.
#   Rscript development/validation/indid/check_trace.R <trace.json> <data dir>

devtools::load_all(quiet = TRUE)
source(file.path("development", "validation", "indid", "metrics.R"))
args <- commandArgs(trailingOnly = TRUE)
trace <- jsonlite::fromJSON(args[1], simplifyVector = FALSE)
data_dir <- args[2]
out_dir <- file.path("development", "validation", "results", "indid")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
as_matrix <- function(x) do.call(rbind, lapply(x, function(r) unlist(r)))
as_params <- function(p) {
  list(W_ih = as_matrix(p$W_ih), W_hh = as_matrix(p$W_hh), b_ih = unlist(p$b_ih), b_hh = unlist(p$b_hh),
       W_out = as_matrix(p$W_out), b_out = unlist(p$b_out))
}
max_difference <- function(a, b) max(vapply(names(b), function(n) max(abs(a[[n]] - b[[n]])), numeric(1)))
rows <- list()

# 1. Losses
prob <- as_matrix(trace$loss$prob)
labels <- as_matrix(trace$loss$labels)
for (name in c("indid", "bce")) {
  ours <- if (name == "indid") heimdall:::.indid_loss(prob, labels, trace$loss$T) else heimdall:::.indid_bce(prob, labels)
  case <- trace$loss$cases[[name]]
  rows[[length(rows) + 1]] <- data.frame(check = paste("loss", name, "(float64)"), value_difference =
                                           abs(ours$value - case$value), gradient_difference =
                                           max(abs(ours$grad - as_matrix(case$grad))), weight_difference = NA)
}

# 2. Training steps
train <- read_synthetic(data_dir, "synthetic_1D", "train")
test <- read_synthetic(data_dir, "synthetic_1D", "test")
init <- as_params(trace$init)
metrics_rows <- list()
for (loss in c("indid", "bce")) {
  settings <- list(loss = loss, T = 32, learning_rate = 0.001)
  net <- heimdall:::.indid_reset_adam(list(params = init))
  for (s in seq_along(trace$batches)) {
    rows_batch <- unlist(trace$batches[[s]])
    step <- heimdall:::.indid_step(net, train$x[rows_batch, , , drop = FALSE], train$labels[rows_batch, , drop = FALSE],
                                   settings)
    net <- step$net
    reference <- trace$steps[[loss]]$float64[[s]]
    rows[[length(rows) + 1]] <- data.frame(
      check = sprintf("step %d, %s (float64)", s, loss), value_difference = abs(step$loss - reference$loss),
      gradient_difference = max_difference(step$grads, as_params(reference$grads)),
      weight_difference = max_difference(net$params, as_params(reference$params)))
    reference32 <- trace$steps[[loss]]$float32[[s]]
    rows[[length(rows) + 1]] <- data.frame(
      check = sprintf("step %d, %s (float32, unchanged CPDModel)", s, loss),
      value_difference = abs(step$loss - reference32$loss), gradient_difference = NA,
      weight_difference = max_difference(net$params, as_params(reference32$params)))
  }
  # 3. Probabilities on the test set and the metrics of evaluation_pipeline
  reference_prob <- as_matrix(trace$steps[[loss]]$test_prob)
  ours_prob <- heimdall:::.indid_forward(net$params, test$x)$p
  rows[[length(rows) + 1]] <- data.frame(check = sprintf("test probabilities, %s (float32)", loss),
                                         value_difference = max(abs(ours_prob - reference_prob)),
                                         gradient_difference = NA, weight_difference = NA)
  ours_metrics <- indid_metrics(reference_prob, test$labels)$summary
  theirs <- trace$steps[[loss]]$metrics
  metrics_rows[[loss]] <- data.frame(loss = loss, source = c("metrics.R", "metrics.py"),
                                     time_to_fa = c(ours_metrics$time_to_fa, theirs$time_to_fa),
                                     delay = c(ours_metrics$delay, theirs$delay), auc = c(ours_metrics$auc, theirs$auc),
                                     f1 = c(ours_metrics$f1, theirs$f1), cover = c(ours_metrics$cover, theirs$cover),
                                     max_cover = c(ours_metrics$max_cover, theirs$max_cover))
}
result <- do.call(rbind, rows)
utils::write.csv(result, file.path(out_dir, "trace_comparison.csv"), row.names = FALSE)
print(format(result, digits = 3), row.names = FALSE)
metrics_check <- do.call(rbind, metrics_rows)
utils::write.csv(metrics_check, file.path(out_dir, "metrics_comparison.csv"), row.names = FALSE)
cat("\nMetrics on the reference probabilities (untrained model after 3 steps)\n")
print(metrics_check, row.names = FALSE)
