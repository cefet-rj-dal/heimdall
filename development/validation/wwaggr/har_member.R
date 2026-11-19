# Trains one member of the WWAggr ensembles on the Human Activity data with
# heimdall, with the settings of the reference configurations
# (configs/human_activity_seq2seq.yaml, _tscp.yaml, _ts2vec.yaml), and saves
# its scores on the test sequences, raw and beta-calibrated on the
# calibration split of the reference (30 % of the training sequences).
#   Rscript development/validation/wwaggr/har_member.R <har.rds> <work dir> <model> <ensemble> <member>
# <model>: bce, indid, tscp or ts2vec. The seed is member + 10 (ensemble - 1),
# as scripts/train_single_models.py.
suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
har <- readRDS(args[1])
work <- args[2]
model_name <- args[3]
ensemble <- as.integer(args[4])
member <- as.integer(args[5])
seed <- member + 10L * (ensemble - 1L)
val_idx <- readBin(file.path(work, "val_idx.bin"), "double", 10000, 8, endian = "little") + 1
rows <- seq_len(dim(har$train$x)[1])
if (endsWith(model_name, "_boot")) {
  # Bootstrap ensembles: the members of ensemble e train on bootstrap samples of the training sequences
  # (as many as the training set) drawn in turn from seed 1000 + e, as cpd_ensemble_fit().
  draws <- heimdall:::.with_rng(heimdall:::.rng_state(1000L + ensemble), function() {
    lapply(0:member, function(i) sample.int(length(rows), length(rows), replace = TRUE))
  })$value
  rows <- draws[[member + 1L]]
}
base <- sub("_boot$", "", model_name)
start <- Sys.time()
model <- switch(base,
                bce = , indid = indid(har$train$x[rows, , , drop = FALSE], har$train$labels[rows, , drop = FALSE],
                                      loss = base, hidden_size = 8, T = 5, learning_rate = 0.001, batch_size = 64,
                                      epochs = 200, patience = 10,
                                      validation = list(x = har$test$x, labels = har$test$labels), seed = seed),
                tscp = tscp2(har$train$x, seed = seed),
                ts2vec = ts2vec(har$train$x, seed = seed))
elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
calibrated <- cpd_calibrate(model, har$train$x[val_idx, , , drop = FALSE], har$train$labels[val_idx, , drop = FALSE])
out <- list(model = model, calibrator = calibrated$calibrator, test = cpd_scores(model, har$test$x),
            test_calibrated = cpd_scores(calibrated, har$test$x), epochs = nrow(model$history), elapsed = elapsed)
dir.create(file.path(work, "members_r"), showWarnings = FALSE)
saveRDS(out, file.path(work, "members_r", sprintf("%s_%d_%d.rds", model_name, ensemble, member)))
cat(sprintf("%s ensemble %d member %d: %d epochs, %.0f s\n", model_name, ensemble, member, out$epochs, elapsed))
