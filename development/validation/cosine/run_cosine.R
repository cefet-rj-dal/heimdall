# Validation of cosine_score() and dfr_cosine() against the custom Model
# Monitor for NLP data drift of Nigenda et al. (2022), "Amazon SageMaker Model
# Monitor: A System for Real-Time Insights into Deployed Machine Learning
# Models", KDD 2022 (Section 6.2), and its code
# (https://github.com/aws-samples/detecting-data-drift-in-nlp-using-amazon-sagemaker-custom-model-monitor).
#
# Run from the package root, inside the devcontainer, after embed_reference.py:
#   Rscript development/validation/cosine/run_cosine.R [embeddings_dir]
#
# Data: CoLA (the baseline of the paper's use case, Figure 4), BERT
# (bert-base-uncased) sentence embeddings computed as the reference code does.
# 1. cosine_score() against the scores of the evaluation.py loop.
# 2. The scores and violations (threshold 0.5) of the monitored sets.
# 3. dfr_cosine() on a stream: CoLA in-domain dev, then out-of-domain dev, then
#    the notebook sentences, with monitoring windows.

devtools::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
emb_dir <- if (length(args) > 0) args[1] else file.path("data-real", "smm", "embeddings")
out_dir <- file.path("development", "validation", "results", "cosine")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
read_matrix <- function(name) matrix(readBin(file.path(emb_dir, paste0(name, ".bin")), "double", n = 1e8),
                                     ncol = 768, byrow = TRUE)
reference <- jsonlite::fromJSON(file.path(emb_dir, "reference_scores.json"), simplifyVector = FALSE)
padded <- read_matrix("baseline_padded")
unpadded <- read_matrix("baseline_unpadded")
sets <- c("notebook", "in_domain_dev", "out_of_domain_dev")
embedded <- lapply(stats::setNames(sets, sets), read_matrix)

# 1. Equivalence ---------------------------------------------------------------------------------
equivalence <- do.call(rbind, lapply(sets, function(set) {
  theirs <- unlist(reference$sets[[set]]$scores)
  ours <- cosine_score(padded, embedded[[set]])
  data.frame(set = set, n = length(ours), max_abs_difference = max(abs(ours - theirs)),
             violations = sum(ours < 0.5), violations_reference = sum(unlist(reference$sets[[set]]$violations)),
             same_violations = identical(ours < 0.5, unlist(reference$sets[[set]]$violations)))
}))
utils::write.csv(equivalence, file.path(out_dir, "equivalence.csv"), row.names = FALSE)
cat("1. cosine_score() against the evaluation.py loop (baseline: CoLA in_domain_train, 8551 embeddings)\n")
print(equivalence, row.names = FALSE, digits = 3)
started <- Sys.time()
invisible(cosine_score(padded, embedded$out_of_domain_dev))
cat(sprintf("cosine_score() on the 516 out-of-domain sentences: %.3f s\n",
            as.numeric(difftime(Sys.time(), started, units = "secs"))))

# 2. Scores ---------------------------------------------------------------------------------------
# The baseline of the reference is padded to 64 tokens (the mean includes the
# [PAD] positions) while the inputs are not; "unpadded" embeds the baseline as
# the inputs.
summary_rows <- list()
for (baseline_name in c("padded", "unpadded")) {
  baseline <- if (baseline_name == "padded") padded else unpadded
  for (set in c("baseline (in_domain_train)", sets)) {
    x <- if (set == "baseline (in_domain_train)") unpadded else embedded[[set]]
    s <- cosine_score(baseline, x)
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      baseline = baseline_name, set = set, n = length(s), mean = mean(s), sd = stats::sd(s),
      q01 = unname(stats::quantile(s, 0.01)), min = min(s), violations_0.5 = mean(s < 0.5))
  }
}
scores <- do.call(rbind, summary_rows)
utils::write.csv(scores, file.path(out_dir, "scores.csv"), row.names = FALSE)
cat("\n2. Scores of each set (inputs embedded as evaluation.py does)\n")
print(scores, row.names = FALSE, digits = 4)
notebook <- data.frame(sentence = unlist(reference$sets$notebook$sentences),
                       score_padded = cosine_score(padded, embedded$notebook),
                       score_unpadded = cosine_score(unpadded, embedded$notebook))
utils::write.csv(notebook, file.path(out_dir, "notebook_sentences.csv"), row.names = FALSE)
cat("\nNotebook sentences\n")
print(notebook, row.names = FALSE, digits = 4)

# 3. Stream ---------------------------------------------------------------------------------------
# Thresholds: 0.5 (reference code), and one calibrated on the baseline itself:
# the 1% quantile of the window means of the baseline sentences (embedded as
# inputs), estimated with 2000 random windows.
stream <- as.data.frame(rbind(embedded$in_domain_dev, embedded$out_of_domain_dev, embedded$notebook))
segment <- rep(c("in_domain_dev", "out_of_domain_dev", "notebook"), sapply(embedded[c("in_domain_dev",
                                                                                     "out_of_domain_dev",
                                                                                     "notebook")], nrow))
set.seed(1)
stream_rows <- list()
for (baseline_name in c("padded", "unpadded")) {
  baseline <- if (baseline_name == "padded") padded else unpadded
  own <- cosine_score(baseline, unpadded)
  for (window in c(1L, 10L, 50L)) {
    calibrated <- unname(stats::quantile(replicate(2000, mean(sample(own, window))), 0.01))
    for (threshold in c(0.5, calibrated)) {
      model <- fit(dfr_cosine(reference = baseline, window_size = window, threshold = threshold), stream)
      ends <- which(!is.na(model$drifter_output$window_score))
      flagged <- model$drifter_output$window_score[ends] < threshold
      # A window belongs to the segment of its last observation.
      for (seg in unique(segment)) {
        inside <- segment[ends] == seg
        stream_rows[[length(stream_rows) + 1]] <- data.frame(
          baseline = baseline_name, window_size = window,
          threshold = if (threshold == 0.5) "0.5" else sprintf("calibrated (%.4f)", threshold),
          segment = seg, windows = sum(inside), drift_rate = mean(flagged[inside]))
      }
    }
  }
}
stream_result <- do.call(rbind, stream_rows)
utils::write.csv(stream_result, file.path(out_dir, "stream.csv"), row.names = FALSE)
cat("\n3. dfr_cosine on the stream in-domain dev -> out-of-domain dev -> notebook sentences\n")
print(stream_result, row.names = FALSE, digits = 3)
cat("COSINE_DONE\n")
