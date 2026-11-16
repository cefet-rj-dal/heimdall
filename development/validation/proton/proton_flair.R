# Validation of dfr_proton() on the ophthalmology benchmark (FLAIR + FIVES) of
# Das et al. (2026), "PROTON: Prototype-Based Test-Time Online OOD Detection
# for Medical VLMs" (Tables 1a, 2 and 3).
#
# Input: data-real/proton/flair_features.csv, produced by extract_features.py
# (one row per image: group, shift, e1..e512, p_normal, p_dr).
#
# For each shift type, the in-distribution images and the OOD images of that
# type are interleaved at random into one unlabeled stream with an ID fraction
# rho (70% by default, as in the paper), fed to dfr_proton() one at a time,
# and the per-sample scores are evaluated with AUROC, AUPR and FPR@95 (OOD is
# the positive class). Results are averaged over five stream orders.
#
# Run from the package root:
#   Rscript development/validation/proton/proton_flair.R [features.csv] [output dir]

devtools::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
features_path <- if (length(args) > 0) args[1] else file.path("data-real", "proton", "flair_features.csv")
out_dir <- if (length(args) > 1) args[2] else file.path("development", "validation", "results")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

features <- utils::read.csv(features_path)
embedding_cols <- grep("^e[0-9]+$", names(features), value = TRUE)
prob_cols <- c("p_normal", "p_dr")
cat(sprintf("%d images, %d embedding dimensions\n", nrow(features), length(embedding_cols)))
print(table(features$group))

# Metrics ---------------------------------------------------------------------
auroc <- function(score, is_ood) {
  r <- rank(score)
  n_pos <- sum(is_ood)
  n_neg <- sum(!is_ood)
  (sum(r[is_ood]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}
# Average precision, as sklearn.metrics.average_precision_score.
aupr <- function(score, is_ood) {
  thresholds <- sort(unique(score), decreasing = TRUE)
  tp <- vapply(thresholds, function(th) sum(is_ood & (score >= th)), numeric(1))
  predicted <- vapply(thresholds, function(th) sum(score >= th), numeric(1))
  recall <- tp / sum(is_ood)
  precision <- tp / predicted
  sum(diff(c(0, recall)) * precision)
}
# FPR at the operating point whose TPR is closest to 95% (as Ju et al.).
fpr95 <- function(score, is_ood) {
  thresholds <- c(Inf, sort(unique(score), decreasing = TRUE))
  tpr <- vapply(thresholds, function(th) mean(score[is_ood] >= th), numeric(1))
  fpr <- vapply(thresholds, function(th) mean(score[!is_ood] >= th), numeric(1))
  fpr[which.min(abs(tpr - 0.95))]
}
metrics <- function(score, is_ood) {
  100 * c(auc = auroc(score, is_ood), aupr = aupr(score, is_ood), fpr95 = fpr95(score, is_ood))
}

# Streams ---------------------------------------------------------------------
id_rows <- which(features$shift == "id")
shifts <- c(semantic = "semantic", covariate = "covariate", far = "far")

# Largest stream with the requested ID fraction that the available images allow.
make_stream <- function(shift, seed, rho = 0.7) {
  set.seed(seed)
  ood_rows <- which(features$shift == shift)
  n_id <- length(id_rows)
  n_ood <- round(n_id * (1 - rho) / rho)
  if (n_ood > length(ood_rows)) {
    n_ood <- length(ood_rows)
    n_id <- round(n_ood * rho / (1 - rho))
  }
  rows <- c(sample(id_rows, n_id), sample(ood_rows, n_ood))
  rows <- rows[sample(length(rows))]
  list(data = features[rows, c(embedding_cols, prob_cols)], is_ood = features$shift[rows] != "id")
}

evaluate <- function(shift, seed, rho = 0.7, ...) {
  stream <- make_stream(shift, seed, rho)
  detector <- fit(dfr_proton(embedding_cols, prob_cols, ...), stream$data)
  out <- detector$drifter_output
  rbind(
    MCM = metrics(out$base_score, stream$is_ood),
    PROTON = metrics(out$score, stream$is_ood)
  )
}

seeds <- 1:5
summarize <- function(results) {
  values <- simplify2array(results)
  list(mean = apply(values, c(1, 2), mean), sd = apply(values, c(1, 2), stats::sd))
}

# Table 2: main results ---------------------------------------------------------
paper <- rbind(
  MCM = c(53.9, 49.7, 90.1, 42.4, 30.0, 94.6, 76.4, 61.9, 54.2),
  PROTON = c(62.7, 59.6, 79.0, 66.3, 49.0, 69.3, 84.5, 70.5, 41.7)
)
table2 <- NULL
for (shift in names(shifts)) {
  s <- summarize(lapply(seeds, function(seed) evaluate(shifts[[shift]], seed)))
  for (method in rownames(s$mean)) {
    table2 <- rbind(table2, data.frame(
      shift = shift, method = method,
      auc = s$mean[method, "auc"], auc_sd = s$sd[method, "auc"],
      aupr = s$mean[method, "aupr"], fpr95 = s$mean[method, "fpr95"]
    ))
  }
}
table2$paper_auc <- c(paper["MCM", 1], paper["PROTON", 1], paper["MCM", 4], paper["PROTON", 4], paper["MCM", 7], paper["PROTON", 7])
table2$paper_aupr <- c(paper["MCM", 2], paper["PROTON", 2], paper["MCM", 5], paper["PROTON", 5], paper["MCM", 8], paper["PROTON", 8])
table2$paper_fpr95 <- c(paper["MCM", 3], paper["PROTON", 3], paper["MCM", 6], paper["PROTON", 6], paper["MCM", 9], paper["PROTON", 9])
cat("\nTable 2 (mean over 5 stream orders; OOD = positive class)\n")
print(format(table2, digits = 3), row.names = FALSE)
utils::write.csv(table2, file.path(out_dir, "proton_table2.csv"), row.names = FALSE)

# Table 3a: components ----------------------------------------------------------
variants <- list(
  "+ Proto. (alpha=.5)" = list(gamma = 0, alpha_min = 0.5, alpha_max = 0.5),
  "+ Adapt. fusion" = list(gamma = 0),
  "+ Conf. gate (Full)" = list()
)
table3a <- NULL
for (name in names(variants)) {
  row <- vapply(names(shifts), function(shift) {
    mean(vapply(seeds, function(seed) do.call(evaluate, c(list(shift = shifts[[shift]], seed = seed), variants[[name]]))["PROTON", "auc"], numeric(1)))
  }, numeric(1))
  table3a <- rbind(table3a, data.frame(variant = name, t(row)))
}
mcm_row <- vapply(names(shifts), function(shift) table2$auc[table2$shift == shift & table2$method == "MCM"], numeric(1))
table3a <- rbind(data.frame(variant = "MCM baseline", t(mcm_row)), table3a)
table3a$paper <- c("42.4 / 53.9 / 76.4", "57.9 / 59.2 / 80.9", "62.7 / 61.4 / 82.9", "66.3 / 62.7 / 84.5")
table3a <- table3a[, c("variant", "covariate", "semantic", "far", "paper")]
cat("\nTable 3a: components (AUROC; paper column is Cov / Sem / Far)\n")
print(format(table3a, digits = 3), row.names = FALSE)
utils::write.csv(table3a, file.path(out_dir, "proton_table3a.csv"), row.names = FALSE)

# Tables 3b and 3c: sensitivity to gamma and M (covariate shift) ---------------
sensitivity <- function(argument, values) {
  do.call(rbind, lapply(values, function(v) {
    s <- summarize(lapply(seeds, function(seed) do.call(evaluate, c(list(shift = "covariate", seed = seed), stats::setNames(list(v), argument)))))
    data.frame(value = v, auc = s$mean["PROTON", "auc"], aupr = s$mean["PROTON", "aupr"], fpr95 = s$mean["PROTON", "fpr95"])
  }))
}
table3b <- sensitivity("gamma", c(0.5, 0.6, 0.7, 0.8, 0.9))
table3c <- sensitivity("bank_size", c(10, 25, 50, 100, 200))
cat("\nTable 3b: gamma (covariate; paper AUROC 66.2 66.3 66.3 66.2 65.5)\n")
print(format(table3b, digits = 3), row.names = FALSE)
cat("\nTable 3c: bank size M (covariate; paper AUROC 67.1 66.1 66.4 66.3 66.2)\n")
print(format(table3c, digits = 3), row.names = FALSE)
utils::write.csv(table3b, file.path(out_dir, "proton_table3b_gamma.csv"), row.names = FALSE)
utils::write.csv(table3c, file.path(out_dir, "proton_table3c_bank_size.csv"), row.names = FALSE)

# Table 1a: ID fraction of the stream -----------------------------------------
table1a <- do.call(rbind, lapply(c(0.9, 0.7, 0.3, 0.1), function(rho) {
  row <- vapply(names(shifts), function(shift) {
    mean(vapply(seeds, function(seed) evaluate(shifts[[shift]], seed, rho = rho)["PROTON", "auc"], numeric(1)))
  }, numeric(1))
  data.frame(rho = rho, t(row))
}))
table1a$paper <- c("63.2 / 67.4 / 84.8", "62.7 / 66.3 / 84.5", "58.8 / 58.5 / 80.4", "55.1 / 50.9 / 77.2")
cat("\nTable 1a: stream ID fraction (PROTON AUROC; paper column is Sem / Cov / Far)\n")
print(format(table1a, digits = 3), row.names = FALSE)
utils::write.csv(table1a, file.path(out_dir, "proton_table1a_rho.csv"), row.names = FALSE)

# Fig. 1b: MCM vs prototype distance on one covariate stream ----------------------
stream <- make_stream("covariate", seed = 1)
out <- fit(dfr_proton(embedding_cols, prob_cols), stream$data)$drifter_output
grDevices::png(file.path(out_dir, "proton_covariate_scores.png"), width = 1200, height = 500)
graphics::par(mfrow = c(1, 2))
ok <- out$calibrated
graphics::plot(out$base_score[ok], out$proto_score[ok], col = ifelse(stream$is_ood[ok], "#F28E2B", "#4E79A7"), pch = 19, cex = 0.7,
               xlab = "S_MCM (softmax)", ylab = "S_proto (embedding distance)", main = "MCM vs prototype distance (covariate)")
graphics::legend("topleft", c("ID", "covariate OOD (UWF)"), col = c("#4E79A7", "#F28E2B"), pch = 19, bty = "n")
graphics::plot(out$alpha, type = "l", ylim = c(0, 1), xlab = "stream position", ylab = "alpha_t", main = "Adaptive fusion weight")
graphics::rug(which(stream$is_ood), col = "#F28E2B")
grDevices::dev.off()
