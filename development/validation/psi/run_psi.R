# Validation of psi() and dfr_psi() against Kurian and Allali (2024),
# "Detecting drifts in data streams using Kullback-Leibler (KL) divergence
# measure for data engineering applications", Journal of Data, Information and
# Management 6, 207-216, and its reference code
# (https://github.com/FrancisKurian/Kullback: Data_Simulation.sas,
# PSI_computation.sas).
#
# Run from the package root, inside the devcontainer:
#   Rscript development/validation/psi/run_psi.R [cores]
#
# 1. The paper's data: the base sample and the "through the door" (TTD) sample
#    of Data_Simulation.sas (100,000 records; four fields with the simulated
#    data issues), drawn in R with 20 seeds, since SAS's random streams cannot
#    be reproduced. The rules are those of the SAS code, which differ from the
#    text of the paper (see README.md).
# 2. PSI at deciles and demi-deciles (Tables 4-6): totals and parts per bin,
#    mean and standard deviation over the seeds, against the paper.
# 3. The stream detector: PSI of windows without drift (its noise level) and
#    detection of the four data issues, for several window sizes.

devtools::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args) > 0) as.integer(args[1]) else 6L
out_dir <- file.path("development", "validation", "results", "psi")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
fields <- c("AdResponse", "SalesVolume", "Deposits", "CBM")

# Data_Simulation.sas ---------------------------------------------------------------
simulate <- function(seed, n = 100000) {
  set.seed(seed)
  base <- data.frame(
    SalesVolume = round(stats::rnorm(n, 350000, 13000), -1),
    Deposits = round(stats::rnorm(n, 75000, 20000), -1),
    AdResponse = round(stats::rnorm(n, 8000, 800), -1),
    CBM = round(stats::rnorm(n, 610, 50))
  )
  ttd <- base
  sales <- (base$SalesVolume >= 0) & (base$SalesVolume <= 341224) & (stats::runif(n) <= 0.5)
  ttd$SalesVolume[sales] <- base$SalesVolume[sales] * 1.1
  deposits <- stats::runif(n) <= 0.2
  ttd$Deposits[deposits] <- base$Deposits[deposits] / 1000
  ad <- stats::runif(n) <= 0.1
  ttd$AdResponse[ad] <- 0
  cbm <- (base$CBM > 643) & (stats::runif(n) <= 0.5)
  ttd$CBM[cbm] <- base$CBM[cbm] - 200
  list(base = base[fields], ttd = ttd[fields])
}

# Paper, Tables 4 and 5: parts per bin and totals.
paper_parts <- list(
  `10` = list(
    AdResponse = c(0.057695, 0.001071, 0.000995, 0.001062, 0.000976, 0.001008, 0.001123, 0.001108, 0.00104, 0.001162),
    SalesVolume = c(0.03445, 0.03493, 0.00638, 0.00001, 0.00002, 0.00009, 0.00034, 0.00124, 0.00644, 0.04062),
    Deposits = c(0.18514, 0.0043, 0.00465, 0.00453, 0.00477, 0.00416, 0.00471, 0.00422, 0.00434, 0.00445),
    CBM = c(0.09691, 0.00002, 0, 0, 0, 0, 0, 0.00632, 0.03397, 0.03385)
  ),
  `20` = list(
    AdResponse = c(0.10097, 0.00053, 0.0005, 0.00058, 0.00043, 0.00057, 0.00053, 0.00053, 0.00053, 0.00045,
                   0.00045, 0.00055, 0.0006, 0.00052, 0.00054, 0.00056, 0.00048, 0.00057, 0.00059, 0.00057),
    SalesVolume = c(0.01759, 0.01687, 0.01724, 0.01769, 0.01562, 0, 0, 0, 0.00001, 0.00001,
                    0.00003, 0.00007, 0.00013, 0.00021, 0.00039, 0.00089, 0.00169, 0.00518, 0.01792, 0.02276),
    Deposits = c(0.29832, 0.00234, 0.00197, 0.00234, 0.00235, 0.0023, 0.0022, 0.00233, 0.00254, 0.00223,
                 0.00207, 0.00209, 0.00235, 0.00237, 0.0022, 0.00203, 0.00213, 0.00222, 0.00217, 0.00228),
    CBM = c(0.14313, 0.00016, 0.00002, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.01648, 0.01726, 0.01672, 0.01658, 0.01727)
  )
)
paper_totals <- data.frame(field = rep(fields, 2), bins = rep(c(10, 20), each = 4),
                           paper = c(0.06724, 0.12452, 0.22527, 0.17107, 0.11105, 0.1343, 0.34083, 0.22762))

# 1-2. Tables 4-6 -------------------------------------------------------------------------
seeds <- 1:20
runs <- parallel::mclapply(seeds, function(seed) {
  data <- simulate(seed)
  lapply(c(10, 20), function(bins) {
    result <- psi(data$base, data$ttd, bins = bins)
    # TTD values strictly between two bins (see ?psi): the reference drops
    # them, psi() puts them in the lower bin.
    gaps <- vapply(fields, function(f) {
      part <- result$bins[result$bins$feature == f, ]
      x <- data$ttd[[f]]
      inside <- outer(x, part$lower, ">=") & outer(x, part$upper, "<=")
      sum(rowSums(inside) == 0 & x > min(part$lower) & x < max(part$upper))
    }, numeric(1))
    # Share of records moved by each data issue in this draw.
    moved <- c(AdResponse = mean(data$ttd$AdResponse != data$base$AdResponse),
               SalesVolume = mean(data$ttd$SalesVolume != data$base$SalesVolume),
               Deposits = mean(data$ttd$Deposits != data$base$Deposits),
               CBM = mean(data$ttd$CBM != data$base$CBM))
    list(summary = cbind(seed = seed, bins = bins, result$summary, gaps = gaps, moved = moved[fields]),
         parts = cbind(seed = seed, bins = bins, result$bins))
  })
}, mc.cores = cores)
runs <- unlist(runs, recursive = FALSE)
summaries <- do.call(rbind, lapply(runs, `[[`, "summary"))
parts <- do.call(rbind, lapply(runs, `[[`, "parts"))
utils::write.csv(summaries, file.path(out_dir, "psi_runs.csv"), row.names = FALSE)

totals <- do.call(rbind, lapply(split(summaries, list(summaries$feature, summaries$bins)), function(p) {
  data.frame(field = p$feature[1], bins = p$bins[1], psi = mean(p$psi), psi_sd = stats::sd(p$psi),
             psi_min = min(p$psi), psi_max = max(p$psi), level = names(which.max(table(p$level))),
             gaps_mean = mean(p$gaps))
}))
totals <- merge(totals, paper_totals, by = c("field", "bins"))
totals$difference <- totals$psi - totals$paper
totals$z <- totals$difference / totals$psi_sd
totals <- totals[order(totals$bins, match(totals$field, fields)), ]
utils::write.csv(totals, file.path(out_dir, "tables_4_6.csv"), row.names = FALSE)
cat("Tables 4-6: total PSI, mean over", length(seeds), "seeds, against the paper\n")
print(format(totals, digits = 4), row.names = FALSE)

by_bin <- do.call(rbind, lapply(split(parts, list(parts$feature, parts$bins, parts$bin), drop = TRUE), function(p) {
  data.frame(field = p$feature[1], bins = p$bins[1], bin = p$bin[1], expected = mean(p$expected),
             actual = mean(p$actual), part = mean(p$part), part_sd = stats::sd(p$part),
             lower = mean(p$lower), upper = mean(p$upper))
}))
by_bin$paper <- mapply(function(f, b, i) paper_parts[[as.character(b)]][[f]][i], by_bin$field, by_bin$bins, by_bin$bin)
by_bin$difference <- by_bin$part - by_bin$paper
by_bin <- by_bin[order(by_bin$bins, match(by_bin$field, fields), by_bin$bin), ]
utils::write.csv(by_bin, file.path(out_dir, "tables_4_5_bins.csv"), row.names = FALSE)
cat("\nParts per bin: largest absolute difference from the paper\n")
print(format(do.call(rbind, lapply(split(by_bin, list(by_bin$field, by_bin$bins)), function(p) {
  worst <- which.max(abs(p$difference))
  data.frame(field = p$field[1], bins = p$bins[1], max_abs_difference = abs(p$difference[worst]),
             at_bin = p$bin[worst], part = p$part[worst], paper = p$paper[worst], part_sd = p$part_sd[worst])
})), digits = 3), row.names = FALSE)
# The PSI of a field depends mostly on the share of records its data issue
# moved, which varies from draw to draw. The paper's decile tables (Appendix,
# Tables 7, 9 and 10) give that share for three fields: AdResponse and Deposits
# send the moved records to bin 1, which gains 90% of them (P - Q of bin 1 over
# 0.9); CBM empties part of bins 8-10 (the sum of their P - Q). SalesVolume
# moves records both ways, so its share cannot be read from the tables. A linear
# fit of the PSI on the moved share over the seeds predicts the PSI of the
# paper's draw.
paper_moved <- c(AdResponse = 0.0902 / 0.9, Deposits = 0.1799 / 0.9, CBM = 0.0237 + 0.0499 + 0.0493)
conditional_runs <- summaries[summaries$feature %in% names(paper_moved), ]
conditional <- do.call(rbind, lapply(split(conditional_runs, list(conditional_runs$feature, conditional_runs$bins),
                                           drop = TRUE), function(p) {
  f <- p$feature[1]
  fit <- stats::lm(psi ~ moved, data = p)
  data.frame(field = f, bins = p$bins[1], moved_mean = mean(p$moved), moved_sd = stats::sd(p$moved),
             paper_moved = paper_moved[[f]],
             predicted = unname(stats::predict(fit, data.frame(moved = paper_moved[[f]]))),
             residual_sd = summary(fit)$sigma)
}))
conditional <- merge(conditional, paper_totals, by = c("field", "bins"))
conditional$difference <- conditional$predicted - conditional$paper
conditional <- conditional[order(conditional$bins, match(conditional$field, fields)), ]
utils::write.csv(conditional, file.path(out_dir, "tables_4_6_conditional.csv"), row.names = FALSE)
cat("\nPSI predicted for the share of moved records of the paper's draw\n")
print(format(conditional, digits = 4), row.names = FALSE)

cat("\nDecile ranges of the first seed (Appendix, Tables 7-10)\n")
first <- parts[parts$seed == 1 & parts$bins == 10, c("feature", "bin", "lower", "upper", "expected", "actual")]
print(format(first, digits = 6), row.names = FALSE)

# 3. Stream detector -----------------------------------------------------------------------
# A baseline of 10,000 base records, then windows of window_size records: 20
# (9 for windows of 5,000) from the base sample (no drift) and as many from the
# TTD sample. The PSI of every
# window is computed with psi(), which is what dfr_psi() does on each window
# (checked below on one stream).
windows <- c(100, 250, 500, 1000, 5000)
# At most 20 windows of each kind, within the 100,000 records of a draw.
detector <- parallel::mclapply(seeds, function(seed) {
  data <- simulate(seed + 100)
  reference <- data$base[1:10000, ]
  do.call(rbind, lapply(windows, function(w) {
    n_windows <- min(20L, floor(90000 / (2 * w)))
    do.call(rbind, lapply(seq_len(n_windows), function(i) {
      clean <- data$base[10000 + (i - 1) * w + seq_len(w), ]
      drifted <- data$ttd[10000 + n_windows * w + (i - 1) * w + seq_len(w), ]
      rbind(data.frame(seed = seed, window = w, phase = "base", psi(reference, clean)$summary[c("feature", "psi")]),
            data.frame(seed = seed, window = w, phase = "ttd", psi(reference, drifted)$summary[c("feature", "psi")]))
    }))
  }))
}, mc.cores = cores)
detector <- do.call(rbind, detector)
noise <- do.call(rbind, lapply(split(detector, list(detector$window, detector$feature, detector$phase)), function(p) {
  data.frame(window = p$window[1], field = p$feature[1], phase = p$phase[1], psi_mean = mean(p$psi),
             psi_p95 = stats::quantile(p$psi, 0.95, names = FALSE), above_0.1 = mean(p$psi > 0.1),
             above_0.25 = mean(p$psi > 0.25))
}))
noise$expected_noise <- ifelse(noise$phase == "base", 9 * (1 / 10000 + 1 / noise$window), NA)
noise <- noise[order(noise$phase, match(noise$field, fields), noise$window), ]
utils::write.csv(noise, file.path(out_dir, "detector_windows.csv"), row.names = FALSE)
cat("\n3. PSI of windows from the base sample (no drift) and from the TTD sample, baseline of 10,000\n")
print(format(noise, digits = 3), row.names = FALSE)

# dfr_psi() on one stream gives the PSI of psi() for every window.
data <- simulate(1000)
stream <- rbind(data$base[1:12000, ], data$ttd[12001:14000, ])
model <- fit(dfr_psi(n_reference = 10000, window_size = 1000), stream)
out <- model$drifter_output
tested <- which(!is.na(out[[1]]))
direct <- t(vapply(seq_along(tested), function(i) {
  psi(stream[1:10000, ], stream[10000 + (i - 1) * 1000 + 1:1000, ])$summary$psi
}, numeric(4)))
cat("\ndfr_psi on a stream of 10,000 + 2 x 1,000 base + 2 x 1,000 TTD records\n")
print(cbind(row = tested, round(out[tested, ], 4)), row.names = FALSE)
cat("Same values as psi():", isTRUE(all.equal(unname(as.matrix(out[tested, ])), unname(direct))),
    "; drift reported:", model$drifted, "\n")
cat("PSI_DONE\n")
