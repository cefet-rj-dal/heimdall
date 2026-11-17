# Validation of dfr_llr2() against Sojo, Bielza and Larrañaga (2027), "A fully
# unsupervised concept drift detector via Bayesian networks for machine
# condition monitoring", Expert Systems with Applications 332, 133634, and the
# results of its evaluation repository (https://github.com/rafasj13/LLR2-Evaluation;
# the detector code itself is not public).
#
# Run from the package root, inside the devcontainer:
#   OPENBLAS_NUM_THREADS=1 Rscript development/validation/llr2/run_llr2.R [cores]
#
# Data: the sixteen synthetic drifted streams of the paper (Model 1-4 x sudden,
# incremental, incremental-recurring, gradual; datasets/synthetic2), 16000
# observations, drifts at the indices of Table 2.
# Settings of Appendix B: window (n_samples) 100, 250, 500; step 10, 50, 100;
# alpha 0.05; with and without the BH correction; baseline of 4096 observations
# (Section 5.1.2); "parameter" update (the paper's parameter-transfer ablation).
# Metrics of util_files/util_plot.py (calculate_drift_metrics, MTRd = MTR /
# drifts) and the best configuration per stream and method chosen as
# sort_mtrd_with_nan (MDR = 0 first, then MTRd, then MTD).

devtools::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args) > 0) as.integer(args[1]) else 6L
reference_dir <- file.path("data-real", "llr2", "reference")
out_dir <- file.path("development", "validation", "results", "llr2")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

types <- c(sudden = "S", incremental = "I", incremental_reocurring = "IR", gradual = "G")
truth <- list(S = c(5000, 7000, 8000, 8666, 9000, 13000, 15000, 15666), I = c(5000, 7000, 8000, 9000, 13000, 15000),
              IR = c(5000, 8000, 9000, 13000, 15000), G = c(5000, 7000, 8000, 9000, 13000, 15000))
streams <- expand.grid(model = 1:4, type = names(types), stringsAsFactors = FALSE)
streams$name <- sprintf("SPBN%d_%s", streams$model, streams$type)
streams$label <- sprintf("Model %d-%s", streams$model, types[streams$type])

# calculate_drift_metrics of util_plot.py.
drift_metrics <- function(known, detections) {
  detection_times <- numeric(0)
  false_detections <- numeric(0)
  was_drift <- FALSE
  previous <- 0
  for (t in sort(unique(c(known, detections)))) {
    if (t %in% known) {
      was_drift <- TRUE
      previous <- t
    }
    if (t %in% detections) {
      if (was_drift) detection_times <- c(detection_times, t - previous) else false_detections <- c(false_detections, t)
      was_drift <- FALSE
    }
  }
  mtfa <- if (length(false_detections) > 1) mean(diff(false_detections)) else NA_real_
  mtd <- if (length(detection_times)) mean(detection_times) else NA_real_
  mdr <- 1 - length(detection_times) / length(known)
  mtr <- mtfa / mtd * (1 - mdr)
  data.frame(mtd = mtd, mdr = mdr, mtfa = mtfa, fa = length(false_detections), drifts = length(detections),
             mtrd = if (length(detections)) mtr / length(detections) else NA_real_)
}

# sort_mtrd_with_nan of util_plot.py: the first row is the winner.
best_row <- function(frame) {
  candidates <- frame[is.na(frame$mtrd) & frame$mdr == 0, ]
  zeros <- frame[!is.na(frame$mtrd) & frame$mdr == 0, ]
  others <- frame[!(frame$mdr == 0), ]
  zeros <- zeros[order(-zeros$mtrd, zeros$mtd), ]
  others <- others[order(others$mdr, -others$mtrd, others$mtd, na.last = TRUE), ]
  candidates <- candidates[order(candidates$mtd), ]
  # Candidates (MDR 0, undefined MTRd) are placed before the first zero-MDR row
  # whose MTD is larger: one wins if its MTD is below that of the best row.
  if (nrow(zeros)) {
    if (nrow(candidates) && candidates$mtd[1] < zeros$mtd[1]) return(candidates[1, ])
    return(zeros[1, ])
  }
  if (nrow(candidates)) candidates[1, ] else others[1, ]
}

parse_drifts <- function(text) {
  text <- gsub("[][ ]", "", text)
  if (!nzchar(text) || text == "nan") return(numeric(0))
  as.numeric(strsplit(text, ",")[[1]])
}

# 1. dfr_llr2 on every stream and configuration -----------------------------------------------
configs <- expand.grid(window = c(100, 250, 500), step = c(10, 50, 100), fdr = c(FALSE, TRUE))
jobs <- merge(streams, configs, by = NULL)
runs <- parallel::mclapply(split(jobs, seq_len(nrow(jobs))), function(job) {
  data <- utils::read.csv(file.path(reference_dir, "datasets", "synthetic2", paste0(job$name, ".csv")), header = FALSE)
  names(data) <- LETTERS[seq_len(ncol(data))]
  started <- Sys.time()
  detector <- fit(dfr_llr2(n_baseline = 4096, window_size = job$window, step = job$step, alpha = 0.05, fdr = job$fdr,
                           update = "parameter"), data)
  seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  detections <- vapply(detector$drifts, `[[`, numeric(1), "observation")
  nodes <- vapply(detector$drifts, function(d) paste(d$nodes, collapse = " "), character(1))
  cbind(job[c("name", "label", "type", "window", "step", "fdr")], seconds = seconds,
        detections = paste(detections, collapse = " "), drift_nodes = paste(nodes, collapse = " | "),
        loglik = sum(detector$drifter_output$loglik, na.rm = TRUE), structure = paste(
          vapply(detector$network$variables, function(v) paste0(v, "<-", paste(detector$network$parents[[v]],
                                                                                 collapse = ",")), character(1)),
          collapse = " "), types = paste(detector$network$node_types, collapse = ""),
        drift_metrics(truth[[types[[job$type]]]], detections))
}, mc.cores = cores, mc.preschedule = FALSE)
runs <- do.call(rbind, runs)
runs$method <- ifelse(runs$fdr, "LLR2-BH", "LLR2")
utils::write.csv(runs, file.path(out_dir, "runs.csv"), row.names = FALSE)

# 2. Reference results: local update (csvs2, Tables 4-7) and parameter-transfer ablation ----------
reference <- do.call(rbind, lapply(seq_len(nrow(streams)), function(i) {
  s <- streams[i, ]
  read_one <- function(path, update) {
    if (!file.exists(path)) return(NULL)
    r <- utils::read.csv(path)
    do.call(rbind, lapply(seq_len(nrow(r)), function(j) {
      detections <- parse_drifts(r$drifts[j])
      m <- drift_metrics(truth[[types[[s$type]]]], detections)
      data.frame(name = s$name, label = s$label, type = s$type, update = update, window = r$n_samples[j],
                 step = r$step[j], fdr = as.logical(r$fdr_on[j]), detections = paste(detections, collapse = " "),
                 reference_mtd = r$mtd[j], reference_mdr = r$mdr[j], m)
    }))
  }
  rbind(read_one(file.path(reference_dir, "results", "csvs2", s$name, "LLR2_transfer5.csv"), "local"),
        read_one(file.path(reference_dir, "results", "ablation_experiments2", s$name, "LLR2_parameter-transfer.csv"),
                 "parameter"))
}))
reference$method <- ifelse(reference$fdr, "LLR2-BH", "LLR2")
utils::write.csv(reference, file.path(out_dir, "reference_runs.csv"), row.names = FALSE)
check <- reference[!is.na(reference$reference_mtd), ]
cat("Metric port against the MTD/MDR columns of the reference CSVs:",
    sprintf("max |MTD difference| %.3g, max |MDR difference| %.3g over %d rows\n",
            max(abs(check$mtd - check$reference_mtd), na.rm = TRUE), max(abs(check$mdr - check$reference_mdr)),
            nrow(check)))

# 3. Configuration by configuration: dfr_llr2 against the reference parameter-transfer runs ----------
paired <- merge(runs, reference[reference$update == "parameter", ], by = c("name", "window", "step", "fdr"),
                suffixes = c("", "_reference"))
paired$detected_true <- (1 - paired$mdr) * vapply(paired$type, function(t) length(truth[[types[[t]]]]), numeric(1))
paired$detected_true_reference <- (1 - paired$mdr_reference) *
  vapply(paired$type, function(t) length(truth[[types[[t]]]]), numeric(1))
pair_summary <- do.call(rbind, lapply(split(paired, list(paired$type, paired$fdr)), function(p) {
  data.frame(type = types[[p$type[1]]], method = ifelse(p$fdr[1], "LLR2-BH", "LLR2"), configurations = nrow(p),
             mdr = mean(p$mdr), mdr_reference = mean(p$mdr_reference), mtd = mean(p$mtd, na.rm = TRUE),
             mtd_reference = mean(p$mtd_reference, na.rm = TRUE), fa = mean(p$fa), fa_reference = mean(p$fa_reference),
             drifts = mean(p$drifts), drifts_reference = mean(p$drifts_reference))
}))
pair_summary <- pair_summary[order(match(pair_summary$type, types), pair_summary$method), ]
utils::write.csv(pair_summary, file.path(out_dir, "configurations_vs_reference.csv"), row.names = FALSE)
cat("\nMean over the 9 configurations x 4 models: dfr_llr2 (parameter update) against the reference",
    "parameter-transfer runs\n")
print(format(pair_summary, digits = 3), row.names = FALSE)

# 4. Tables 4-7: best configuration per stream and method ------------------------------------------
winners <- function(frame) do.call(rbind, lapply(split(frame, list(frame$name, frame$method), drop = TRUE), best_row))
ours <- winners(runs)
theirs_local <- winners(reference[reference$update == "local", ])
theirs_parameter <- winners(reference[reference$update == "parameter", ])
paper <- utils::read.csv(text = "label,method,mtrd,mdr,mtd,mtfa,fa,drifts
Model 1-S,LLR2,NA,0,22.25,NA,0,8
Model 1-S,LLR2-BH,NA,0,58.50,NA,0,8
Model 2-S,LLR2,NA,0,8.50,NA,0,8
Model 2-S,LLR2-BH,NA,0,13.50,NA,0,8
Model 3-S,LLR2,NA,0,57.25,NA,0,8
Model 3-S,LLR2-BH,NA,0.12,29.71,NA,0,7
Model 4-S,LLR2,NA,0,11.00,NA,0,8
Model 4-S,LLR2-BH,NA,0,21.00,NA,0,8
Model 1-I,LLR2,1.19,0,358.33,3825.00,3,9
Model 1-I,LLR2-BH,1.47,0,416.67,4900.00,2,8
Model 2-I,LLR2,0.70,0,383.33,2700.00,4,10
Model 2-I,LLR2-BH,1.27,0,466.67,4740.00,2,8
Model 3-I,LLR2,1.08,0,400.00,3900.00,3,9
Model 3-I,LLR2-BH,0.91,0,353.33,3206.67,4,10
Model 4-I,LLR2,0.51,0,366.67,2062.50,5,11
Model 4-I,LLR2-BH,1.15,0,475.00,4900.00,3,9
Model 1-IR,LLR2,0.79,0,380.00,2400.00,3,8
Model 1-IR,LLR2-BH,0.81,0,366.00,2370.00,3,8
Model 2-IR,LLR2,0.61,0,326.00,1982.50,5,10
Model 2-IR,LLR2-BH,1.68,0,210.00,3166.67,4,9
Model 3-IR,LLR2,0.95,0,460.00,3500.00,3,8
Model 3-IR,LLR2-BH,0.50,0,510.00,2025.00,3,8
Model 4-IR,LLR2,2.70,0,310.00,5850.00,2,7
Model 4-IR,LLR2-BH,0.31,0,470.00,1030.00,2,7
Model 1-G,LLR2,4.08,0,133.33,4900.00,3,9
Model 1-G,LLR2-BH,3.42,0,140.00,4310.00,3,9
Model 2-G,LLR2,3.05,0,185.00,4520.00,2,8
Model 2-G,LLR2-BH,NA,0,278.33,NA,0,6
Model 3-G,LLR2,2.92,0,163.33,4290.00,3,9
Model 3-G,LLR2-BH,NA,0,310.00,NA,1,7
Model 4-G,LLR2,1.73,0,250.00,3900.00,3,9
Model 4-G,LLR2-BH,NA,0,285.00,NA,1,7")
keep <- c("label", "method", "window", "step", "mdr", "mtd", "fa", "drifts", "mtrd")
table <- rbind(cbind(source = "dfr_llr2", ours[keep]), cbind(source = "reference (parameter)", theirs_parameter[keep]),
               cbind(source = "reference (local)", theirs_local[keep]),
               cbind(source = "paper", window = NA, step = NA, paper[c("label", "method", "mdr", "mtd", "fa", "drifts",
                                                                     "mtrd")])[c("source", keep)])
order_labels <- unlist(lapply(types, function(t) sprintf("Model %d-%s", 1:4, t)))
table <- table[order(match(table$label, order_labels), table$method,
                     match(table$source, c("dfr_llr2", "reference (parameter)", "reference (local)", "paper"))), ]
utils::write.csv(table, file.path(out_dir, "tables_4_7.csv"), row.names = FALSE)
cat("\nTables 4-7: best configuration per stream and method\n")
print(format(table, digits = 3), row.names = FALSE)
summary_type <- do.call(rbind, lapply(split(table, list(table$source, substring(table$label, 9), table$method),
                                            drop = TRUE), function(p) {
  data.frame(source = p$source[1], type = substring(p$label[1], 9), method = p$method[1], mdr = mean(p$mdr),
             mtd = mean(p$mtd, na.rm = TRUE), fa = mean(p$fa), drifts = mean(p$drifts))
}))
summary_type <- summary_type[order(match(summary_type$type, types), summary_type$method,
                                   match(summary_type$source, c("dfr_llr2", "reference (parameter)",
                                                                "reference (local)", "paper"))), ]
utils::write.csv(summary_type, file.path(out_dir, "tables_4_7_by_type.csv"), row.names = FALSE)
cat("\nMeans over the four models of each drift type\n")
print(format(summary_type, digits = 3), row.names = FALSE)
cat("Seconds per run of dfr_llr2:", round(mean(runs$seconds), 1), "\n")
cat("LLR2_DONE\n")
