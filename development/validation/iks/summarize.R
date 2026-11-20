# Summary of the IKS experiments: classification (Tables 2 and 3 of dos Reis
# et al., 2016, and the errata of the reference repository for Arabic) and
# timing (Figures 1 and 2).
#   Rscript development/validation/iks/summarize.R <results dir> <work dir>
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
work <- args[2]
letters_map <- c(A = "Arabic", B = "Posture", C = "Bike", D = "Keystroke", E = "Insects", F = "AbruptInsects")
columns <- c("NN BL1", "NN BL2", "NN MR", "NN AB", "NN TL", "NB BL1", "NB BL2", "NB MR", "NB MRP", "NB CD", "NB TL",
             "DT BL1", "DT BL2", "DT MR", "DT AT", "DT TL")
accuracy <- rbind(
  A = c(29.39, 29.59, 37.18, 29.31, 38.24, 17.28, 17.98, 20.11, 17.28, 18.15, 22.08, 19.95, 18.98, 22.83, 20.85, 23.47),
  B = c(40.84, 50.25, 53.95, 47.52, 53.89, 41.78, 44.96, 46.17, 45.62, 43.06, 45.74, 38.73, 45.60, 47.61, 48.02, 48.65),
  C = c(49.36, 67.04, 75.40, 75.87, 75.41, 48.33, 70.71, 70.60, 70.93, 69.75, 74.76, 58.21, 64.83, 65.42, 65.90, 72.88),
  D = c(59.75, 77.11, 84.56, 84.13, 84.88, 48.31, 71.22, 74.31, 71.81, 48.31, 79.94, 39.75, 70.79, 74.56, 76.25, 82.94),
  E = c(58.14, 71.67, 87.81, 82.01, 88.13, 54.01, 70.43, 77.99, 76.53, 68.38, 86.61, 49.93, 65.29, 71.06, 71.81, 79.91),
  F = c(57.86, 69.75, 79.87, 79.17, 80.06, 58.97, 69.23, 76.54, 77.22, 66.63, 79.42, 52.17, 59.61, 65.31, 65.30, 66.35))
labels <- rbind(
  A = c(5.68, 25.40, 28.41, 5.68, 100, 5.68, 25.67, 28.41, 5.67, 11.36, 100, 5.68, 25.51, 28.41, 14.91, 100),
  B = c(0.30, 5.91, 6.07, 0.91, 100, 0.30, 5.90, 6.07, 2.73, 0.61, 100, 0.30, 5.90, 6.07, 5.91, 100),
  C = c(0.58, 62.25, 98.39, 32.22, 100, 0.58, 62.54, 98.39, 74.80, 44.31, 100, 0.58, 62.22, 98.39, 92.13, 100),
  D = c(6.25, 12.15, 43.75, 43.75, 100, 6.25, 12.05, 43.75, 25.00, 6.25, 100, 6.25, 12.36, 37.50, 24.13, 100),
  E = c(1.88, 57.56, 84.51, 9.39, 100, 1.88, 57.40, 84.51, 39.44, 30.05, 100, 1.88, 57.64, 69.48, 60.90, 100),
  F = c(1.88, 21.84, 24.41, 9.39, 100, 1.88, 21.93, 24.41, 9.39, 5.63, 100, 1.88, 22.11, 13.15, 15.74, 100))
colnames(accuracy) <- colnames(labels) <- columns
# Errata of the repository (fixed Arabic, nearest neighbour): accuracy and labels.
errata <- data.frame(method = c("BL1", "MR", "AB", "TL"), accuracy = c(61.77, 78.11, 72.88, 78.24),
                     labels = c(5.68, 45.45, 11.36, 100))

res <- utils::read.csv(file.path(work, "classification_heimdall.csv"))
rows <- list()
for (l in names(letters_map)) for (col in columns) {
  parts <- strsplit(col, " ")[[1]]
  h <- res[res$dataset == letters_map[[l]] & res$classifier == parts[1] & res$method == parts[2], ]
  paper_acc <- accuracy[l, col]
  paper_lab <- labels[l, col]
  note <- ""
  if (l == "A" && parts[1] == "NN" && parts[2] %in% errata$method) {
    paper_acc <- errata$accuracy[errata$method == parts[2]]
    paper_lab <- errata$labels[errata$method == parts[2]]
    note <- "errata (fixed Arabic)"
  } else if (l == "A") {
    note <- "paper used the broken Arabic data (errata)"
  }
  rows[[length(rows) + 1L]] <- data.frame(
    data = l, dataset = letters_map[[l]], classifier = parts[1], method = parts[2],
    paper_accuracy = paper_acc, heimdall_accuracy = if (nrow(h)) round(h$accuracy, 2) else NA,
    paper_labels = paper_lab, heimdall_labels = if (nrow(h)) round(h$labels, 2) else NA,
    heimdall_retrainings = if (nrow(h)) h$retrainings else NA, note = note)
}
table <- do.call(rbind, rows)
utils::write.csv(table, file.path(dir, "classification_summary.csv"), row.names = FALSE)
print(table, right = FALSE)

# Agreement of the qualitative conclusions of Section 6.2.
ok <- table[!is.na(table$heimdall_accuracy), ]
wide <- function(v) {
  w <- reshape(ok[, c("data", "classifier", "method", v)], idvar = c("data", "classifier"), timevar = "method",
               direction = "wide")
  names(w) <- sub(paste0(v, "\\."), "", names(w))
  w
}
acc <- wide("heimdall_accuracy")
lab <- wide("heimdall_labels")
cat("\nMR more accurate than BL1:", sum(acc$MR > acc$BL1), "of", nrow(acc), "\n")
cat("MR more accurate than BL2:", sum(acc$MR > acc$BL2, na.rm = TRUE), "of", sum(!is.na(acc$BL2)), "\n")
cat("MR accuracy / TL accuracy (mean, %):", round(100 * mean(acc$MR / acc$TL), 2), "\n")
cat("MR labels (mean, %):", round(mean(lab$MR), 2), "\n")
same_labels <- table$method == "MR" & !is.na(table$heimdall_labels) & table$data != "A"
cat("MR: same percentage of labels as the paper:", sum(abs(table$paper_labels[same_labels] -
    table$heimdall_labels[same_labels]) < 0.01), "of", sum(same_labels), "(datasets B-F)\n")

timing <- file.path(dir, "timing_heimdall.csv")
if (file.exists(timing)) {
  t <- utils::read.csv(timing)
  s <- aggregate(seconds ~ experiment + size + method, t, mean)
  utils::write.csv(s, file.path(dir, "timing_summary.csv"), row.names = FALSE)
  print(reshape(s, idvar = c("experiment", "size"), timevar = "method", direction = "wide"), digits = 3)
}
