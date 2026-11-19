# Human Activity sequences of the WWAggr reference (HumanActivityDataset of
# src/datasets/datasets.py), built from the UCI "Smartphone-Based Recognition
# of Human Activities and Postural Transitions" data (HAPT, Train/ and Test/).
#   Rscript development/validation/wwaggr/prepare_har.R <hapt_dir> <out_dir>
#
# For every subject: "normal" sequences are the windows of 20 rows inside the
# segments of constant activity (rows of the postural transitions, labels 7 to
# 12, removed; every segment but the last), and sequences with a change are
# the windows of 20 rows around the start of every postural transition (from
# the end of the previous transition to the start of the next one, at most 20
# rows on each side), labelled 0 before the start and 1 from it. The features
# are the first 28 of the 30 "Mean" columns listed by the reference.
args <- commandArgs(trailingOnly = TRUE)
hapt <- args[1]
out_dir <- args[2]
seq_len <- 20L
features <- c("tBodyAcc-Mean-1", "tBodyAcc-Mean-2", "tBodyAcc-Mean-3", "tGravityAcc-Mean-1", "tGravityAcc-Mean-2",
              "tGravityAcc-Mean-3", "tBodyAccJerk-Mean-1", "tBodyAccJerk-Mean-2", "tBodyAccJerk-Mean-3",
              "tBodyGyro-Mean-1", "tBodyGyro-Mean-2", "tBodyGyro-Mean-3", "tBodyGyroJerk-Mean-1",
              "tBodyGyroJerk-Mean-2", "tBodyGyroJerk-Mean-3", "tBodyAccMag-Mean-1", "tGravityAccMag-Mean-1",
              "tBodyAccJerkMag-Mean-1", "tBodyGyroMag-Mean-1", "tBodyGyroJerkMag-Mean-1", "fBodyAcc-Mean-1",
              "fBodyAcc-Mean-2", "fBodyAcc-Mean-3", "fBodyGyro-Mean-1", "fBodyGyro-Mean-2", "fBodyGyro-Mean-3",
              "fBodyAccMag-Mean-1", "fBodyAccJerkMag-Mean-1", "fBodyGyroMag-Mean-1", "fBodyGyroJerkMag-Mean-1")[1:28]

load_part <- function(part) {
  dir <- file.path(hapt, if (part == "train") "Train" else "Test")
  x <- as.matrix(utils::read.table(file.path(dir, paste0("X_", part, ".txt"))))
  names <- gsub(" ", "", readLines(file.path(hapt, "features.txt")))
  colnames(x) <- names
  list(x = x[, features], labels = scan(file.path(dir, paste0("y_", part, ".txt")), quiet = TRUE),
       subject = scan(file.path(dir, paste0("subject_id_", part, ".txt")), quiet = TRUE))
}

build <- function(part) {
  data <- load_part(part)
  n <- nrow(data$x)
  windows <- list()
  labels <- list()
  add <- function(rows, lab) {
    for (j in seq_len(length(rows) - seq_len) - 1L) {
      windows[[length(windows) + 1L]] <<- data$x[rows[j + seq_len(seq_len)], , drop = FALSE]
      labels[[length(labels) + 1L]] <<- lab[j + seq_len(seq_len)]
    }
  }
  subjects <- unique(data$subject)
  # Normal sequences (all subjects first, as the reference concatenates normal + anomaly data).
  for (s in subjects) {
    rows <- which(data$subject == s & !(data$labels %in% 7:12))
    ends <- which(diff(data$labels[rows]) != 0)
    start <- 1L
    for (i in seq_len(length(ends) - 1L)) {
      end <- ends[i]
      segment <- rows[start:end]
      if (length(segment) > seq_len) add(segment, rep(0, length(segment)))
      start <- end + 1L
    }
  }
  # Sequences with a change.
  for (s in subjects) {
    rows <- which(data$subject == s)
    change_rows <- rows[data$labels[rows] %in% 7:12]
    starts <- c(change_rows[1], change_rows[which(diff(data$labels[change_rows]) != 0) + 1L])
    # 0-based row labels of the reference (pandas index) are row - 1.
    idx <- c(-1L, starts - 1L)
    for (i in seq(2L, length(idx) - 1L)) {
      if (length(idx) < 3L) break
      change <- idx[i]
      from <- max(idx[i - 1L] + 1L, change - seq_len)
      to <- min(idx[i + 1L] - 1L, change + seq_len)
      slice <- rows[(rows - 1L) >= from & (rows - 1L) <= to]
      position <- match(change, slice - 1L)
      lab <- numeric(length(slice))
      lab[position:length(slice)] <- 1
      if (length(slice) > seq_len) add(slice, lab)
    }
  }
  x <- aperm(array(unlist(windows), c(seq_len, length(features), length(windows))), c(3, 1, 2))
  dimnames(x) <- list(NULL, NULL, features)
  list(x = x, labels = do.call(rbind, labels))
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
har <- list(train = build("train"), test = build("test"))
saveRDS(har, file.path(out_dir, "har.rds"))
for (part in c("train", "test")) {
  cat(sprintf("%s: %d sequences, %d with a change\n", part, dim(har[[part]]$x)[1], sum(rowSums(har[[part]]$labels) > 0)))
}
