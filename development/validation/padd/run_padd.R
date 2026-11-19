# Validation of dfr_padd() against Komorniczak and Ksieniewicz (2024),
# "Unsupervised Concept Drift Detection based on Parallel Activations of Neural
# Network" (arXiv:2404.07776), and its code (https://github.com/w4k2/padd).
#
# Run from the package root, inside the devcontainer, after padd_reference.py
# (modes streams, trace and run):
#   Rscript development/validation/padd/run_padd.R [cores]
#
# 1. The drift detection errors (utils.dderror, analyze_e2.py) ported to R and
#    applied to the detections stored in res/exp1_comp_final.npy, against the
#    tables of the repository (Figure 6 of the paper).
# 2. Step-by-step equivalence: dfr_padd with the weights and the samples of
#    the instrumented reference (trace.json): p-values and detections.
# 3. Experiment 2 for PADD: dfr_padd (its own random draws) and the unchanged
#    reference class on the same 240 regenerated streams, against the paper.

devtools::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args) > 0) as.integer(args[1]) else 7L
ref_dir <- file.path("data-real", "padd", "reference")
work <- file.path("data-real", "padd", "work")
out_dir <- file.path("development", "validation", "results", "padd")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
features <- c(30, 60, 90)
spacings <- c(5, 999)
drifts <- c(3, 5, 10, 15)
n_chunks <- 250

# Minimal .npy reader (little-endian float64, C order).
read_npy <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  magic <- readBin(con, "raw", 6)
  version <- readBin(con, "raw", 2)
  header_length <- if (as.integer(version[1]) == 1L) readBin(con, "integer", 1, size = 2, signed = FALSE) else
    readBin(con, "integer", 1, size = 4)
  header <- rawToChar(readBin(con, "raw", header_length))
  stopifnot(grepl("'descr': '<f8'", header), grepl("'fortran_order': False", header))
  shape <- as.integer(strsplit(gsub(".*'shape': \\(([^)]*)\\).*", "\\1", header), ",\\s*")[[1]])
  values <- readBin(con, "double", prod(shape))
  # C order: reverse the dimensions and permute back.
  aperm(array(values, rev(shape)), rev(seq_along(shape)))
}

# get_real_drfs of analyze_e2.py.
real_drifts <- function(n_drifts) {
  starts <- seq(0, n_chunks, length.out = n_drifts + 1)[-(n_drifts + 1)]
  as.integer(starts + starts[2] / 2)
}

# dderror of utils.py (0-based chunk indices; no detection = every chunk).
dderror <- function(drift_idx, detection_idx) {
  if (length(detection_idx) == 0) detection_idx <- 0:(n_chunks - 1)
  distance <- abs(outer(drift_idx, detection_idx, "-"))
  c(d1 = mean(apply(distance, 2, min)), d2 = mean(apply(distance, 1, min)),
    r = abs(length(drift_idx) / length(detection_idx) - 1))
}

# Mean errors over the replications for a detections array [rep, feature, spacing, drifts, chunk].
errors_table <- function(detections) {
  rows <- list()
  for (c_id in seq_along(spacings)) for (d_id in seq_along(drifts)) for (f_id in seq_along(features)) {
    errs <- t(vapply(seq_len(dim(detections)[1]), function(r) {
      dderror(real_drifts(drifts[d_id]), which(detections[r, f_id, c_id, d_id, ] == 1) - 1L)
    }, numeric(3)))
    rows[[length(rows) + 1]] <- data.frame(stream = sprintf("%s %dD %dF", c("G", "S")[c_id], drifts[d_id],
                                                            features[f_id]),
                                           d1 = mean(errs[, "d1"]), d2 = mean(errs[, "d2"]), r = mean(errs[, "r"]),
                                           detections = mean(rowSums(matrix(detections[, f_id, c_id, d_id, ],
                                                                            dim(detections)[1]))))
  }
  do.call(rbind, rows)
}

# PADD column of the LaTeX tables of the repository.
read_table <- function(file) {
  lines <- readLines(file.path(ref_dir, "tables", file), warn = FALSE)
  lines <- grep("^ [GS] [0-9]+D", lines, value = TRUE)
  data.frame(stream = trimws(sub("&.*", "", lines)),
             value = as.numeric(vapply(strsplit(lines, "&"), function(p) trimws(p[4]), character(1))))
}
paper <- Reduce(function(a, b) merge(a, b, by = "stream"),
                list(stats::setNames(read_table("table_d1.txt"), c("stream", "d1_paper")),
                     stats::setNames(read_table("table_d2.txt"), c("stream", "d2_paper")),
                     stats::setNames(read_table("table_cnt_ratio.txt"), c("stream", "r_paper"))))

# 1. Metric port --------------------------------------------------------------------------------------
stored <- read_npy(file.path(ref_dir, "res", "exp1_comp_final.npy"))[, , , , 4, , drop = TRUE]
stored_errors <- errors_table(stored)
check <- merge(paper, stored_errors, by = "stream")
cat("1. Errors of the stored PADD detections against the tables of the repository (24 streams)\n")
cat(sprintf("largest difference: D1 %.2g, D2 %.2g, R %.2g (tables print 3 decimals)\n",
            max(abs(check$d1 - check$d1_paper)), max(abs(check$d2 - check$d2_paper)),
            max(abs(check$r - check$r_paper))))

# 2. Trace -----------------------------------------------------------------------------------------
trace <- jsonlite::fromJSON(file.path(work, "trace.json"), simplifyVector = FALSE)
read_stream <- function(name, n_f) {
  matrix(readBin(file.path(work, "streams", paste0(name, ".bin")), "double", n = 250 * 200 * n_f, size = 4),
         ncol = n_f, byrow = TRUE)
}
trace_rows <- list()
for (name in names(trace)) {
  t <- trace[[name]]
  weights <- lapply(t$weights, function(layer) do.call(rbind, lapply(layer, unlist)))
  x <- read_stream(name, 30)
  indices <- t$index
  position <- 0L
  # The reference draws, per output and replication, the past sample then the current one.
  draw <- function(output, n_past, n_current) {
    past <- matrix(0L, 12, 50)
    current <- matrix(0L, 12, 50)
    for (r in 1:12) {
      past[r, ] <- unlist(indices[[position + 1L]]) + 1L
      current[r, ] <- unlist(indices[[position + 2L]]) + 1L
      position <<- position + 2L
    }
    list(past = past, current = current)
  }
  obj <- dfr_padd(alpha = t$alpha, threshold = t$threshold, outputs = 12, replications = 12, sample_size = 50,
                  weights = weights)
  obj$state$features <- paste0("V", 1:30)
  p_values <- numeric(0)
  detections <- logical(0)
  for (chunk in seq_len(n_chunks)) {
    rows <- ((chunk - 1) * 200 + 1):(chunk * 200)
    before <- position
    current <- heimdall:::.padd_activations(obj$weights, x[rows, ])
    if (!is.null(obj$state$past)) {
      # p-values of this chunk, in the reference order (output, replication).
      for (output in 1:12) {
        index <- draw(output, nrow(obj$state$past), nrow(current))
        a <- matrix(obj$state$past[index$past, output], 12)
        b <- matrix(current[index$current, output], 12)
        p_values <- c(p_values, heimdall:::.padd_ttest(a, b))
      }
      position <- before
    }
    result <- heimdall:::.padd_chunk(obj, x[rows, ], draw)
    obj <- result$obj
    detections <- c(detections, result$drift)
  }
  reference_p <- vapply(t$p, function(v) if (is.null(v)) NA_real_ else v, numeric(1))
  trace_rows[[name]] <- data.frame(stream = name, tests = length(p_values), tests_reference = length(reference_p),
                                   p_max_difference = max(abs(p_values - reference_p), na.rm = TRUE),
                                   detections = sum(detections),
                                   detections_reference = sum(unlist(t$detections)),
                                   same_detections = identical(detections, unlist(t$detections)))
}
trace_check <- do.call(rbind, trace_rows)
utils::write.csv(trace_check, file.path(out_dir, "trace_comparison.csv"), row.names = FALSE)
cat("\n2. dfr_padd with the weights and samples of the reference (trace.json)\n")
print(trace_check, row.names = FALSE, digits = 3)

# 3. Experiment 2 -----------------------------------------------------------------------------------
settings <- list(c(alpha = 0.13, threshold = 0.26), c(alpha = 0.07, threshold = 0.19))
jobs <- expand.grid(rep = 0:9, f_id = 1:3, c_id = 1:2, d_id = 1:4)
started <- Sys.time()
run_job <- function(i) {
  job <- jobs[i, ]
  n_f <- features[job$f_id]
  x <- read_stream(sprintf("r%d_f%d_c%d_d%d", job$rep, n_f, spacings[job$c_id], drifts[job$d_id]), n_f)
  obj <- dfr_padd(alpha = settings[[job$c_id]][["alpha"]], threshold = settings[[job$c_id]][["threshold"]],
                  outputs = 12, replications = 12, sample_size = 50, seed = 1000 + i)
  obj$state$features <- paste0("V", seq_len(n_f))
  vapply(seq_len(n_chunks), function(chunk) {
    result <- heimdall:::.padd_chunk(obj, x[((chunk - 1) * 200 + 1):(chunk * 200), ])
    obj <<- result$obj
    as.numeric(result$drift)
  }, numeric(1))
}
runs <- parallel::mclapply(seq_len(nrow(jobs)), run_job, mc.cores = cores, mc.preschedule = FALSE)
# A job whose forked worker died is run again in the main process.
failed <- which(!vapply(runs, function(r) is.numeric(r) && length(r) == n_chunks, logical(1)))
if (length(failed) > 0) {
  cat(sprintf("\n%d job(s) rerun sequentially after a worker failure\n", length(failed)))
  runs[failed] <- lapply(failed, run_job)
}
seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
ours <- array(0, c(10, 3, 2, 4, n_chunks))
for (i in seq_len(nrow(jobs))) {
  job <- jobs[i, ]
  ours[job$rep + 1, job$f_id, job$c_id, job$d_id, ] <- runs[[i]]
}
rerun <- read_npy(file.path(work, "reference_detections.npy"))
combined <- Reduce(function(a, b) merge(a, b, by = "stream"), list(
  paper,
  stats::setNames(stored_errors, c("stream", "d1_stored", "d2_stored", "r_stored", "detections_stored")),
  stats::setNames(errors_table(rerun), c("stream", "d1_reference", "d2_reference", "r_reference",
                                         "detections_reference")),
  stats::setNames(errors_table(ours), c("stream", "d1_ours", "d2_ours", "r_ours", "detections_ours"))))
order_streams <- stored_errors$stream
combined <- combined[match(order_streams, combined$stream), ]
utils::write.csv(combined, file.path(out_dir, "experiment2.csv"), row.names = FALSE)
cat(sprintf("\n3. Experiment 2 (240 streams), dfr_padd in %.0f s on %d cores\n", seconds, cores))
print(combined[, c("stream", "d1_paper", "d1_reference", "d1_ours", "d2_paper", "d2_reference", "d2_ours",
                   "r_paper", "r_reference", "r_ours", "detections_stored", "detections_reference",
                   "detections_ours")], row.names = FALSE, digits = 3)
type <- substr(combined$stream, 1, 1)
summary <- do.call(rbind, lapply(c("G", "S"), function(k) {
  rows <- combined[type == k, ]
  data.frame(type = k, d1_paper = mean(rows$d1_paper), d1_reference = mean(rows$d1_reference),
             d1_ours = mean(rows$d1_ours), d2_paper = mean(rows$d2_paper), d2_reference = mean(rows$d2_reference),
             d2_ours = mean(rows$d2_ours), r_paper = mean(rows$r_paper), r_reference = mean(rows$r_reference),
             r_ours = mean(rows$r_ours))
}))
utils::write.csv(summary, file.path(out_dir, "experiment2_summary.csv"), row.names = FALSE)
cat("\nMeans over the 12 streams of each drift type\n")
print(summary, row.names = FALSE, digits = 3)

# fit() on rows gives the detections of the chunk loop.
x <- read_stream("r0_f30_c999_d10", 30)
model <- fit(dfr_padd(alpha = 0.07, threshold = 0.19, outputs = 12, replications = 12, sample_size = 50,
                      seed = 1), as.data.frame(x))
obj <- dfr_padd(alpha = 0.07, threshold = 0.19, outputs = 12, replications = 12, sample_size = 50, seed = 1)
obj$state$features <- paste0("V", 1:30)
loop <- vapply(seq_len(n_chunks), function(chunk) {
  result <- heimdall:::.padd_chunk(obj, x[((chunk - 1) * 200 + 1):(chunk * 200), ])
  obj <<- result$obj
  result$drift
}, logical(1))
fitted <- model$drifter_output$significant[seq(200, 50000, by = 200)] > 0.19
cat(sprintf("\nfit() on rows against the chunk loop: same detections %s\n",
            identical(fitted[-1], loop[-1]) && !loop[1]))
cat("PADD_DONE\n")
