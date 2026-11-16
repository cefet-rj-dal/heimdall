# Validation of the benchmark generator (dsg_*) against the experiments of
# Hinder et al. (2026), "Real vs. Virtual Drift: Creating Realistic Stream
# Learning Benchmarks" (Section 3, Fig. 2), following Run_Benchmarks.py and
# Analyze.ipynb of https://github.com/FabianHinder/Creating-Drift-Benchmarks.
#
# For each data source and drift type ("v" or "r"), every repetition draws a
# stream "---<type>-" of 5 segments of 200 points:
#   train   = segments 1-2 (400 points)       test_n = segment 3 (no drift)
#   test_d  = segment 4 (after the drift)     extra  = segment 5 (drifted)
# Each model is trained on `train` ("s") and on `train` + `extra` ("e") and
# scored with the Matthews correlation coefficient (MCC). Fig. 2 reports, per
# source, model and drift type, the share of repetitions where
#   (a) MCC(test_n) >= MCC(test_d)            for the model trained on "s";
#   (b) MCC("e" model) <= MCC("s" model)      on test_n.
#
# The streams are generated here, in R, with the dsg_* functions. The models
# are fitted by evaluate_models.py with the scikit-learn classifiers and
# settings of Run_Benchmarks.py, so the only part that differs from the
# reference is the stream generator under validation.
#
# Run from the package root, inside the devcontainer:
#   Rscript development/validation/drift_benchmarks/run_benchmarks.R [reps] [cores] [sources] [output dir]
# e.g. Rscript ... 500 8 Electricity,SEA

devtools::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
n_reps <- if (length(args) > 0) as.integer(args[1]) else 500L
n_cores <- if (length(args) > 1) as.integer(args[2]) else max(1L, parallel::detectCores() - 1L)
selected <- if (length(args) > 2 && nzchar(args[3])) strsplit(args[3], ",")[[1]] else NULL
out_dir <- if (length(args) > 3) args[4] else file.path("development", "validation", "results", "drift_benchmarks")
stream_dir <- file.path(out_dir, "streams")
dir.create(stream_dir, showWarnings = FALSE, recursive = TRUE)
data_dir <- file.path("data-real", "drift_benchmarks")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

# Data sources ------------------------------------------------------------------
# Real-world streams as bundled with the reference code (and as configured in
# its gen_data.py). ExtraTrees has no counterpart in the installed packages, so
# every labelling model is a random forest with the reference's leaf size.
read_stream <- function(file, drop = NULL) {
  path <- file.path(data_dir, file)
  if (!file.exists(path)) {
    utils::download.file(
      sprintf("https://raw.githubusercontent.com/FabianHinder/Creating-Drift-Benchmarks/main/stream_ds/%s", file),
      path, mode = "wb", quiet = TRUE
    )
  }
  data <- utils::read.csv(xzfile(path))
  data[setdiff(names(data), c("X", drop))]
}
real_world <- list(
  Electricity = list(file = "electricity.csv.xz", drop = "date", target = "class", real = 3, virtual = 2, nodesize = 10),
  ForestCover = list(file = "forestCoverType.csv.xz", drop = "Id", target = "Cover_Type", real = 3, virtual = 2, nodesize = 3),
  Weather = list(file = "NEweather.csv.xz", drop = NULL, target = "y", real = 3, virtual = 3, nodesize = 3),
  Http = list(file = "kdd99_http.csv.xz", drop = NULL, target = "service", real = 3, virtual = 2, nodesize = 3),
  CreditCard = list(file = "creditcard.csv.xz", drop = "Time", target = "Class", real = 2, virtual = 3, nodesize = 3)
)
# As in Analyze.ipynb: natural real drift for three streams, and label flips
# (k-means based, n_clust = 20) for all five.
sources <- c(
  Agrawal = "synthetic", Mixed = "synthetic", SEA = "synthetic", Sine = "synthetic",
  Electricity = "real-world", ForestCover = "real-world", Weather = "real-world",
  ElectricityFlip = "real-world", CreditCardFlip = "real-world", ForestCoverFlip = "real-world",
  HttpFlip = "real-world", WeatherFlip = "real-world"
)
if (!is.null(selected)) sources <- sources[names(sources) %in% selected]

build_real_world <- function(name) {
  flip <- grepl("Flip$", name)
  cfg <- real_world[[sub("Flip$", "", name)]]
  data <- read_stream(cfg$file, cfg$drop)
  if (!is.numeric(data[[cfg$target]])) data[[cfg$target]] <- as.integer(factor(data[[cfg$target]])) - 1L
  dsg_source_data(data, cfg$target, real_blocks = cfg$real, virtual_blocks = cfg$virtual,
                  nodesize = cfg$nodesize, n_clusters = if (flip) 20 else 0)
}
synthetic_builders <- list(Agrawal = dsg_agrawal, Mixed = dsg_mixed, SEA = dsg_sea, Sine = dsg_sine)

# One stream (generate_stream of Run_Benchmarks.py) ---------------------------------
generate <- function(source, rep_id, type) {
  source$state <- sample.int(nrow(source$states), 1L)
  # As in the reference, a drift that falls back to the strongest candidate
  # (with a warning) is kept, and flagged as not verified; a drift that cannot
  # be created at all skips the repetition.
  verified <- TRUE
  stream <- tryCatch(
    withCallingHandlers(
      dsg_stream(source, paste0("---", type, "-"), batch_size = 200),
      warning = function(w) {
        verified <<- FALSE
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  if (is.null(stream)) return(NULL)
  y <- stream$data$y
  # Same validity check as generate_stream: in each of the first three splits,
  # at least two classes and more than 10 points per class.
  splits <- list(1:400, 401:600, 601:800)
  valid <- all(vapply(splits, function(rows) {
    counts <- table(y[rows])
    length(counts) > 1 && min(counts) > 10
  }, logical(1)))
  if (!valid) return(NULL)
  cbind(rep = rep_id, drift_type = type, verified = verified, stream$data)
}

# Streams --------------------------------------------------------------------------
# The reference creates a new source for every repetition. Synthetic sources are
# cheap and are rebuilt every time. For the real-world streams, whose labelling
# model is a random forest on the whole stream, a few independent sources are
# built (5, or 2 for the large Http and CreditCard streams), one at a time, and
# the repetitions are spread over them.
large <- c("Http", "CreditCard")
n_builds_for <- function(name) if (sub("Flip$", "", name) %in% large) 2L else 5L
# Each forked worker ends up with its own copy of the forest (about 1 GB for
# the flipped Electricity stream), so the real-world sources use at most 4
# workers. The forests of the large streams do not fit in memory more than
# once (7.6 GB in the devcontainer), so those run without forking.
cores_for <- function(name) {
  if (sources[[name]] == "synthetic") n_cores
  else if (sub("Flip$", "", name) %in% large) 1L
  else min(n_cores, 4L)
}
tasks <- expand.grid(rep = seq_len(n_reps), type = c("v", "r"), stringsAsFactors = FALSE)
for (name in names(sources)) {
  stream_file <- file.path(stream_dir, paste0(name, ".csv.gz"))
  quality_file <- file.path(stream_dir, paste0(name, "_quality.csv"))
  if (file.exists(stream_file)) {
    cat(sprintf("%s: streams already generated\n", name))
    next
  }
  start <- Sys.time()
  seed <- match(name, names(sources))
  run <- function(ids, build) {
    parallel::mclapply(ids, function(i) {
      set.seed(1000L * seed + i)
      source <- if (is.null(build)) synthetic_builders[[name]]() else build
      generate(source, tasks$rep[i], tasks$type[i])
    }, mc.cores = cores_for(name), mc.preschedule = FALSE)
  }
  if (sources[[name]] == "synthetic") {
    parts <- run(seq_len(nrow(tasks)), NULL)
  } else {
    parts <- list()
    quality <- list()
    n_builds <- n_builds_for(name)
    for (b in seq_len(n_builds)) {
      set.seed(seed * 100L + b)
      build <- build_real_world(name)
      quality[[b]] <- data.frame(source = name, build = b, oob_accuracy = build$quality$oob_accuracy,
                                 block_importance = build$quality$block_importance)
      parts <- c(parts, run(which((tasks$rep - 1L) %% n_builds + 1L == b), build))
      rm(build)
      gc()
    }
    utils::write.csv(do.call(rbind, quality), quality_file, row.names = FALSE)
  }
  failed <- vapply(parts, function(p) inherits(p, "try-error"), logical(1))
  if (any(failed)) stop(sprintf("%s: %d workers failed: %s", name, sum(failed), as.character(parts[failed][[1]])))
  streams <- do.call(rbind, parts)
  connection <- gzfile(paste0(stream_file, ".tmp"), "w")
  utils::write.csv(streams, connection, row.names = FALSE)
  close(connection)
  file.rename(paste0(stream_file, ".tmp"), stream_file)
  first <- !duplicated(streams[c("rep", "drift_type")])
  cat(sprintf("%s: %d valid streams of %d (%d with an unverified drift), %.0f s\n", name, sum(first), nrow(tasks),
              sum(!streams$verified[first]), as.numeric(difftime(Sys.time(), start, units = "secs"))))
}

# Models (Run_Benchmarks.py, scikit-learn) ------------------------------------------
status <- system2("python3", c(file.path("development", "validation", "drift_benchmarks", "evaluate_models.py"),
                               out_dir, n_cores))
if (status != 0) stop("evaluate_models.py failed")
score_files <- file.path(out_dir, "scores", paste0(names(sources), ".csv"))
results <- do.call(rbind, lapply(score_files[file.exists(score_files)], utils::read.csv))
quality_files <- file.path(stream_dir, paste0(names(sources), "_quality.csv"))
quality <- do.call(rbind, lapply(quality_files[file.exists(quality_files)], utils::read.csv))
if (!is.null(quality)) utils::write.csv(quality, file.path(out_dir, "labelling_model_quality.csv"), row.names = FALSE)
unverified <- do.call(rbind, lapply(names(sources), function(name) {
  path <- file.path(stream_dir, paste0(name, ".csv.gz"))
  if (!file.exists(path)) return(NULL)
  s <- utils::read.csv(path)[c("rep", "drift_type", "verified")]
  s <- s[!duplicated(s[c("rep", "drift_type")]), ]
  stats::aggregate(list(valid_streams = s$verified >= 0, verified = s$verified), list(source = rep(name, nrow(s)), drift_type = s$drift_type), sum)
}))
utils::write.csv(unverified, file.path(out_dir, "streams_summary.csv"), row.names = FALSE)

# Analysis (Analyze.ipynb) ----------------------------------------------------------
results$source_type <- unname(sources[results$source])
results <- results[!is.na(results$source_type), ]
s <- results[results$train_type == "s", ]
fig2a <- stats::aggregate(list(effect = s$mcc_no_drift >= s$mcc_drift),
                          s[c("source", "source_type", "model", "drift_type")], mean, na.rm = TRUE)
paired <- merge(
  results[results$train_type == "s", c("source", "source_type", "rep", "drift_type", "model", "mcc_no_drift")],
  results[results$train_type == "e", c("source", "rep", "drift_type", "model", "mcc_no_drift")],
  by = c("source", "rep", "drift_type", "model"), suffixes = c("_s", "_e")
)
fig2b <- stats::aggregate(list(effect = paired$mcc_no_drift_e <= paired$mcc_no_drift_s),
                          paired[c("source", "source_type", "model", "drift_type")], mean, na.rm = TRUE)
utils::write.csv(fig2a, file.path(out_dir, "fig2a_per_source.csv"), row.names = FALSE)
utils::write.csv(fig2b, file.path(out_dir, "fig2b_per_source.csv"), row.names = FALSE)

medians <- function(fig) {
  m <- stats::aggregate(effect ~ model + source_type + drift_type, fig, stats::median)
  stats::reshape(m, idvar = c("model", "source_type"), timevar = "drift_type", direction = "wide")
}
cat("\nFig. 2a: P(decrease) between training and drifted test, median over sources\n")
print(format(medians(fig2a), digits = 3), row.names = FALSE)
cat("\nFig. 2b: P(decrease) from adding drifted samples to the training set, median over sources\n")
print(format(medians(fig2b), digits = 3), row.names = FALSE)
utils::write.csv(medians(fig2a), file.path(out_dir, "fig2a_medians.csv"), row.names = FALSE)
utils::write.csv(medians(fig2b), file.path(out_dir, "fig2b_medians.csv"), row.names = FALSE)

# Box plots laid out as Fig. 2: per model, real-world then synthetic, real (blue)
# then virtual (green) drift.
plot_fig2 <- function(fig, title, file, hline = NULL) {
  order <- expand.grid(drift_type = c("r", "v"), source_type = c("real-world", "synthetic"), model = c("LR", "MLP", "RF", "SVM", "kNN"), stringsAsFactors = FALSE)
  groups <- lapply(seq_len(nrow(order)), function(i) fig$effect[fig$model == order$model[i] & fig$source_type == order$source_type[i] & fig$drift_type == order$drift_type[i]])
  grDevices::png(file.path(out_dir, file), width = 900, height = 520)
  colors <- ifelse(order$drift_type == "r", "#1F77B4", "#2CA02C")
  graphics::boxplot(groups, border = colors, names = rep("", length(groups)), main = title, ylab = "P(performance decrease)", ylim = c(0, 1))
  shaded <- which(order$source_type == "real-world" & order$drift_type == "r")
  graphics::rect(shaded - 0.5, -1, shaded + 1.5, 2, col = grDevices::adjustcolor("grey", 0.25), border = NA)
  graphics::boxplot(groups, border = colors, names = rep("", length(groups)), add = TRUE, ylim = c(0, 1))
  for (i in seq(1, length(groups), by = 2)) {
    graphics::lines(c(i, i + 1), c(stats::median(groups[[i]]), stats::median(groups[[i + 1]])), lty = 2, col = "orange")
  }
  if (!is.null(hline)) graphics::abline(h = hline, col = "grey50")
  graphics::axis(1, at = seq(1.5, length(groups), by = 2), labels = rep(c("RW", "synth."), 5), tick = FALSE, line = -0.5)
  graphics::axis(1, at = seq(2.5, length(groups), by = 4), labels = c("Log.Reg", "MLP", "RF", "SVM (lin.)", "k-NN"), tick = FALSE, line = 1)
  graphics::legend("bottomleft", c("real drift", "virtual drift"), col = c("#1F77B4", "#2CA02C"), lty = 1, bty = "n")
  grDevices::dev.off()
}
plot_fig2(fig2a, "(a) Decrease induced by drift between train and test", "fig2a.png")
plot_fig2(fig2b, "(b) Decrease induced by impurity in the training set", "fig2b.png", hline = 0.5)
