# Validation of dfr_qts() against Ramos et al. (2024), "Unsupervised drift
# detection using quadtree spatial mapping" (ESANN 2024), and its reference
# code (https://github.com/beraram/QTS-method), on the datasets bundled with
# the reference: Toy and RBF (synthetic, Table 1), Electricity and Weather
# (real-world, Table 2 and Fig. 2).
#
# Run from the package root, inside the devcontainer, after
# reference_qts.py has written results/qts/reference/ (see README.md):
#   Rscript development/validation/qts/run_qts.R [cores]
#
# 1. Equivalence: dfr_qts() and the reference loop on the same data must give
#    the same occupancy curve and the same detections.
# 2. Table 1: recall, precision and F1 with a delay range of 1000 samples, on
#    Toy (drifts at 5000, 10000 and 15000) and RBF (drift at 10000).
# 3. Detections on Electricity and Weather for the classifier pipeline of
#    Table 2 (evaluate_pipeline.py), and the occupancy curve of Fig. 2.

devtools::load_all(quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args) > 0) as.integer(args[1]) else 4L
data_dir <- file.path("data-real", "qts")
out_dir <- file.path("development", "validation", "results", "qts")
reference_dir <- file.path(out_dir, "reference")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

datasets <- c(Toy = "Toy", RBF = "RBF", Electricity = "Electricity_Norm", Weather = "Weather_Norm")
true_drifts <- list(Toy = c(5000, 10000, 15000), RBF = 10000)

# The data as QTS_method.py sees them: pandas takes the first row as the
# header, the features are min-max scaled over the whole file and the first
# remaining row only sets the dimension of the tree. Observation i of the
# stream is therefore sample i + 1 of the reference (n_samples).
load_dataset <- function(file) {
  raw <- utils::read.csv(file.path(data_dir, paste0(file, ".csv")), header = FALSE, strip.white = TRUE)
  raw <- raw[-1, , drop = FALSE]
  features <- raw[, -ncol(raw), drop = FALSE]
  # The arithmetic of sklearn's MinMaxScaler, so that values on a cell
  # boundary fall on the same side.
  features[] <- lapply(features, function(x) {
    span <- max(x) - min(x)
    scale <- if (span == 0) 1 else 1 / span
    x * scale + (-min(x) * scale)
  })
  names(features) <- paste0("x", seq_along(features))
  list(stream = features[-1, , drop = FALSE], label = raw[[ncol(raw)]])
}

run_one <- function(name) {
  data <- load_dataset(datasets[[name]])
  started <- Sys.time()
  code <- fit(dfr_qts(), data$stream)
  seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  paper <- fit(dfr_qts(derivative_rule = "paper"), data$stream)
  list(name = name, code = code, paper = paper, seconds = seconds, n = nrow(data$stream))
}

runs <- parallel::mclapply(names(datasets), run_one, mc.cores = cores)
names(runs) <- names(datasets)

# Delay-based metrics of Section 3: within the 1000 samples after a drift, the
# samples after the first detection are true positives and those before it
# false negatives; every other detection is a false positive.
delay_metrics <- function(detected, drifts, delay = 1000) {
  tp <- 0
  fn <- 0
  used <- logical(length(detected))
  found <- 0
  for (drift in drifts) {
    inside <- which((detected > drift) & (detected <= drift + delay))
    if (length(inside) == 0L) {
      fn <- fn + delay
      next
    }
    first <- inside[1]
    used[first] <- TRUE
    found <- found + 1
    tp <- tp + (drift + delay - detected[first])
    fn <- fn + (detected[first] - drift)
  }
  fp <- sum(!used)
  recall <- tp / (tp + fn)
  precision <- if (tp + fp > 0) tp / (tp + fp) else 0
  f1 <- if (recall + precision > 0) 2 * recall * precision / (recall + precision) else 0
  data.frame(recall = recall, precision = precision, f1 = f1, detections = length(detected),
             drifts = sprintf("%d/%d", found, length(drifts)))
}

equivalence <- list()
table1 <- list()
detections <- list()
for (name in names(runs)) {
  run <- runs[[name]]
  samples_code <- run$code$detections$observation + 1
  samples_paper <- run$paper$detections$observation + 1
  detections[[name]] <- rbind(
    data.frame(dataset = name, rule = "code", sample = samples_code, strategy = run$code$detections$strategy),
    data.frame(dataset = name, rule = "paper", sample = samples_paper, strategy = run$paper$detections$strategy)
  )

  reference_file <- file.path(reference_dir, paste0(datasets[[name]], "_detections.csv"))
  if (file.exists(reference_file)) {
    reference_samples <- utils::read.csv(reference_file)$sample
    reference_occupancy <- utils::read.csv(file.path(reference_dir, paste0(datasets[[name]], "_occupancy.csv")))$occupancy
    occupancy <- run$code$drifter_output$occupancy
    equivalence[[name]] <- data.frame(
      dataset = name, samples = run$n,
      occupancy_equal = (length(occupancy) == length(reference_occupancy)) && all(occupancy == reference_occupancy),
      max_occupancy_difference = max(abs(occupancy - reference_occupancy)),
      detections_r = paste(samples_code, collapse = " "),
      detections_reference = paste(reference_samples, collapse = " "),
      detections_equal = identical(as.numeric(samples_code), as.numeric(reference_samples)),
      seconds_r = round(run$seconds, 1)
    )
  }
  if (name %in% names(true_drifts)) {
    table1[[name]] <- rbind(
      cbind(dataset = name, rule = "code", delay_metrics(samples_code, true_drifts[[name]])),
      cbind(dataset = name, rule = "paper", delay_metrics(samples_paper, true_drifts[[name]]))
    )
  }
}

equivalence <- do.call(rbind, equivalence)
table1 <- do.call(rbind, table1)
detections <- do.call(rbind, detections)
utils::write.csv(equivalence, file.path(out_dir, "equivalence.csv"), row.names = FALSE)
utils::write.csv(table1, file.path(out_dir, "table1.csv"), row.names = FALSE)
utils::write.csv(detections, file.path(out_dir, "detections.csv"), row.names = FALSE)

cat("1. dfr_qts against the reference loop\n")
print(equivalence, row.names = FALSE)
cat("\n2. Table 1 (delay range 1000)\n")
print(format(table1, digits = 4), row.names = FALSE)
cat("\n3. Detections (sample positions)\n")
for (name in names(runs)) {
  for (rule in c("code", "paper")) {
    part <- detections[(detections$dataset == name) & (detections$rule == rule), ]
    cat(sprintf("%-11s %-5s %s\n", name, rule, paste(sprintf("%d(%s)", part$sample, substr(part$strategy, 1, 3)),
                                                     collapse = " ")))
  }
}

# Occupancy curves with the detections (Fig. 2 for Electricity).
grDevices::png(file.path(out_dir, "occupancy.png"), width = 1000, height = 700)
graphics::par(mfrow = c(2, 2))
for (name in names(runs)) {
  output <- runs[[name]]$code$drifter_output
  samples <- seq_len(nrow(output)) + 1
  graphics::plot(samples, output$occupancy, type = "l", col = "red", xlab = "Samples", ylab = "Occupancy", main = name)
  graphics::lines(samples, output$occupancy_ewm, col = "grey30")
  graphics::abline(v = runs[[name]]$code$detections$observation + 1, lty = 2, col = "darkgreen")
  if (name %in% names(true_drifts)) {
    graphics::abline(v = true_drifts[[name]], lty = 3, col = "blue")
  }
}
grDevices::dev.off()
