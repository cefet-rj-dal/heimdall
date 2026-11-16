# Step-by-step comparison of dfr_mcd() with the reference code: replays, in R,
# the draws recorded by reference_trace.py (sample-set indices and noise) from
# the same initial weights, and compares the test distances, the thresholds
# and the encoder weights after every window.
#   Rscript development/validation/mcd/check_trace.R <trace.json> [<trace.json> ...]

devtools::load_all(quiet = TRUE)
out_dir <- file.path("development", "validation", "results", "mcd")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
as_matrix <- function(x) do.call(rbind, lapply(x, unlist))
as_params <- function(w) list(W1 = as_matrix(w$W1), b1 = unlist(w$b1), W2 = as_matrix(w$W2), b2 = unlist(w$b2))

files <- commandArgs(trailingOnly = TRUE)
results <- do.call(rbind, lapply(files, function(file) {
  trace <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  name <- sub("^trace_", "", sub("\\.json$", "", basename(file)))
  data_name <- sub("_float(32|64)$", "", name)
  data <- as.matrix(utils::read.csv(file.path("data-real", "mcdd", "csv", paste0(data_name, ".csv")), header = FALSE))
  settings <- dfr_mcd(window_size = trace$win_size, m = trace$m, hidden_size = trace$hidden,
                      output_size = trace$output)$state$settings
  params <- as_params(trace$init)
  net <- list(params = params, adam = list(m = lapply(params, function(p) p * 0),
                                           v = lapply(params, function(p) p * 0), t = 0L))
  slide <- trace$win_size %/% 10
  do.call(rbind, lapply(seq_along(trace$windows), function(i) {
    record <- trace$windows[[i]]
    window <- data[(i - 1) * slide + seq_len(trace$win_size), , drop = FALSE]
    distance_difference <- NA_real_
    decision_equal <- NA
    if (!is.null(record$distances)) {
      ours <- heimdall:::.mcd_test(net, window, settings, sets = lapply(record$test_sets, unlist))
      theirs <- unlist(record$distances)
      distance_difference <- max(abs(ours - theirs))
      decision_equal <- (ours[9] > threshold_previous) == (theirs[9] > record_previous$threshold)
    }
    draws <- list(sets = lapply(record$sets, unlist), weak = lapply(record$weak, as_matrix),
                  strong = as_matrix(record$strong), threshold_sets = lapply(record$threshold_sets, unlist))
    trained <- heimdall:::.mcd_train(net, window, settings, draws = draws)
    reference <- as_params(record$weights)
    weight_difference <- max(vapply(names(reference), function(p) max(abs(trained$net$params[[p]] - reference[[p]])),
                                    numeric(1)))
    net <<- trained$net
    threshold_previous <<- trained$threshold
    record_previous <<- record
    data.frame(trace = name, window = i, start = record$start, distance_difference = distance_difference,
               decision_equal = decision_equal, threshold = trained$threshold, threshold_reference = record$threshold,
               threshold_difference = abs(trained$threshold - record$threshold),
               weight_difference = weight_difference)
  }))
}))
utils::write.csv(results, file.path(out_dir, "trace_comparison.csv"), row.names = FALSE)
print(format(results, digits = 3), row.names = FALSE)
