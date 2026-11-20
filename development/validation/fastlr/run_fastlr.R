# Experiments of Tatti (2019), "Fast likelihood-based change point detection"
# (Section 7), with heimdall's dfr_fastlr() / fastlr_split().
#   Rscript development/validation/fastlr/run_fastlr.R tau <out dir>
#   Rscript development/validation/fastlr/run_fastlr.R eps <out dir> [cores]
#   Rscript development/validation/fastlr/run_fastlr.R hill <out dir> [cores]
#
# Synthetic sequences of the paper (200 000 entries each):
#   Ind:   Bern(1/2);
#   Step:  10 000 x Bern(1/4) then 10 000 x Bern(3/4), repeated 10 times
#          (19 true change points);
#   Slope: 10 segments of 10 000 x Bern(p), p increasing linearly from 1/4 to 3/4,
#          then 10 000 x Bern(p), p decreasing from 3/4 to 1/4;
#   Hill:  10 sequences of 100 000 .. 1 000 000 entries, p increasing linearly
#          from 1/4 to 3/4 (threshold raised so that no change is declared).
# tau  (Figure 1a, 1b): Step, eps = 0, tau = 0.5 .. 10: average delay to a true
#      change and number of declared changes.
# eps  (Figures 1c and 2): Ind, Step, Slope, tau = 6, eps = 0 .. 0.9: after every
#      entry the approximate score g and the optimum OPT on the same window
#      (minimum and average g / OPT over the entries with OPT > 0), tested
#      change points / window size and / blocks, candidates of FindCands /
#      blocks (both as the mean of the per-entry ratios and as total / total),
#      running time (a separate run without OPT) and, for Step, the delay.
# hill (Figure 3): Hill, eps = 0, 0.1, 0.5, 0.9: running time, tested change
#      points / blocks and candidates of FindCands / blocks.
# blocks = border indices of the sequence plus those of the flipped sequence.
args <- commandArgs(trailingOnly = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))
mode <- args[1]
out <- args[2]
cores <- if (length(args) >= 3) as.integer(args[3]) else 1L
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sequence <- function(name, seed = 1) {
  set.seed(seed)
  switch(name,
    Ind = rbinom(200000, 1, 0.5),
    Step = unlist(lapply(1:10, function(i) c(rbinom(10000, 1, 0.25), rbinom(10000, 1, 0.75)))),
    Slope = unlist(lapply(1:10, function(i) rbinom(20000, 1, c(seq(0.25, 0.75, length.out = 10000),
                                                               seq(0.75, 0.25, length.out = 10000))))))
}
step_changes <- 10000 * (1:19) + 1

# Runs the detector over x; returns the alarms, change points and per-entry metrics.
run <- function(x, tau, eps, optimum = FALSE) {
  n <- length(x)
  st <- heimdall:::.fastlr_state(tau, eps)
  alarms <- integer(0)
  cps <- numeric(0)
  g <- opt <- if (optimum) numeric(n) else NULL
  tests <- cands <- window <- blocks <- numeric(n)
  start <- proc.time()[3]
  for (t in seq_len(n)) {
    r <- heimdall:::.fastlr_step(st, x[t], optimum)
    st <- r$state
    if (r$drift) {
      alarms <- c(alarms, t)
      cps <- c(cps, st$change_point)
    }
    tests[t] <- st$tests
    cands[t] <- st$cands
    window[t] <- st$window
    blocks[t] <- st$blocks
    if (optimum) {
      g[t] <- st$score
      opt[t] <- st$optimum
    }
  }
  list(alarms = alarms, change_points = cps, seconds = proc.time()[3] - start, g = g, opt = opt, tests = tests, cands = cands,
       window = window, blocks = blocks)
}

delays <- function(alarms, changes, n) {
  ends <- c(changes[-1], n + 1)
  d <- mapply(function(c, e) {
    a <- alarms[alarms >= c & alarms < e]
    if (length(a)) a[1] - c + 1 else NA
  }, changes, ends)
  c(mean_delay = mean(d, na.rm = TRUE), missed = sum(is.na(d)))
}

if (mode == "tau") {
  x <- sequence("Step")
  taus <- c(0.5, seq(1, 10, by = 0.5))
  rows <- parallel::mclapply(taus, function(tau) {
    r <- run(x, tau, 0)
    d <- delays(r$alarms, step_changes, length(x))
    data.frame(tau = tau, changes = length(r$alarms), mean_delay = d[["mean_delay"]], missed = d[["missed"]],
               seconds = r$seconds)
  }, mc.cores = cores)
  res <- do.call(rbind, rows)
  utils::write.csv(res, file.path(out, "tau_step.csv"), row.names = FALSE)
  print(res)
} else if (mode == "eps") {
  grid <- expand.grid(eps = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), data = c("Ind", "Step", "Slope"),
                      stringsAsFactors = FALSE)
  rows <- parallel::mclapply(seq_len(nrow(grid)), function(i) {
    x <- sequence(grid$data[i])
    eps <- grid$eps[i]
    timed <- run(x, 6, eps)
    r <- run(x, 6, eps, optimum = TRUE)
    stopifnot(identical(timed$alarms, r$alarms))
    ok <- r$opt > 0
    ratio <- r$g[ok] / r$opt[ok]
    d <- if (grid$data[i] == "Step") delays(r$alarms, step_changes, length(x)) else c(mean_delay = NA, missed = NA)
    cat(sprintf("%s eps %.1f: %d changes, min ratio %.4f, %.0f s\n", grid$data[i], eps, length(r$alarms),
                min(ratio), timed$seconds))
    data.frame(data = grid$data[i], eps = eps, changes = length(r$alarms), mean_delay = d[["mean_delay"]],
               missed = d[["missed"]], min_ratio = min(ratio), avg_ratio = mean(ratio),
               tests_per_n = mean(r$tests / r$window), tests_per_k = mean(r$tests / r$blocks),
               cands_per_k = mean(r$cands / r$blocks), total_tests_per_n = sum(r$tests) / sum(r$window),
               total_cands_per_k = sum(r$cands) / sum(r$blocks),
               seconds = timed$seconds)
  }, mc.cores = cores, mc.preschedule = FALSE)
  res <- do.call(rbind, rows)
  utils::write.csv(res, file.path(out, "eps_synthetic.csv"), row.names = FALSE)
  print(res)
} else if (mode == "hill") {
  grid <- expand.grid(eps = c(0, 0.1, 0.5, 0.9), length = 100000 * (1:10))
  grid <- grid[order(-grid$length), ]
  rows <- parallel::mclapply(seq_len(nrow(grid)), function(i) {
    n <- grid$length[i]
    set.seed(n)
    x <- rbinom(n, 1, seq(0.25, 0.75, length.out = n))
    r <- run(x, 1e9, grid$eps[i])
    cat(sprintf("Hill %d eps %.1f: %.0f s\n", n, grid$eps[i], r$seconds))
    data.frame(length = n, eps = grid$eps[i], seconds = r$seconds, tests_per_k = mean(r$tests / r$blocks),
               cands_per_k = mean(r$cands / r$blocks), total_cands_per_k = sum(r$cands) / sum(r$blocks),
               final_blocks = r$blocks[n])
  }, mc.cores = cores, mc.preschedule = FALSE)
  res <- do.call(rbind, rows)
  utils::write.csv(res, file.path(out, "hill.csv"), row.names = FALSE)
  print(res)
}
