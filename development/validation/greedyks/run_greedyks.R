# Experiments of Cardoso and Galeno (2023) with heimdall's GreedyKS, as the
# protocols of util.py of greedyks_experiments (see greedyks_reference.py):
#   Rscript development/validation/greedyks/run_greedyks.R error <experiment> <out dir> [cores]
#   Rscript development/validation/greedyks/run_greedyks.R effectiveness <experiment> <out dir> [cores]
#   Rscript development/validation/greedyks/run_greedyks.R callcenter <raw dir> <out dir> [cores]
# error: absolute error of the estimate of D against the exact statistic
# (ks.test), 100 runs per setting and distribution. effectiveness: the drift
# detector dfr_greedyks() on a stream whose first 10 % follow the reference
# distribution and the rest the shifted one; the outcome is the number of
# observations after the drift until the alarm (negative: premature; NA: none).
# callcenter: weekly samples of the Anonymous Bank call center, drift instants
# of GreedyKS and of the hourly mini-batch test, and their DTW distance; and the
# drift point of Table 6. R's random numbers are used (the runs differ from the
# reference's ones, the protocol is the same).
args <- commandArgs(trailingOnly = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))
mode <- args[1]
cores <- if (length(args) >= 4) as.integer(args[4]) else 4L
reps <- 100L
distribs <- c("normal", "uniform", "exp")

distributions <- function(mean_diff, std_diff, type) {
  mean <- 1
  std <- 1
  switch(type,
    normal = list(cdf = function(q) pnorm(q, mean, std),
                  ref = function(k) rnorm(k, mean, std),
                  alt = function(k) rnorm(k, mean + mean_diff, std + std_diff)),
    uniform = {
      loc0 <- mean - std * sqrt(12) / 2
      sc0 <- std * sqrt(12)
      loc1 <- mean + mean_diff - (std + std_diff) * sqrt(12) / 2
      sc1 <- (std + std_diff) * sqrt(12)
      list(cdf = function(q) punif(q, loc0, loc0 + sc0), ref = function(k) runif(k, loc0, loc0 + sc0),
           alt = function(k) runif(k, loc1, loc1 + sc1))
    },
    exp = {
      loc1 <- (mean + mean_diff) - (std + std_diff)
      list(cdf = function(q) pexp(q - (mean - std), 1 / std), ref = function(k) (mean - std) + rexp(k, 1 / std),
           alt = function(k) loc1 + rexp(k, 1 / (std + std_diff)))
    })
}

grid <- function(sample_size = 1e4, memo_units = 100, mean_diff = 0.1, std_diff = 0) {
  g <- expand.grid(type = distribs, std_diff = std_diff, mean_diff = mean_diff, memo_units = memo_units,
                   sample_size = sample_size, run = seq_len(reps) - 1L, stringsAsFactors = FALSE)
  g$index <- seq_len(nrow(g))
  g
}

error_settings <- list(sample_size = list(sample_size = 10^(3:5)), memo_units = list(memo_units = 10^(1:3)),
                       mean_diff = list(mean_diff = 10^(-3:-1)),
                       std_diff = list(std_diff = 10^(-3:-1), mean_diff = 0))
effectiveness_settings <- list(sample_size = list(sample_size = as.integer(10^c(3.5, 4, 4.5))),
                               memo_units = list(memo_units = as.integer(10^c(1, 1.5, 2))),
                               mean_diff = list(mean_diff = 10^c(-2, -1.5, -1)),
                               std_diff = list(std_diff = 10^c(-2, -1.5, -1), mean_diff = 0))

ks_statistic <- function(x, cdf) {
  f <- cdf(sort(x))
  n <- length(x)
  max(seq_len(n) / n - f, f - (seq_len(n) - 1) / n)
}

if (mode %in% c("error", "effectiveness")) {
  experiment <- args[2]
  out <- args[3]
  settings <- (if (mode == "error") error_settings else effectiveness_settings)[[experiment]]
  g <- do.call(grid, settings)
  task <- function(i) {
    r <- g[i, ]
    set.seed(r$index)
    d <- distributions(r$mean_diff, r$std_diff, r$type)
    if (mode == "error") {
      x <- d$alt(r$sample_size)
      sketch <- greedyks_update(greedyks(d$cdf, r$memo_units), x)
      exact <- ks_statistic(x, d$cdf)
      return(data.frame(r, exact_D = exact, greedyks_D = greedyks_statistic(sketch),
                        error = abs(greedyks_statistic(sketch) - exact)))
    }
    drift_point <- r$sample_size %/% 10
    sample <- d$alt(r$sample_size)
    sample <- sample[seq_len(floor(r$sample_size * 0.9))]
    stream <- c(d$ref(drift_point), sample)
    detector <- dfr_greedyks(cdf = d$cdf, m = r$memo_units, alpha = 0.01, grace_period = r$memo_units)
    output <- list(obj = detector, drift = FALSE)
    alarm <- NA_real_
    for (t in seq_along(stream)) {
      output <- update_state(output$obj, stream[t])
      if (output$drift) {
        alarm <- (t - 1) - drift_point
        break
      }
    }
    data.frame(r, greedyks = alarm)
  }
  start <- Sys.time()
  rows <- parallel::mclapply(seq_len(nrow(g)), task, mc.cores = cores, mc.preschedule = FALSE)
  bad <- vapply(rows, function(x) inherits(x, "try-error"), logical(1))
  if (any(bad)) stop(as.character(rows[[which(bad)[1]]]))
  result <- do.call(rbind, rows)
  utils::write.csv(result, file.path(out, sprintf("%s_%s_heimdall.csv", mode, experiment)), row.names = FALSE)
  cat(mode, experiment, nrow(result), "runs,", format(Sys.time() - start), "\n")
}

dtw_distance <- function(a, b) {
  if (length(a) == 0L || length(b) == 0L) return(Inf)
  cost <- matrix(Inf, length(a) + 1L, length(b) + 1L)
  cost[1, 1] <- 0
  for (i in seq_along(a)) {
    d <- (a[i] - b)^2
    for (j in seq_along(b)) {
      cost[i + 1, j + 1] <- d[j] + min(cost[i, j + 1], cost[i + 1, j], cost[i, j])
    }
  }
  sqrt(cost[length(a) + 1L, length(b) + 1L])
}

expon_fit <- function(x) {
  loc <- min(x)
  scale <- mean(x) - loc
  function(q) pexp(q - loc, 1 / scale)
}

if (mode == "callcenter") {
  raw <- args[2]
  out <- args[3]
  files <- sort(list.files(raw, pattern = "\\.txt$", full.names = TRUE))
  stamps <- unlist(lapply(files, function(f) {
    d <- utils::read.table(f, header = TRUE, colClasses = "character")
    as.numeric(as.POSIXct(paste0(d$date, d$vru_entry), format = "%y%m%d%H:%M:%S", tz = "UTC"))
  }))
  stamps <- sort(stamps)
  time <- as.POSIXct(stamps, origin = "1970-01-01", tz = "UTC")
  week <- as.integer(format(time, "%V"))
  hour_key <- format(time, "%Y%m%d%H")
  cat("records", length(stamps), "weeks", length(unique(week)), "\n")

  # eval_call_center for GreedyKS: drift instants of the mini-batch test and of GreedyKS.
  week_task <- function(args) {
    idx <- which(week == args$week)
    m <- args$m
    s <- stamps[idx]
    hours <- split(s, factor(hour_key[idx], levels = unique(hour_key[idx])))
    resp <- list(minibatch = numeric(0), greedyks = numeric(0))
    sketch <- NULL
    mb_cdf <- NULL
    full <- numeric(0)
    t <- 0
    for (h in hours) {
      batch <- diff(h)
      var <- length(unique(batch))
      hour_cdf <- NULL
      full <- c(full, batch)
      if (is.null(mb_cdf) || ks_sf(ks_statistic(full, mb_cdf), length(full)) < 0.01) {
        if (var > 3) {
          hour_cdf <- mb_cdf <- expon_fit(batch)
          full <- batch
          resp$minibatch <- c(resp$minibatch, t)
        } else {
          mb_cdf <- NULL
        }
      }
      for (x in batch) {
        t <- t + 1
        if (!is.null(sketch)) {
          sketch <- greedyks_update(sketch, x)
          if (greedyks_pvalue(sketch) <= 0.01) {
            resp$greedyks <- c(resp$greedyks, t)
            sketch <- NULL
          }
        }
      }
      if (var > 3 && is.null(sketch)) {
        if (is.null(hour_cdf)) hour_cdf <- expon_fit(batch)
        sketch <- greedyks_update(greedyks(hour_cdf, m), batch)
      }
    }
    data.frame(m = m, week = args$week, n_minibatch = length(resp$minibatch), n_greedyks = length(resp$greedyks),
               dtw = dtw_distance(resp$greedyks, resp$minibatch),
               greedyks = paste(resp$greedyks, collapse = " "), minibatch = paste(resp$minibatch, collapse = " "))
  }
  weeks <- sort(unique(week))
  jobs <- unlist(lapply(as.integer(10^c(1.5, 2, 2.5)), function(m) lapply(weeks, function(w) list(m = m, week = w))),
                 recursive = FALSE)
  start <- Sys.time()
  rows <- parallel::mclapply(jobs, week_task, mc.cores = cores, mc.preschedule = FALSE)
  bad <- vapply(rows, function(x) inherits(x, "try-error"), logical(1))
  if (any(bad)) stop(as.character(rows[[which(bad)[1]]]))
  result <- do.call(rbind, rows)
  result$week <- match(result$week, weeks) - 1L
  utils::write.csv(result, file.path(out, "callcenter_dtw_heimdall.csv"), row.names = FALSE)
  cat("callcenter DTW", format(Sys.time() - start), "\n")

  # Table 6: the reference fitted to 8:00-8:59 of the first work day (first week), then the detector.
  first <- which(week == weeks[1])
  from <- first[which(as.integer(format(time[first], "%H")) == 8)[1]]:max(first)
  td <- diff(stamps[from])
  first_hour <- from[hour_key[from] == hour_key[from[1]]]
  td_first <- diff(stamps[first_hour])
  cdf <- expon_fit(td_first)
  detector <- dfr_greedyks(cdf = cdf, m = 100, alpha = 0.01, grace_period = length(td_first))
  output <- list(obj = detector, drift = FALSE)
  alarm <- NA
  for (t in seq_along(td)) {
    output <- update_state(output$obj, td[t])
    if (output$drift) {
      alarm <- t - 1
      break
    }
  }
  table6 <- data.frame(index = alarm, time_at_index = format(time[from[alarm + 1]]),
                       time_after = format(time[from[alarm + 2]]), D = ks_statistic(td[seq_len(alarm)], cdf),
                       first_hour = length(td_first))
  utils::write.csv(table6, file.path(out, "callcenter_table6_heimdall.csv"), row.names = FALSE)
  print(table6)
}
