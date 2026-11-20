# Summary of the GreedyKS experiments: heimdall (run_greedyks.R), reference
# (greedyks_reference.py) and the values of the paper (Cardoso and Galeno, 2023:
# Tables 2-6, and Figures 2, 3, 5 read off the plots).
#   Rscript development/validation/greedyks/summarize.R <results dir>
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
distribs <- c("normal", "uniform", "exp")

# Tables 2-5 of the paper: premature / missed / mean delay, GreedyKS and Exact KS.
paper <- list(
  memo_units = list(values = c(10, 31, 100),
    greedyks = c("12/0/1778.3", "10/0/1665.7", "10/0/1625.0", "10/0/2231.6", "12/0/2212.1", "6/0/1837.1",
                 "6/0/646.5", "6/0/583.8", "6/0/571.5"),
    exact = c("14/0/1523.9", "7/0/1436.8", "10/0/1518.7", "13/0/1770.7", "11/0/1888.1", "6/0/1763.1",
              "8/0/569.5", "6/0/534.2", "7/0/556.3")),
  sample_size = list(values = c(3162, 10000, 31622),
    greedyks = c("3/0/1051.4", "10/0/1604.2", "8/0/2421.9", "5/7/1282.9", "6/0/1951.6", "14/0/2590.3",
                 "2/0/372.1", "4/0/572.3", "9/0/952.8"),
    exact = c("3/0/1040.0", "11/0/1539.1", "9/0/2321.7", "6/5/1268.5", "6/0/1881.3", "15/0/2486.7",
              "2/0/364.6", "4/0/553.0", "10/0/924.8")),
  mean_diff = list(values = 10^c(-2, -1.5, -1),
    greedyks = c("7/83/2801.6", "7/56/3865.4", "10/0/1537.1", "6/83/2646.0", "11/49/5277.6", "6/0/1872.4",
                 "9/76/4318.7", "13/0/2547.4", "13/0/574.7"),
    exact = c("9/79/3592.6", "6/52/4055.3", "10/0/1490.4", "6/80/2640.1", "8/49/5137.4", "6/0/1796.3",
              "9/73/3905.0", "10/0/2432.5", "13/0/553.9")),
  std_diff = list(values = 10^c(-2, -1.5, -1),
    greedyks = c("10/82/2556.2", "11/75/5049.4", "6/0/2734.6", "7/85/3923.8", "9/50/6083.8", "6/0/1494.4",
                 "4/82/3124.1", "12/0/3031.1", "8/0/668.2"),
    exact = c("9/82/2728.2", "12/71/4925.5", "8/0/2614.4", "8/82/3430.9", "8/41/6246.8", "6/0/1449.9",
              "4/80/3683.9", "12/0/2901.2", "8/0/661.1")))

fmt <- function(x) sprintf("%d/%d/%.1f", sum(x < 0, na.rm = TRUE), sum(is.na(x)), mean(x[!is.na(x) & x >= 0]))
key <- function(v) signif(v, 3)
tables <- list()
for (e in names(paper)) {
  h <- file.path(dir, sprintf("effectiveness_%s_heimdall.csv", e))
  r <- file.path(dir, sprintf("effectiveness_%s_reference.csv", e))
  if (!file.exists(h) || !file.exists(r)) next
  h <- utils::read.csv(h)
  r <- utils::read.csv(r, na.strings = c("NA", "nan"))
  col <- e
  h_col <- if (e == "std_diff") "std_diff" else e
  rows <- list()
  i <- 0
  for (d in distribs) for (v in paper[[e]]$values) {
    i <- i + 1
    hv <- h$greedyks[h$type == d & key(h[[h_col]]) == key(v)]
    rv <- r[r$distrib == d & key(r[[col]]) == key(v), ]
    rows[[i]] <- data.frame(experiment = e, distrib = d, value = v, paper_greedyks = paper[[e]]$greedyks[i],
                            reference_greedyks = fmt(rv$greedyks), heimdall = fmt(hv),
                            paper_exact = paper[[e]]$exact[i], reference_exact = fmt(rv$exact_ks),
                            runs_heimdall = length(hv), runs_reference = nrow(rv))
  }
  tables[[e]] <- do.call(rbind, rows)
}
if (length(tables)) {
  effectiveness <- do.call(rbind, tables)
  utils::write.csv(effectiveness, file.path(dir, "effectiveness_summary.csv"), row.names = FALSE)
  print(effectiveness, right = FALSE)
}

# Mean absolute error of D (Figures 2-5), with approximate readings of the plots of GreedyKS.
read_figure <- list(
  memo_units = list(values = c(10, 100, 1000), normal = c(2e-3, 5e-4, 1.5e-4), uniform = c(2e-3, 5e-4, 1.5e-4),
                    exp = c(2.5e-4, 2e-4, 8e-5)),
  sample_size = list(values = c(1e3, 1e4, 1e5), normal = c(1.3e-3, 4.5e-4, 1.8e-4),
                     uniform = c(1e-3, 4.5e-4, 2e-4), exp = c(8e-4, 2e-4, 4.5e-5)),
  mean_diff = list(values = c(1e-3, 1e-2, 1e-1), normal = c(4.5e-4, 4.5e-4, 4.5e-4),
                   uniform = c(4.5e-4, 4.5e-4, 5e-4), exp = c(4.5e-4, 4.5e-4, 2.5e-4)),
  std_diff = list(values = c(1e-3, 1e-2, 1e-1), normal = c(5e-4, 5e-4, 5e-4), uniform = c(4.5e-4, 5e-4, 7e-4),
                  exp = c(4.5e-4, 4e-4, 2e-4)))
errors <- list()
for (e in names(read_figure)) {
  h <- file.path(dir, sprintf("error_%s_heimdall.csv", e))
  r <- file.path(dir, sprintf("error_%s_reference.csv", e))
  if (!file.exists(h) || !file.exists(r)) next
  h <- utils::read.csv(h)
  r <- utils::read.csv(r)
  for (d in distribs) for (k in seq_along(read_figure[[e]]$values)) {
    v <- read_figure[[e]]$values[k]
    hv <- h$error[h$type == d & key(h[[e]]) == key(v)]
    rv <- r$error[r$distrib == d & key(r[[e]]) == key(v)]
    errors[[length(errors) + 1L]] <- data.frame(experiment = e, distrib = d, value = v,
                                                paper_plot = read_figure[[e]][[d]][k], reference = mean(rv),
                                                heimdall = mean(hv), runs = length(hv),
                                                wilcoxon_p = suppressWarnings(stats::wilcox.test(hv, rv)$p.value))
  }
}
if (length(errors)) {
  errors <- do.call(rbind, errors)
  utils::write.csv(errors, file.path(dir, "error_summary.csv"), row.names = FALSE)
  print(errors, digits = 3)
}

# Call center (Figure 7 and Table 6).
h <- file.path(dir, "callcenter_dtw_heimdall.csv")
r <- file.path(dir, "callcenter_dtw_reference.csv")
if (file.exists(h) && file.exists(r)) {
  h <- utils::read.csv(h)
  r <- utils::read.csv(r)
  both <- merge(h[, c("m", "week", "dtw", "n_greedyks", "n_minibatch")], r, by = c("m", "week"),
                suffixes = c("_heimdall", "_reference"))
  cc <- do.call(rbind, lapply(split(both, both$m), function(x) {
    data.frame(m = x$m[1], weeks = nrow(x),
               median_dtw_heimdall = stats::median(x$dtw_heimdall), median_dtw_reference = stats::median(x$dtw_reference),
               same_dtw = sum(abs(x$dtw_heimdall - x$dtw_reference) <= 1e-9 * pmax(1, x$dtw_reference)),
               same_minibatch = sum(x$n_minibatch_heimdall == x$n_minibatch_reference),
               same_alarms = sum(x$n_greedyks_heimdall == x$n_greedyks_reference))
  }))
  utils::write.csv(cc, file.path(dir, "callcenter_summary.csv"), row.names = FALSE)
  print(cc)
}
t6 <- file.path(dir, "callcenter_table6_heimdall.csv")
if (file.exists(t6)) {
  cat("Table 6 (paper: GreedyKS 1999-01-04 21:41:26, D = 0.039804)\n")
  print(utils::read.csv(t6))
  ref <- file.path(dir, "callcenter_table6_reference.json")
  if (file.exists(ref)) str(jsonlite::fromJSON(ref))
}
