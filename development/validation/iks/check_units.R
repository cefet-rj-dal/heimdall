# Step-by-step equivalence of iks() with the reference IKS (iks_reference.py, mode units).
#   Rscript development/validation/iks/check_units.R <work dir> <out.csv>
args <- commandArgs(trailingOnly = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))
units <- jsonlite::fromJSON(file.path(args[1], "units.json"), simplifyDataFrame = FALSE)
rows <- list()
for (case in units$cases) {
  sketch <- iks(seed = 1)
  stats <- case$stats
  after <- vapply(stats, function(s) s$after, numeric(1))
  ks <- kuiper <- numeric(length(stats))
  test <- logical(length(stats))
  ks2 <- numeric(length(stats))
  k <- 1L
  live <- list(numeric(0), numeric(0))
  for (i in seq_along(case$ops)) {
    op <- case$ops[[i]]
    sketch <- if (op$op == "add") iks_add(sketch, op$x, op$group, op$tie) else iks_remove(sketch, op$x, op$group, op$tie)
    g <- op$group + 1L
    if (op$op == "add") live[[g]] <- c(live[[g]], op$x) else live[[g]] <- live[[g]][-match(op$x, live[[g]])]
    while (k <= length(after) && after[k] == i) {
      ks[k] <- iks_statistic(sketch)
      kuiper[k] <- iks_kuiper(sketch)
      test[k] <- iks_test(sketch, ca = 1.95)
      ks2[k] <- unname(suppressWarnings(stats::ks.test(live[[1]], live[[2]]))$statistic)
      k <- k + 1L
    }
  }
  ref_ks <- vapply(stats, function(s) s$ks, numeric(1))
  ref_kuiper <- vapply(stats, function(s) s$kuiper, numeric(1))
  ref_test <- vapply(stats, function(s) s$test, logical(1))
  ref_scipy <- vapply(stats, function(s) s$ks_2samp, numeric(1))
  rows[[length(rows) + 1L]] <- data.frame(
    case = case$case, operations = length(case$ops), checks = length(stats),
    max_diff_ks_reference = max(abs(ks - ref_ks)), max_diff_kuiper_reference = max(abs(kuiper - ref_kuiper)),
    different_tests = sum(test != ref_test),
    max_diff_iks_vs_ks_test = max(abs(ks - ks2)), max_diff_ks_test_vs_scipy = max(abs(ks2 - ref_scipy)),
    checks_iks_above_ks_test = sum(ks > ks2 + 1e-12))
}
result <- do.call(rbind, rows)
result$note <- ""
result$note[grepl("ties=True tiebreak=False", result$case)] <-
  "ties without tie-breaking: IKS can exceed the exact statistic (noted by the authors), in both implementations"
utils::write.csv(result, args[2], row.names = FALSE)
print(result[, -ncol(result)], digits = 3)
cat("iks_ca(0.001) =", iks_ca(0.001), "; reference CAForPValue(0.001) =", units[["ca_for_pvalue_0.001"]],
    "(the reference omits the factor 1/2 of alpha; its default ca is the table value 1.95)\n")
