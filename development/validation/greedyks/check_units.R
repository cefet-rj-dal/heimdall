# Step-by-step equivalence of greedyks() / ks_sf() / dfr_greedyks() with the
# reference GreedyKS (greedyks_reference.py, mode units).
#   Rscript development/validation/greedyks/check_units.R <work dir> <out.csv>
args <- commandArgs(trailingOnly = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))
units <- jsonlite::fromJSON(file.path(args[1], "units.json"), simplifyDataFrame = FALSE)
rows <- list()
add <- function(component, case, difference, scale, note = "") {
  rows[[length(rows) + 1L]] <<- data.frame(component = component, case = case, max_abs_difference = difference,
                                           reference_scale = scale, relative = difference / max(scale, 1e-300),
                                           note = note)
}

# Distribution of the statistic.
grid <- do.call(rbind, lapply(units$grid, as.data.frame))
exact <- ks_sf(grid$d, grid$n)
asym <- ks_sf(grid$d, grid$n, exact = FALSE)
rel <- function(a, b) abs(a - b) / pmax(abs(b), 1e-300)
for (region in c("n <= 140", "n > 140")) {
  sel <- if (region == "n <= 140") grid$n <= 140 else grid$n > 140
  add("ks_sf exact (scipy kstwo.sf)", region, max(abs(exact[sel] - grid$kstwo[sel])), max(grid$kstwo[sel]),
      sprintf("largest relative difference where p > 1e-300: %.2e",
              max(rel(exact[sel], grid$kstwo[sel])[grid$kstwo[sel] > 1e-300])))
  add("ks_sf limiting (scipy kstwobign.sf)", region, max(abs(asym[sel] - grid$kstwobign[sel])),
      max(grid$kstwobign[sel]),
      sprintf("largest relative difference where p > 1e-300: %.2e",
              max(rel(asym[sel], grid$kstwobign[sel])[grid$kstwobign[sel] > 1e-300])))
}
boundary <- do.call(rbind, lapply(units$boundary, as.data.frame))
add("p-value at n d^2 = 2.2, n > 140", "minimum over n (scipy)", 0, 1,
    sprintf("min p = %.4f at n = %d (the detector approximates p only below n d^2 = 2.2)",
            min(boundary$p), boundary$n[which.min(boundary$p)]))
# ks.test exact p-values agree as well (small n, R's own algorithm).
set.seed(1)
diffs <- vapply(c(5, 20, 50, 99), function(n) {
  x <- rnorm(n, 0.3)
  test <- ks.test(x, "pnorm", exact = TRUE)
  abs(ks_sf(unname(test$statistic), n) - test$p.value)
}, numeric(1))
add("ks_sf exact (R ks.test exact)", "n = 5, 20, 50, 99", max(diffs), 1)

# Traces of GreedyKS.
families <- list(normal = function(p) function(q) pnorm(q, p[1], p[2]),
                 uniform = function(p) function(q) punif(q, p[1], p[1] + p[2]),
                 exp = function(p) function(q) pexp(q - p[1], 1 / p[2]))
same_drift <- 0
total_drift <- 0
for (tr in units$traces) {
  x <- readBin(file.path(args[1], paste0(tr$name, "_x.bin")), "double", 3000, 8, endian = "little")
  cdf <- families[[tr$distrib]](unlist(tr$params))
  sketch <- greedyks(cdf, m = tr$m, min_instances = tr$min_instances)
  D <- dn <- dp <- numeric(length(x))
  drift <- logical(length(x))
  for (i in seq_along(x)) {
    sketch <- greedyks_update(sketch, x[i])
    D[i] <- greedyks_statistic(sketch)
    dn[i] <- sketch$dn
    dp[i] <- sketch$dp
    drift[i] <- greedyks_pvalue(sketch) <= 0.01
  }
  case <- sprintf("%s m=%d min_instances=%d", tr$distrib, tr$m, tr$min_instances)
  add("GreedyKS D after every observation", case, max(abs(D - unlist(tr$D))), max(unlist(tr$D)),
      sprintf("D+ %.1e, D- %.1e, final indexes %s", max(abs(dp - unlist(tr$dp))), max(abs(dn - unlist(tr$dn))),
              if (identical(c(sketch$old_p, sketch$old_n), as.integer(unlist(tr$final)))) "same" else "differ"))
  ref_drift <- unlist(tr$drift)
  same_drift <- same_drift + sum(drift == ref_drift)
  total_drift <- total_drift + length(drift)
  add("GreedyKS detected_change (p <= 0.01) after every observation", case, sum(drift != ref_drift), length(drift),
      "number of observations with a different decision")
}

# The detector gives the same first alarm as the sketch.
first <- 0
agree <- 0
for (tr in units$traces) {
  if (tr$min_instances != 0) next
  x <- readBin(file.path(args[1], paste0(tr$name, "_x.bin")), "double", 3000, 8, endian = "little")
  cdf <- families[[tr$distrib]](unlist(tr$params))
  det <- fit(dfr_greedyks(cdf = cdf, m = tr$m, grace_period = 0), x)
  alarm <- which(det$drifter_output$drift)[1]
  ref <- which(unlist(tr$drift))[1]
  first <- first + 1
  agree <- agree + identical(alarm, ref)
}
add("dfr_greedyks first alarm = reference first detected_change", sprintf("%d traces", first), first - agree, first,
    "number of traces with a different first alarm")

result <- do.call(rbind, rows)
utils::write.csv(result, args[2], row.names = FALSE)
print(result[, c("component", "case", "max_abs_difference", "relative")], digits = 3)
