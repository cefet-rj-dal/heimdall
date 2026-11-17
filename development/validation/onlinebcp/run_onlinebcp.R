# Validation of online_bcp() and dfr_onlinebcp() against Xu, Yigiter and Chen
# (2022), "onlineBcp: An R package for online change point detection using a
# Bayesian approach", SoftwareX 17, 100999, and its code
# (https://github.com/ElsevierSoftwareX/SOFTX-D-21-00221, onlineBcp 0.1.4).
#
# Run from the package root, inside the devcontainer:
#   Rscript development/validation/onlinebcp/run_onlinebcp.R
#
# Data: the three example data sets of the paper, shipped with the package
# (data/aCGH.rda, cnv_H2347.rda, covid.rda), with the calls of Section 4.
# 1. online_bcp(numerics = "reference") against online_cp() and
#    summary.BayesCP() of the reference (sourced from R/online_cp.R): the
#    trace max_p and the change points and segments.
# 2. The change points and segments against the outputs printed in the paper.
# 3. numerics = "stable" against "reference" (the gamma overflow).
# 4. dfr_onlinebcp() on the same data as streams.

suppressMessages(library(daltoolbox))
devtools::load_all(quiet = TRUE)
reference_dir <- file.path("data-real", "onlinebcp", "reference", "onlineBcp")
out_dir <- file.path("development", "validation", "results", "onlinebcp")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
ref <- new.env()
sys.source(file.path(reference_dir, "R", "online_cp.R"), envir = ref)
for (f in c("aCGH", "cnv_H2347", "covid")) load(file.path(reference_dir, "data", paste0(f, ".rda")), envir = ref)

examples <- list(
  aCGH = list(x = ref$aCGH$transNorm, theta = 0.95, alpha = 1, beta = 1, threshold = 0.7),
  cnv_H2347 = list(x = ref$cnv_H2347[, 2] / 1000, theta = 0.9, alpha = 1, beta = 1, threshold = 0.7),
  covid = list(x = sqrt(ref$covid$New_cases + 3 / 8), theta = 0.7, alpha = 1.1, beta = 1.2, threshold = 0.3)
)

# 1. Equivalence -------------------------------------------------------------------------------
rows <- list()
results <- list()
for (name in names(examples)) {
  e <- examples[[name]]
  started <- Sys.time()
  theirs <- ref$online_cp(e$x, theta = e$theta, alpha = e$alpha, beta = e$beta, th_cp = e$threshold)
  seconds_reference <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  summary_theirs <- utils::capture.output(theirs <- ref$summary.BayesCP(theirs, norm.test = TRUE))
  started <- Sys.time()
  ours <- online_bcp(e$x, theta = e$theta, alpha = e$alpha, beta = e$beta, threshold = e$threshold,
                     numerics = "reference", normality = TRUE)
  seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  stable <- online_bcp(e$x, theta = e$theta, alpha = e$alpha, beta = e$beta, threshold = e$threshold,
                       normality = TRUE)
  results[[name]] <- list(reference = theirs, ours = ours, stable = stable)
  their_segments <- as.matrix(theirs$result$segment)
  our_segments <- as.matrix(ours$segments)
  same_segments <- identical(dim(their_segments), dim(our_segments))
  rows[[name]] <- data.frame(
    data = name, n = length(e$x),
    trace_max_prob = max(abs(ours$trace[, "max_prob"] - theirs$max_p[, 1])),
    trace_locations_equal = identical(unname(ours$trace[, "location"]), unname(theirs$max_p[, 2])),
    trace_runs_equal = identical(unname(ours$trace[, "run"]), unname(theirs$max_p[, 3])),
    changepoints = nrow(ours$changepoints), changepoints_reference = nrow(theirs$result$changepoint),
    locations_equal = identical(as.numeric(ours$changepoints$location), as.numeric(theirs$result$changepoint$location)),
    post_prob_difference = max(abs(ours$changepoints$post.prob - theirs$result$changepoint$post.prob)),
    segments_difference = if (same_segments) max(abs(our_segments - their_segments), na.rm = TRUE) else NA,
    seconds = seconds, seconds_reference = seconds_reference,
    changepoints_stable = nrow(stable$changepoints))
}
equivalence <- do.call(rbind, rows)
utils::write.csv(equivalence, file.path(out_dir, "equivalence.csv"), row.names = FALSE)
cat("1. online_bcp(numerics = 'reference') against online_cp() + summary.BayesCP() of onlineBcp 0.1.4\n")
print(format(equivalence, digits = 3), row.names = FALSE)

# 2. Paper outputs --------------------------------------------------------------------------------
paper_acgh <- data.frame(location = c(341, 682, 1023, 1463, 1513, 1517, 1519),
                         post.prob = c(0.9178, 0.8638, 0.8093, 0.7002, 0.7049, 0.7600, 0.7394))
paper_acgh_segments <- rbind(
  c(1, 341, -0.025865, 0.09757, -0.034556, -0.017174), c(342, 682, -0.034751, 0.10949, -0.044503, -0.024998),
  c(683, 1023, -0.004692, 0.10863, -0.014368, 0.004984), c(1024, 1463, -0.002773, 0.09607, -0.010306, 0.004761),
  c(1464, 1513, 0.437200, 0.11344, 0.410811, 0.463589), c(1514, 1517, 0.060000, 0.15642, -0.068643, 0.188643),
  c(1518, 1519, 0.090000, 0.08485, -0.008691, 0.188691), c(1520, 2046, 0.013036, 0.10133, 0.005776, 0.020296))
paper_acgh_normality <- c(0.47402, 0.52583, 0.08132, 0.12409, 0.77165, 0.51823, 0.99916, 0.25524)
paper_covid_segments <- rbind(
  c(1, 3, 1.1810, 0.9850, 0.76600), c(4, 8, 0.9694, 0.5424, 0.59196), c(9, 16, 0.9756, 0.4450, 0.49920),
  c(17, 19, 0.7991, 0.3234, 0.76600), c(20, 23, 0.7524, 0.2801, 0.41674), c(24, 27, 0.8446, 0.4644, 0.41674),
  c(28, 36, 1.5039, 1.6209, 0.15795), c(37, 41, 1.2398, 0.8929, 0.54012), c(42, 43, 1.0767, 0.6567, 0.99916),
  c(44, 47, 5.9430, 1.7534, 0.84611), c(48, 49, 0.6124, 0.0000, 0.03663), c(50, 52, 10.5655, 8.6381, 0.70005),
  c(53, 57, 12.1448, 8.4023, 0.64715), c(58, 61, 57.3231, 10.9355, 0.85537), c(62, 63, 0.6124, 0.0000, 0.03663),
  c(64, 68, 79.9748, 72.6929, 0.59095), c(69, 158, 157.4994, 17.9319, 0.90041), c(159, 219, 232.2585, 22.3241, 0.86449))
acgh <- results$aCGH$ours
covid <- results$covid$ours
paper_rows <- list(
  data.frame(check = "aCGH change point locations (7)", value = paste(acgh$changepoints$location, collapse = " "),
             paper = paste(paper_acgh$location, collapse = " ")),
  data.frame(check = "aCGH posterior probabilities", value = paste(round(acgh$changepoints$post.prob, 4), collapse = " "),
             paper = paste(paper_acgh$post.prob, collapse = " ")),
  data.frame(check = "aCGH segments: largest difference (begin, end, mean, SD, CI)",
             value = format(max(abs(as.matrix(acgh$segments[, 1:6]) - paper_acgh_segments)), digits = 3),
             paper = "printed with 4-6 digits"),
  data.frame(check = "aCGH normality p-values: largest difference",
             value = format(max(abs(acgh$segments$`normality p-value` - paper_acgh_normality)), digits = 3),
             paper = "printed with 5 digits"),
  data.frame(check = "cnv_H2347 change points", value = nrow(results$cnv_H2347$ours$changepoints), paper = "51"),
  data.frame(check = "COVID-19 change points", value = nrow(covid$changepoints), paper = "17"),
  data.frame(check = "COVID-19 segments: largest difference (begin, end, mean, SD, normality)",
             value = if (nrow(covid$segments) == 18) format(max(abs(as.matrix(covid$segments[, c(1:4, 7)]) -
                                                                      paper_covid_segments)), digits = 3) else
               paste(nrow(covid$segments), "segments"),
             paper = "18 segments, printed with 4-5 digits"))
paper_check <- do.call(rbind, paper_rows)
utils::write.csv(paper_check, file.path(out_dir, "paper_outputs.csv"), row.names = FALSE)
cat("\n2. Against the outputs printed in the paper\n")
print(paper_check, row.names = FALSE)

# 3. Stable numerics ---------------------------------------------------------------------------------
cat("\n3. numerics = 'stable': change points where 'reference' differs\n")
for (name in names(results)) {
  a <- results[[name]]$ours$changepoints$location
  b <- results[[name]]$stable$changepoints$location
  cat(sprintf("%-10s reference %d, stable %d; only in reference: %s; only in stable: %s\n", name, length(a), length(b),
              paste(utils::head(setdiff(a, b), 12), collapse = " "), paste(utils::head(setdiff(b, a), 12), collapse = " ")))
}
utils::write.csv(results$aCGH$stable$changepoints, file.path(out_dir, "acgh_stable_changepoints.csv"), row.names = FALSE)

# 4. Stream detector ------------------------------------------------------------------------------------
cat("\n4. dfr_onlinebcp on the data as streams\n")
stream_rows <- lapply(c("aCGH", "covid"), function(name) {
  e <- examples[[name]]
  model <- fit(dfr_onlinebcp(theta = e$theta, alpha = e$alpha, beta = e$beta, threshold = e$threshold,
                             numerics = "reference"), data.frame(x = e$x))
  online <- results[[name]]$ours$trace
  locations <- online[online[, "location"] != 0 & online[, "run"] == 0, "location"]
  data.frame(data = name, drifts = nrow(model$changepoints),
             same_locations_as_trace = identical(as.numeric(model$changepoints$location),
                                                 as.numeric(locations[c(TRUE, diff(locations) != 0)])),
             mean_delay = mean(model$changepoints$detected_at - model$changepoints$location))
})
stream <- do.call(rbind, stream_rows)
print(stream, row.names = FALSE)
utils::write.csv(stream, file.path(out_dir, "stream.csv"), row.names = FALSE)
cat("ONLINEBCP_DONE\n")
