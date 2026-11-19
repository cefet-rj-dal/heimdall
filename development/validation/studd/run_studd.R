# Validation of dfr_studd() against Cerqueira, Gomes, Bifet and Torgo (2023),
# "STUDD: a student-teacher method for unsupervised concept drift detection"
# (Machine Learning 112, 4351-4378), and its code
# (https://github.com/vcerqueira/studd).
#
# Run from the package root, inside the devcontainer, after
# run_reference.sh (studd_reference.py, modes trace and run):
#   Rscript development/validation/studd/run_studd.R [cores]
#
# 1. Step-by-step equivalence: dfr_studd with models that replay the calls
#    recorded in the instrumented reference (trace mode), and the Page-Hinkley
#    test of scikit-multiflow 0.5.3 ported below: the rows and labels given to
#    every fit, the mimicking errors and the alarms must be the same.
# 2. dfr_page_hinkley (the detector of the package) against the Page-Hinkley
#    test of scikit-multiflow 0.5.3 on the mimicking errors of the traces.
# 3. dfr_studd with randomForest models on the data streams, over several
#    seeds, against the reference (scikit-learn random forests, same seeds
#    count) and the values of the paper (Tables 3 and 4).

devtools::load_all(quiet = TRUE)
suppressMessages(library(daltoolbox))
args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args) > 0) as.integer(args[1]) else 6L
datasets_dir <- file.path("data-real", "studd", "datasets")
work <- file.path("data-real", "studd", "work")
out_dir <- file.path("development", "validation", "results", "studd")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
small <- c("AbruptInsects", "Insects", "Keystroke")

read_stream <- function(name) {
  frame <- utils::read.csv(file.path(datasets_dir, paste0(name, ".data")), header = FALSE, nrows = 150000,
                           stringsAsFactors = FALSE, strip.white = FALSE)
  x <- as.data.frame(lapply(frame[-ncol(frame)], as.numeric))
  names(x) <- paste0("X", seq_along(x) - 1L)
  x$class <- as.character(frame[[ncol(frame)]])
  x
}

kappa <- function(y, prediction) {
  levels <- union(y, prediction)
  table <- table(factor(y, levels), factor(prediction, levels))
  n <- sum(table)
  observed <- sum(diag(table)) / n
  expected <- sum(rowSums(table) * colSums(table)) / n^2
  (observed - expected) / (1 - expected)
}

# Page-Hinkley test of scikit-multiflow 0.5.3 (drift_detection/page_hinkley.py):
# sum = max(0, alpha * sum + (x - mean - delta)); reset after a change.
ph_skmultiflow <- function(delta = 0.001, min_instances = 30, threshold = 50, alpha = 1 - 1e-4) {
  obj <- dist_based(target_feat = NULL)
  obj$settings <- list(delta = delta, min_instances = min_instances, threshold = threshold, alpha = alpha)
  obj$state <- list(mean = 0, sum = 0, count = 1)
  class(obj) <- append("ph_skmultiflow", class(obj))
  obj
}
update_state.ph_skmultiflow <- function(obj, value, ...) {
  s <- obj$settings
  state <- obj$state
  state$mean <- state$mean + (value - state$mean) / state$count
  state$sum <- max(0, s$alpha * state$sum + (value - state$mean - s$delta))
  state$count <- state$count + 1
  obj$last_drifter_output <- state$sum
  drift <- state$count >= s$min_instances && state$sum > s$threshold
  obj$state <- if (drift) list(mean = 0, sum = 0, count = 1) else state
  if (drift) obj$drifted <- TRUE
  list(obj = obj, drift = drift)
}
registerS3method("update_state", "ph_skmultiflow", update_state.ph_skmultiflow, envir = asNamespace("heimdall"))
reset_state.ph_skmultiflow <- function(obj) {
  obj$state <- list(mean = 0, sum = 0, count = 1)
  obj$drifted <- FALSE
  obj
}
registerS3method("reset_state", "ph_skmultiflow", reset_state.ph_skmultiflow, envir = asNamespace("heimdall"))

# Models that replay the calls recorded by the reference (fit: rows and
# labels are compared; predict: the recorded predictions are returned).
replay_model <- function(role, log, slevels) {
  obj <- daltoolbox::classification("class", slevels)
  obj$role <- role
  obj$log <- log
  class(obj) <- append("replay_model", class(obj))
  obj
}
next_call <- function(obj, kind, rows) {
  log <- obj$log
  log$position[[obj$role]] <- log$position[[obj$role]] + 1L
  call <- log$calls[[obj$role]][[log$position[[obj$role]]]]
  if (is.null(call) || call$call != kind || call$rows != rows) {
    stop(sprintf("replay: %s call %d of the %s is not a %s of %d rows", obj$role, log$position[[obj$role]],
                 obj$role, kind, rows))
  }
  call
}
registerS3method("fit", "replay_model", function(obj, data, ...) {
  call <- next_call(obj, "fit", nrow(data))
  obj$log$fits <- rbind(obj$log$fits, data.frame(role = obj$role, rows = nrow(data),
                                                 same_labels = identical(as.character(data$class),
                                                                         unlist(call$labels))))
  obj
}, envir = asNamespace("daltoolbox"))
registerS3method("predict", "replay_model", function(object, x, ...) {
  call <- next_call(object, "predict", nrow(x))
  labels <- unlist(call$labels)
  probabilities <- matrix(0, length(labels), length(object$slevels), dimnames = list(NULL, object$slevels))
  probabilities[cbind(seq_along(labels), match(labels, object$slevels))] <- 1
  as.data.frame(probabilities)
}, envir = asNamespace("stats"))

# 1. Replay -----------------------------------------------------------------------------------------
trace_files <- list.files(file.path(work, "traces"), full.names = TRUE)
replay_rows <- list()
error_streams <- list()
for (file in trace_files) {
  trace <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  stream <- read_stream(trace$dataset)
  slevels <- sort(unique(stream$class))
  log <- new.env()
  log$calls <- trace$log
  log$position <- list(teacher = 0L, student = 0L)
  log$fits <- NULL
  detector <- dfr_studd(replay_model("teacher", log, slevels), replay_model("student", log, slevels),
                        detector = ph_skmultiflow(delta = 0.001), window_size = trace$window, seed = 1)
  model <- fit(detector, stream)
  output <- model$drifter_output
  online <- seq.int(trace$window + 1L, nrow(stream))
  # Mimicking errors of the reference: online predictions alternate teacher/student, one row each.
  single <- function(role) {
    calls <- Filter(function(call) call$call == "predict" && call$rows == 1L, trace$log[[role]])
    vapply(calls, function(call) unlist(call$labels), character(1))
  }
  reference_error <- as.numeric(single("teacher") != single("student"))
  error_streams[[trace$dataset]] <- list(error = reference_error, alarms = unlist(trace$alarms) - trace$window + 1L)
  alarms <- which(output$drift == 1) - 1L
  teacher_prediction <- slevels[output$teacher[online]]
  replay_rows[[file]] <- data.frame(
    dataset = trace$dataset, rows = nrow(stream), window = trace$window,
    fits = nrow(log$fits), fits_reference = sum(vapply(trace$log, function(calls)
      sum(vapply(calls, function(call) call$call == "fit", logical(1))), numeric(1))),
    same_fit_labels = all(log$fits$same_labels),
    same_errors = identical(output$error[online], reference_error),
    alarms = paste(alarms, collapse = " "), alarms_reference = paste(unlist(trace$alarms), collapse = " "),
    kappa = kappa(stream$class[online], teacher_prediction), kappa_reference = trace$kappa,
    labels = model$labels_used - trace$window, labels_reference = trace$samples_used,
    all_calls_used = log$position$teacher == length(trace$log$teacher) &&
      log$position$student == length(trace$log$student))
}
replay <- do.call(rbind, replay_rows)
utils::write.csv(replay, file.path(out_dir, "replay.csv"), row.names = FALSE)
cat("1. dfr_studd replaying the models of the reference (trace mode)\n")
print(replay, row.names = FALSE, digits = 4)

# 2. dfr_page_hinkley against scikit-multiflow 0.5.3 on the mimicking errors ------------------------
ph_rows <- list()
for (name in names(error_streams)) {
  errors <- error_streams[[name]]$error
  run <- function(detector) {
    alarms <- integer(0)
    for (i in seq_along(errors)) {
      result <- update_state(detector, errors[i])
      detector <- result$obj
      if (result$drift) alarms <- c(alarms, i)
    }
    alarms
  }
  reference <- run(ph_skmultiflow(delta = 0.001))
  package <- run(dfr_page_hinkley(delta = 0.001))
  ph_rows[[name]] <- data.frame(dataset = name, alarms_skmultiflow = paste(reference, collapse = " "),
                                alarms_dfr_page_hinkley = paste(package, collapse = " "),
                                same = identical(reference, package))
}
ph <- do.call(rbind, ph_rows)
utils::write.csv(ph, file.path(out_dir, "page_hinkley.csv"), row.names = FALSE)
cat("\n2. Page-Hinkley on the mimicking errors of the traces (online row of each alarm)\n")
print(ph, row.names = FALSE)

# 3. dfr_studd with randomForest over several seeds ---------------------------------------------------
paper <- data.frame(dataset = c("AbruptInsects", "Insects", "Keystroke", "Bike", "Posture"),
                    kappa_paper = c(0.73, 0.81, 0.84, 0.32, 0.50), labels_paper = c(0.28, 0.09, 0.31, 0.06, 0.02),
                    kappa_static_paper = c(0.59, 0.81, 0.84, 0.31, 0.34))
jobs <- rbind(expand.grid(dataset = c("Bike", "AbruptInsects", "Insects", "Keystroke"), seed = 1:10,
                          stringsAsFactors = FALSE),
              data.frame(dataset = "Posture", seed = 1:3))
streams <- lapply(stats::setNames(unique(jobs$dataset), unique(jobs$dataset)), read_stream)
run_job <- function(i) {
  job <- jobs[i, ]
  stream <- streams[[job$dataset]]
  window <- if (job$dataset %in% small) 500L else 1000L
  slevels <- sort(unique(stream$class))
  # scikit-learn defaults: 100 trees, leaves of 1 observation, sqrt(p) features per split.
  forest <- cla_rf("class", slevels, ntree = 100, nodesize = 1, mtry = floor(sqrt(ncol(stream) - 1)))
  started <- Sys.time()
  model <- fit(dfr_studd(forest, window_size = window, seed = job$seed), stream)
  online <- seq.int(window + 1L, nrow(stream))
  output <- model$drifter_output
  data.frame(dataset = job$dataset, seed = job$seed, rows = nrow(stream),
             alarms = sum(output$drift, na.rm = TRUE),
             kappa = kappa(stream$class[online], slevels[output$teacher[online]]),
             labels = model$labels_used / nrow(stream),
             seconds = as.numeric(difftime(Sys.time(), started, units = "secs")))
}
started <- Sys.time()
runs <- parallel::mclapply(seq_len(nrow(jobs)), run_job, mc.cores = cores, mc.preschedule = FALSE)
failed <- which(!vapply(runs, is.data.frame, logical(1)))
if (length(failed) > 0) {
  cat(sprintf("\n%d job(s) rerun sequentially after a worker failure\n", length(failed)))
  runs[failed] <- lapply(failed, run_job)
}
ours <- do.call(rbind, runs)
utils::write.csv(ours, file.path(out_dir, "runs_dfr_studd.csv"), row.names = FALSE)

reference_runs <- do.call(rbind, lapply(list.files(file.path(work, "runs"), full.names = TRUE), function(file) {
  run <- jsonlite::fromJSON(file)
  data.frame(dataset = run$dataset, seed = run$seed, alarms = length(run$alarms), kappa = run$kappa,
             kappa_static = run$kappa_static, labels = (run$window + run$samples_used) / run$rows)
}))
utils::write.csv(reference_runs, file.path(out_dir, "runs_reference.csv"), row.names = FALSE)

describe <- function(runs, prefix) {
  do.call(rbind, lapply(split(runs, runs$dataset), function(rows) {
    stats::setNames(data.frame(rows$dataset[1], nrow(rows), mean(rows$kappa), stats::sd(rows$kappa),
                               mean(rows$alarms), min(rows$alarms), max(rows$alarms), mean(rows$labels)),
                    c("dataset", paste0(prefix, c("_runs", "_kappa", "_kappa_sd", "_alarms", "_alarms_min",
                                                  "_alarms_max", "_labels"))))
  }))
}
static <- aggregate(kappa_static ~ dataset, reference_runs, mean)
summary <- Reduce(function(a, b) merge(a, b, by = "dataset"),
                  list(paper, describe(reference_runs, "reference"), describe(ours, "ours"), static))
summary <- summary[match(paper$dataset, summary$dataset), ]
utils::write.csv(summary, file.path(out_dir, "summary.csv"), row.names = FALSE)
cat(sprintf("\n3. Data streams, dfr_studd with randomForest (%.0f s on %d cores) against the reference\n",
            as.numeric(difftime(Sys.time(), started, units = "secs")), cores))
print(summary, row.names = FALSE, digits = 3)

# Rank agreement of the kappa of every seed (Mann-Whitney test per data set).
tests <- do.call(rbind, lapply(paper$dataset, function(name) {
  a <- ours$kappa[ours$dataset == name]
  b <- reference_runs$kappa[reference_runs$dataset == name]
  data.frame(dataset = name, p_kappa = suppressWarnings(stats::wilcox.test(a, b)$p.value),
             p_alarms = suppressWarnings(stats::wilcox.test(ours$alarms[ours$dataset == name],
                                                            reference_runs$alarms[reference_runs$dataset == name])$p.value))
}))
utils::write.csv(tests, file.path(out_dir, "tests.csv"), row.names = FALSE)
cat("\nMann-Whitney tests, dfr_studd against the reference over the seeds\n")
print(tests, row.names = FALSE, digits = 3)
cat("STUDD_DONE\n")
