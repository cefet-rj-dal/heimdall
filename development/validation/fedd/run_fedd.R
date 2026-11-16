# Validation of dfr_ensemble() / ensemble_vote() against Giusti Tavares et al.
# (2025), "Fuzzy-based ensemble method for robust concept drift detection in
# multivariate time series", IJCNN 2025, and its reference code
# (https://github.com/cefet-rj-dal/reproduce, folder fedd/: core/4-ensemble-fuzzy.R
# for the combination, core/3-combination-analysis.R for the evaluation, and
# results.zip with the detection series of the base detectors and of the
# ensembles on the 3W data set).
#
# Run from the package root, inside the devcontainer:
#   Rscript development/validation/fedd/run_fedd.R
#
# 1. Equivalence: the detection series of the six base detectors selected in
#    the paper (ADWIN, KLDIST, KSWIN, MCDD, Page-Hinkley, AEDD) are combined by
#    ensemble_vote() and compared, time step by time step, with the reference
#    ensembles of results.zip: FEDD MV (k = 4000), FEDD TV (k = 4000,
#    lambda = 4), CEDD EFER, CEDD MV and CEDD TV (lambda = 2).
# 2. Table IV: SoftED precision, recall, F1 (time tolerance 4000) and coverage
#    of the base detectors and of the ensembles on the test examples, with the
#    protocol of 3-combination-analysis.R.

devtools::load_all(quiet = TRUE)
out_dir <- file.path("development", "validation", "results", "fedd")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
reference_dir <- file.path("data-real", "fedd", "reference", "fedd")
zip_path <- file.path(reference_dir, "results.zip")
zip_files <- utils::unzip(zip_path, list = TRUE)$Name
read_result <- function(name) {
  path <- grep(paste0("(^|/)", gsub("([.])", "\\\\\\1", name), "$"), zip_files, value = TRUE)
  if (length(path) == 0L) return(NULL)
  utils::read.csv(unz(zip_path, path[1]))
}

members <- c(adwin = "cla_majority-dfr_adwin-zscore-distribution-incremental",
             kldist = "cla_majority-dfr_kldist-fixed_zscore-distribution-incremental",
             kswin = "cla_majority-dfr_kswin-fixed_zscore-distribution-incremental",
             mcdd = "cla_majority-dfr_mcdd-fixed_zscore-distribution-incremental",
             page_hinkley = "cla_majority-dfr_page_hinkley-zscore-distribution-incremental",
             aedd = "cla_majority-dfr_aedd-fixed_zscore-distribution-incremental")
ensembles <- list(
  `FEDD MV (k = 4000)` = list(file = "dfr_multi_criteria_best_fuzzy-combination", voting = "majority", tolerance = 4000),
  `FEDD TV (k = 4000, lambda = 4)` = list(file = "dfr_multi_criteria_best_fuzzy_tv-combination", voting = "threshold",
                                          threshold = 4, tolerance = 4000),
  `CEDD EFER` = list(file = "dfr_multi_criteria_best_or-combination", voting = "efer", tolerance = 0),
  `CEDD MV` = list(file = "dfr_multi_criteria_best_and-combination", voting = "majority", tolerance = 0),
  `CEDD TV (lambda = 2)` = list(file = "dfr_multi_criteria_best_tv-combination", voting = "threshold", threshold = 2,
                                tolerance = 0)
)
examples <- unique(sub("^.*-([0-9]WELL[0-9_]+)\\.csv$", "\\1",
                       grep(members[["adwin"]], zip_files, value = TRUE, fixed = TRUE)))

# 1. Equivalence ---------------------------------------------------------------------------
equivalence <- do.call(rbind, lapply(examples, function(example) {
  base <- lapply(members, function(m) read_result(sprintf("%s-%s.csv", m, example)))
  lengths <- vapply(base, nrow, integer(1))
  if (length(unique(lengths)) > 1L) {
    # 4WELL1_3: one member has more rows; the reference leaves it out ("Missing One").
    cat("skipped", example, "(member series of different lengths:", paste(lengths, collapse = ", "), ")\n")
    return(NULL)
  }
  detections <- sapply(base, function(b) as.logical(b$drift))
  do.call(rbind, lapply(names(ensembles), function(name) {
    e <- ensembles[[name]]
    reference <- read_result(sprintf("%s-%s.csv", e$file, example))
    if (is.null(reference)) return(NULL)
    ours <- ensemble_vote(detections, voting = e$voting, threshold = e$threshold, tolerance = e$tolerance)
    theirs <- as.logical(reference$drift)
    data.frame(example = example, ensemble = name, rows = nrow(detections), member_detections = sum(detections),
               reference_detections = sum(theirs), detections = sum(ours), same_length = length(theirs) == length(ours),
               mismatches = if (length(theirs) == length(ours)) sum(theirs != ours) else NA)
  }))
}))
utils::write.csv(equivalence, file.path(out_dir, "equivalence.csv"), row.names = FALSE)
cat("1. ensemble_vote() against the reference ensembles (", length(examples), "examples )\n")
print(do.call(rbind, lapply(split(equivalence, equivalence$ensemble), function(p) {
  data.frame(ensemble = p$ensemble[1], examples = nrow(p), identical = sum(p$mismatches == 0, na.rm = TRUE),
             mismatched_steps = sum(p$mismatches, na.rm = TRUE), detections = sum(p$detections),
             reference_detections = sum(p$reference_detections))
})), row.names = FALSE)
bad <- equivalence[is.na(equivalence$mismatches) | equivalence$mismatches > 0, ]
if (nrow(bad) > 0) {
  cat("\nExamples with differences\n")
  print(bad, row.names = FALSE)
}
cat("FEDD_PART1_DONE\n")

# 2. Table IV --------------------------------------------------------------------------------
# Test examples and series of 3-combination-analysis.R. Its list repeats the
# key 3WELL14_2 (the second entry, 3WELL14_3, is never read and 3WELL14_2 counts
# twice) and names 1WELL6_1 "1WELL6", which matches no result row; both quirks
# are kept so that the averages are those of the reference script.
test_list <- list(
  c("1WELL2", "1", "WELL-00002_20140126200050"), c("1WELL6", "1", "WELL-00006_20170801063614"),
  c("1WELL6_2", "1", "WELL-00006_20170802123000"), c("2WELL3_1", "2", "WELL-00003_20141122214325"),
  c("2WELL3_2", "2", "WELL-00003_20170728150240"), c("2WELL3_3", "2", "WELL-00003_20180206182917"),
  c("3WELL14_1", "3", "WELL-00014_20170917190000"), c("3WELL14_2", "3", "WELL-00014_20170917140000"),
  c("3WELL14_2", "3", "WELL-00014_20170917140000"), c("4WELL1_2", "4", "WELL-00001_20170316130000"),
  c("5WELL15_2", "5", "WELL-00015_20171013140047"), c("5WELL16_1", "5", "WELL-00016_20180405020345"),
  c("5WELL16_2", "5", "WELL-00016_20180426142005"), c("6WELL2_2", "6", "WELL-00002_20140301151700"),
  c("6WELL2_3", "6", "WELL-00002_20140325170304"), c("6WELL4_1", "6", "WELL-00004_20171031181509"),
  c("7WELL6_1", "7", "WELL-00006_20180618110721"), c("7WELL6_2", "7", "WELL-00006_20180620181348"),
  c("8WELL19", "8", "WELL-00019_20170301182317"), c("8WELL20", "8", "WELL-00020_20120410192326"),
  c("8WELL21", "8", "WELL-00021_20170509013517")
)
one_event <- c("3WELL1", "3WELL14_1", "3WELL14_2", "4WELL1_1", "4WELL1_2", "4WELL1_3", "5WELL15_1", "5WELL16_2",
               "7WELL6_1", "7WELL6_2")

# Events: the first observation of the second and third classes of the series
# (the reference rule). Types 3 and 4 come from the reference RData files (with
# the class column); the other types from the full 3W data of
# cefet-rj-dal/united, whose change points mark the same observations.
series_env <- new.env()
# The reference RData files hold arrow-backed (ALTREP) vectors; arrow, installed
# in a private library (see README.md), is needed to read them.
suppressPackageStartupMessages(library(arrow, lib.loc = file.path("data-real", "fedd", "rlib")))
load(file.path(reference_dir, "data", "data_3w_tp3_real_sample_exp.RData"), envir = series_env)
load(file.path(reference_dir, "data", "data_3w_tp4_sample.RData"), envir = series_env)
events_of <- function(entry) {
  if (entry[2] %in% c("3", "4")) {
    data <- series_env[[if (entry[2] == "3") "data_3w_tp3_real_sample_exp" else "data_3w_tp4_sample"]][[entry[3]]]
  } else {
    name <- paste0("oil_3w_Type_", entry[2])
    if (!exists(name, envir = series_env)) {
      load(file.path("data-real", "fedd", "3w", "full", paste0(name, ".RData")), envir = series_env)
    }
    data <- series_env[[name]][[entry[3]]]
  }
  data <- as.data.frame(data)
  if ("class" %in% names(data)) {
    # Reference rule on the class labels.
    data$class[is.na(data$class)] <- 0
    classes <- unique(data$class)
    index <- seq_len(nrow(data))
    events <- index[data$class == classes[2]][1]
    if (!(entry[1] %in% one_event)) events <- c(events, index[data$class == classes[3]][1])
    attr(events, "source") <- "class"
  } else {
    # Series of the harbinger format, with the change points marked.
    events <- which(data$event)
    events <- if (entry[1] %in% one_event) events[1] else events[1:2]
    attr(events, "source") <- "event"
  }
  events
}

softed <- harbinger::har_eval_soft(sw_size = 4000)
evaluate_series <- function(drift, index, events) {
  event <- index %in% events
  m <- daltoolbox::evaluate(softed, drift, event)
  data.frame(precision = m$precision, recall = m$recall, F1 = m$F1, detections = sum(drift))
}
base_names <- c(adwin = "dfr_adwin", kldist = "dfr_kldist", kswin = "dfr_kswin", mcdd = "dfr_mcdd",
                page_hinkley = "dfr_page_hinkley", aedd = "dfr_aedd")
rows <- lapply(test_list, function(entry) {
  example <- entry[1]
  events <- events_of(entry)
  base <- lapply(members, function(m) read_result(sprintf("%s-%s.csv", m, example)))
  # "1WELL6" matches no file of its own (the files are 1WELL6_1, 1WELL6_2).
  if (any(vapply(base, is.null, logical(1)))) return(NULL)
  index <- base[[1]]$index
  detections <- sapply(base, function(b) as.logical(b$drift))
  out <- do.call(rbind, lapply(names(members), function(m) {
    cbind(example = example, events = paste(events, collapse = " "), event_source = attr(events, "source"),
          method = base_names[[m]], source = "reference",
          evaluate_series(as.logical(base[[m]]$drift), base[[m]]$index, events))
  }))
  for (name in names(ensembles)) {
    e <- ensembles[[name]]
    reference <- read_result(sprintf("%s-%s.csv", e$file, example))
    if (!is.null(reference)) {
      out <- rbind(out, cbind(example = example, events = paste(events, collapse = " "),
                              event_source = attr(events, "source"), method = name, source = "reference",
                              evaluate_series(as.logical(reference$drift), reference$index, events)))
    }
    ours <- ensemble_vote(detections, voting = e$voting, threshold = e$threshold, tolerance = e$tolerance)
    # The reference fuzzy files index the rows from 1, the crisp ones keep the
    # index of the member files; the same is done here.
    our_index <- if (e$tolerance > 1) seq_along(ours) else index
    out <- rbind(out, cbind(example = example, events = paste(events, collapse = " "),
                            event_source = attr(events, "source"), method = name, source = "ensemble_vote",
                            evaluate_series(as.logical(ours), our_index, events)))
  }
  out
})
per_example <- do.call(rbind, rows)
utils::write.csv(per_example, file.path(out_dir, "table4_examples.csv"), row.names = FALSE)
per_example$covered <- stats::complete.cases(per_example[c("precision", "recall", "F1")])
table4 <- do.call(rbind, lapply(split(per_example, list(per_example$method, per_example$source), drop = TRUE), function(p) {
  data.frame(method = p$method[1], source = p$source[1], examples = nrow(p),
             precision = mean(p$precision, na.rm = TRUE), recall = mean(p$recall, na.rm = TRUE),
             F1 = mean(p$F1, na.rm = TRUE), coverage = mean(p$covered))
}))
paper <- data.frame(method = c("dfr_page_hinkley", "dfr_kldist", "dfr_mcdd", "dfr_aedd", "dfr_kswin", "dfr_adwin",
                               "CEDD TV (lambda = 2)", "FEDD TV (k = 4000, lambda = 4)", "FEDD MV (k = 4000)",
                               "CEDD EFER", "CEDD MV"),
                    source = "paper", examples = NA,
                    precision = c(0.46, 0.28, 0.38, 0.39, 0.34, 0.35, 0.45, 0.38, 0.38, 0.14, 0),
                    recall = c(0.60, 0.09, 0.65, 0.66, 0.65, 0.64, 0.53, 0.28, 0.55, 0.83, 0),
                    F1 = c(0.46, 0.45, 0.41, 0.40, 0.38, 0.38, 0.49, 0.41, 0.39, 0.21, NA),
                    coverage = c(0.90, 0.15, 0.91, 0.95, 0.91, 0.95, 0.75, 0.55, 0.90, 0.95, 0))
table4 <- rbind(table4, paper)
table4 <- table4[order(match(table4$method, paper$method), match(table4$source, c("reference", "ensemble_vote", "paper"))), ]
utils::write.csv(table4, file.path(out_dir, "table4.csv"), row.names = FALSE)
cat("\n2. Table IV: SoftED (k = 4000) on the test examples\n")
print(format(table4, digits = 2), row.names = FALSE)
cat("FEDD_DONE\n")
