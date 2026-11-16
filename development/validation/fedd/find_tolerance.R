# Tolerance k of the reference FEDD files in results.zip: for every example and
# fuzzy ensemble (MV, TV with lambda = 4), the values of k (50 to 6000, step
# 50, and 10% of the series length, as in a commented line of
# 4-ensemble-fuzzy.R) for which ensemble_vote() reproduces the reference file
# exactly.
#   Rscript development/validation/fedd/find_tolerance.R [cores]

devtools::load_all(quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
cores <- if (length(args) > 0) as.integer(args[1]) else 6L
out_dir <- file.path("development", "validation", "results", "fedd")
zip_path <- file.path("data-real", "fedd", "reference", "fedd", "results.zip")
files <- utils::unzip(zip_path, list = TRUE)$Name
read_result <- function(name) {
  path <- grep(name, files, value = TRUE, fixed = TRUE)
  if (length(path) == 0L) NULL else utils::read.csv(unz(zip_path, path[1]))
}
members <- c("dfr_adwin-zscore", "dfr_kldist-fixed_zscore", "dfr_kswin-fixed_zscore", "dfr_mcdd-fixed_zscore",
             "dfr_page_hinkley-zscore", "dfr_aedd-fixed_zscore")
examples <- unique(sub("^.*-([0-9]WELL[0-9_]+)[.]csv$", "\\1",
                       grep("dfr_multi_criteria_best_fuzzy-combination", files, value = TRUE)))
grid <- seq(50, 6000, by = 50)
results <- parallel::mclapply(examples, function(example) {
  base <- lapply(members, function(m) read_result(paste0("cla_majority-", m, "-distribution-incremental-", example, ".csv")))
  if (length(unique(vapply(base, nrow, integer(1)))) > 1L) return(NULL)
  detections <- sapply(base, function(b) b$drift)
  do.call(rbind, lapply(list(c("MV", "dfr_multi_criteria_best_fuzzy-combination", "majority"),
                             c("TV", "dfr_multi_criteria_best_fuzzy_tv-combination", "threshold")), function(e) {
    reference <- read_result(paste0(e[2], "-", example, ".csv"))
    if (is.null(reference)) return(NULL)
    theirs <- as.logical(reference$drift)
    ks <- sort(unique(c(grid, as.integer(nrow(detections) * 0.1))))
    match <- vapply(ks, function(k) {
      ours <- ensemble_vote(detections, voting = e[3], threshold = if (e[1] == "TV") 4 else NULL, tolerance = k)
      identical(ours[seq_along(theirs)] == 1, theirs == 1) && length(ours) == length(theirs)
    }, logical(1))
    matching <- ks[match]
    data.frame(example = example, ensemble = e[1], rows = nrow(detections), reference_detections = sum(theirs),
               matching_k = if (length(matching)) paste(range(matching), collapse = "-") else "none",
               k_4000 = 4000 %in% matching, k_10pct = as.integer(nrow(detections) * 0.1) %in% matching)
  }))
}, mc.cores = cores)
results <- do.call(rbind, results)
utils::write.csv(results, file.path(out_dir, "reference_tolerance.csv"), row.names = FALSE)
print(results, row.names = FALSE)
