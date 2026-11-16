# Compares the QuantTree port of R/dfr_mmqt.R with the reference code, using
# the outputs of reference_check.py (results/mmqt/reference/).
#   Rscript development/validation/mmqt/check_reference.R

devtools::load_all(quiet = TRUE)
ref_dir <- file.path("development", "validation", "results", "mmqt", "reference")
out_dir <- file.path("development", "validation", "results", "mmqt")
read_matrix <- function(file) as.matrix(utils::read.csv(file.path(ref_dir, file), header = FALSE))

# 1. Partitionings: the reference random choices (dimension and side of each
# bin) are replayed, and the boundaries and bins must be identical.
partitions <- do.call(rbind, lapply(0:4, function(seed) {
  prefix <- file.path("partitions", sprintf("seed%d_", seed))
  data <- read_matrix(paste0(prefix, "data.csv"))
  new <- read_matrix(paste0(prefix, "new.csv"))
  leaves <- read_matrix(paste0(prefix, "leaves.csv"))
  bins <- nrow(leaves)
  tree <- heimdall:::.qt_build(data, rep(1 / bins, bins), dims = leaves[, 1] + 1, sides = leaves[, 4])
  bins_data <- utils::read.csv(file.path(ref_dir, paste0(prefix, "bins_data.csv")), header = FALSE)[[1]] + 1
  bins_new <- utils::read.csv(file.path(ref_dir, paste0(prefix, "bins_new.csv")), header = FALSE)[[1]] + 1
  data.frame(seed = seed, dim = ncol(data), bins = bins, n_train = nrow(data),
             boundaries_equal = identical(tree$lower, unname(leaves[, 2])) && identical(tree$upper, unname(leaves[, 3])),
             training_bins_equal = identical(heimdall:::.qt_find_bin(tree, data), as.integer(bins_data)),
             new_bins_equal = identical(heimdall:::.qt_find_bin(tree, new), as.integer(bins_new)))
}))

# 2. Thresholds (in counts, as in the reference): pre-computed table and
# reference simulation for non-uniform bin probabilities, against the
# Dirichlet-multinomial simulation of the port (10^5 simulations).
set.seed(1)
uniform <- utils::read.csv(file.path(ref_dir, "thresholds.csv"))
uniform$r <- mapply(function(bins, n_train, nu, alpha) {
  heimdall:::.qt_threshold(rep(1 / bins, bins), n_train, nu, alpha, 1e5) * nu
}, uniform$bins, uniform$n_train, uniform$nu, uniform$alpha)
nonuniform <- utils::read.csv(file.path(ref_dir, "thresholds_nonuniform.csv"))
nonuniform$r <- mapply(function(case, n_train, nu, alpha) {
  pi_values <- utils::read.csv(file.path(ref_dir, sprintf("pi_%s.csv", case)), header = FALSE)[[1]]
  heimdall:::.qt_threshold(pi_values, n_train, nu, alpha, 1e5) * nu
}, nonuniform$case, nonuniform$n_train, nonuniform$nu, nonuniform$alpha)
names(uniform)[names(uniform) == "threshold"] <- "reference"
names(nonuniform)[names(nonuniform) == "threshold"] <- "reference"

# FPR actually reached by the port thresholds under the reference simulation
# is not available here; the difference in counts is reported instead.
uniform$difference <- uniform$r - uniform$reference
nonuniform$difference <- nonuniform$r - nonuniform$reference

utils::write.csv(partitions, file.path(out_dir, "check_partitions.csv"), row.names = FALSE)
utils::write.csv(uniform, file.path(out_dir, "check_thresholds_uniform.csv"), row.names = FALSE)
utils::write.csv(nonuniform, file.path(out_dir, "check_thresholds_nonuniform.csv"), row.names = FALSE)
cat("Partitionings\n")
print(partitions, row.names = FALSE)
cat("\nThresholds, uniform bins (pre-computed table of the reference)\n")
print(uniform, row.names = FALSE)
cat("\nThresholds, non-uniform bins (reference simulation, 20000 runs)\n")
print(nonuniform, row.names = FALSE)
