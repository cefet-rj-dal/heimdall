# Compares the Human Activity sequences of prepare_har.R with those of the
# reference HumanActivityDataset (har_reference.py dataset).
#   Rscript development/validation/wwaggr/check_har_data.R <har.rds> <reference work dir>
args <- commandArgs(trailingOnly = TRUE)
har <- readRDS(args[1])
read <- function(name, dims) {
  v <- readBin(file.path(args[2], paste0(name, ".bin")), "double", prod(dims), 8, endian = "little")
  array(v, rev(dims))
}
for (part in c("train", "test")) {
  d <- dim(har[[part]]$x)
  x <- aperm(read(paste0("x_", part), d), c(3, 2, 1))
  y <- t(read(paste0("y_", part), d[1:2]))
  cat(sprintf("%s: max |x difference| %.3g, labels identical %s\n", part, max(abs(x - har[[part]]$x)),
              identical(unname(y), unname(har[[part]]$labels))))
}
