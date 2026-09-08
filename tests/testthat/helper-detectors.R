# Constructors exercised without a Python runtime or an autoencoder backend.
error_based_detectors <- function() {
  list(
    dfr_cusum = dfr_cusum(),
    dfr_ddm = dfr_ddm(),
    dfr_ecdd = dfr_ecdd(),
    dfr_eddm = dfr_eddm(),
    dfr_hddm = dfr_hddm()
  )
}

univariate_detectors <- function() {
  list(
    dfr_kswin = dfr_kswin(window_size = 40, stat_size = 10, alpha = 0.01, exact = NULL),
    dfr_lbdd = dfr_lbdd(window_size = 20, alpha = 0.05),
    dfr_mcdd = dfr_mcdd(window_size = 20, alpha = 0.05),
    dfr_page_hinkley = dfr_page_hinkley()
  )
}

vector_detectors <- function() {
  c(error_based_detectors(), univariate_detectors())
}

drifting_error_stream <- function() {
  c(rep(0, 200), rep(1, 200))
}

drifting_numeric_stream <- function() {
  set.seed(42)
  c(stats::rnorm(200, mean = 0), stats::rnorm(200, mean = 20))
}

drifting_frame <- function() {
  set.seed(42)
  data.frame(serie = c(stats::rnorm(200, mean = 0), stats::rnorm(200, mean = 20)))
}
