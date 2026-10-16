# Constructors that can be exercised without extra runtimes.
# dfr_aedd needs an autoencoder and is covered in test-aedd.R.
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
    dfr_adwin = dfr_adwin(),
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

# Runs the documented streaming loop (reset after every alarm) and returns the
# positions where a drift was reported. Data frames are fed one row at a time.
detect_indices <- function(model, stream) {
  output <- list(obj = model, drift = FALSE)
  indices <- integer(0)
  rows <- is.data.frame(stream)
  for (i in seq_len(NROW(stream))) {
    observation <- if (rows) stream[i, , drop = FALSE] else stream[i]
    output <- update_state(output$obj, observation)
    if (output$drift) {
      indices <- c(indices, i)
      output$obj <- reset_state(output$obj)
    }
  }
  indices
}

# Statistical checks used by test-statistical.R.
stream_length <- 1200
change_point <- 600

first_delay <- function(make_detector, stream) {
  detections <- detect_indices(make_detector(), stream)
  detections <- detections[detections > change_point]
  if (length(detections) == 0L) Inf else detections[1] - change_point
}

check_detector <- function(name, make_detector, stationary, shifted, max_false_alarms, max_delay, seeds = 1:3) {
  for (seed in seeds) {
    set.seed(seed)
    false_alarms <- length(detect_indices(make_detector(), stationary()))
    testthat::expect_lte(false_alarms, max_false_alarms, label = sprintf("%s false alarms (seed %d)", name, seed))

    set.seed(100 + seed)
    delay <- first_delay(make_detector, shifted())
    testthat::expect_lte(delay, max_delay, label = sprintf("%s detection delay (seed %d)", name, seed))
  }
}
