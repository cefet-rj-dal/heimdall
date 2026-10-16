# Regression tests: the detection positions below were recorded with the
# default parameters of each detector. They are not ground truth, but any change
# to them must be intentional and reported in NEWS.md.

test_that("distribution-based detectors keep their detections on st_drift_examples", {
  data("st_drift_examples", package = "heimdall", envir = environment())
  serie <- st_drift_examples$uv_virtual_drift$serie
  # dfr_kswin subsamples its reference window
  set.seed(1)

  expected <- list(
    dfr_adwin = c(224L, 352L, 448L),
    dfr_kldist = c(245L, 357L),
    dfr_kswin = c(300L, 500L),
    dfr_lbdd = 300L,
    dfr_mcdd = 300L,
    dfr_page_hinkley = 205L
  )
  detectors <- list(
    dfr_adwin = dfr_adwin(),
    dfr_kldist = dfr_kldist(),
    dfr_kswin = dfr_kswin(),
    dfr_lbdd = dfr_lbdd(),
    dfr_mcdd = dfr_mcdd(),
    dfr_page_hinkley = dfr_page_hinkley()
  )
  for (name in names(expected)) {
    expect_identical(detect_indices(detectors[[name]], serie), expected[[name]], info = name)
  }
})

test_that("error-based detectors keep their detections on a fixed error stream", {
  stream <- regression_error_stream()

  expected <- list(
    dfr_cusum = 603L,
    dfr_ddm = 524L,
    dfr_ecdd = c(195L, 503L),
    dfr_eddm = c(340L, 517L),
    dfr_hddm = 511L
  )
  for (name in names(expected)) {
    detector <- error_based_detectors()[[name]]
    expect_identical(detect_indices(detector, stream), expected[[name]], info = name)
  }
})

test_that("hddm reacts to an increase of the error rate and stays quiet otherwise", {
  stream <- regression_error_stream()
  first <- detect_indices(dfr_hddm(), stream)[1]
  expect_true(first > 500)
  expect_true(first < 560)

  set.seed(7)
  expect_length(detect_indices(dfr_hddm(), stats::rbinom(3000, 1, 0.2)), 0)
})
