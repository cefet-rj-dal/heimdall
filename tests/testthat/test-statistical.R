# Statistical behaviour of the detectors with their default parameters: false
# alarms on stationary streams and detection delay after an abrupt change,
# across several seeds. The bounds leave room for sampling variation; they
# check behaviour, not exact detection positions.

test_that("distribution-based detectors are quiet on stationary data and catch a mean shift", {
  skip_on_cran()
  stationary <- function() stats::rnorm(stream_length)
  shifted <- function() c(stats::rnorm(change_point), stats::rnorm(stream_length - change_point, mean = 3))

  detectors <- list(
    dfr_adwin = function() dfr_adwin(),
    dfr_page_hinkley = function() dfr_page_hinkley(),
    dfr_kswin = function() dfr_kswin(),
    dfr_lbdd = function() dfr_lbdd(),
    dfr_mcdd = function() dfr_mcdd(),
    dfr_kldist = function() dfr_kldist()
  )
  for (name in names(detectors)) {
    check_detector(name, detectors[[name]], stationary, shifted, max_false_alarms = 1, max_delay = 100)
  }
})

test_that("error-based detectors keep false alarms rare and catch an error-rate increase", {
  skip_on_cran()
  stationary <- function() stats::rbinom(stream_length, 1, 0.1)
  shifted <- function() c(stats::rbinom(change_point, 1, 0.1), stats::rbinom(stream_length - change_point, 1, 0.5))

  # ECDD is calibrated for an average run length of 400 observations and EDDM
  # is known to be sensitive, so a few alarms per 1200 observations are expected.
  detectors <- list(
    dfr_cusum = list(make = function() dfr_cusum(), false_alarms = 1, delay = 200),
    dfr_ddm = list(make = function() dfr_ddm(), false_alarms = 3, delay = 100),
    dfr_ecdd = list(make = function() dfr_ecdd(), false_alarms = 6, delay = 100),
    dfr_eddm = list(make = function() dfr_eddm(), false_alarms = 6, delay = 100),
    dfr_hddm = list(make = function() dfr_hddm(), false_alarms = 1, delay = 100)
  )
  for (name in names(detectors)) {
    detector <- detectors[[name]]
    check_detector(name, detector$make, stationary, shifted, detector$false_alarms, detector$delay)
  }
})

test_that("the multivariate kl distance catches a shift in one of several columns", {
  skip_on_cran()
  set.seed(7)
  frame <- data.frame(a = stats::rnorm(stream_length), b = stats::rnorm(stream_length))
  frame$a[(change_point + 1):stream_length] <- frame$a[(change_point + 1):stream_length] + 4
  detections <- detect_indices(dfr_kldist(), frame)
  expect_length(detections[detections <= change_point], 0)
  expect_true(any(detections > change_point & detections <= change_point + 100))
})
