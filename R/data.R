#' Synthetic time series for concept drift detection
#' @description A list of multivariate time series for drift detection
#' \itemize{
#' \item example1: a bivariate dataset with one multivariate concept drift example
#' }
#'#'
#' @docType data
#' @usage data(st_drift_examples)
#' @format A list of time series.
#' @keywords datasets
#' @references \href{https://github.com/cefet-rj-dal/heimdall}{Stealthy package}
#' @source \href{https://github.com/cefet-rj-dal/heimdall}{Stealthy package}
#' @examples
#' data(st_drift_examples)
#' dataset <- st_drift_examples$example1
"st_drift_examples"

#' Real time series for concept drift detection
#' @description A list of real multivariate time series for concept drift detection
#' \itemize{
#' \item bfd1: Brazilian Flights Data 2023
#' }
#'#'
#' @docType data
#' @usage data(st_real_examples)
#' @format A list of real multivariate time series.
#' @keywords datasets
#' @references \href{https://github.com/cefet-rj-dal/heimdall}{Stealthy package}
#' @source \href{https://github.com/cefet-rj-dal/heimdall}{Stealthy package}
#' @examples
#' data(st_real_examples)
#' dataset <- st_real_examples$bfd1
"st_real_examples"

