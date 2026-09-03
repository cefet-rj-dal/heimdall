#' Synthetic time series for concept drift detection
#'
#' @description A list of time series for drift detection. It contains, among
#' others:
#' \itemize{
#' \item `example1`: a bivariate dataset with one multivariate concept drift example
#' \item `univariate`: a univariate series with four known change points
#' }
#'
#' @docType data
#' @usage data(st_drift_examples)
#' @format A list of time series.
#' @keywords datasets
#' @references \href{https://github.com/cefet-rj-dal/heimdall}{heimdall package}
#' @source \href{https://github.com/cefet-rj-dal/heimdall}{heimdall package}
#' @examples
#' data(st_drift_examples)
#' dataset <- st_drift_examples$example1
"st_drift_examples"
