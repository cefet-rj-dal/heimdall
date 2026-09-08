#' Synthetic time series for concept drift detection
#'
#' @description A list of univariate and multivariate time series for drift
#' detection. Each element is a data frame:
#' \itemize{
#' \item `uv_virtual_drift`: univariate series with a change in the feature distribution
#' \item `uv_vct_real_drift`: univariate series with a virtual change followed by a real drift
#' \item `uv_vdp_real_drift`: univariate series with a real drift and no change in the feature distribution
#' \item `mv_real_drift`: bivariate series with a real drift
#' \item `mv_vct_real_drift`: bivariate series with a virtual change followed by a real drift
#' }
#'
#' @docType data
#' @usage data(st_drift_examples)
#' @format A list of data frames.
#' @keywords datasets
#' @references \href{https://github.com/cefet-rj-dal/heimdall}{heimdall package}
#' @source \href{https://github.com/cefet-rj-dal/heimdall}{heimdall package}
#' @examples
#' data(st_drift_examples)
#' dataset <- st_drift_examples$uv_virtual_drift
"st_drift_examples"
