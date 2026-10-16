#' Synthetic time series for concept drift detection
#'
#' @description A list of univariate and multivariate time series for drift
#' detection. Each element is a data frame:
#' \itemize{
#' \item `uv_virtual_drift`: univariate series (`serie`) with a change in the
#' feature distribution at four known points, flagged in `event`;
#' \item `uv_vct_real_drift`: univariate series with a virtual change followed
#' by a real drift;
#' \item `uv_vdp_real_drift`: univariate series with a real drift and no change
#' in the feature distribution;
#' \item `mv_real_drift`: bivariate series with a real drift;
#' \item `mv_vct_real_drift`: bivariate series with a virtual change followed
#' by a real drift;
#' \item `mv_vdp_real_drift`: bivariate series with a real drift and no change
#' in the feature distribution.
#' }
#' Except for `uv_virtual_drift`, every data frame holds the series (`serie`,
#' or `serie1` and `serie2`), a logical `target`, the index `i` and a `drift`
#' column flagging the change point.
#'
#' @docType data
#' @usage data(st_drift_examples)
#' @format A list of six data frames with 500 rows each.
#' @keywords datasets
#' @references \href{https://github.com/cefet-rj-dal/heimdall}{heimdall package}
#' @source \href{https://github.com/cefet-rj-dal/heimdall}{heimdall package}
#' @examples
#' data(st_drift_examples)
#' dataset <- st_drift_examples$mv_real_drift
#' which(dataset$drift == 1)
"st_drift_examples"
