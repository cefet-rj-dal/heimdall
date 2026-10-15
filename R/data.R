#' Synthetic time series for concept drift detection
#'
#' @description A list of time series for drift detection:
#' \itemize{
#' \item `dataset1`: two numeric series (`serie1`, `serie2`), a logical
#' `target`, the index `i` and a `drift` column flagging the change point;
#' \item `dataset2`: the same series without the target;
#' \item `univariate`: a univariate series (`serie`) with four known change
#' points flagged in `event`.
#' }
#'
#' @docType data
#' @usage data(st_drift_examples)
#' @format A list of three data frames with 500 rows each.
#' @keywords datasets
#' @references \href{https://github.com/cefet-rj-dal/heimdall}{heimdall package}
#' @source \href{https://github.com/cefet-rj-dal/heimdall}{heimdall package}
#' @examples
#' data(st_drift_examples)
#' dataset <- st_drift_examples$dataset1
#' which(dataset$drift == 1)
"st_drift_examples"
