#'@title Metric
#'@description Ancestor class for metric calculation
#'@return Metric object
#'@examples
#'# See ?metric for an example of DDM drift detector
#'@import daltoolbox
#'@importFrom Metrics precision recall
#'@export
metric <- function(){
  obj <- dal_base()
  attr(obj, 'class') <- 'metric'
  return(obj)
}

#'@title Accuracy Calculator
#'@description Class for accuracy calculation
#'@return Metric object
#'@examples
#'# See ?mt_accuracy for an example of Accuracy Calculator
#'@export
mt_accuracy <- function(){
  obj <- metric()
  class(obj) <- append("mt_accuracy", class(obj))
  return(obj)
}

#'@export
evaluate.mt_accuracy <- function(obj, y_pred, y_true, ...){
  return(mean(y_pred==y_true, na.rm=TRUE))
}

#'@title Precision Calculator
#'@description Class for precision calculation
#'@return Metric object
#'@examples
#'# See ?mt_precision for an example of Precision Calculator
#'@export
mt_precision <- function(){
  obj <- metric()
  class(obj) <- append("mt_precision", class(obj))
  return(obj)
}

#'@export
evaluate.mt_precision <- function(obj, y_pred, y_true, ...){
  return(Metrics::precision(y_true, y_pred))
}

#'@title Recall Calculator
#'@description Class for recall calculation
#'@return Metric object
#'@examples
#'# See ?mt_recall for an example of Recall Calculator
#'@export
mt_recall <- function() {
  obj <- metric()
  class(obj) <- append("mt_recall", class(obj))
  return(obj)
}

#'@export
evaluate.mt_recall <- function(obj, y_pred, y_true, ...){
  return(Metrics::recall(y_true, y_pred))
}

#'@title FScore Calculator
#'@description Class for FScore calculation
#'@param f The beta parameter of the F-beta score. `f = 1` (the default) gives the usual F1 score, values below 1 weight precision more heavily and values above 1 weight recall more heavily.
#'@return Metric object
#'@examples
#'# See ?mt_fscore for an example of FScore Calculator
#'@export
mt_fscore <- function(f=1) {
  if (!is.numeric(f) || (length(f) != 1L) || is.na(f) || (f <= 0)) {
    stop("f must be a single positive numeric value", call. = FALSE)
  }
  obj <- metric()
  obj$f <- f
  class(obj) <- append("mt_fscore", class(obj))
  return(obj)
}

#'@export
evaluate.mt_fscore <- function(obj, y_pred, y_true, ...){
  beta <- obj$f
  if (is.null(beta)) {
    beta <- 1
  }

  precision <- evaluate(mt_precision(), y_pred, y_true)
  recall <- evaluate(mt_recall(), y_pred, y_true)

  denominator <- (beta^2 * precision) + recall
  if (is.na(denominator) || (denominator == 0)) {
    return(NA_real_)
  }

  fscore <- (1 + beta^2) * (precision * recall) / denominator

  return(fscore)
}

#'@title ROC AUC Calculator
#'@description Class for QOC AUC calculation
#'@return Metric object
#'@examples
#'# See ?mt_rocauc for an example of ROC AUC Calculator
#'@export
mt_rocauc <- function() {
  obj <- metric()
  class(obj) <- append("mt_rocauc", class(obj))
  return(obj)
}

#'@importFrom pROC auc
#'@export
evaluate.mt_rocauc <- function(obj, y_pred, y_true, ...){
  y_pred[is.na(y_pred)] <- FALSE

  pred_values <- unlist(y_pred, use.names = FALSE)
  true_values <- unlist(y_true, use.names = FALSE)

  if ((length(unique(true_values)) < 2) || (length(unique(pred_values)) < 2)) {
    warning('mt_rocauc: ROC AUC is undefined when the observed or the predicted values are constant. Returning NA.')
    return(NA_real_)
  }

  lv <- levels(y_true)
  if (is.null(lv)) {
    lv <- levels(as.factor(true_values))
  }

  rocauc <- pROC::auc(pROC::roc(y_true, y_pred, levels = lv, direction = '<', quiet = TRUE))

  return(as.numeric(rocauc))
}
