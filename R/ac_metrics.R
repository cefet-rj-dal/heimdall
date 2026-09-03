#'@title Metric
#'@description Ancestor class for metric calculation
#'@return Metric object
#'@examples
#'obj <- metric()
#'class(obj)
#'@import daltoolbox
#'@importFrom Metrics precision recall
#'@export
metric <- function() {
  obj <- dal_base()
  attr(obj, 'class') <- 'metric'
  return(obj)
}

#'@title Accuracy Calculator
#'@description Class for accuracy calculation
#'@return Metric object
#'@examples
#'library(daltoolbox)
#'obj <- mt_accuracy()
#'evaluate(obj, c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
#'@export
mt_accuracy <- function() {
  obj <- metric()
  class(obj) <- append("mt_accuracy", class(obj))
  return(obj)
}

#'@export
evaluate.mt_accuracy <- function(obj, y_pred, y_true, ...) {
  return(mean(y_pred == y_true, na.rm = TRUE))
}

#'@title Precision Calculator
#'@description Class for precision calculation
#'@return Metric object
#'@examples
#'library(daltoolbox)
#'obj <- mt_precision()
#'evaluate(obj, c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
#'@export
mt_precision <- function() {
  obj <- metric()
  class(obj) <- append("mt_precision", class(obj))
  return(obj)
}

#'@export
evaluate.mt_precision <- function(obj, y_pred, y_true, ...) {
  return(Metrics::precision(y_true, y_pred))
}

#'@title Recall Calculator
#'@description Class for recall calculation
#'@return Metric object
#'@examples
#'library(daltoolbox)
#'obj <- mt_recall()
#'evaluate(obj, c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
#'@export
mt_recall <- function() {
  obj <- metric()
  class(obj) <- append("mt_recall", class(obj))
  return(obj)
}

#'@export
evaluate.mt_recall <- function(obj, y_pred, y_true, ...) {
  return(Metrics::recall(y_true, y_pred))
}

#'@title FScore Calculator
#'@description Class for F-Score calculation. The `f` parameter is the beta of
#'the F-beta score, so `f = 1` (the default) gives the usual F1 score, values
#'below 1 weight precision more heavily and values above 1 weight recall more
#'heavily.
#'@param f The beta parameter for the F-Score metric
#'@return Metric object
#'@examples
#'library(daltoolbox)
#'obj <- mt_fscore(f = 1)
#'evaluate(obj, c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
#'@export
mt_fscore <- function(f = 1) {
  if (!is.numeric(f) || (length(f) != 1L) || is.na(f) || (f <= 0)) {
    stop("f must be a single positive numeric value", call. = FALSE)
  }
  obj <- metric()
  obj$f <- f
  class(obj) <- append("mt_fscore", class(obj))
  return(obj)
}

#'@export
evaluate.mt_fscore <- function(obj, y_pred, y_true, ...) {
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
#'@description Class for ROC AUC calculation
#'@return Metric object
#'@examples
#'library(daltoolbox)
#'obj <- mt_rocauc()
#'evaluate(obj, c(0.9, 0.2, 0.8, 0.1), factor(c(TRUE, TRUE, FALSE, FALSE)))
#'@export
mt_rocauc <- function() {
  obj <- metric()
  class(obj) <- append("mt_rocauc", class(obj))
  return(obj)
}

#'@importFrom pROC auc
#'@export
evaluate.mt_rocauc <- function(obj, y_pred, y_true, ...) {
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
