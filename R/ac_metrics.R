#'@title Metric
#'@description Ancestor class for metric calculation
#'@return Metric object
#'@examples
#'# See ?metric for an example of DDM drift detector
#'@import daltoolbox
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
  return(mean(y_true[y_pred==TRUE], na.rm=TRUE))
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
  return(mean(y_pred[(y_true==TRUE)], na.rm=TRUE))
}

#'@title FScore Calculator
#'@description Class for FScore calculation
#'@param f The F parameter for the F-Score metric
#'@return Metric object
#'@examples
#'# See ?mt_fscore for an example of FScore Calculator
#'@export
mt_fscore <- function(f=1) {
  obj <- metric()
  obj$f <- f
  class(obj) <- append("mt_fscore", class(obj))
  return(obj)
}

#'@export
evaluate.mt_fscore <- function(obj, y_pred, y_true, ...){
  precision <- evaluate(mt_precision(), y_pred, y_true)
  recall <- evaluate(mt_recall(), y_pred, y_true)
  
  fscore <- (2 * (precision * recall)) / (precision + recall)
  
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

#'@import pROC
#'@export
evaluate.mt_rocauc <- function(obj, y_pred, y_true, ...){
  y_pred[is.na(y_pred)] <- FALSE
  
  if((sum(as.numeric(unlist(y_pred))-1) == 0) | (sum(as.numeric(unlist(y_true))-1) == 0)){
    return(0)
  }else{
    rocauc <- pROC::auc(pROC::roc(y_true, y_pred, levels=levels(y_true), direction='<'))
    
    return(rocauc)
  }
}