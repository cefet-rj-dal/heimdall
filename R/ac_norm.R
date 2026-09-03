#'@title Normalizer base class
#'@description Ancestor class for normalization techniques.
#'@details This constructor was named `norm()` up to version 1.2.x. It was
#'renamed to `nrm_base()` because `norm()` masks [base::norm()] once the
#'package is attached.
#'@param norm_class Normalizer class
#'@return Norm object
#'@examples
#'library(daltoolbox)
#'obj <- nrm_base(norm_class = minmax())
#'class(obj)
#'@import daltoolbox
#'@export
nrm_base <- function(norm_class) {
  obj <- dal_base()

  obj$model <- norm_class
  obj$data <- NULL

  attr(obj, 'class') <- 'norm'
  return(obj)
}

#'@title Memory Normalizer
#'@description Normalizer that keeps its own memory of the data seen so far, so
#'that the normalization parameters are estimated over the whole history rather
#'than over the most recent batch alone.
#'@param norm_class Normalizer class
#'@return Norm object
#'@examples
#'library(daltoolbox)
#'
#'obj <- nrm_memory(norm_class = minmax())
#'obj <- fit(obj, data.frame(x = c(1, 2, 3)))
#'transform(obj, data.frame(x = c(1, 2, 3)))
#'@export
nrm_memory <- function(norm_class = daltoolbox::minmax()) {
  obj <- nrm_base(norm_class = norm_class)
  class(obj) <- append("nrm_memory", class(obj))
  return(obj)
}

#'@export
fit.nrm_memory <- function(obj, data, ...) {
  data <- as.data.frame(data)

  if (is.null(obj$data) || (nrow(obj$data) == 0L)) {
    obj$data <- data
  } else {
    missing_in_history <- setdiff(names(data), names(obj$data))
    if (length(missing_in_history) > 0L) {
      warning('nrm_memory: Some categories present in most recent data are not on the history dataset. Creating zero columns.')
      obj$data[missing_in_history] <- 0
    }

    missing_in_recent <- setdiff(names(obj$data), names(data))
    if (length(missing_in_recent) > 0L) {
      warning('nrm_memory: Some categories present in history data are not on the most recent dataset. Creating zero columns.')
      data[missing_in_recent] <- 0
    }

    obj$data <- rbind(obj$data, data[names(obj$data)])
  }

  obj$data <- obj$data[!duplicated(obj$data), , drop = FALSE]
  obj$model <- fit(obj$model, obj$data)

  return(obj)
}

#'@export
transform.nrm_memory <- function(obj, data, ...) {
  data <- as.data.frame(data)

  if (!is.null(obj$data)) {
    missing_in_recent <- setdiff(names(obj$data), names(data))
    if (length(missing_in_recent) > 0L) {
      warning('nrm_memory: Some categories present in history data are not on the most recent dataset. Creating zero columns.')
      data[missing_in_recent] <- 0
    }
    data <- data[names(obj$data)]
  }

  tf_data <- transform(obj$model, data)

  return(tf_data)
}

#'@export
inverse_transform.nrm_memory <- function(obj, data, ...) {
  tf_data <- inverse_transform(obj$model, as.data.frame(data))

  return(tf_data)
}
