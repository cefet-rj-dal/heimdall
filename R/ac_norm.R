#'@title Norm
#'@description Ancestor class for normalizarion techniques
#'@return Norm object
#'@examples
#'# See ?norm for an example of DDM drift detector
#'@import daltoolbox
#'@export
norm <- function(norm_class){
  obj <- dal_base()
  
  obj$model <- norm_class
  obj$data <- c()
  
  attr(obj, 'class') <- 'norm'
  return(obj)
}

#'@title Memory Normalizer
#'@description Normalizer that has own memory
#'@return Norm object
#'@examples
#'# See ?nrm_mimax for an example of Memory Normalizer
#'@export
nrm_memory <- function(norm_class=minmax()){
  obj <- norm(norm_class=norm_class)
  class(obj) <- append("nrm_memory", class(obj))
  return(obj)
}

#'@export
fit.nrm_memory <- function(obj, data, ...){
  
  if(!all(names(data) %in% names(obj$data))){
    warning('nrm_memory: Some categories present in most recent data are not on the history dataset. Creating zero columns.')
    for (feat in names(data)){
      if (!(feat %in% names(obj$data))){
        obj$data[feat] <- 0
      }
    }
  }
  if(!all(names(obj$data) %in% names(data))){
    warning('nrm_memory: Some categories present in history data are not on the most recent dataset. Creating zero columns.')
    for (feat in names(obj$data)){
      if (!(feat %in% names(data))){
        data[feat] <- 0
      }
    }
  }
  
  obj$data <- rbind(obj$data, data)
  obj$data <- obj$data[!duplicated(obj$data),]
  obj$model <- fit(obj$model, obj$data)
  
  return(obj)
}

#'@export
transform.nrm_memory <- function(obj, data, ...){
  
  if(!all(names(obj$data) %in% names(data))){
    warning('nrm_memory: Some categories present in history data are not on the most recent dataset. Creating zero columns.')
    for (feat in names(obj$data)){
      if (!(feat %in% names(data))){
        data[feat] <- 0
      }
    }
  }
  
  tf_data <- transform(obj$model, data)
  
  return(tf_data)
}

#'@export
inverse_transform.nrm_memory <- function(obj, data, ...){
  
  tf_data <- inverse_transform(obj$model, obj$data)
  
  return(tf_data)
}
