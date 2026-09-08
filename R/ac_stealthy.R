#'@title Stealthy
#'@description Ancestor class for drift adaptive models. It wraps a predictive
#'model and a drift detector, retraining the model whenever a drift is
#'reported.
#'@param model The algorithm object to be used for predictions
#'@param drift_method The algorithm object to detect drifts
#'@param norm_class Class used to perform normalization
#'@param warmup_size Number of rows used to warmup the drifter. No drift will be detected during this phase
#'@param th The threshold to be used with classification algorithms
#'@param target_uni_drifter Passes the prediction target to the drifts as the target feat when the drifter is univariate and dist_based.
#'@param incremental_memory If true, the model will retrain with all available data whenever the fit is called. If false, it only retrains when a drift is detected.
#'@param active_warmup If true, the models will be retrained for each batch while the number of rows for the warmup_size is not met.
#'@param class_balance Class balancing strategy. `"buffer"` sets a buffer with the last `warmup_size/2` rows for each class. `"inactive"` does nothing.
#'@param obsolete_model Strategy used by `predict()` while no model is fitted. One of `"null"` (default, returns all `FALSE`), `"majority"` (majority class classifier) or `"last_model"` (keeps using the previous model).
#'@param reporting If TRUE, the per-batch drifter inputs and outputs are accumulated in `drifter_input` and `drifter_output`. Keep it FALSE on long streams, since the accumulated frames grow with the whole history.
#'@param verbose if TRUE shows drift messages
#'@return Stealthy object
#'@details The batch is evaluated before the model is updated: error-based
#'detectors see prequential residuals, computed with the model as it was at the
#'beginning of the call, and the detector is fed the incoming batch exactly
#'once per call.
#'@examples
<<<<<<< HEAD
#'# See ?dfr_ddm for a drift detector that can be plugged here, and the
#'# online prediction walkthroughs at
#'# https://github.com/cefet-rj-dal/heimdall/tree/main/examples/2_online_prediction
=======
#'# See ?dfr_ddm for an example of a drift detector that can be plugged here.
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
#'@import daltoolbox
#'@import stats
#'@importFrom caret dummyVars
#'@export
<<<<<<< HEAD
stealthy <- function(model, drift_method, norm_class=daltoolbox::zscore(), warmup_size=100, th=0.5, target_uni_drifter=FALSE, incremental_memory=FALSE, active_warmup=FALSE, class_balance="inactive", obsolete_model="null", reporting=FALSE, verbose=FALSE){
  class_balance <- match.arg(class_balance, c('inactive', 'buffer'))
  obsolete_model <- match.arg(obsolete_model, c('null', 'majority', 'last_model'))
  .check_positive_integer(warmup_size, "warmup_size", min_value = 1L)
  .check_probability(th, "th")

=======
stealthy <- function(model, drift_method, monitored_features = NULL, norm_class = daltoolbox::zscore(), warmup_size = 100, th = 0.5, target_uni_drifter = FALSE, incremental_memory = TRUE, verbose = FALSE, reporting = FALSE) {
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
  obj <- dal_base()
  obj$dummy <- NULL
  obj$model <- model
  obj$fitted <- FALSE
  obj$drift_method <- drift_method
  obj$drifted <- FALSE
  obj$x_train <- data.frame()
  obj$y_train <- data.frame()
  obj$th <- th
  obj$norm_model <- norm_class
  obj$warmup_size <- warmup_size
  obj$target_uni_drifter <- target_uni_drifter
  obj$incremental_memory <- incremental_memory
  obj$active_warmup <- active_warmup
  obj$class_balance <- class_balance
  obj$obsolete_model <- obsolete_model
  obj$reporting <- reporting
  obj$train_model <- TRUE
  if (class_balance == 'buffer'){
    obj$class_buffer <- list(
      true = list(x = NULL, y = NULL),
      false = list(x = NULL, y = NULL)
    )
  }
  obj$verbose <- verbose
  attr(obj, 'class') <- 'stealthy'
  return(obj)
}

#'@export
<<<<<<< HEAD
update_state.stealthy <- function(obj, value, ...){
  output <- update_state(obj$drift_method, value)
  obj$drift_method <- output$obj
  if (output$drift){
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = output$drift))
}

# One-hot encodes and normalizes an incoming batch using the encoders that were
# fitted on the training data.
#' @noRd
.stealthy_project_batch <- function(obj, x){
  x_oh <- data.frame(predict(obj$dummy, newdata = x))
  if (!all(obj$dummy$feat_names %in% names(x_oh))){
    warning('stealthy: Some categories present on train are not on the most recent dataset. Creating zero columns.')
    for (feat in obj$dummy$feat_names){
      if (!(feat %in% names(x_oh))){
        x_oh[feat] <- 0
      }
    }
  }
  x_oh <- x_oh[obj$dummy$feat_names]
  if('nrm_memory' %in% class(obj$norm_model)){
    obj$norm_model$data <- rbind(obj$norm_model$data, x_oh)
  }
  return(list(obj, transform(obj$norm_model, x_oh)))
}

#'@export
fit.stealthy <- function(obj, x, y, ...){

  fit_drifter_input <- NULL
  obj$drift_method$drifter_output <- NULL

  obj$drifted <- FALSE

  # Drift check. It always runs before the models are updated, so that
  # error-based detectors observe prequential residuals.
  if (obj$fitted && (nrow(obj$x_train) >= obj$warmup_size)){
    proj_batch_result <- .stealthy_project_batch(obj, x)
    obj <- proj_batch_result[[1]]
    norm_batch <- proj_batch_result[[2]]

    if ('error_based' %in% class(obj$drift_method)){
      predictions <- predict(obj$model, norm_batch)
      y_pred <- predictions[, 2] > obj$th

      model_result <- !(as.logical(y[, 1]) == y_pred)
      model_result <- model_result[complete.cases(model_result)]

      fit_drifter_input <- model_result
      obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
    } else if ('dist_based' %in% class(obj$drift_method)){
      if (is.null(obj$drift_method$target_feat)){
        fit_drifter_input <- rowMeans(norm_batch)
      } else if (obj$target_uni_drifter){
        fit_drifter_input <- y[, 1] * 1
      } else {
        fit_drifter_input <- norm_batch[, obj$drift_method$target_feat]
=======
update_state.stealthy <- function(obj, value, ...) {
  output <- update_state(obj$drift_method, value)
  obj$drift_method <- output$obj
  if (output$drift) {
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = output$drift))
}

#'@export
fit.stealthy <- function(obj, x, y, ...) {
  if (obj$reporting) {
    drift_input <- c()
  }
  if (is.null(obj$monitored_features)) {
    monitored_features <- names(x)
  } else {
    monitored_features <- obj$monitored_features
  }

  # Check Drift
  obj$drifted <- FALSE
  if (obj$fitted && (nrow(obj$x_train) >= obj$warmup_size)) {
    x_oh <- data.frame(predict(obj$dummy, newdata = x))
    if (!all(obj$dummy$feat_names %in% names(x_oh))) {
      warning('stealthy: Some categories present on train are not on the most recent dataset. Creating zero columns.')
      for (feat in obj$dummy$feat_names) {
        if (!(feat %in% names(x_oh))) {
          x_oh[feat] <- 0
        }
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
      }
      obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
    } else if (any(c('mv_dist_based', 'multi_criteria', 'dummy') %in% class(obj$drift_method))){
      fit_drifter_input <- norm_batch
      obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
    }
<<<<<<< HEAD

    if (obj$drift_method$drifted){
      if (obj$verbose){
        message('Stealthy detected a drift, discarding old data')
=======
    norm_x_oh <- transform(obj$norm_model, x_oh)
    if (obj$reporting) {
      obj$norm_x_oh <- norm_x_oh
    }

    if ('error_based' %in% class(obj$drift_method)) {
      predictions <- predict(obj$model, norm_x_oh)
      y_pred <- predictions[, 2] > obj$th

      model_result <- !(as.logical(y[, 1]) == y_pred)
      model_result <- model_result[complete.cases(model_result)]

      if (obj$reporting) {
        drift_input <- model_result
      }
      obj$drift_method <- fit(obj$drift_method, model_result)
    }

    if ('dist_based' %in% class(obj$drift_method)) {
      if (is.null(obj$drift_method$target_feat)) {
        norm_x_oh[, 'mean'] <- rowMeans(norm_x_oh)
        if (obj$reporting) {
          drift_input <- norm_x_oh[, 'mean']
        }
        obj$drift_method <- fit(obj$drift_method, norm_x_oh[, 'mean'])
      } else if (obj$target_uni_drifter) {
        if (obj$reporting) {
          drift_input <- y[, 1] * 1
        }
        obj$drift_method <- fit(obj$drift_method, y[, 1] * 1)
      } else {
        if (obj$reporting) {
          drift_input <- x_oh[, obj$drift_method$target_feat]
        }
        obj$drift_method <- fit(obj$drift_method, x_oh[, obj$drift_method$target_feat])
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
      }
      obj$x_train <- data.frame()
      obj$y_train <- data.frame()
      obj$drift_method <- reset_state(obj$drift_method)
      obj$drifted <- TRUE
      obj$fitted <- FALSE
    }
<<<<<<< HEAD
  }

  # Class balance
  if (obj$class_balance == 'buffer'){
    obj$class_buffer[['true']][['x']] <- tail(rbind(obj$class_buffer[['true']][['x']], x[y == 1, , drop=FALSE]), obj$warmup_size/2)
    obj$class_buffer[['true']][['y']] <- tail(rbind(obj$class_buffer[['true']][['y']], y[y == 1, 1, drop=FALSE]), obj$warmup_size/2)
    obj$class_buffer[['false']][['x']] <- tail(rbind(obj$class_buffer[['false']][['x']], x[y == 0, , drop=FALSE]), obj$warmup_size/2)
    obj$class_buffer[['false']][['y']] <- tail(rbind(obj$class_buffer[['false']][['y']], y[y == 0, 1, drop=FALSE]), obj$warmup_size/2)

    enough_true <- NROW(obj$class_buffer[['true']][['y']]) >= (obj$warmup_size/2)
    enough_false <- NROW(obj$class_buffer[['false']][['y']]) >= (obj$warmup_size/2)
    obj$train_model <- enough_true && enough_false
  } else {
    obj$train_model <- TRUE
  }

  # Update models
  if (obj$incremental_memory || (!obj$fitted) || (obj$active_warmup && (nrow(obj$x_train) < obj$warmup_size))){
    obj$x_train <- rbind(obj$x_train, x)
    obj$y_train <- rbind(obj$y_train, y)

    enough_rows <- (nrow(obj$x_train) >= obj$warmup_size) || obj$active_warmup
    if (enough_rows && obj$train_model){
      if (obj$class_balance == 'buffer'){
        x_model_train <- rbind(obj$class_buffer[['false']][['x']], obj$class_buffer[['true']][['x']])
        y_model_train <- rbind(obj$class_buffer[['false']][['y']], obj$class_buffer[['true']][['y']])
      } else {
        x_model_train <- obj$x_train
        y_model_train <- obj$y_train
      }

      # One hot encoding, shared by the model and by the drift monitoring
      obj$dummy <- caret::dummyVars(" ~ .", data = x_model_train)
      x_train_dummy <- data.frame(predict(obj$dummy, newdata = x_model_train))
=======

    if ('mv_dist_based' %in% class(obj$drift_method)) {
      if (obj$reporting) {
        drift_input <- norm_x_oh
      }
      obj$drift_method <- fit(obj$drift_method, norm_x_oh)
    }

    if ('multi_criteria' %in% class(obj$drift_method)) {
      if (obj$reporting) {
        drift_input <- norm_x_oh
      }
      obj$drift_method <- fit(obj$drift_method, norm_x_oh)
    }

    if (obj$drift_method$drifted) {
      if (obj$verbose) {
        message('Stealthy detected a drift, discarding old data')
      }
      obj$x_train <- c()
      obj$y_train <- c()
      obj$drift_method <- reset_state(obj$drift_method)
      obj$drifted <- TRUE
      obj$fitted <- FALSE
    }
  }

  # Define update models
  if (obj$incremental_memory || (!obj$fitted)) {
    # Aggregate new data
    obj$x_train <- rbind(obj$x_train, x)
    obj$y_train <- rbind(obj$y_train, y)

    if (nrow(obj$x_train) >= obj$warmup_size) {
      # One Hot Encoding
      obj$dummy <- caret::dummyVars(" ~ .", data = obj$x_train)
      x_train_dummy <- data.frame(predict(obj$dummy, newdata = obj$x_train))
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
      obj$dummy$feat_names <- names(x_train_dummy)

      # Normalize
      obj$norm_model <- fit(obj$norm_model, x_train_dummy)
<<<<<<< HEAD
      norm_train <- transform(obj$norm_model, x_train_dummy)

      # Fit model
      norm_data <- cbind(norm_train, y_model_train)
=======
      norm_data <- cbind(transform(obj$norm_model, x_train_dummy), obj$y_train)

      # Fit model
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
      obj$model <- fit(obj$model, norm_data)
      obj$model$feat_names <- names(norm_data)

      obj$fitted <- TRUE
    }
  }

<<<<<<< HEAD
  # Reporting
  if (obj$reporting){
    fit_drifter_output <- .stealthy_align_output(obj, x)
    if (is.null(obj$drifter_output)){
      obj$drifter_output <- fit_drifter_output
    } else {
      obj$drifter_output <- rbind(obj$drifter_output[, names(fit_drifter_output), drop=FALSE], fit_drifter_output)
    }
    rownames(obj$drifter_output) <- seq_len(nrow(obj$drifter_output))
    obj$drifter_input <- rbind(obj$drifter_input, cbind(x, y))
=======
  if (obj$reporting) {
    obj$drift_input <- drift_input
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
  }

  return(obj)
}

# Pads the detector diagnostics so that every batch contributes exactly nrow(x)
# rows to the reporting frame.
#' @noRd
.stealthy_align_output <- function(obj, x){
  fit_drifter_output <- tail(obj$drift_method$drifter_output, nrow(x))

  if (is.null(fit_drifter_output) || (nrow(fit_drifter_output) == 0L)){
    fit_drifter_output <- as.data.frame(matrix(NA, nrow = nrow(x), ncol = 1))
    return(fit_drifter_output)
  }

  missing_n <- nrow(x) - nrow(fit_drifter_output)
  if (missing_n > 0){
    extra <- as.data.frame(matrix(NA, nrow = missing_n, ncol = ncol(fit_drifter_output)))
    names(extra) <- names(fit_drifter_output)
    fit_drifter_output <- rbind(fit_drifter_output, extra)
  }
  return(fit_drifter_output)
}

#'@export
<<<<<<< HEAD
predict.stealthy <- function(object, data, ...){

  if (!object$fitted){
    if (object$obsolete_model == 'null'){
      output <- c()
      for (i in seq_along(object$model$slevels)){
        output <- cbind(output, vector(mode='logical', length=nrow(data)))
      }
      output <- as.data.frame(output)
      names(output) <- object$model$slevels
      return(output)
    }
    if (is.null(object$dummy)){
      stop("predict.stealthy: no model has been fitted yet, so 'obsolete_model' cannot fall back to a previous one.", call. = FALSE)
=======
predict.stealthy <- function(object, data, ...) {
  # Return format if not fitted
  if (!object$fitted) {
    output <- c()
    for (i in seq_along(object$model$slevels)) {
      output <- cbind(output, vector(mode = 'logical', length = nrow(data)))
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
    }
  }

<<<<<<< HEAD
  prediction_model <- object$model
  if ((!object$fitted) && (object$obsolete_model == 'majority')){
    prediction_model <- daltoolbox::cla_majority(object$model$attribute, object$model$slevels)
  }

  data_oh <- data.frame(predict(object$dummy, newdata = data))
  for (feat in object$dummy$feat_names){
    if (!(feat %in% names(data_oh))){
=======
  # Prediction if fitted
  data_oh <- data.frame(predict(object$dummy, newdata = data))
  for (feat in object$model$feat_names) {
    if (!(feat %in% names(data_oh))) {
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
      data_oh[feat] <- 0
    }
  }
  data_oh <- data_oh[object$dummy$feat_names]
  norm_data_oh <- transform(object$norm_model, data_oh)

  return(predict(prediction_model, norm_data_oh))
}
