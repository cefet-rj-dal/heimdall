#' @title Stealthy
#' @description Ancestor class for drift adaptive models. It wraps a predictive
#' model and a drift detector, retraining the model whenever a drift is
#' reported.
#' @param model The algorithm object to be used for predictions
#' @param drift_method The algorithm object to detect drifts
#' @param norm_class Class used to perform normalization
#' @param warmup_size Number of rows used to warmup the drifter. No drift will be detected during this phase
#' @param th The threshold to be used with classification algorithms
#' @param target_uni_drifter Passes the prediction target to the drifts as the target feat when the drifter is univariate and dist_based. The target must be binary (logical, 0/1 or a factor with those labels).
#' @param incremental_memory If true, the model will retrain with all available data whenever the fit is called. If false, it only retrains when a drift is detected.
#' @param active_warmup If true, the models will be retrained for each batch while the number of rows for the warmup_size is not met.
#' @param class_balance Class balancing strategy. `"buffer"` sets a buffer with the last `warmup_size/2` rows for each class. `"inactive"` does nothing.
#' @param obsolete_model Strategy used by `predict()` while no model is fitted. One of `"null"` (default, returns all `FALSE`), `"majority"` (majority class classifier) or `"last_model"` (keeps using the previous model).
#' @param reporting If TRUE, every batch given to `fit()` (`x` and `y`) is accumulated in `drifter_input`.
#' @param history_size Maximum number of rows kept in `drifter_output` and
#' `drifter_input`; the oldest rows are dropped first. `NULL` (the default) keeps
#' the whole stream, so set it on long streams to bound memory.
#' @param verbose if TRUE shows drift messages
#' @return Stealthy object
#' @details The drifter decides what it monitors through its own `target_feat`,
#' given in terms of the one-hot encoded and normalized columns: a univariate
#' drifter with a `target_feat` receives that column, a univariate drifter
#' without one receives the row mean of every column, and multivariate,
#' multi-criteria and dummy drifters receive the selected columns (every column
#' when their `target_feat` is `NULL`).
#'
#' The batch is evaluated before the model is updated: error-based
#' detectors see prequential residuals, computed with the model as it was at the
#' beginning of the call, and the detector is fed the incoming batch exactly
#' once per call.
#'
#' `obj$drifter_output` holds the diagnostics reported by the drifter (for
#' example the window width of ADWIN or the p-values of KSWIN; see the
#' `drifter_output` of each detector), with one row per row of `x` across every
#' call to `fit()`. Rows are `NA` while the drifter is not monitoring (during the
#' warmup, or after a drift until the model is retrained) and for observations
#' it skipped, such as error-based rows without a prediction.
#' @examples
#' # See ?dfr_ddm for a drift detector that can be plugged here, and the
#' # online prediction walkthroughs at
#' # https://github.com/cefet-rj-dal/heimdall/tree/main/examples/2_online_prediction
#' @import daltoolbox
#' @import stats
#' @export
stealthy <- function(model, drift_method, norm_class = daltoolbox::zscore(), warmup_size = 100, th = 0.5, target_uni_drifter = FALSE, incremental_memory = FALSE, active_warmup = FALSE, class_balance = "inactive", obsolete_model = "null", reporting = FALSE, history_size = NULL, verbose = FALSE) {
  class_balance <- match.arg(class_balance, c("inactive", "buffer"))
  obsolete_model <- match.arg(obsolete_model, c("null", "majority", "last_model"))
  .check_positive_integer(warmup_size, "warmup_size", min_value = 1L)
  .check_probability(th, "th")
  if (!is.null(history_size)) {
    .check_positive_integer(history_size, "history_size", min_value = 1L)
  }
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
  obj$history_size <- history_size
  obj$train_model <- TRUE
  if (class_balance == "buffer") {
    obj$class_buffer <- list(
      true = list(x = NULL, y = NULL),
      false = list(x = NULL, y = NULL)
    )
  }
  obj$verbose <- verbose
  attr(obj, "class") <- "stealthy"
  return(obj)
}

#' @export
update_state.stealthy <- function(obj, value, ...) {
  output <- update_state(obj$drift_method, value)
  obj$drift_method <- output$obj
  if (output$drift) {
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = output$drift))
}

# One-hot encodes and normalizes an incoming batch using the encoders that were
# fitted on the training data.
#' @noRd
.stealthy_project_batch <- function(obj, x) {
  x_oh <- .one_hot_apply(obj$dummy, x)
  if ("nrm_memory" %in% class(obj$norm_model)) {
    obj$norm_model$data <- .keep_last_rows(rbind(obj$norm_model$data, x_oh), obj$norm_model$max_size)
  }
  return(list(obj, transform(obj$norm_model, x_oh)))
}

#' @export
fit.stealthy <- function(obj, x, y, ...) {
  fit_drifter_input <- NULL
  obj$drift_method$drifter_output <- NULL
  # Drifter diagnostics for the rows of x; NA while the drifter is not running.
  batch_output <- NULL

  obj$drifted <- FALSE

  # Drift check. It always runs before the models are updated, so that
  # error-based detectors observe prequential residuals.
  if (obj$fitted && (nrow(obj$x_train) >= obj$warmup_size)) {
    proj_batch_result <- .stealthy_project_batch(obj, x)
    obj <- proj_batch_result[[1]]
    norm_batch <- proj_batch_result[[2]]

    if ("error_based" %in% class(obj$drift_method)) {
      predictions <- predict(obj$model, norm_batch)
      y_pred <- predictions[, 2] > obj$th

      model_result <- !(as.logical(y[, 1]) == y_pred)
      evaluated_rows <- which(complete.cases(model_result))

      fit_drifter_input <- model_result[evaluated_rows]
      obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
      batch_output <- .stealthy_batch_output(obj$drift_method$drifter_output, nrow(x), evaluated_rows)
    } else if ("dist_based" %in% class(obj$drift_method)) {
      if (is.null(obj$drift_method$target_feat)) {
        fit_drifter_input <- rowMeans(norm_batch)
      } else if (obj$target_uni_drifter) {
        fit_drifter_input <- as.numeric(.as_binary(y[, 1], "y"))
      } else {
        fit_drifter_input <- norm_batch[, obj$drift_method$target_feat]
      }
      obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
    } else if (any(c("mv_dist_based", "multi_criteria", "dummy") %in% class(obj$drift_method))) {
      fit_drifter_input <- norm_batch
      obj$drift_method <- fit(obj$drift_method, fit_drifter_input)
    }
    if (is.null(batch_output)) {
      batch_output <- .stealthy_batch_output(obj$drift_method$drifter_output, nrow(x))
    }

    if (obj$drift_method$drifted) {
      if (obj$verbose) {
        message("Stealthy detected a drift, discarding old data")
      }
      obj$x_train <- data.frame()
      obj$y_train <- data.frame()
      obj$drift_method <- reset_state(obj$drift_method)
      obj$drifted <- TRUE
      obj$fitted <- FALSE
    }
  }

  # Class balance
  if (obj$class_balance == "buffer") {
    obj$class_buffer[["true"]][["x"]] <- tail(rbind(obj$class_buffer[["true"]][["x"]], x[y == 1, , drop = FALSE]), obj$warmup_size / 2)
    obj$class_buffer[["true"]][["y"]] <- tail(rbind(obj$class_buffer[["true"]][["y"]], y[y == 1, 1, drop = FALSE]), obj$warmup_size / 2)
    obj$class_buffer[["false"]][["x"]] <- tail(rbind(obj$class_buffer[["false"]][["x"]], x[y == 0, , drop = FALSE]), obj$warmup_size / 2)
    obj$class_buffer[["false"]][["y"]] <- tail(rbind(obj$class_buffer[["false"]][["y"]], y[y == 0, 1, drop = FALSE]), obj$warmup_size / 2)

    enough_true <- NROW(obj$class_buffer[["true"]][["y"]]) >= (obj$warmup_size / 2)
    enough_false <- NROW(obj$class_buffer[["false"]][["y"]]) >= (obj$warmup_size / 2)
    obj$train_model <- enough_true && enough_false
  } else {
    obj$train_model <- TRUE
  }

  # Update models
  if (obj$incremental_memory || (!obj$fitted) || (obj$active_warmup && (nrow(obj$x_train) < obj$warmup_size))) {
    obj$x_train <- rbind(obj$x_train, x)
    obj$y_train <- rbind(obj$y_train, y)

    enough_rows <- (nrow(obj$x_train) >= obj$warmup_size) || obj$active_warmup
    if (enough_rows && obj$train_model) {
      if (obj$class_balance == "buffer") {
        x_model_train <- rbind(obj$class_buffer[["false"]][["x"]], obj$class_buffer[["true"]][["x"]])
        y_model_train <- rbind(obj$class_buffer[["false"]][["y"]], obj$class_buffer[["true"]][["y"]])
      } else {
        x_model_train <- obj$x_train
        y_model_train <- obj$y_train
      }

      # One hot encoding, shared by the model and by the drift monitoring
      obj$dummy <- .one_hot_fit(x_model_train)
      x_train_dummy <- .one_hot_apply(obj$dummy, x_model_train)

      # Normalize
      obj$norm_model <- fit(obj$norm_model, x_train_dummy)
      norm_train <- transform(obj$norm_model, x_train_dummy)

      # Fit model
      norm_data <- cbind(norm_train, y_model_train)
      obj$model <- fit(obj$model, norm_data)
      obj$model$feat_names <- names(norm_data)

      obj$fitted <- TRUE
    }
  }

  # Drifter output, one row per row of x across the whole stream
  if (is.null(batch_output)) {
    batch_output <- data.frame(row.names = seq_len(nrow(x)))
  }
  obj$drifter_output <- .keep_last_rows(.bind_rows_fill(obj$drifter_output, batch_output), obj$history_size)

  # Reporting
  if (obj$reporting) {
    obj$drifter_input <- .keep_last_rows(rbind(obj$drifter_input, cbind(x, y)), obj$history_size)
  }

  return(obj)
}

# Places the diagnostics of the observations fed to the drifter on the rows of
# the batch they come from; the other rows are NA.
#' @noRd
.stealthy_batch_output <- function(drifter_output, n_rows, rows = seq_len(n_rows)) {
  if (is.null(drifter_output) || (ncol(drifter_output) == 0L)) {
    return(data.frame(row.names = seq_len(n_rows)))
  }
  output <- lapply(drifter_output, function(column) {
    values <- rep(NA, n_rows)
    values[rows] <- column
    values
  })
  output <- as.data.frame(output, check.names = FALSE)
  rownames(output) <- seq_len(n_rows)
  return(output)
}

#' @export
predict.stealthy <- function(object, data, ...) {
  if (!object$fitted) {
    if (object$obsolete_model == "null") {
      output <- c()
      for (i in seq_along(object$model$slevels)) {
        output <- cbind(output, vector(mode = "logical", length = nrow(data)))
      }
      output <- as.data.frame(output)
      names(output) <- object$model$slevels
      return(output)
    }
    if (is.null(object$dummy)) {
      stop("predict.stealthy: no model has been fitted yet, so 'obsolete_model' cannot fall back to a previous one.", call. = FALSE)
    }
  }
  prediction_model <- object$model
  if ((!object$fitted) && (object$obsolete_model == "majority")) {
    prediction_model <- daltoolbox::cla_majority(object$model$attribute, object$model$slevels)
  }

  data_oh <- .one_hot_apply(object$dummy, data)
  norm_data_oh <- transform(object$norm_model, data_oh)

  return(predict(prediction_model, norm_data_oh))
}
