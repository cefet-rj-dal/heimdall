# Internal helpers shared by the heimdall detectors.
# These functions are not exported.

#' Validate a probability-like parameter
#' @noRd
.check_probability <- function(value, name) {
  if (!is.numeric(value) || (length(value) != 1L) || is.na(value)) {
    stop(sprintf("%s must be a single numeric value", name), call. = FALSE)
  }
  if ((value < 0) || (value > 1)) {
    stop(sprintf("%s must be between 0 and 1", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a real-valued parameter
#' @noRd
.check_number <- function(value, name, min_value = -Inf) {
  if (!is.numeric(value) || (length(value) != 1L) || is.na(value) || is.infinite(value)) {
    stop(sprintf("%s must be a single finite numeric value", name), call. = FALSE)
  }
  if (value < min_value) {
    stop(sprintf("%s must be greater than or equal to %s", name, min_value), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a positive integer-like parameter
#' @noRd
.check_positive_integer <- function(value, name, min_value = 1L) {
  .check_number(value, name, min_value)
  if (value != floor(value)) {
    stop(sprintf("%s must be a whole number", name), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate the name of the monitored feature
#' @noRd
.check_target_feat <- function(target_feat) {
  if (!is.null(target_feat) &&
      (!is.character(target_feat) || (length(target_feat) != 1L) || is.na(target_feat))) {
    stop("target_feat must be NULL or a single column name", call. = FALSE)
  }
  invisible(TRUE)
}

#' Initial window of the sliding-window detectors
#'
#' Keeps only the most recent `window_size` values of the warm-start data.
#' @noRd
.initial_window <- function(data, window_size) {
  if (is.null(data)) {
    return(numeric(0))
  }
  data <- suppressWarnings(as.numeric(unlist(data, use.names = FALSE)))
  data <- data[is.finite(data)]
  return(utils::tail(data, window_size))
}

#' Levene's test for homogeneity of variance (Brown-Forsythe variant)
#'
#' Replaces the previous dependency on `car::leveneTest`. Group spread is
#' centred on the median, which is the same default used by `car`.
#'
#' @param values numeric vector of observations
#' @param group grouping factor with the same length as `values`
#' @return the p-value of the test, or `NA_real_` when it cannot be computed
#' @noRd
.levene_pvalue <- function(values, group) {
  values <- as.numeric(values)
  group <- factor(group)

  keep <- !is.na(values) & !is.na(group)
  values <- values[keep]
  group <- droplevels(group[keep])

  k <- nlevels(group)
  n <- length(values)
  if ((k < 2L) || (n <= k)) {
    return(NA_real_)
  }

  centers <- tapply(values, group, stats::median)
  z <- abs(values - centers[as.character(group)])

  z_mean <- mean(z)
  group_means <- tapply(z, group, mean)
  group_sizes <- tapply(z, group, length)

  between <- sum(group_sizes * (group_means - z_mean)^2)
  within <- sum((z - group_means[as.character(group)])^2)

  if (!is.finite(within) || (within <= 0)) {
    return(NA_real_)
  }

  f_stat <- (between / (k - 1)) / (within / (n - k))
  stats::pf(f_stat, df1 = k - 1, df2 = n - k, lower.tail = FALSE)
}

#' Feed a univariate stream to a detector, one observation at a time
#' @noRd
.fit_vector_stream <- function(obj, data) {
  data <- .select_stream(data, obj$target_feat)
  if (length(data) == 0L) {
    stop("fit: 'data' must contain at least one observation", call. = FALSE)
  }
  output <- list(obj = obj, drift = FALSE)
  for (i in seq_along(data)) {
    output <- update_state(output$obj, data[i])
  }
  return(output$obj)
}

#' Feed a multivariate stream to a detector, one row at a time
#' @noRd
.fit_row_stream <- function(obj, data) {
  data <- as.data.frame(data)
  if (nrow(data) == 0L) {
    stop("fit: 'data' must contain at least one row", call. = FALSE)
  }
  output <- list(obj = obj, drift = FALSE)
  for (i in seq_len(nrow(data))) {
    output <- update_state(output$obj, data[i, , drop = FALSE])
  }
  return(output$obj)
}

#' Pick the monitored column of a tabular stream
#'
#' A data frame or matrix must either have a single column or contain
#' `target_feat`; other inputs are returned as a plain vector.
#' @noRd
.select_stream <- function(data, target_feat = NULL, where = "fit") {
  if (is.data.frame(data) || is.matrix(data)) {
    data <- as.data.frame(data)
    if (!is.null(target_feat)) {
      if (!(target_feat %in% names(data))) {
        stop(sprintf("%s: the monitored feature '%s' is not in the data", where, target_feat), call. = FALSE)
      }
      return(data[[target_feat]])
    }
    if (ncol(data) == 1L) {
      return(data[[1L]])
    }
    stop(
      sprintf("%s: the data has %d columns; set 'target_feat' to choose the monitored one", where, ncol(data)),
      call. = FALSE
    )
  }
  if (is.list(data)) {
    data <- unlist(data, use.names = FALSE)
  }
  return(data)
}

#' Coerce an incoming observation to a single numeric value
#'
#' Factors are read through their labels, logical values become 0/1 and
#' non-finite values are treated as missing.
#' @noRd
.as_scalar <- function(value, target_feat = NULL) {
  value <- .select_stream(value, target_feat, where = "update_state")
  if (length(value) == 0L) {
    return(NA_real_)
  }
  if (length(value) != 1L) {
    stop(sprintf("update_state expects a single observation, got %d values", length(value)), call. = FALSE)
  }
  if (is.factor(value)) {
    value <- as.character(value)
  }
  if (is.character(value)) {
    if (value %in% c("TRUE", "FALSE")) {
      value <- (value == "TRUE")
    } else {
      number <- suppressWarnings(as.numeric(value))
      if (is.na(number) && !is.na(value)) {
        stop(sprintf("update_state: '%s' is not a numeric observation", value), call. = FALSE)
      }
      value <- number
    }
  }
  if (!is.numeric(value) && !is.logical(value)) {
    stop("update_state expects a numeric observation", call. = FALSE)
  }
  value <- as.numeric(value)
  if (is.infinite(value)) {
    warning("update_state: a non-finite observation was treated as missing", call. = FALSE)
    return(NA_real_)
  }
  return(value)
}

#' Coerce an observation of an error stream
#'
#' Error-based detectors expect `1` (wrong prediction) or `0` (correct
#' prediction); missing values are treated as correct predictions.
#' @noRd
.as_error <- function(value) {
  value <- .as_scalar(value)
  if (is.na(value)) {
    return(0)
  }
  if (!(value %in% c(0, 1))) {
    stop(
      sprintf("error-based detectors expect 1 (error) or 0 (correct prediction), got %s", format(value)),
      call. = FALSE
    )
  }
  return(value)
}

#' Coerce binary labels to logical
#'
#' Accepts logical values, 0/1 numbers, and factors or character vectors with
#' the labels `TRUE`/`FALSE` or `1`/`0`.
#' @noRd
.as_binary <- function(x, name) {
  x <- unlist(x, use.names = FALSE)
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.logical(x)) {
    return(x)
  }
  if (is.numeric(x) && all(x[!is.na(x)] %in% c(0, 1))) {
    return(x == 1)
  }
  if (is.character(x) && all(x[!is.na(x)] %in% c("TRUE", "FALSE", "1", "0"))) {
    binary <- x %in% c("TRUE", "1")
    binary[is.na(x)] <- NA
    return(binary)
  }
  stop(
    sprintf("%s must hold binary labels (TRUE/FALSE or 1/0)", name),
    call. = FALSE
  )
}

#' Ensure predictions and labels have the same length
#' @noRd
.check_same_length <- function(y_pred, y_true) {
  n_pred <- length(unlist(y_pred, use.names = FALSE))
  n_true <- length(unlist(y_true, use.names = FALSE))
  if (n_pred != n_true) {
    stop(sprintf("y_pred has %d values but y_true has %d", n_pred, n_true), call. = FALSE)
  }
  invisible(TRUE)
}

#' Precision of a binary classifier
#'
#' Share of the predicted positives that are actual positives; `NaN` when
#' nothing is predicted as positive.
#' @noRd
.precision <- function(actual, predicted) {
  actual <- .as_binary(actual, "y_true")
  predicted <- .as_binary(predicted, "y_pred")
  return(mean(actual[which(predicted)]))
}

#' Recall of a binary classifier
#'
#' Share of the actual positives that were predicted as positives; `NaN` when
#' there are no actual positives.
#' @noRd
.recall <- function(actual, predicted) {
  actual <- .as_binary(actual, "y_true")
  predicted <- .as_binary(predicted, "y_pred")
  return(mean(predicted[which(actual)]))
}

#' Learn a one-hot encoding for a data frame
#'
#' Factor and character columns are expanded into one indicator column per
#' level, logical columns into `FALSE`/`TRUE` indicators, and numeric columns are
#' kept as they are. Replaces
#' `caret::dummyVars(" ~ .", data)` and keeps its column names: factor levels
#' are named `<column>.<level>`, character and logical values `<column><value>`.
#' @noRd
.one_hot_fit <- function(data) {
  data <- as.data.frame(data)
  column_levels <- lapply(data, function(column) {
    if (is.factor(column)) {
      return(levels(column))
    }
    if (is.character(column)) {
      return(sort(unique(column[!is.na(column)])))
    }
    if (is.logical(column)) {
      return(c("FALSE", "TRUE"))
    }
    return(NULL)
  })
  separators <- vapply(data, function(column) if (is.factor(column)) "." else "", character(1))
  encoder <- list(columns = names(data), levels = column_levels, separators = separators)
  template <- .one_hot_apply(encoder, data[0, , drop = FALSE])
  encoder$feat_names <- names(template)
  # Original column of every encoded column, in the same order.
  encoder$origin <- attr(template, "origin")
  return(encoder)
}

#' Apply a one-hot encoding learned by `.one_hot_fit()`
#'
#' Levels that were not seen when the encoding was learned produce a row of
#' zeros; missing values propagate as `NA`.
#' @noRd
.one_hot_apply <- function(encoder, data) {
  data <- as.data.frame(data)
  missing_columns <- setdiff(encoder$columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf("the data is missing the columns: %s", paste(missing_columns, collapse = ", ")),
      call. = FALSE
    )
  }

  encoded <- list()
  origin <- character(0)
  for (column in encoder$columns) {
    lv <- encoder$levels[[column]]
    values <- data[[column]]
    if (is.null(lv)) {
      encoded[[column]] <- as.numeric(values)
      origin <- c(origin, column)
    } else {
      values <- as.character(values)
      for (level in lv) {
        encoded[[paste(column, level, sep = encoder$separators[[column]])]] <- as.numeric(values == level)
        origin <- c(origin, column)
      }
    }
  }
  encoded <- as.data.frame(encoded, check.names = TRUE)
  if (length(encoded) == 0L) {
    encoded <- data.frame(row.names = seq_len(nrow(data)))
  }
  attr(encoded, "origin") <- origin
  return(encoded)
}
