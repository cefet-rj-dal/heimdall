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

#' Validate a positive integer-like parameter
#' @noRd
.check_positive_integer <- function(value, name, min_value = 1L) {
  if (!is.numeric(value) || (length(value) != 1L) || is.na(value)) {
    stop(sprintf("%s must be a single numeric value", name), call. = FALSE)
  }
  if (value < min_value) {
    stop(sprintf("%s must be greater than or equal to %s", name, min_value), call. = FALSE)
  }
  invisible(TRUE)
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
  data <- unlist(data, use.names = FALSE)
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

#' Coerce an incoming observation to a single numeric value
#' @noRd
.as_scalar <- function(value) {
  if (is.data.frame(value) || is.matrix(value)) {
    value <- value[1L, 1L]
  }
  suppressWarnings(as.numeric(unlist(value, use.names = FALSE))[1L])
}
