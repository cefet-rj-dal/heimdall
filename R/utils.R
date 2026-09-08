<<<<<<< HEAD
# Internal helpers shared by the heimdall detectors. Not exported.

=======
# Internal helpers shared by the heimdall detectors.
# These functions are not exported.

#' Validate a probability-like parameter
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
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

<<<<<<< HEAD
=======
#' Validate a positive integer-like parameter
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
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

<<<<<<< HEAD
# Canonical window policies. 'sliding' keeps at most window_size observations;
# 'anchored' keeps every observation, so the reference half stays pinned to the
# beginning of the stream. The legacy dfr_aedd values are mapped here so that
# existing code keeps its exact behaviour.
#' @noRd
.check_window_type <- function(window_type) {
  if (!is.character(window_type) || (length(window_type) != 1L)) {
    stop("window_type must be a single character value", call. = FALSE)
  }
  legacy <- c(fixed = "sliding", moving = "anchored")
  if (window_type %in% names(legacy)) {
    mapped <- unname(legacy[window_type])
    warning(
      sprintf(
        "window_type = '%s' is deprecated; use '%s' instead. See NEWS for details.",
        window_type, mapped
      ),
      call. = FALSE
    )
    return(mapped)
  }
  match.arg(window_type, c("sliding", "anchored"))
}

# Drops the oldest observations when the policy asks for a bounded window.
#' @noRd
.trim_window <- function(window, window_size, window_type) {
  if (window_type != "sliding") {
    return(window)
  }
  n <- NROW(window)
  if (n <= window_size) {
    return(window)
  }
  keep <- seq.int(n - window_size + 1L, n)
  if (is.data.frame(window) || is.matrix(window)) {
    return(window[keep, , drop = FALSE])
  }
  return(window[keep])
}

# Levene's test for homogeneity of variance, Brown-Forsythe variant (group
# spread centred on the median, which is what car::leveneTest does by default).
=======
#' Levene's test for homogeneity of variance (Brown-Forsythe variant)
#'
#' Replaces the previous dependency on `car::leveneTest`. Group spread is
#' centred on the median, which is the same default used by `car`.
#'
#' @param values numeric vector of observations
#' @param group grouping factor with the same length as `values`
#' @return the p-value of the test, or `NA_real_` when it cannot be computed
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
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

<<<<<<< HEAD
# Levene's test applied column by column, combined with a Bonferroni
# correction. Returns a single p-value for the whole (possibly multivariate)
# reconstruction error.
#' @noRd
.levene_pvalue_mv <- function(history, recent) {
  history <- as.data.frame(history)
  recent <- as.data.frame(recent)

  features <- intersect(names(history), names(recent))
  if (length(features) == 0L) {
    return(NA_real_)
  }

  p_values <- vapply(
    features,
    function(feat) {
      .levene_pvalue(
        values = c(history[[feat]], recent[[feat]]),
        group = rep(c("History", "Recent"), c(nrow(history), nrow(recent)))
      )
    },
    numeric(1)
  )

  p_values <- p_values[!is.na(p_values)]
  if (length(p_values) == 0L) {
    return(NA_real_)
  }
  min(1, min(p_values) * length(p_values))
}

=======
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
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
#' @noRd
.as_scalar <- function(value) {
  if (is.data.frame(value) || is.matrix(value)) {
    value <- value[1L, 1L]
  }
  suppressWarnings(as.numeric(unlist(value, use.names = FALSE))[1L])
}
<<<<<<< HEAD

# Feeds a univariate stream to a detector, one observation at a time, keeping
# the per-observation diagnostics exposed through drifter_output.
#' @noRd
.fit_vector_stream <- function(obj, data, output_names = NULL) {
  data <- unlist(data, use.names = FALSE)
  if (length(data) == 0L) {
    stop("fit: 'data' must contain at least one observation", call. = FALSE)
  }

  obj$drifter_output <- NULL
  obj$last_drifter_output <- NULL

  output <- list(obj = obj, drift = FALSE)
  collected <- NULL
  for (i in seq_along(data)) {
    output <- update_state(output$obj, data[i])
    collected <- rbind(collected, output$obj$last_drifter_output)
  }

  output$obj$drifter_output <- .as_drifter_output(collected, output_names)
  return(output$obj)
}

# Same as above for detectors that consume one row at a time.
#' @noRd
.fit_row_stream <- function(obj, data, output_names = NULL) {
  data <- as.data.frame(data)
  if (nrow(data) == 0L) {
    stop("fit: 'data' must contain at least one row", call. = FALSE)
  }

  obj$drifter_output <- NULL
  obj$last_drifter_output <- NULL

  output <- list(obj = obj, drift = FALSE)
  collected <- NULL
  for (i in seq_len(nrow(data))) {
    output <- update_state(output$obj, data[i, , drop = FALSE])
    collected <- rbind(collected, output$obj$last_drifter_output)
  }

  output$obj$drifter_output <- .as_drifter_output(collected, output_names)
  return(output$obj)
}

#' @noRd
.as_drifter_output <- function(collected, output_names = NULL) {
  if (is.null(collected)) {
    return(NULL)
  }
  collected <- as.data.frame(collected)
  if (!is.null(output_names) && (ncol(collected) == length(output_names))) {
    names(collected) <- output_names
  }
  rownames(collected) <- seq_len(nrow(collected))
  return(collected)
}
=======
>>>>>>> 264a2e411549c065608d2ace35dec5ebfba3725e
