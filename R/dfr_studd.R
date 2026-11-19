#' @title Student-Teacher Unsupervised Drift Detection (STUDD)
#' @description Unsupervised drift detector of Cerqueira, Gomes, Bifet and
#' Torgo (2023). A classifier (the teacher) is trained on an initial window of
#' `window_size` labelled observations, and a second classifier (the student)
#' is trained on the same observations to mimic the teacher, with the
#' teacher's predictions as its target. After that no label is used: every new
#' observation is predicted by both models, and the mimicking error (1 when
#' the student disagrees with the teacher, 0 otherwise) feeds a change
#' detector, by default the Page-Hinkley test. When the detector raises an
#' alarm, the labels of the last `window_size` observations are requested to
#' retrain the teacher, and the student is retrained on the new teacher's
#' predictions. Since the mimicking error does not depend on the labels, it is
#' used for **virtual concept drift**.
#' @param teacher A `daltoolbox` classification object (for example
#' `daltoolbox::cla_rf("class", levels)`), whose `attribute` is the label
#' column of the stream.
#' @param student A `daltoolbox` classification object for the student, with
#' the same `attribute` and levels. By default the same algorithm as the
#' teacher.
#' @param detector Univariate detector fed with the mimicking errors (a
#' `dist_based` or `error_based` object of this package). The default is the
#' Page-Hinkley test with `delta = 0.001`, as in the paper.
#' @param window_size Number of observations of the initial training window
#' and of the window of labels requested after every drift (`W` in the paper).
#' @param student_memory Observations used to retrain the student after a
#' drift: `"all"` (the default, as in the reference code) uses every
#' observation seen since the end of the initial window; `"window"` only the
#' last `window_size` ones, which bounds memory.
#' @param fit_teacher If `TRUE` (the default), the teacher is trained by the
#' detector on the labels of the initial window and retrained after every
#' drift. If `FALSE`, the teacher must already be fitted, it is never
#' retrained and the stream needs no label column: only the student is
#' trained, on the teacher's predictions.
#' @param target_feat Names of the feature columns. `NULL` (the default) uses
#' every numeric column except the label column.
#' @param seed Seed of the random number generator used when the models are
#' trained. `NULL` draws it from the global generator when the detector is
#' created; the global random state is left unchanged.
#' @details Labels are read only for the rows used to (re)train the teacher;
#' every other row may have a missing label. The initial window is used only
#' for training: no drift is checked on it. The rows of a drift alarm are
#' included in the window used to retrain the teacher, as in the reference
#' implementation (<https://github.com/vcerqueira/studd>, `studd_batch.py`).
#'
#' A model is always trained on the classes present in its training data, so
#' that, as in scikit-learn, a class missing from a window does not stop the
#' training. When a single class is present, the model predicts that class.
#' Predictions are the class of highest probability (the first one in case of
#' ties). `reset_state()` resets the detector and keeps the models.
#' @references Cerqueira, V., Gomes, H. M., Bifet, A. and Torgo, L. (2023).
#' STUDD: a student-teacher method for unsupervised concept drift detection.
#' *Machine Learning*, 112, 4351-4378. <doi:10.1007/s10994-022-06188-7>
#' @return `dfr_studd` object. After `fit()`, `drifter_output` holds, for every
#' monitored row, the class predicted by the teacher (`teacher`) and by the
#' student (`student`), the mimicking error (`error`) and the detector
#' statistic (`statistic`), and whether a drift was reported (`drift`). The
#' classes are given by their position in the levels of the teacher.
#' `obj$labels_used` counts the labels used to train the teacher.
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' # Two classes separated by x1 + x2 = 0; from row 1201 on, the observations
#' # concentrate along the class boundary, where the student imitates the
#' # teacher worse.
#' n <- 2400
#' x <- matrix(stats::rnorm(n * 2), ncol = 2)
#' x[1201:n, 2] <- -x[1201:n, 1] + stats::rnorm(1200, 0, 0.2)
#' y <- ifelse(x[, 1] + x[, 2] > 0, "a", "b")
#' stream <- data.frame(x1 = x[, 1], x2 = x[, 2], class = y)
#' teacher <- cla_rf("class", c("a", "b"), ntree = 20)
#' detector <- dfr_studd(teacher, window_size = 400, seed = 1)
#' model <- fit(detector, stream)
#' which(model$drifter_output$drift == 1)
#' model$labels_used
#' @export
dfr_studd <- function(teacher, student = teacher, detector = dfr_page_hinkley(delta = 0.001), window_size = 500,
                      student_memory = "all", fit_teacher = TRUE, target_feat = NULL, seed = NULL) {
  if (!inherits(teacher, "classification") || !inherits(student, "classification")) {
    stop("dfr_studd: teacher and student must be daltoolbox classification objects", call. = FALSE)
  }
  if (!identical(teacher$attribute, student$attribute) || !identical(teacher$slevels, student$slevels)) {
    stop("dfr_studd: teacher and student must have the same attribute and levels", call. = FALSE)
  }
  if (!inherits(detector, c("dist_based", "error_based")) || inherits(detector, "mv_dist_based")) {
    stop("dfr_studd: detector must be a univariate drift detector", call. = FALSE)
  }
  .check_positive_integer(window_size, "window_size", min_value = 2L)
  student_memory <- match.arg(student_memory, c("all", "window"))
  if (!is.logical(fit_teacher) || length(fit_teacher) != 1L || is.na(fit_teacher)) {
    stop("dfr_studd: fit_teacher must be TRUE or FALSE", call. = FALSE)
  }
  .check_target_feat(target_feat, multiple = TRUE)
  obj <- mv_dist_based(target_feat = target_feat)
  obj$teacher_template <- teacher
  obj$student_template <- student
  obj$detector <- detector
  obj$settings <- list(window_size = window_size, student_memory = student_memory, fit_teacher = fit_teacher,
                       attribute = teacher$attribute)
  obj$teacher <- if (fit_teacher) NULL else list(model = teacher, constant = NULL)
  obj$student <- NULL
  obj$labels_used <- 0
  obj$state <- list(features = NULL, memory = .studd_memory(), monitoring = FALSE, rng = .rng_state(seed))
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  class(obj) <- append("dfr_studd", class(obj))
  return(obj)
}

# Rows kept by the detector (features and labels), in a matrix that grows by doubling.
#' @noRd
.studd_memory <- function() {
  list(x = NULL, y = character(0), n = 0L)
}

#' @noRd
.studd_append <- function(memory, x, y, keep = NULL) {
  if (is.null(memory$x)) {
    memory$x <- matrix(NA_real_, 64L, length(x), dimnames = list(NULL, names(x)))
    memory$y <- rep(NA_character_, 64L)
  }
  # With a bounded memory, the last `keep` rows are moved to the top whenever
  # 2 * keep rows are stored.
  if (!is.null(keep) && memory$n >= 2L * keep) {
    rows <- seq.int(memory$n - keep + 1L, memory$n)
    memory$x[seq_len(keep), ] <- memory$x[rows, , drop = FALSE]
    memory$y[seq_len(keep)] <- memory$y[rows]
    memory$n <- keep
  }
  if (memory$n == nrow(memory$x)) {
    memory$x <- rbind(memory$x, matrix(NA_real_, nrow(memory$x), ncol(memory$x)))
    memory$y <- c(memory$y, rep(NA_character_, length(memory$y)))
  }
  memory$n <- memory$n + 1L
  memory$x[memory$n, ] <- x
  memory$y[memory$n] <- y
  memory
}

#' @noRd
.studd_rows <- function(memory, last = NULL) {
  first <- if (is.null(last)) 1L else max(1L, memory$n - last + 1L)
  rows <- seq.int(first, memory$n)
  list(x = memory$x[rows, , drop = FALSE], y = memory$y[rows])
}

# Trains a copy of `template` on the classes present in `labels`.
#' @noRd
.studd_fit <- function(template, x, labels, attribute) {
  keep <- !is.na(labels)
  x <- x[keep, , drop = FALSE]
  labels <- labels[keep]
  if (length(labels) == 0L) {
    stop("dfr_studd: no label is available to train the teacher", call. = FALSE)
  }
  unknown <- setdiff(unique(labels), template$slevels)
  if (length(unknown) > 0L) {
    stop(sprintf("dfr_studd: labels not in the levels of the model: %s", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  present <- template$slevels[template$slevels %in% labels]
  if (length(present) == 1L) {
    return(list(model = NULL, constant = present))
  }
  template$slevels <- present
  template$ilevels <- seq_along(present)
  data <- as.data.frame(x)
  data[[attribute]] <- factor(labels, levels = present)
  list(model = fit(template, data), constant = NULL)
}

#' @noRd
.studd_predict <- function(fitted, x) {
  if (!is.null(fitted$constant)) {
    return(rep(fitted$constant, nrow(x)))
  }
  probabilities <- as.matrix(predict(fitted$model, as.data.frame(x)))
  colnames(probabilities)[max.col(probabilities, ties.method = "first")]
}

# Trains the teacher (if it is trained by the detector) on the last
# `window_size` rows and the student on the teacher's predictions.
#' @noRd
.studd_train <- function(obj, teacher_rows, student_rows) {
  attribute <- obj$settings$attribute
  trained <- .with_rng(obj$state$rng, function() {
    teacher <- obj$teacher
    labels_used <- 0
    if (obj$settings$fit_teacher) {
      teacher <- .studd_fit(obj$teacher_template, teacher_rows$x, teacher_rows$y, attribute)
      labels_used <- nrow(teacher_rows$x)
    }
    student <- .studd_fit(obj$student_template, student_rows$x, .studd_predict(teacher, student_rows$x), attribute)
    list(teacher = teacher, student = student, labels_used = labels_used)
  })
  obj$state$rng <- trained$rng
  obj$teacher <- trained$value$teacher
  obj$student <- trained$value$student
  obj$labels_used <- obj$labels_used + trained$value$labels_used
  obj
}

#' @export
update_state.dfr_studd <- function(obj, value, ...) {
  settings <- obj$settings
  state <- obj$state
  value <- as.data.frame(value)
  if (settings$fit_teacher && !(settings$attribute %in% names(value))) {
    stop(sprintf("dfr_studd: the label column '%s' is not in the data", settings$attribute), call. = FALSE)
  }
  label <- if (settings$attribute %in% names(value)) as.character(value[[settings$attribute]]) else NA_character_
  features <- value[setdiff(names(value), settings$attribute)]
  row <- .select_features(features, obj$target_feat)
  if (is.null(state$features)) {
    state$features <- names(row)
  } else if (!identical(names(row), state$features)) {
    stop("dfr_studd: the monitored features changed during the stream", call. = FALSE)
  }
  if (anyNA(row)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }
  keep <- if (settings$student_memory == "window") settings$window_size else NULL
  state$memory <- .studd_append(state$memory, row, label, keep)
  obj$state <- state
  if (!state$monitoring) {
    if (state$memory$n < settings$window_size) {
      return(list(obj = obj, drift = FALSE))
    }
    initial <- .studd_rows(state$memory)
    obj <- .studd_train(obj, initial, initial)
    # The initial window is not part of the monitored stream.
    obj$state$memory <- .studd_memory()
    obj$state$monitoring <- TRUE
    return(list(obj = obj, drift = FALSE))
  }
  x <- matrix(row, 1L, dimnames = list(NULL, names(row)))
  teacher <- .studd_predict(obj$teacher, x)
  student <- .studd_predict(obj$student, x)
  error <- as.numeric(teacher != student)
  detected <- update_state(obj$detector, error)
  obj$detector <- detected$obj
  statistic <- obj$detector$last_drifter_output
  obj$last_drifter_output <- c(teacher = match(teacher, obj$teacher_template$slevels),
                               student = match(student, obj$teacher_template$slevels), error = error,
                               statistic = if (length(statistic) == 1L) as.numeric(statistic) else NA_real_,
                               drift = as.numeric(detected$drift))
  if (detected$drift) {
    student_rows <- if (settings$student_memory == "all") .studd_rows(obj$state$memory) else
      .studd_rows(obj$state$memory, settings$window_size)
    obj <- .studd_train(obj, .studd_rows(obj$state$memory, settings$window_size), student_rows)
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = detected$drift))
}

#' @export
fit.dfr_studd <- function(obj, data, ...) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  return(.fit_row_stream(obj, data))
}

#' @export
reset_state.dfr_studd <- function(obj) {
  obj$drifted <- FALSE
  obj$detector <- reset_state(obj$detector)
  return(obj)
}
