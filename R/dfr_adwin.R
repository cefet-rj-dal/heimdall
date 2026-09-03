#'@title ADWIN method
#'@description ADWIN (Adaptive Windowing) is a sequential change detector that maintains a variable-length window and tests whether the means of two subwindows differ significantly. In this package, the implementation is primarily used for **virtual concept drift** when it monitors a numeric feature stream, although the same mechanism can also detect **real concept drift** if applied to an error or loss stream. The theoretical basis follows Bifet and Gavalda (2007) <doi:10.1137/1.9781611972771.42>.
#'@param target_feat Feature to be monitored.
#'@param delta The significance parameter for the ADWIN algorithm.
#'@details This detector is a thin wrapper around a Python implementation and
#'therefore needs the suggested package `reticulate` together with a Python
#'installation providing `numpy`. The Python module is loaded once per session
#'and cached. Because the state lives in a Python object, a `dfr_adwin` object
#'cannot be serialized with [base::saveRDS()] and restored in another session.
#ADWIN detection: Bifet, Albert, and Ricard Gavalda. "Learning from time-changing data with adaptive windowing." In Proceedings of the 2007 SIAM international conference on data mining, pp. 443-448. Society for Industrial and Applied Mathematics, 2007.
#'@references Bifet, A., and Gavalda, R. (2007). Learning from time-changing data with adaptive windowing. In *Proceedings of the 2007 SIAM International Conference on Data Mining*, 443-448. <doi:10.1137/1.9781611972771.42>
#'@return `dfr_adwin` object
#'@examples
#'library(daltoolbox)
#'library(heimdall)
#'
#'\donttest{
#'if (requireNamespace("reticulate", quietly = TRUE) &&
#'    reticulate::py_module_available("numpy")) {
#'
#'  data(st_drift_examples)
#'  data <- st_drift_examples$univariate
#'  data$event <- NULL
#'
#'  model <- dfr_adwin(target_feat='serie')
#'
#'  detection <- NULL
#'  output <- list(obj=model, drift=FALSE)
#'  for (i in seq_along(data$serie)){
#'    output <- update_state(output$obj, data$serie[i])
#'    if (output$drift){
#'      type <- 'drift'
#'      output$obj <- reset_state(output$obj)
#'    }else{
#'      type <- ''
#'    }
#'    detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
#'  }
#'
#'  print(detection[detection$type == 'drift',])
#'}
#'}
#'@export
dfr_adwin <- function(target_feat = NULL, delta = 2e-05) {
  .check_probability(delta, "delta")

  obj <- dist_based(target_feat = target_feat)

  # Attributes
  state <- list()

  state$delta <- delta
  state$adwin <- .adwin_module()$ADWIN(delta = delta)

  obj$drifted <- FALSE
  obj$state <- state
  class(obj) <- append("dfr_adwin", class(obj))
  return(obj)
}

# Cache for the Python side of the ADWIN detector, so that the module is
# sourced at most once per session.
.heimdall_python <- new.env(parent = emptyenv())

#' @noRd
.adwin_module <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "dfr_adwin() requires the 'reticulate' package. Install it with install.packages('reticulate').",
      call. = FALSE
    )
  }
  if (is.null(.heimdall_python$adwin)) {
    module_env <- new.env(parent = emptyenv())
    module_path <- system.file("python", "adwin.py", package = "heimdall")
    if (module_path == "") {
      stop("dfr_adwin(): could not locate the bundled adwin.py module.", call. = FALSE)
    }
    tryCatch(
      reticulate::source_python(module_path, envir = module_env),
      error = function(e) {
        stop(
          sprintf(
            "dfr_adwin() needs a Python installation providing 'numpy'. Original error: %s",
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
    .heimdall_python$adwin <- module_env
  }
  return(.heimdall_python$adwin)
}

#'@export
update_state.dfr_adwin <- function(obj, value, ...) {
  state <- obj$state

  value <- .as_scalar(value)
  if (is.na(value)) {
    obj$state <- state
    return(list(obj = obj, drift = FALSE))
  }

  state$adwin$add_element(value)

  obj$state <- state
  has_drift <- isTRUE(state$adwin$detected_change())
  if (has_drift) {
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = has_drift))
}

#'@export
fit.dfr_adwin <- function(obj, data, ...) {
  return(.fit_vector_stream(obj, data))
}

#'@export
reset_state.dfr_adwin <- function(obj) {
  obj$drifted <- FALSE
  obj$state <- dfr_adwin(
    target_feat = obj$target_feat,
    delta = obj$state$delta
  )$state
  return(obj)
}
