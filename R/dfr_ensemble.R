#' @title Ensemble drift detector (CEDD and FEDD)
#' @description Combines the detections of several drift detectors by voting
#' (Giusti Tavares et al., 2025). At every time step the vote is the sum of the
#' members' scores, and the ensemble reports a drift when the vote passes the
#' voting rule:
#' * `"efer"` (early find, early report): vote >= 1;
#' * `"majority"`: vote > n / 2 (n members);
#' * `"threshold"`: vote >= `threshold`.
#'
#' With `tolerance` k <= 1 the scores are crisp: 1 when the member detects a
#' drift at that time step, 0 otherwise (crisp ensemble drift detector, CEDD).
#' With k > 1 they are fuzzy (fuzzy ensemble drift detector, FEDD). A detection
#' at time d gives time t the score `1 - |t - d| / k` when `|t - d| < k`. A
#' member's score is the largest over its detections. So detections that are
#' close in time but not simultaneous can still add up to a majority.
#' @param drifter_list Named list of drifters (`dist_based` or `mv_dist_based`)
#' run in parallel on the stream.
#' @param voting Voting rule: `"majority"` (default), `"efer"` or
#' `"threshold"`.
#' @param threshold Vote needed by the `"threshold"` rule (lambda).
#' @param tolerance Time tolerance k of the fuzzy scores; `0` (default) gives
#' the crisp ensemble.
#' @param lookahead With fuzzy scores, `TRUE` (default) uses detections on both
#' sides of a time step, as in the paper. The vote of time t is then known only
#' when t + k - 1 has arrived, so drifts are reported k - 1 observations after
#' the time they refer to (`obj$events` holds those times). `FALSE` uses only
#' the detections up to the current observation and reports at once.
#' @details The combination follows the reference code
#' (<https://github.com/cefet-rj-dal/reproduce>, `fedd/core/4-ensemble-fuzzy.R`,
#' with the triangular fuzzification of `harbinger`):
#'
#' * The majority rule is strict, vote > n / 2 (the paper writes vote >= the
#'   ceiling of n / 2).
#' * After a fuzzy ensemble drift at time t, the detections before t are
#'   forgotten. Each member with a positive score at t also loses its first
#'   detection at or after t, or its next detection if none has arrived yet.
#'   So one detection does not raise a second ensemble drift.
#' * The scores are `(k - j) * (1 / k)` at distance j, as in
#'   `harbinger`. The paper's Equation 3 prints `|t - t'| / k`, which would give
#'   0 at the detection itself.
#'
#' Members with a `target_feat` receive that column of each row, the other
#' univariate members the row mean, and multivariate members the row (as
#' [dfr_multi_criteria()]). The members are not reset by the ensemble. The
#' paper's members are also run as separate detectors, each with its own warm-up
#' and normalisation, before the vote.
#'
#' [ensemble_vote()] applies the same voting to detection series computed
#' beforehand.
#' @references Giusti Tavares, L., Lima, J., Melo, M., Chen, C., Garibaldi,
#' J., Scatena, G. S., Costa, A. H. R., Gomi, E. S., Salles, R., Pacitti, E.,
#' Santos, I., Siqueira, I. G., Carvalho, D., Coutinho, R., Porto, F. and
#' Ogasawara, E. (2025). Fuzzy-based ensemble method for robust concept drift
#' detection in multivariate time series. *2025 International Joint Conference
#' on Neural Networks (IJCNN)*. <doi:10.1109/IJCNN64981.2025.11228919>
#' @return `dfr_ensemble` object. After `fit()`, `drifter_output` holds, on
#' every observation, the decision of each member, the time step whose vote was
#' decided (`time`) and the vote (`vote`). `obj$events` holds the time steps of
#' the ensemble drifts, including those decided when the stream ended.
#' @seealso [ensemble_vote()], [dfr_multi_criteria()].
#' @examples
#' library(daltoolbox)
#' library(heimdall)
#' set.seed(1)
#' data <- data.frame(serie = c(stats::rnorm(500), stats::rnorm(500, 3)))
#' members <- list(ph = dfr_page_hinkley(), kswin = dfr_kswin(), adwin = dfr_adwin())
#' # Crisp majority: the members must agree on the same observation.
#' crisp <- fit(dfr_ensemble(members, voting = "majority"), data)
#' crisp$events
#' # Fuzzy majority with a tolerance of 50 observations.
#' fuzzy <- fit(dfr_ensemble(members, voting = "majority", tolerance = 50), data)
#' fuzzy$events
#' @export
dfr_ensemble <- function(drifter_list, voting = c("majority", "efer", "threshold"), threshold = NULL, tolerance = 0,
                         lookahead = TRUE) {
  voting <- match.arg(voting)
  drifter_list <- .ens_members(drifter_list)
  settings <- .ens_settings(length(drifter_list), voting, threshold, tolerance, lookahead)
  obj <- drifter()
  obj$drifter_list <- drifter_list
  obj$settings <- settings
  obj$state <- list(members = drifter_list, vote = .ens_engine(length(drifter_list)))
  obj$events <- integer(0)
  class(obj) <- append("dfr_ensemble", class(obj))
  return(obj)
}

#' @export
update_state.dfr_ensemble <- function(obj, value, ...) {
  state <- obj$state
  flags <- stats::setNames(logical(length(state$members)), names(state$members))
  for (name in names(state$members)) {
    member <- state$members[[name]]
    if (inherits(member, "dist_based") && is.null(member$target_feat)) {
      output <- update_state(member, mean(as.numeric(unlist(value)), na.rm = TRUE))
    } else {
      output <- update_state(member, value)
    }
    state$members[[name]] <- output$obj
    flags[[name]] <- isTRUE(output$drift)
  }
  step <- .ens_step(state$vote, flags, obj$settings)
  state$vote <- step$engine
  obj$state <- state
  obj$last_drifter_output <- c(as.list(flags), time = step$time, vote = step$vote)
  drift <- length(step$events) > 0L
  if (drift) {
    obj$events <- c(obj$events, step$events)
    obj$drifted <- TRUE
  }
  return(list(obj = obj, drift = drift))
}

#' @export
fit.dfr_ensemble <- function(obj, data, ...) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    data <- data.frame(V1 = unlist(data, use.names = FALSE))
  }
  obj <- .fit_row_stream(obj, data)
  # The stream ended: the votes still waiting for later detections are decided
  # with the detections seen so far.
  flushed <- .ens_flush(obj$state$vote, obj$settings)
  obj$state$vote <- flushed$engine
  if (length(flushed$events) > 0L) {
    obj$events <- c(obj$events, flushed$events)
    obj$drifted <- TRUE
  }
  return(obj)
}

#' @export
reset_state.dfr_ensemble <- function(obj) {
  members <- lapply(obj$drifter_list, reset_state)
  obj$drifted <- FALSE
  obj$state <- list(members = members, vote = .ens_engine(length(members)))
  obj$events <- integer(0)
  obj$last_drifter_output <- NULL
  obj$drifter_output <- NULL
  return(obj)
}

#' @title Ensemble vote over detection series
#' @description Applies the voting of [dfr_ensemble()] to detection series
#' computed beforehand, one column per detector (crisp ensemble with
#' `tolerance` <= 1, fuzzy ensemble otherwise). This is how the paper combines
#' its detectors.
#' @param detections Logical (or 0/1) matrix or data frame, one row per time
#' step and one column per detector.
#' @inheritParams dfr_ensemble
#' @return A logical vector with the ensemble drifts, with attribute `vote`
#' (the vote of every time step, evaluated with the detections left after the
#' earlier ensemble drifts).
#' @examples
#' detections <- matrix(FALSE, 100, 3)
#' detections[c(40, 43, 47), 1:3] <- diag(3) == 1
#' # No crisp majority: the detections are not simultaneous.
#' which(ensemble_vote(detections, "majority"))
#' # A fuzzy majority with a tolerance of 10 time steps.
#' which(ensemble_vote(detections, "majority", tolerance = 10))
#' @export
ensemble_vote <- function(detections, voting = c("majority", "efer", "threshold"), threshold = NULL, tolerance = 0,
                          lookahead = TRUE) {
  voting <- match.arg(voting)
  detections <- as.matrix(detections)
  if (!(is.logical(detections) || is.numeric(detections)) || anyNA(detections)) {
    stop("ensemble_vote: detections must be a logical or 0/1 matrix without missing values", call. = FALSE)
  }
  settings <- .ens_settings(ncol(detections), voting, threshold, tolerance, lookahead)
  n <- nrow(detections)
  # The whole series is known, so the votes are computed at once up to the
  # next ensemble drift, which changes the detections (same result as the
  # step-by-step engine of dfr_ensemble()).
  times <- lapply(seq_len(ncol(detections)), function(j) which(detections[, j] != 0))
  result <- logical(n)
  vote <- rep(NA_real_, n)
  start <- 1L
  while (start <= n) {
    span <- start:n
    scores <- vapply(times, function(d) .ens_series_scores(d, span, settings, lookahead), numeric(length(span)))
    scores <- matrix(scores, nrow = length(span))
    votes <- rowSums(scores)
    vote[span] <- votes
    passes <- which(.ens_passes(votes, settings))
    if (length(passes) == 0L) break
    first <- passes[1]
    time <- span[first]
    result[time] <- TRUE
    for (j in which(scores[first, ] != 0)) {
      later <- which(times[[j]] >= time)
      if (length(later) > 0L) times[[j]] <- times[[j]][-later[1]]
    }
    times <- lapply(times, function(d) d[d >= time])
    if (time < n) vote[(time + 1L):n] <- NA_real_
    start <- time + 1L
  }
  attr(result, "vote") <- vote
  result
}

# Score of one member at the time steps `span` from its (sorted) detection
# times: the triangle of its nearest detection (only earlier ones without
# lookahead), or 1 at the detection times for the crisp ensemble.
#' @noRd
.ens_series_scores <- function(d, span, settings, lookahead) {
  if (length(d) == 0L) return(numeric(length(span)))
  position <- findInterval(span, d)
  before <- ifelse(position > 0L, span - d[pmax(position, 1L)], Inf)
  after <- if (lookahead || !settings$fuzzy) ifelse(position < length(d), d[pmin(position + 1L, length(d))] - span, Inf) else Inf
  distance <- pmin(before, after)
  if (!settings$fuzzy) return((distance == 0) * 1)
  k <- settings$tolerance
  ifelse(distance == 0, 1, ifelse(distance < k, (k - distance) * (1 / k), 0))
}

# Engine -------------------------------------------------------------------------------------

#' @noRd
.ens_members <- function(drifter_list) {
  if (!is.list(drifter_list) || (length(drifter_list) == 0L) || inherits(drifter_list, "drifter")) {
    stop("dfr_ensemble: drifter_list must be a non-empty list of drifters", call. = FALSE)
  }
  if (is.null(names(drifter_list)) || any(names(drifter_list) == "")) {
    names(drifter_list) <- paste0("drifter_", seq_along(drifter_list))
  }
  supported <- vapply(drifter_list, function(d) inherits(d, "dist_based") || inherits(d, "mv_dist_based"),
                      logical(1))
  if (!all(supported)) {
    stop(sprintf("dfr_ensemble: drifter_list elements must inherit from 'dist_based' or 'mv_dist_based': %s",
                 paste(names(drifter_list)[!supported], collapse = ", ")), call. = FALSE)
  }
  drifter_list
}

#' @noRd
.ens_settings <- function(n, voting, threshold, tolerance, lookahead) {
  .check_number(tolerance, "tolerance", min_value = 0)
  if (!is.logical(lookahead) || (length(lookahead) != 1L) || is.na(lookahead)) {
    stop("lookahead must be TRUE or FALSE", call. = FALSE)
  }
  if (voting == "threshold") {
    if (is.null(threshold)) {
      stop("dfr_ensemble: the threshold voting needs a threshold", call. = FALSE)
    }
    .check_number(threshold, "threshold", min_value = 0)
  }
  fuzzy <- tolerance > 1
  list(n = n, voting = voting, threshold = threshold, tolerance = tolerance, fuzzy = fuzzy,
       # Time steps a vote waits for: the detections up to t + k - 1 matter.
       delay = if (fuzzy && lookahead) ceiling(tolerance) - 1L else 0L)
}

# State of the vote: the detection times kept for every member, the number of
# future detections each member has to drop, the start of the current concept
# (`cut`), the last time step seen and the next one to decide.
#' @noRd
.ens_engine <- function(n) {
  list(detections = replicate(n, integer(0), simplify = FALSE), skip = integer(n), cut = 1L, t = 0L, next_time = 1L)
}

#' @noRd
.ens_scores <- function(engine, time, settings) {
  k <- settings$tolerance
  vapply(engine$detections, function(d) {
    d <- d[d >= engine$cut]
    if (length(d) == 0L) return(0)
    distance <- abs(d - time)
    if (!settings$fuzzy) return(if (any(distance == 0)) 1 else 0)
    near <- distance[distance < k]
    if (length(near) == 0L) return(0)
    # Triangle of harbinger's har_fuzzify_detections_triangle.
    max(ifelse(near == 0, 1, (k - near) * (1 / k)))
  }, numeric(1))
}

#' @noRd
.ens_passes <- function(vote, settings) {
  switch(settings$voting,
         efer = vote >= 1,
         majority = vote > settings$n / 2,
         threshold = vote >= settings$threshold)
}

# Decides the vote of time step `time` and, on a drift, forgets the earlier
# detections and the detection of every member that contributed.
#' @noRd
.ens_decide <- function(engine, time, settings) {
  scores <- .ens_scores(engine, time, settings)
  vote <- sum(scores)
  drift <- .ens_passes(vote, settings)
  if (drift) {
    engine$cut <- time
    for (i in which(scores != 0)) {
      later <- which(engine$detections[[i]] >= time)
      if (length(later) > 0L) {
        engine$detections[[i]] <- engine$detections[[i]][-later[which.min(engine$detections[[i]][later])]]
      } else {
        engine$skip[i] <- engine$skip[i] + 1L
      }
    }
    engine$detections <- lapply(engine$detections, function(d) d[d >= time])
  }
  list(engine = engine, vote = vote, drift = drift)
}

# A new time step with the members' decisions; decides every vote that no
# longer depends on later detections.
#' @noRd
.ens_step <- function(engine, flags, settings) {
  engine$t <- engine$t + 1L
  for (i in which(flags)) {
    if (engine$skip[i] > 0L) {
      engine$skip[i] <- engine$skip[i] - 1L
    } else {
      engine$detections[[i]] <- c(engine$detections[[i]], engine$t)
    }
  }
  .ens_decide_until(engine, engine$t - settings$delay, settings)
}

#' @noRd
.ens_flush <- function(engine, settings) {
  .ens_decide_until(engine, engine$t, settings)
}

#' @noRd
.ens_decide_until <- function(engine, last, settings) {
  events <- integer(0)
  times <- integer(0)
  votes <- numeric(0)
  while (engine$next_time <= last) {
    time <- engine$next_time
    decision <- .ens_decide(engine, time, settings)
    engine <- decision$engine
    times <- c(times, time)
    votes <- c(votes, decision$vote)
    if (decision$drift) events <- c(events, time)
    engine$next_time <- time + 1L
  }
  # Detections too old to reach the next votes are dropped.
  reach <- if (settings$fuzzy) ceiling(settings$tolerance) else 1L
  engine$detections <- lapply(engine$detections, function(d) d[d > engine$next_time - reach])
  list(engine = engine, events = events, times = times, votes = votes,
       time = if (length(times)) times[length(times)] else NA_integer_,
       vote = if (length(votes)) votes[length(votes)] else NA_real_)
}
