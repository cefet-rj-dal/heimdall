# Step-by-step engine of dfr_ensemble() on a detection matrix.
ens_online <- function(detections, ...) {
  settings <- heimdall:::.ens_settings(ncol(detections), ...)
  engine <- heimdall:::.ens_engine(ncol(detections))
  events <- integer(0)
  for (t in seq_len(nrow(detections))) {
    step <- heimdall:::.ens_step(engine, detections[t, ], settings)
    engine <- step$engine
    events <- c(events, step$events)
  }
  c(events, heimdall:::.ens_flush(engine, settings)$events)
}

test_that("crisp voting follows EFER, majority and threshold rules", {
  detections <- matrix(FALSE, 10, 4)
  detections[3, 1:2] <- TRUE
  detections[5, 1:3] <- TRUE
  detections[8, 4] <- TRUE
  expect_equal(which(ensemble_vote(detections, "efer")), c(3L, 5L, 8L))
  # Majority is strict: more than half of the members.
  expect_equal(which(ensemble_vote(detections, "majority")), 5L)
  expect_equal(which(ensemble_vote(detections, "threshold", threshold = 2)), c(3L, 5L))
  expect_equal(attr(ensemble_vote(detections, "efer"), "vote")[1:5], c(0, 0, 2, 0, 3))
})

test_that("fuzzy voting adds triangular scores of nearby detections", {
  detections <- matrix(FALSE, 100, 3)
  detections[40, 1] <- TRUE
  detections[43, 2] <- TRUE
  detections[47, 3] <- TRUE
  expect_length(which(ensemble_vote(detections, "majority")), 0L)
  fuzzy <- ensemble_vote(detections, "majority", tolerance = 10)
  # t = 38: 0.8 + 0.5 + 0.1 = 1.4; t = 39: 0.9 + 0.6 + 0.2 = 1.7 > 1.5, the first majority.
  expect_equal(which(fuzzy), 39L)
  vote <- attr(ensemble_vote(detections, "threshold", threshold = 10, tolerance = 10), "vote")
  expect_equal(vote[40], 1 + 0.7 + 0.3)
  expect_equal(vote[30], 0)
  # Without lookahead only earlier detections count, so the vote waits for them.
  causal <- ensemble_vote(detections, "majority", tolerance = 10, lookahead = FALSE)
  # t = 43: 0.7 + 1 (the third detection has not happened yet).
  expect_equal(which(causal), 43L)
  # A detection is used by one ensemble drift only.
  twice <- matrix(FALSE, 60, 2)
  twice[c(10, 30), 1] <- TRUE
  twice[12, 2] <- TRUE
  expect_equal(which(ensemble_vote(twice, "threshold", threshold = 1.5, tolerance = 5)), 10L)
})

test_that("the vectorised vote and the step-by-step engine agree", {
  set.seed(1)
  for (r in 1:15) {
    detections <- matrix(stats::runif(400 * 5) < 0.01, 400, 5)
    for (setting in list(list("majority", NULL, 0, TRUE), list("efer", NULL, 0, TRUE),
                         list("threshold", 2, 0, TRUE), list("majority", NULL, 30, TRUE),
                         list("threshold", 2.2, 25, TRUE), list("majority", NULL, 30, FALSE),
                         list("threshold", 1.5, 12, FALSE), list("efer", NULL, 40, TRUE))) {
      expected <- which(ensemble_vote(detections, setting[[1]], threshold = setting[[2]], tolerance = setting[[3]],
                                      lookahead = setting[[4]]))
      expect_equal(ens_online(detections, setting[[1]], setting[[2]], setting[[3]], setting[[4]]), expected)
    }
  }
})

test_that("dfr_ensemble runs its members and votes on their detections", {
  set.seed(2)
  data <- data.frame(serie = c(stats::rnorm(400), stats::rnorm(400, 3), stats::rnorm(400)))
  members <- list(ph = dfr_page_hinkley(), kswin = dfr_kswin(), adwin = dfr_adwin())
  model <- fit(dfr_ensemble(members, "majority", tolerance = 60), data)
  expect_s3_class(model, "dfr_ensemble")
  output <- model$drifter_output
  expect_equal(names(output), c("ph", "kswin", "adwin", "time", "vote"))
  # The members' own detections, run alone.
  alone <- sapply(members, function(m) {
    run <- fit(m, data)
    flags <- logical(nrow(data))
    state <- m
    for (i in seq_len(nrow(data))) {
      step <- update_state(state, data$serie[i])
      state <- step$obj
      flags[i] <- step$drift
    }
    flags
  })
  expect_equal(unname(as.matrix(output[c("ph", "kswin", "adwin")])), unname(alone))
  expect_equal(model$events, which(ensemble_vote(alone, "majority", tolerance = 60)))
  expect_true(length(model$events) >= 1L)
  expect_true(model$drifted)
  # With lookahead the decision about time t comes k - 1 observations later.
  decided <- which(!is.na(output$time))
  expect_equal(output$time[decided], decided - 59L)
  model <- reset_state(model)
  expect_false(model$drifted)
  expect_length(model$events, 0L)
  expect_error(dfr_ensemble(list()), "non-empty")
  expect_error(dfr_ensemble(list(a = dfr_ddm())), "dist_based")
  expect_error(dfr_ensemble(members, "threshold"), "needs a threshold")
  expect_error(ensemble_vote(matrix(NA, 2, 2)), "missing")
})
