proton_rows <- function(n, center, p1, sd = 0.05) {
  e <- sweep(matrix(stats::rnorm(length(center) * n, sd = sd), n), 2, center, "+")
  colnames(e) <- paste0("e", seq_along(center))
  data.frame(e, p1 = p1, p2 = 1 - p1)
}

proton_detector <- function(...) {
  dfr_proton(embedding_cols = c("e1", "e2", "e3"), prob_cols = c("p1", "p2"), ...)
}

normalize_rows <- function(m) m / sqrt(rowSums(m^2))

auroc <- function(score, is_ood) {
  as.numeric(pROC::auc(pROC::roc(is_ood, score, levels = c(FALSE, TRUE), direction = "<", quiet = TRUE)))
}

test_that("proton falls back to the MCM score until every class is calibrated", {
  set.seed(1)
  # Only class 1 is ever predicted with confidence, so the bank of class 2
  # never reaches k_min.
  stream <- proton_rows(30, c(1, 0, 0), stats::runif(30, 0.75, 0.95))
  detector <- fit(proton_detector(), stream)
  out <- detector$drifter_output
  expect_equal(out$score, -pmax(stream$p1, stream$p2))
  expect_true(all(out$alpha == 1))
  expect_true(all(is.na(out$proto_score)))
  expect_false(any(out$calibrated))
})

test_that("proton follows Algorithm 1 step by step", {
  set.seed(2)
  stream <- rbind(
    proton_rows(40, c(1, 0, 0), stats::runif(40, 0.6, 0.95)),
    proton_rows(40, c(0, 1, 0), stats::runif(40, 0.05, 0.4))
  )
  stream <- stream[sample(nrow(stream)), ]
  gamma <- 0.7
  bank_size <- 7
  k_min <- 3
  detector <- fit(proton_detector(gamma = gamma, bank_size = bank_size, k_min = k_min), stream)
  out <- detector$drifter_output

  e <- normalize_rows(as.matrix(stream[, c("e1", "e2", "e3")]))
  p <- unname(as.matrix(stream[, c("p1", "p2")]))
  base <- -apply(p, 1, max)
  predicted <- apply(p, 1, which.max)
  gated <- apply(p, 1, max) >= gamma

  for (t in seq_len(nrow(stream))) {
    variance <- if (t > 1) stats::var(base[1:t]) else 0
    expect_equal(out$variance[t], variance)
    # Banks as they were before observation t: the last bank_size gated
    # embeddings of each class.
    previous <- seq_len(t - 1)
    banks <- lapply(1:2, function(c) utils::tail(previous[gated[previous] & (predicted[previous] == c)], bank_size))
    calibrated <- all(lengths(banks) >= k_min)
    expect_equal(out$calibrated[t], calibrated)
    if (calibrated) {
      prototypes <- normalize_rows(t(vapply(banks, function(rows) colMeans(e[rows, , drop = FALSE]), numeric(3))))
      proto <- 1 - max(prototypes %*% e[t, ])
      alpha <- 0.7 - stats::plogis(100 * (variance - 0.02)) * 0.4
      expect_equal(out$proto_score[t], proto)
      expect_equal(out$alpha[t], alpha)
      expect_equal(out$score[t], alpha * base[t] + (1 - alpha) * proto)
    } else {
      expect_equal(out$score[t], unname(base[t]))
    }
    expect_equal(out$enqueued[t], unname(gated[t]))
    expect_equal(out$class[t], unname(predicted[t]))
  }
  expect_equal(nrow(detector$state$banks[[1]]), bank_size)
})

test_that("the bank is a FIFO queue and the prototype its normalized mean", {
  set.seed(3)
  stream <- proton_rows(15, c(1, 1, 0), 0.9, sd = 0.3)
  detector <- fit(proton_detector(bank_size = 4, k_min = 1), stream)
  e <- normalize_rows(as.matrix(stream[, c("e1", "e2", "e3")]))
  expect_equal(detector$state$banks[[1]], e[12:15, ], ignore_attr = TRUE)
  prototype <- colMeans(e[12:15, ])
  expect_equal(detector$state$prototypes[1, ], prototype / sqrt(sum(prototype^2)), ignore_attr = TRUE)
  expect_equal(nrow(detector$state$banks[[2]]), 0L)
})

test_that("the confidence gate keeps uncertain inputs out of the bank", {
  stream <- data.frame(e1 = c(1, 0, 1), e2 = c(0, 1, 1), e3 = 0, p1 = c(0.9, 0.55, 0.69), p2 = c(0.1, 0.45, 0.31))
  detector <- fit(proton_detector(gamma = 0.7), stream)
  expect_equal(detector$drifter_output$enqueued, c(TRUE, FALSE, FALSE))
  expect_equal(nrow(detector$state$banks[[1]]), 1L)
  expect_equal(nrow(detector$state$banks[[2]]), 0L)
})

test_that("proton separates covariate shift that the MCM score misses", {
  # Covariate-shifted inputs get the same confident softmax as in-distribution
  # ones, but their embeddings lie elsewhere (the setting of Fig. 1 of the
  # paper). They also pass the confidence gate, so they partly contaminate the
  # prototypes, which limits the gain, as with the real benchmark.
  set.seed(4)
  confidence <- function(n) stats::runif(n, 0.85, 0.99)
  id <- rbind(
    proton_rows(150, c(1, 0.2, 0), confidence(150), sd = 0.1),
    proton_rows(150, c(0.2, 1, 0), 1 - confidence(150), sd = 0.1)
  )
  covariate <- proton_rows(150, c(0.6, 0.6, 0.8), confidence(150), sd = 0.1)
  stream <- rbind(id, covariate)
  is_ood <- rep(c(FALSE, TRUE), c(300, 150))
  order <- sample(nrow(stream))
  stream <- stream[order, ]
  is_ood <- is_ood[order]

  detector <- fit(proton_detector(), stream)
  out <- detector$drifter_output
  mcm <- auroc(out$base_score, is_ood)
  proton <- auroc(out$score, is_ood)
  calibrated <- out$calibrated
  expect_lt(abs(mcm - 0.5), 0.1)
  expect_gt(proton, mcm + 0.1)
  expect_gt(auroc(out$proto_score[calibrated], is_ood[calibrated]), proton)
})

test_that("a custom base score replaces the MCM score", {
  set.seed(5)
  stream <- proton_rows(20, c(1, 0, 0), 0.9)
  stream$energy <- seq(0, 1, length.out = 20)
  detector <- fit(dfr_proton(c("e1", "e2", "e3"), c("p1", "p2"), score_col = "energy"), stream)
  expect_equal(detector$drifter_output$base_score, stream$energy)
})

test_that("fit and update_state give the same scores", {
  set.seed(6)
  stream <- rbind(proton_rows(30, c(1, 0, 0), 0.9), proton_rows(30, c(0, 1, 0), 0.1))
  stream <- stream[sample(nrow(stream)), ]
  fitted <- fit(proton_detector(k_min = 2), stream)
  output <- list(obj = proton_detector(k_min = 2), drift = FALSE)
  scores <- numeric(nrow(stream))
  for (i in seq_len(nrow(stream))) {
    output <- update_state(output$obj, stream[i, ])
    scores[i] <- output$obj$last_drifter_output$score
  }
  expect_equal(scores, fitted$drifter_output$score)
  expect_equal(output$obj$state, fitted$state)
})

test_that("a threshold turns the scores into OOD alarms", {
  set.seed(7)
  stream <- rbind(proton_rows(30, c(1, 0, 0), 0.9), proton_rows(30, c(0, 1, 0), 0.1), proton_rows(5, c(0, 0, 1), 0.55))
  quiet <- fit(proton_detector(), stream)
  expect_false(quiet$drifted)

  output <- update_state(proton_detector(threshold = -0.6), stream[61, ])
  expect_true(output$drift)
  expect_true(output$obj$drifted)
  expect_false(update_state(proton_detector(threshold = -0.6), stream[1, ])$drift)
})

test_that("incomplete rows are skipped", {
  stream <- data.frame(e1 = c(1, NA, 1), e2 = 0, e3 = 0, p1 = c(0.9, 0.9, NA), p2 = c(0.1, 0.1, 0.1))
  detector <- fit(proton_detector(), stream)
  expect_equal(detector$state$n, 1)
  expect_true(is.na(detector$drifter_output$score[2]))
  expect_false(update_state(proton_detector(), stream[2, ])$drift)
})

test_that("reset_state clears the bank and the running variance", {
  set.seed(8)
  detector <- fit(proton_detector(gamma = 0.6), proton_rows(20, c(1, 0, 0), 0.9))
  detector <- reset_state(detector)
  expect_equal(detector$state$n, 0)
  expect_equal(nrow(detector$state$banks[[1]]), 0L)
  expect_equal(detector$gamma, 0.6)
})

test_that("dfr_proton validates its inputs", {
  expect_error(dfr_proton(character(0), c("p1", "p2")), "embedding_cols")
  expect_error(dfr_proton("e1", "p1"), "at least two")
  expect_error(dfr_proton("e1", c("e1", "p2")), "must not share")
  expect_error(proton_detector(gamma = 2), "gamma")
  expect_error(proton_detector(k_min = 10, bank_size = 5), "k_min")
  expect_error(proton_detector(alpha_min = 0.8, alpha_max = 0.5), "alpha_min")
  expect_error(fit(proton_detector(), data.frame(e1 = 1)), "not in the data")
})
