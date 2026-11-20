# Change by brute force over every split (O(n)).
brute_change <- function(x) {
  n <- length(x)
  ll <- function(a, b) (if (a > 0) a * log(a / (a + b)) else 0) + (if (b > 0) b * log(b / (a + b)) else 0)
  a <- sum(x)
  base <- ll(a, n - a)
  cs <- c(0, cumsum(x))
  s <- vapply(1:(n + 1), function(i) {
    a1 <- cs[i]
    b1 <- (i - 1) - a1
    ll(a1, b1) + ll(a - a1, (n - i + 1) - (a - a1)) - base
  }, numeric(1))
  list(score = max(s), index = which.max(s))
}

test_that("blocks are the border indices with increasing proportions", {
  x <- c(1, 0, 0, 1, 1, 0, 1, 1, 1, 0)
  b <- heimdall:::.fastlr_blocks(x)
  u <- diff(c(0, b$cu))
  v <- diff(c(0, b$cv))
  expect_equal(sum(u), sum(x))
  expect_equal(sum(u + v), length(x))
  expect_true(all(diff(u / (u + v)) > 0))
  # border definition: no x < j < y with mean(x..j-1) >= mean(j..y-1)
  starts <- cumsum(c(1, head(u + v, -1)))
  border <- function(j) {
    for (a in seq_len(j - 1)) for (y in (j + 1):(length(x) + 1)) {
      if (mean(x[a:(j - 1)]) >= mean(x[j:(y - 1)])) return(FALSE)
    }
    TRUE
  }
  expect_true(all(vapply(starts[-1], border, logical(1))))
})

test_that("eps = 0 solves Change exactly and eps > 0 keeps the guarantee", {
  set.seed(1)
  for (rep in 1:40) {
    n <- sample(c(5, 30, 200, 600), 1)
    p <- runif(2)
    cut <- sample(n, 1)
    x <- c(rbinom(cut, 1, p[1]), rbinom(n - cut, 1, p[2]))
    if (rep %% 4 == 0) x <- rbinom(n, 1, seq(0.2, 0.8, length.out = n))
    opt <- brute_change(x)
    r <- fastlr_split(x)
    expect_equal(r$score, opt$score, tolerance = 1e-10)
    for (eps in c(0.1, 0.5, 0.9)) {
      ra <- fastlr_split(x, eps)
      expect_lte(ra$score, opt$score + 1e-9)
      expect_gte(ra$score, (1 - eps) * opt$score - 1e-9)
      expect_lte(ra$tests, r$tests)
    }
  }
  expect_equal(fastlr_split(c(0, 0, 0))$score, 0)
  expect_equal(fastlr_split(c(0, 0, 1, 1))$index, 3)
  expect_error(fastlr_split(c(0, 2)), "0 or 1")
  expect_error(fastlr_split(c(0, 1), eps = 1), "smaller than 1")
})

test_that("dfr_fastlr detects the changes of a Step stream", {
  set.seed(2)
  x <- c(rbinom(2000, 1, 0.25), rbinom(2000, 1, 0.75), rbinom(2000, 1, 0.25))
  det <- fit(dfr_fastlr(tau = 6, eps = 0.5), x)
  out <- det$drifter_output
  expect_named(out, c("score", "change_point"))
  alarms <- which(!is.na(out$change_point))
  expect_length(alarms, 2)
  expect_true(all(alarms > c(2000, 4000) & alarms < c(2100, 4100)))
  expect_true(all(abs(out$change_point[alarms] - c(2001, 4001)) < 30))
  expect_true(det$drifted)
  expect_false(reset_state(det)$drifted)

  # the stream detector and the offline split agree before the first change
  det0 <- fit(dfr_fastlr(tau = 6), x[1:1500])
  expect_equal(tail(det0$drifter_output$score, 1), fastlr_split(x[1:1500])$score)
  expect_false(any(fit(dfr_fastlr(), rbinom(3000, 1, 0.5))$drifter_output$score > 6 + log(3000)))
  expect_error(dfr_fastlr(eps = -1), "eps")
  expect_error(update_state(dfr_fastlr(), 3), "expect 1")
})
