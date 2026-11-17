# InDiD stream detector

Monitors a stream with a trained
[`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md)
model: the observations are fed one by one to the recurrent network, and
a drift is reported when the probability of change exceeds `threshold`.
Since the model is trained on labelled sequences of the monitored
features, it detects the kinds of change it was trained on (**virtual
concept drift** of those features).

## Usage

``` r
dfr_indid(model, threshold = 0.5, target_feat = NULL, horizon = NULL)
```

## Arguments

- model:

  A trained `indid` object.

- threshold:

  Alarm threshold on the probability of change.

- target_feat:

  Names of the columns fed to the model, in the order of training.
  `NULL` (the default) uses the model's feature names, or every numeric
  column when the model has none.

- horizon:

  If set, the state of the network is reset every `horizon`
  observations, so that the stream is read as consecutive sequences of
  the training length (`model$seq_len` is a natural choice). `NULL`
  (default) never resets it except after a drift.

## Value

`dfr_indid` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds the probability of change of every observation (`probability`).

## Details

After a drift the state of the network is reset, so the data after the
change are read as the start of a new sequence. The model has learned
what the beginning of a sequence without change looks like, so it keeps
raising alarms if the new regime does not resemble it. Rows with missing
values are skipped.

## References

Romanenkova, E., Stepikin, A., Morozov, M. and Zaytsev, A. (2022).
InDiD: Instant disorder detection via a principled neural network.
*Proceedings of the 30th ACM International Conference on Multimedia*,
3152-3162. <doi:10.1145/3503161.3548182>

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
make <- function(n) {
  change <- sample(2:32, n, replace = TRUE)
  change[seq_len(n / 2)] <- 33
  x <- t(vapply(change, function(c) stats::rnorm(32, ifelse(seq_len(32) < c, 1, 6)), numeric(32)))
  labels <- t(vapply(change, function(c) (seq_len(32) >= c) * 1, numeric(32)))
  list(x = x, labels = labels)
}
train <- make(200)
model <- indid(train$x, train$labels, T = 8, learning_rate = 0.02, epochs = 30, seed = 1)
stream <- data.frame(value = c(stats::rnorm(20, 1), stats::rnorm(10, 6)))
detector <- fit(dfr_indid(model), stream)
which(detector$drifter_output$probability > 0.5)[1]
#> [1] 21
```
