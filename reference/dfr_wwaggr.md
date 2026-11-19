# WWAggr ensemble drift detector

Monitors a stream with an ensemble of change point models
([`cpd_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md),
preferably calibrated with
[`cpd_calibrate()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_calibrate.md)):
every member scores the observations as they arrive, the scores are
aggregated by WWAggr (Stepikin et al., 2025) or by a naive aggregation
([`wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/wwaggr.md)),
and a drift is reported when the aggregated score exceeds `threshold`.
Since the members are trained on sequences of the monitored features, it
detects changes of their distribution (**virtual concept drift**).

## Usage

``` r
dfr_wwaggr(
  ensemble,
  threshold = 0.5,
  method = c("wasserstein", "mmd", "mean", "min", "max", "median", "quantile"),
  window_size = 3,
  anchor = c("prev", "start", "combined"),
  p = 1,
  kernel = c("rbf", "multiscale"),
  bandwidth = NULL,
  q = 0.5,
  target_feat = NULL,
  horizon = NULL
)
```

## Arguments

- ensemble:

  A `cpd_ensemble`.

- threshold:

  Alarm threshold on the aggregated score. With calibrated members and
  the 1-Wasserstein distance, the paper finds that a threshold near 0.5
  is close to the best one.

- method, window_size, anchor, p, kernel, bandwidth, q:

  Aggregation, as in
  [`wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/wwaggr.md).

- target_feat:

  Names of the columns fed to the members, in the order of training.
  `NULL` (default) uses the feature names of the ensemble, or every
  numeric column when it has none.

- horizon:

  If set, the members and the aggregation restart every `horizon`
  observations, so that the stream is read as consecutive sequences of
  the training length. `NULL` (default) restarts them only after a
  drift.

## Value

`dfr_wwaggr` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds, for every observation, the aggregated score (`score`, `NA` when
there is none) and whether a drift was reported (`drift`).

## Details

The members score each observation online, as on whole sequences: the
LSTM models
([`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md))
update their state; the representation models
([`tscp2()`](https://cefet-rj-dal.github.io/heimdall/reference/tscp2.md),
[`ts2vec()`](https://cefet-rj-dal.github.io/heimdall/reference/ts2vec.md))
compare the embeddings of the last two windows of `window` observations
(every `step` observations, with the exponential smoothing `alpha`), so
they have no score for the first `2 * window - 1` observations of a
sequence. The aggregation starts when all members have a score, and its
score at an observation compares the window of the `window_size`
previous scores with the anchor (see
[`wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/wwaggr.md)).
After a drift, the members and the aggregation restart. Rows with
missing values are skipped.

## References

Stepikin, A., Romanenkova, E. and Zaytsev, A. (2025). WWAggr: A window
Wasserstein-based aggregation for ensemble change point detection. *2025
IEEE International Conference on Data Mining (ICDM)*, 1535-1544.
<doi:10.1109/ICDM65498.2025.00163>

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
make <- function(n) {
  change <- sample(5:24, n, replace = TRUE)
  change[seq_len(n / 2)] <- 25
  x <- t(vapply(change, function(c) stats::rnorm(24, ifelse(seq_len(24) < c, 0, 3)), numeric(24)))
  list(x = x, labels = t(vapply(change, function(c) (seq_len(24) >= c) * 1, numeric(24))))
}
train <- make(96)
ensemble <- cpd_ensemble_fit(train$x, train$labels, model = "bce", n_models = 3, epochs = 10,
                             learning_rate = 0.01, seed = 1)
ensemble <- cpd_calibrate(ensemble, train$x, train$labels)
stream <- data.frame(value = c(stats::rnorm(15), stats::rnorm(15, 3)))
detector <- fit(dfr_wwaggr(ensemble, window_size = 2), stream)
which(detector$drifter_output$drift == 1)
#> [1] 19
```
