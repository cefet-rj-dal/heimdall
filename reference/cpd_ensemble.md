# Ensembles of change point models

`cpd_ensemble()` groups trained change point models
([`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md)
with BCE or InDiD loss,
[`tscp2()`](https://cefet-rj-dal.github.io/heimdall/reference/tscp2.md),
[`ts2vec()`](https://cefet-rj-dal.github.io/heimdall/reference/ts2vec.md),
or calibrated ones) into an ensemble, possibly mixing types;
`cpd_ensemble_fit()` trains the members of an ensemble of one type. The
scores of the members
([`cpd_scores()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_scores.md))
are aggregated by
[`wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/wwaggr.md)
or monitored on a stream by
[`dfr_wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_wwaggr.md).

## Usage

``` r
cpd_ensemble(members)

cpd_ensemble_fit(
  x,
  labels = NULL,
  model = c("bce", "indid", "tscp2", "ts2vec"),
  n_models = 10,
  boot_sample_size = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- members:

  A list of trained change point models with the same input features.

- x:

  Training sequences, in the formats accepted by
  [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md).

- labels:

  Their labels (0/1 matrix `[sequence, time]`), for the supervised
  models.

- model:

  Type of the members: `"bce"` and `"indid"`
  ([`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md)
  with that loss), `"tscp2"` or `"ts2vec"`.

- n_models:

  Number of members.

- boot_sample_size:

  `NULL` (default): every member is trained on all the sequences and the
  members differ only by their seeds (the "naive" ensembles of the
  paper). Otherwise every member is trained on a bootstrap sample of
  `boot_sample_size` sequences.

- seed:

  Seed of the ensemble: member `i` is trained with seed `seed + i - 1`,
  and the bootstrap samples are drawn with `seed`. `NULL` draws one from
  the session.

- ...:

  Settings of the members, passed to
  [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md),
  [`tscp2()`](https://cefet-rj-dal.github.io/heimdall/reference/tscp2.md)
  or
  [`ts2vec()`](https://cefet-rj-dal.github.io/heimdall/reference/ts2vec.md).

## Value

A `cpd_ensemble` object (`members`).

## Details

The paper also compares ensembles trained with stochastic gradient
Langevin dynamics; the reference code has no implementation of them and
they are not provided.

## References

Stepikin, A., Romanenkova, E. and Zaytsev, A. (2025). WWAggr: A window
Wasserstein-based aggregation for ensemble change point detection. *2025
IEEE International Conference on Data Mining (ICDM)*, 1535-1544.
<doi:10.1109/ICDM65498.2025.00163>

## Examples

``` r
set.seed(1)
make <- function(n) {
  change <- sample(5:24, n, replace = TRUE)
  x <- t(vapply(change, function(c) stats::rnorm(24, ifelse(seq_len(24) < c, 0, 3)), numeric(24)))
  list(x = x, labels = t(vapply(change, function(c) (seq_len(24) >= c) * 1, numeric(24))))
}
train <- make(64)
ensemble <- cpd_ensemble_fit(train$x, train$labels, model = "bce", n_models = 3,
                             epochs = 5, seed = 1)
dim(cpd_scores(ensemble, train$x[1:2, ]))
#> [1]  3  2 24
```
