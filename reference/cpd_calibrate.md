# Calibration of change point models

Post-hoc calibration of the scores of a change point model
([`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md),
[`tscp2()`](https://cefet-rj-dal.github.io/heimdall/reference/tscp2.md),
[`ts2vec()`](https://cefet-rj-dal.github.io/heimdall/reference/ts2vec.md))
or of every member of an ensemble
([`cpd_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md))
on labelled sequences, as the WWAggr pipeline does before aggregating:
beta calibration (Kull et al., 2017, the method of the paper) or
temperature scaling (Guo et al., 2017).

## Usage

``` r
cpd_calibrate(
  model,
  x,
  labels,
  method = c("beta", "temperature"),
  parameters = c("abm", "am", "ab", "a")
)
```

## Arguments

- model:

  A change point model or a `cpd_ensemble`.

- x:

  Calibration sequences, in the formats accepted by
  [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md).

- labels:

  Their labels, a 0/1 matrix `[sequence, time]` (1 from the change on).

- method:

  `"beta"` (default) or `"temperature"`.

- parameters:

  Parameters of the beta calibration: `"abm"` (default), `"am"`, `"ab"`
  or `"a"`.

## Value

A `cpd_calibrated` model (the model and its calibration map), or a
`cpd_ensemble` of calibrated members.

## Details

Every score of every step (the steps without score of the representation
models excluded) is a sample, labelled by the label of its step. The
beta calibration follows the `betacal` package used by the reference
(<https://github.com/betacal/python>): a logistic regression of the
label on `log(s)` and `-log(1 - s)` (`"abm"`; when one coefficient is
negative, the regression is refitted on the other feature alone), on the
logit of `s` (`"am"`), on `log(2 s)` and `log(2 (1 - s))` without
intercept (`"ab"`), or on the logit without intercept (`"a"`). Scores
are clipped to `[eps, 1 - eps]` with the machine epsilon of single
precision (the reference scores are float32). The regressions are
unpenalised (`glm`; the reference uses scikit-learn's
`LogisticRegression` with `C = 1e11`).

Temperature scaling divides the logit of the score by the temperature
that minimises the log-loss on the calibration data. In the reference
code the fitted temperature is stored on the model wrapper and does not
change its predictions, so this option follows Guo et al. (2017) rather
than the code.

## References

Kull, M., Silva Filho, T. M. and Flach, P. (2017). Beyond sigmoids: How
to obtain well-calibrated probabilities from binary classifiers with
beta calibration. *Electronic Journal of Statistics*, 11(2), 5052-5080.
<doi:10.1214/17-EJS1338SI>

Guo, C., Pleiss, G., Sun, Y. and Weinberger, K. Q. (2017). On
calibration of modern neural networks. *Proceedings of the 34th
International Conference on Machine Learning*, 1321-1330.

## Examples

``` r
set.seed(1)
make <- function(n) {
  change <- sample(5:24, n, replace = TRUE)
  x <- t(vapply(change, function(c) stats::rnorm(24, ifelse(seq_len(24) < c, 0, 3)), numeric(24)))
  list(x = x, labels = t(vapply(change, function(c) (seq_len(24) >= c) * 1, numeric(24))))
}
train <- make(64)
model <- indid(train$x, train$labels, loss = "bce", epochs = 5, seed = 1)
calibrated <- cpd_calibrate(model, train$x, train$labels)
calibrated$calibrator$map
#>          a          b          m 
#>  0.0000000 86.1514795  0.6543955 
```
