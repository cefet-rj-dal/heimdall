# Change point scores of a sequence model

Scores in `[0, 1]` of every time step of every sequence: the probability
of change of an
[`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md)
model, or the processed similarity of consecutive windows of a
[`tscp2()`](https://cefet-rj-dal.github.io/heimdall/reference/tscp2.md)
or
[`ts2vec()`](https://cefet-rj-dal.github.io/heimdall/reference/ts2vec.md)
model. For calibrated models
([`cpd_calibrate()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_calibrate.md))
the calibrated scores, and for ensembles
([`cpd_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md))
the scores of every member.

## Usage

``` r
cpd_scores(model, x, ...)
```

## Arguments

- model:

  A model: `indid`, `tscp2`, `ts2vec`, `cpd_calibrated` or
  `cpd_ensemble`.

- x:

  Sequences, in the formats accepted by
  [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md).

- ...:

  Unused.

## Value

A matrix `[sequence, time]`; for an ensemble an array
`[member, sequence, time]`. The representation models (TS-CP2 and
SN-TS2Vec) have no score for the first `2 * window - 1` steps (`NA`).

## Examples

``` r
set.seed(1)
# 8 sequences of 12 steps with 2 features; the mean moves at step 7.
x <- array(stats::rnorm(8 * 12 * 2), c(8, 12, 2))
x[, 7:12, ] <- x[, 7:12, ] + 3
model <- ts2vec(x, window = 3, output_dim = 4, hidden_dim = 8, depth = 2, epochs = 5, seed = 1)
round(cpd_scores(model, x[1:2, , ]), 2)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#> [1,]   NA   NA   NA   NA   NA 0.26 0.16 0.62    1     1     1  0.01
#> [2,]   NA   NA   NA   NA   NA 0.36 0.34 0.33    1     1     1  0.06
```
