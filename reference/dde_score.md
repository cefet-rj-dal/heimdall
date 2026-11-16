# DDE drift scores

Predicts the drift score of unmonitored features from the drifting
monitored features (Szucs and Nemeth, 2025, Eq. 25).

## Usage

``` r
dde_score(curve, data, drifting, unmonitored)
```

## Arguments

- curve:

  DDE curve, as returned by
  [`dde_curve()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_curve.md).

- data:

  Data frame or matrix with the features in the reference window, where
  the Pearson correlations are measured.

- drifting:

  Names of the monitored features that drift.

- unmonitored:

  Names of the features to score.

## Value

named numeric vector of scores, one per unmonitored feature.

## Details

The score of an unmonitored feature \\f_i\\ is the mean, over the
drifting monitored features \\f_j\\, of the DDE probability at
\\\|\rho\_{ij}\|\\: the probability of the largest threshold that does
not exceed the correlation. Thresholds without pairs take the
probability of the previous threshold. Scores are limited to \\\[0,
1\]\\; they are 0 when no monitored feature drifts.

## Examples

``` r
set.seed(1)
a <- stats::rnorm(200)
b <- stats::rnorm(200)
data <- data.frame(a1 = a, a2 = a + stats::rnorm(200, sd = 0.2),
                   a3 = a + stats::rnorm(200, sd = 0.5),
                   b1 = b, b2 = b + stats::rnorm(200, sd = 0.2),
                   b3 = b + stats::rnorm(200, sd = 0.5))
curve <- dde_curve(data, c("a1", "a2", "a3"))
# a1 drifts: its neighbours get high scores, the "b" features low ones.
dde_score(curve, data, drifting = "a1", unmonitored = c("a2", "a3", "b2", "b3"))
#>  a2  a3  b2  b3 
#> 1.0 1.0 0.4 0.4 
```
