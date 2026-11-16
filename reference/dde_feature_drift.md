# Feature-wise drift tests between two windows

Tests every feature for drift between a reference window and an analysis
window, with the univariate tests used by Szucs and Nemeth (2025).

## Usage

``` r
dde_feature_drift(reference, current, test = "js", threshold = NULL)
```

## Arguments

- reference:

  Data frame or matrix with the reference window.

- current:

  Data frame or matrix with the analysis window, with the same columns.

- test:

  One of `"js"`, `"ks"` or `"wasserstein"`.

- threshold:

  Drift threshold. `NULL` uses the default of the test.

## Value

data frame with one row per feature and the columns `feature`,
`statistic` (the distance, or the p-value for `"ks"`) and `drift`.

## Details

The tests follow the definitions and defaults of the Evidently library:

- `"js"`: Jensen-Shannon distance (natural logarithm) between the
  histograms of the two windows, on common bins chosen with Sturges'
  rule over both windows; empty bins get a proportion of 1e-4. Drift
  when the distance is at least `threshold` (default 0.1).

- `"ks"`: two-sample Kolmogorov-Smirnov test. Drift when the p-value is
  below `threshold` (default 0.05).

- `"wasserstein"`: Wasserstein distance divided by the standard
  deviation of the reference window (at least 0.001). Drift when it is
  at least `threshold` (default 0.1).

Missing values are dropped; features with fewer than two values in
either window are not tested.

The distances of the `"js"` test do not shrink to 0 between two samples
of the same distribution: with a few hundred observations per window the
Jensen-Shannon distance of identical distributions is often close to the
default threshold. Use windows of about a thousand observations, or a
higher threshold, with small windows.

## Examples

``` r
set.seed(1)
reference <- data.frame(x = stats::rnorm(300), y = stats::rnorm(300))
current <- data.frame(x = stats::rnorm(300, mean = 1), y = stats::rnorm(300))
dde_feature_drift(reference, current, test = "ks")
#>   feature    statistic drift
#> 1       x 2.914443e-18  TRUE
#> 2       y 7.870443e-01 FALSE
```
