# Synthetic time series for concept drift detection

A list of univariate and multivariate time series for drift detection.
Each element is a data frame:

- `uv_virtual_drift`: univariate series (`serie`) with a change in the
  feature distribution at four known points, flagged in `event`;

- `uv_vct_real_drift`: univariate series with a virtual change followed
  by a real drift;

- `uv_vdp_real_drift`: univariate series with a real drift and no change
  in the feature distribution;

- `mv_real_drift`: bivariate series with a real drift;

- `mv_vct_real_drift`: bivariate series with a virtual change followed
  by a real drift;

- `mv_vdp_real_drift`: bivariate series with a real drift and no change
  in the feature distribution.

Except for `uv_virtual_drift`, every data frame holds the series
(`serie`, or `serie1` and `serie2`), a logical `target`, the index `i`
and a `drift` column flagging the change point.

## Usage

``` r
data(st_drift_examples)
```

## Format

A list of six data frames with 500 rows each.

## Source

[heimdall package](https://github.com/cefet-rj-dal/heimdall)

## References

[heimdall package](https://github.com/cefet-rj-dal/heimdall)

## Examples

``` r
data(st_drift_examples)
dataset <- st_drift_examples$mv_real_drift
which(dataset$drift == 1)
#> [1] 250
```
