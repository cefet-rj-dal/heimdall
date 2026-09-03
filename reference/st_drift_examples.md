# Synthetic time series for concept drift detection

A list of time series for drift detection. It contains, among others:

- `example1`: a bivariate dataset with one multivariate concept drift
  example

- `univariate`: a univariate series with four known change points

## Usage

``` r
data(st_drift_examples)
```

## Format

A list of time series.

## Source

[heimdall package](https://github.com/cefet-rj-dal/heimdall)

## References

[heimdall package](https://github.com/cefet-rj-dal/heimdall)

## Examples

``` r
data(st_drift_examples)
dataset <- st_drift_examples$example1
```
