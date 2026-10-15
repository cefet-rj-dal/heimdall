# Synthetic time series for concept drift detection

A list of time series for drift detection:

- `dataset1`: two numeric series (`serie1`, `serie2`), a logical
  `target`, the index `i` and a `drift` column flagging the change
  point;

- `dataset2`: the same series without the target;

- `univariate`: a univariate series (`serie`) with four known change
  points flagged in `event`.

## Usage

``` r
data(st_drift_examples)
```

## Format

A list of three data frames with 500 rows each.

## Source

[heimdall package](https://github.com/cefet-rj-dal/heimdall)

## References

[heimdall package](https://github.com/cefet-rj-dal/heimdall)

## Examples

``` r
data(st_drift_examples)
dataset <- st_drift_examples$dataset1
which(dataset$drift == 1)
#> [1] 250
```
