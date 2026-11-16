# Test batches with a Multimodal QuantTree

Assigns each batch to the closest modality of an
[`mmqt()`](https://cefet-rj-dal.github.io/heimdall/reference/mmqt.md)
model (Eq. 7) and compares its total variation statistic with the
threshold of that modality (Eqs. 8-9).

## Usage

``` r
mmqt_test(model, batches)
```

## Arguments

- model:

  An `mmqt` object.

- batches:

  A batch (matrix or data frame) or a list of batches, with the columns
  of the training set and `model$nu` rows each.

## Value

A data frame with one row per batch: the assigned `modality`, the
`statistic`, the `threshold` of the modality and whether a change is
detected (`drift`).

## Examples

``` r
set.seed(1)
training <- lapply(1:40, function(i) matrix(stats::rnorm(128), ncol = 2))
model <- mmqt(training, bins = 8, n_modalities = 1, n_sim = 2000)
mmqt_test(model, list(matrix(stats::rnorm(128), ncol = 2), matrix(stats::rnorm(128, 1), ncol = 2)))
#>   modality statistic threshold drift
#> 1        1 0.1565429 0.2029298 FALSE
#> 2        1 0.4060060 0.2029298  TRUE
```
