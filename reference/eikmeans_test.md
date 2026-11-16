# Test a sample with an EI-kMeans partitioning

Assigns the points of a sample to the bins of an
[`eikmeans()`](https://cefet-rj-dal.github.io/heimdall/reference/eikmeans.md)
model and compares the counts with those of the training sample by
Pearson's chi-square test.

## Usage

``` r
eikmeans_test(model, sample)
```

## Arguments

- model:

  An `eikmeans` object.

- sample:

  The sample to test (matrix or data frame with the training features).

## Value

A data frame with the chi-square `statistic`, the degrees of freedom
(`df`), the `p_value` and whether a drift is detected (`drift`,
`p_value < alpha`).

## Examples

``` r
set.seed(1)
model <- eikmeans(matrix(stats::rnorm(4000), ncol = 2), n_test = 200)
eikmeans_test(model, matrix(stats::rnorm(400, 0.5), ncol = 2))
#>   statistic df      p_value drift
#> 1  85.09929 18 1.082198e-10  TRUE
```
