# FScore Calculator

Class for F-Score calculation. The `f` parameter is the beta of the
F-beta score, so `f = 1` (the default) gives the usual F1 score, values
below 1 weight precision more heavily and values above 1 weight recall
more heavily.

## Usage

``` r
mt_fscore(f = 1)
```

## Arguments

- f:

  The beta parameter for the F-Score metric

## Value

Metric object

## Examples

``` r
library(daltoolbox)
obj <- mt_fscore(f = 1)
evaluate(obj, c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
#> [1] 0.8
```
