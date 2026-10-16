# FScore Calculator

Class for FScore calculation

## Usage

``` r
mt_fscore(f = 1)
```

## Arguments

- f:

  The beta parameter of the F-beta score. `f = 1` (the default) gives
  the usual F1 score, values below 1 weight precision more heavily and
  values above 1 weight recall more heavily.

## Value

Metric object

## Examples

``` r
library(daltoolbox)
obj <- mt_fscore(f = 1)
evaluate(obj, c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
#> [1] 0.8
```
