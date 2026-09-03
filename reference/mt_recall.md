# Recall Calculator

Class for recall calculation

## Usage

``` r
mt_recall()
```

## Value

Metric object

## Examples

``` r
library(daltoolbox)
obj <- mt_recall()
evaluate(obj, c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
#> [1] 0.6666667
```
