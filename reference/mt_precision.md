# Precision Calculator

Class for precision calculation

## Usage

``` r
mt_precision()
```

## Value

Metric object

## Examples

``` r
library(daltoolbox)
obj <- mt_precision()
evaluate(obj, c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
#> [1] 1
```
