# ROC AUC Calculator

Class for ROC AUC calculation

## Usage

``` r
mt_rocauc()
```

## Value

Metric object

## Examples

``` r
library(daltoolbox)
obj <- mt_rocauc()
evaluate(obj, c(0.9, 0.2, 0.8, 0.1), factor(c(TRUE, TRUE, FALSE, FALSE)))
#> [1] 0.75
```
