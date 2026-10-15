# Accuracy Calculator

Class for accuracy calculation. Binary labels (logical, 0/1 or factors
with those labels) are compared by value; other labels are compared as
text.

## Usage

``` r
mt_accuracy()
```

## Value

Metric object

## Examples

``` r
library(daltoolbox)
obj <- mt_accuracy()
evaluate(obj, c(TRUE, FALSE, TRUE), c(TRUE, TRUE, TRUE))
#> [1] 0.6666667
```
