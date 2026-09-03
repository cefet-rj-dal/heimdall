# Error Based Drifter sub-class

Implements Error Based drift detectors. These detectors monitor the
residuals of a predictive model, where `1` (or `TRUE`) means a wrong
prediction and `0` (or `FALSE`) a correct one. Missing values are
treated as correct predictions.

## Usage

``` r
error_based()
```

## Value

Drifter object

## Examples

``` r
obj <- error_based()
class(obj)
#> [1] "error_based" "drifter"     "dal_base"   
```
