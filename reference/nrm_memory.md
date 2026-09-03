# Memory Normalizer

Normalizer that keeps its own memory of the data seen so far, so that
the normalization parameters are estimated over the whole history rather
than over the most recent batch alone.

## Usage

``` r
nrm_memory(norm_class = daltoolbox::minmax())
```

## Arguments

- norm_class:

  Normalizer class

## Value

Norm object

## Examples

``` r
library(daltoolbox)

obj <- nrm_memory(norm_class = minmax())
obj <- fit(obj, data.frame(x = c(1, 2, 3)))
transform(obj, data.frame(x = c(1, 2, 3)))
#>     x
#> 1 0.0
#> 2 0.5
#> 3 1.0
```
