# Normalizer base class

Ancestor class for normalization techniques.

## Usage

``` r
nrm_base(norm_class)
```

## Arguments

- norm_class:

  Normalizer class

## Value

Norm object

## Details

This constructor was named [`norm()`](https://rdrr.io/r/base/norm.html)
up to version 1.2.x. It was renamed to `nrm_base()` because
[`norm()`](https://rdrr.io/r/base/norm.html) masks
[`base::norm()`](https://rdrr.io/r/base/norm.html) once the package is
attached.

## Examples

``` r
library(daltoolbox)
obj <- nrm_base(norm_class = minmax())
class(obj)
#> [1] "norm"
```
