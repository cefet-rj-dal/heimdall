# Draw samples from a drift benchmark source

Draws `n` labelled samples from the current concept of a
[`dsg_source()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source.md).

## Usage

``` r
dsg_take(source, n = 1)
```

## Arguments

- source:

  `dsg_source` object

- n:

  Number of samples.

## Value

list with the features `x` (data frame) and the labels `y`

## Examples

``` r
set.seed(1)
sample <- dsg_take(dsg_sea(), 5)
sample$x
#>          x1       x2        x3
#> 1 8.7229738 1.307512 0.5246878
#> 2 6.1844805 6.831062 7.6775719
#> 3 0.4812483 7.676564 8.1652354
#> 4 7.2161401 2.656034 8.3028128
#> 5 1.5615210 6.981399 8.5809743
sample$y
#> [1] 1 1 0 1 0
```
