# Distribution Based Drifter sub-class

Implements univariate Distribution Based drift detectors. These
detectors monitor a single numeric feature; observations that are
missing are skipped instead of being imputed.

## Usage

``` r
dist_based(target_feat)
```

## Arguments

- target_feat:

  Feature to be monitored.

## Value

Drifter object

## Examples

``` r
obj <- dist_based(target_feat = 'serie')
obj$target_feat
#> [1] "serie"
```
