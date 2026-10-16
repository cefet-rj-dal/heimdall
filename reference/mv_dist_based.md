# Multivariate Distribution Based Drifter sub-class

Implements Multivariate Distribution Based drift detectors. These
detectors consume one row of a data frame at a time and monitor its
numeric columns.

## Usage

``` r
mv_dist_based(target_feat = NULL)
```

## Arguments

- target_feat:

  Names of the columns to be monitored. `NULL` (the default) monitors
  every numeric column.

## Value

Drifter object

## Examples

``` r
obj <- mv_dist_based(target_feat = c("serie1", "serie2"))
obj$target_feat
#> [1] "serie1" "serie2"
```
