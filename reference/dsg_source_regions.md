# Benchmark source from a synthetic generator

Builds a
[`dsg_source()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source.md)
from a generator of feature vectors and a list of labelling rules (the
real drift states). Virtual drift states are made by regional
sub-sampling, as in Hinder et al. (2026): the feature space is split
into the Voronoi cells of a k-means clustering, the cells are dealt into
`n_variants` groups, and each virtual state keeps only the samples
falling in its group.

## Usage

``` r
dsg_source_regions(
  generator,
  labelers,
  n_clusters = 20,
  n_variants = 2,
  init_size = 1000,
  ...
)
```

## Arguments

- generator:

  Function `generator(n)` returning a data frame with `n` feature
  vectors.

- labelers:

  List of labelling functions, see
  [`dsg_source()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source.md).

- n_clusters:

  Approximate number of k-means clusters; rounded up to the next
  multiple of `n_variants` above `n_clusters`, as in the reference code.

- n_variants:

  Number of virtual drift states.

- init_size:

  Number of samples used to fit the clustering.

- ...:

  Further arguments for
  [`dsg_source()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source.md).

## Value

`dsg_source` object

## Examples

``` r
set.seed(1)
source <- dsg_source_regions(
  generator = function(n) data.frame(a = stats::runif(n), b = stats::runif(n)),
  labelers = list(function(x) as.integer(x$a > x$b), function(x) as.integer(x$a > 0.5)),
  test_size = 200, n_perm = 200
)
source
#> <dsg_source_regions> 2 feature distributions x 2 labelling rules; current concept: virtual 2, real 1
```
