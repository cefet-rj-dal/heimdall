# Benchmark source from a real-world stream

Turns a real-world data stream into a
[`dsg_source()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source.md)
whose real and virtual drift are controlled (Hinder et al., 2026). The
stream, assumed to be in time order, is split into blocks twice:

- `virtual_blocks` time segments give the feature distributions, sampled
  with replacement from the rows of each segment (virtual drift);

- `real_blocks` time segments are given to a model of \\P(y \mid
  \tilde{T}, x)\\, where \\\tilde{T}\\ is the segment index; the labels
  of every concept are sampled from that model with the index of a
  segment fixed (real drift).

Labelling the data of any feature segment with the model of any label
segment separates the virtual drift between segments from the real drift
learned by the model, giving `(virtual_blocks + 1) * (real_blocks + 1)`
concepts with known drift between them.

## Usage

``` r
dsg_source_data(
  data,
  target,
  real_blocks = 5,
  virtual_blocks = 5,
  model = NULL,
  nodesize = 10,
  n_clusters = 0,
  n_ignore = 3,
  ...
)
```

## Arguments

- data:

  Data frame with the stream in time order. Every column but `target`
  must be numeric.

- target:

  Name of the label column.

- real_blocks, virtual_blocks:

  Number of segment boundaries (giving one more segment), or the
  boundaries themselves as relative positions in (0, 1) or as row
  indices.

- model:

  Function `model(x, y)` that fits a classifier on the data frame `x`
  (with the segment index in column `block`) and returns a function
  giving a matrix of class probabilities (one column per class, named
  after the classes) for new data. `NULL` uses a random forest from the
  `randomForest` package (100 trees, `nodesize` as given).

- nodesize:

  Minimum leaf size of the default random forest.

- n_clusters:

  When greater than 0, the real drift is enhanced with region-wise label
  flips: the data are split into
  `(n_ignore + 1) * real_blocks * n_clusters` k-means clusters, and in
  each segment the labels of a segment-specific subset of the clusters
  are permuted among the classes.

- n_ignore:

  Controls the share of clusters that are never flipped.

- ...:

  Further arguments for
  [`dsg_source()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source.md),
  such as `test_size`.

## Value

`dsg_source` object (also of class `dsg_source_data`)

## Details

With the default model, `obj$quality` holds the out-of-bag accuracy of
the forest (how well the labelling model fits the data) and the
permutation importance of the segment index (how much real drift the
model found).

## References

Hinder, F., Brinkrolf, J., Lammers, K., and Hammer, B. (2026). Real vs.
Virtual Drift: Creating Realistic Stream Learning Benchmarks. In *ESANN
2026*, 133-138.

## Examples

``` r
# \donttest{
set.seed(1)
n <- 600
x1 <- c(stats::rnorm(300), stats::rnorm(300, 2))
x2 <- stats::rnorm(n)
y <- as.integer(x2 > c(rep(0, 300), rep(1, 300)))
stream <- data.frame(x1, x2, y)
source <- dsg_source_data(stream, "y", real_blocks = 1, virtual_blocks = 1,
                          test_size = 200, n_perm = 200)
dsg_stream(source, "-v-r", batch_size = 100)$segments
#>   segment type drift virtual_state real_state p_value label_change
#> 1       1    -     0             2          2      NA           NA
#> 2       2    v     1             1          2       0           NA
#> 3       3    -     0             1          2      NA           NA
#> 4       4    r     2             1          1      NA        0.305
# }
```
