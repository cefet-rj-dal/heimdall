# Drift benchmark source

Creates a data source with controlled real and virtual drift, following
the pipeline of Hinder et al. (2026), "Real vs. Virtual Drift: Creating
Realistic Stream Learning Benchmarks". A source combines `nv` feature
distributions (virtual drift states, given by `samplers`) with `nr`
labelling rules (real drift states, given by `labelers`), so it has
`nv * nr` concepts. Moving between two concepts that share the labelling
rule is pure virtual drift; moving between two that share the feature
distribution is pure real drift.

Most users will create sources with
[`dsg_source_data()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source_data.md)
(from a real-world stream) or with the synthetic generators
[`dsg_sea()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md),
[`dsg_agrawal()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md),
[`dsg_sine()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md)
and
[`dsg_mixed()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md),
draw samples with
[`dsg_take()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_take.md),
induce drift with
[`dsg_drift()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_drift.md),
and build whole streams with
[`dsg_stream()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_stream.md).

## Usage

``` r
dsg_source(
  samplers,
  labelers,
  real_drift_level = 0.01,
  virtual_p_value = 0.001,
  test_size = 1000,
  n_perm = 2500,
  max_tries = 100
)
```

## Arguments

- samplers:

  List of functions; `samplers[[v]](n)` returns a data frame (or matrix)
  with up to `n` rows drawn from feature distribution `v`. It may return
  fewer rows;
  [`dsg_take()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_take.md)
  calls it again until it has enough.

- labelers:

  List of functions; `labelers[[r]](x)` returns one label per row of the
  data frame `x` according to labelling rule `r`.

- real_drift_level:

  Smallest share of labels that a real drift must change (on `test_size`
  points) to be accepted.

- virtual_p_value:

  Largest p-value of the kernel two-sample test
  ([`dsg_mmd_test()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_mmd_test.md))
  for a virtual drift to be accepted.

- test_size:

  Number of points drawn to verify each drift.

- n_perm:

  Number of permutations of the two-sample test.

- max_tries:

  Number of candidate concepts tried by
  [`dsg_drift()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_drift.md)
  before it falls back to the strongest real drift found.

## Value

`dsg_source` object

## References

Hinder, F., Brinkrolf, J., Lammers, K., and Hammer, B. (2026). Real vs.
Virtual Drift: Creating Realistic Stream Learning Benchmarks. In
*Proceedings of the European Symposium on Artificial Neural Networks,
Computational Intelligence and Machine Learning (ESANN 2026)*, 133-138.

## Examples

``` r
# Two feature distributions and two labelling rules: four concepts.
source <- dsg_source(
  samplers = list(
    function(n) data.frame(x = stats::runif(n, 0, 1)),
    function(n) data.frame(x = stats::runif(n, 0.5, 1.5))
  ),
  labelers = list(
    function(x) as.integer(x$x > 0.5),
    function(x) as.integer(x$x > 1)
  ),
  test_size = 200, n_perm = 200
)
set.seed(1)
stream <- dsg_stream(source, "-v-r", batch_size = 100)
stream$segments
#>   segment type drift virtual_state real_state p_value label_change
#> 1       1    -     0             2          2      NA           NA
#> 2       2    v     1             1          2       0           NA
#> 3       3    -     0             1          2      NA           NA
#> 4       4    r     2             1          1      NA        0.475
```
