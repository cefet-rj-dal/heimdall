# GreedyKS sketch of the one-sample Kolmogorov-Smirnov statistic

Online approximation of the statistic `D = sup |F_n(x) - F(x)|` of the
one-sample Kolmogorov-Smirnov test between a stream and a reference
distribution with CDF `F`, in `O(log m)` time per observation and `O(m)`
memory, whatever the number of observations (Cardoso and Galeno, 2023).

## Usage

``` r
greedyks(cdf, m = 100, min_instances = 0)

greedyks_update(sketch, x)

greedyks_statistic(sketch)

greedyks_pvalue(sketch, exact = TRUE)
```

## Arguments

- cdf:

  Cumulative distribution function of the reference distribution (for
  example `function(x) pnorm(x, 1, 1)`).

- m:

  Number of buckets (the memory units of the paper).

- min_instances:

  Number of observations before the statistic is estimated (it is 0
  before).

- sketch:

  A `greedyks` sketch.

- x:

  Numeric vector of observations, processed in order. Missing values are
  skipped.

- exact:

  Passed to
  [`ks_sf()`](https://cefet-rj-dal.github.io/heimdall/reference/ks_sf.md):
  the exact distribution of the statistic (`TRUE`, the default, as
  `scipy.stats.kstwo` in the reference) or the limiting Kolmogorov
  distribution (`FALSE`).

## Value

`greedyks()` returns a `greedyks` sketch. `greedyks_update()` returns
the sketch after the observations `x`, in order. `greedyks_statistic()`
returns the estimate of `D`, and `greedyks_pvalue()` its p-value for the
number of observations processed (see
[`ks_sf()`](https://cefet-rj-dal.github.io/heimdall/reference/ks_sf.md)).

## Details

Each observation `x` is mapped to `f = F(x)` and to the bucket
`b = ceiling(f m)` of `0, ..., m`, which keeps the number of
observations and the smallest and largest `f` it received. The largest
gaps between the ECDF and the CDF, `D+` and `D-`, are updated greedily
(Algorithm 2 of the paper): with the bucket just updated and with the
buckets that gave the current maxima, re-evaluated at every step. The
counts before a bucket come from a Fenwick tree (binary indexed tree).
The estimate is `max(D+, D-)`.

The reference implementation uses `ceiling(f m)` (the paper writes
`floor`), so there are `m + 1` buckets; heimdall does the same.

## References

Cardoso, D. O. and Galeno, T. D. (2023). Online evaluation of the
Kolmogorov-Smirnov test on arbitrarily large samples. *Journal of
Computational Science*, 67, 101959. <doi:10.1016/j.jocs.2023.101959>

## Examples

``` r
set.seed(1)
x <- rnorm(5000, 1.1, 1)
sketch <- greedyks_update(greedyks(function(q) pnorm(q, 1, 1), m = 100), x)
greedyks_statistic(sketch)
#> [1] 0.04062458
unname(ks.test(x, "pnorm", 1, 1)$statistic)   # exact value
#> [1] 0.04101958
greedyks_pvalue(sketch)
#> [1] 1.316868e-07
```
