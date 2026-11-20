# Incremental two-sample Kolmogorov-Smirnov test

A sketch of two samples A (group 0) and B (group 1) that supports the
insertion and removal of observations in `O(log N)` expected time and
gives the two-sample Kolmogorov-Smirnov statistic
`D = max |F_A(x) - F_B(x)|` in `O(1)` (dos Reis et al., 2016).

## Usage

``` r
iks(ratio = 1, seed = NULL)

iks_add(sketch, x, group, tie = 0)

iks_remove(sketch, x, group, tie = 0)

iks_statistic(sketch)

iks_kuiper(sketch)

iks_test(sketch, ca = 1.95)

iks_ca(alpha)
```

## Arguments

- ratio:

  The constant ratio r = \|A\| / \|B\| at which the statistic is
  computed (`1`, the default, for samples of equal sizes). Each
  observation of B weighs `r`.

- seed:

  Seed of the private generator (Park-Miller) of the priorities of the
  treap and of the tie-breaking values; `NULL` draws it from R's
  generator. The priorities do not change the statistic.

- sketch:

  An `iks` sketch.

- x:

  The observation.

- group:

  `0` for sample A (the reference), `1` for sample B.

- tie:

  Tie-breaking value of the observation (`0` by default; a random value,
  as `stats::runif(1)`, makes keys distinct). An observation is removed
  with the same `x` and `tie` it was inserted with.

- ca:

  Coefficient of the critical value `ca sqrt((n + m) / (n m))`; the
  default `1.95` is the one of the paper and of the reference code
  (`alpha = 0.001`).

- alpha:

  Significance level.

## Value

`iks()` returns an `iks` sketch; `iks_add()` and `iks_remove()` the
updated sketch; `iks_statistic()` the statistic `D`; `iks_kuiper()` the
Kuiper statistic `(max G - min G) / |A|`; `iks_test()` `TRUE` when the
null hypothesis of equal distributions is rejected; `iks_ca()` the
coefficient `c(alpha) = sqrt(-log(alpha / 2) / 2)`.

## Details

Observations are sorted by the key `(x, tie, group)`. The treap stores,
at every observation, `G = F'_A - F'_B` (the counts of A minus `r` times
the counts of B up to and including it); an insertion adds `1` or `-r`
to every later observation with a lazily propagated bulk operation, and
the subtree maxima and minima give `D = max(max G, -min G) / |A|`.

As noted by the authors, the statistic is exact only when the
observations are distinct: with tied values it can be larger than the
one of [`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html). A
random `tie` value per observation (as `IKSSW` of the reference code)
breaks the ties.

## References

dos Reis, D. M., Flach, P., Matwin, S. and Batista, G. (2016). Fast
unsupervised online drift detection using incremental Kolmogorov-Smirnov
test. In *Proceedings of the 22nd ACM SIGKDD International Conference on
Knowledge Discovery and Data Mining*, 1545-1554.
<doi:10.1145/2939672.2939836>

## Examples

``` r
set.seed(1)
a <- rnorm(200)
b <- rnorm(200, 0.3)
sketch <- iks()
for (i in 1:200) sketch <- iks_add(iks_add(sketch, a[i], 0), b[i], 1)
iks_statistic(sketch)
#> [1] 0.195
unname(ks.test(a, b)$statistic)
#> [1] 0.195
# slide the window of B by one observation
sketch <- iks_add(iks_remove(sketch, b[1], 1), 2, 1)
iks_test(sketch, ca = iks_ca(0.001))
#> [1] TRUE
```
