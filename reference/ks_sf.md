# Distribution of the Kolmogorov-Smirnov statistic

Upper tail probability `P(D_n >= d)` of the one-sample, two-sided
Kolmogorov-Smirnov statistic `D_n = sup |F_n(x) - F(x)|` of a sample of
size `n` from a continuous distribution.

## Usage

``` r
ks_sf(d, n, exact = TRUE)
```

## Arguments

- d:

  Value(s) of the statistic, in `[0, 1]`.

- n:

  Sample size (a positive integer, recycled with `d`).

- exact:

  `TRUE` (the default) for the exact distribution of `D_n`, as
  `scipy.stats.kstwo`; `FALSE` for the limiting Kolmogorov distribution
  of `sqrt(n) D_n`, as `scipy.stats.kstwobign`.

## Value

The probabilities `P(D_n >= d)`.

## Details

The exact distribution is computed as Simard and L'Ecuyer (2011), with
the same choices of method as `scipy.stats.kstwo`: Ruben-Gambino
formulas at the ends of the support; for `n <= 140` the Durbin matrix
(`n d^2 <= 0.754693`), the Pomeranz recursion (`n d^2 <= 4`) or twice
the one-sided Smirnov tail; for larger `n` twice the one-sided tail
(`n d^2 >= 2.2`), the Durbin matrix (`n d^1.5 <= 1.4`, `n <= 100000`) or
the Pelz-Good approximation. The one-sided tail is the Birnbaum-Tingey
sum.

## References

Simard, R. and L'Ecuyer, P. (2011). Computing the two-sided
Kolmogorov-Smirnov distribution. *Journal of Statistical Software*,
39(11), 1-18. <doi:10.18637/jss.v039.i11>

Marsaglia, G., Tsang, W. W. and Wang, J. (2003). Evaluating Kolmogorov's
distribution. *Journal of Statistical Software*, 8(18), 1-4.
<doi:10.18637/jss.v008.i18>

## Examples

``` r
ks_sf(0.1, 100)
#> [1] 0.2526928
ks_sf(0.1, 100, exact = FALSE)
#> [1] 0.2699997
# same as the p-value of ks.test()
x <- c(0.61, 0.29, 0.06, 0.59, -1.73, -0.74, 0.51, -0.56, 0.39, 1.64)
ks.test(x, "pnorm")$p.value
#> [1] 0.6213478
ks_sf(unname(ks.test(x, "pnorm")$statistic), 10)
#> [1] 0.6213478
```
