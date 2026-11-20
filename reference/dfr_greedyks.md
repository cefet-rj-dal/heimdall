# GreedyKS drift detector

Online one-sample Kolmogorov-Smirnov test of a numeric stream against a
reference distribution, with the GreedyKS sketch of Cardoso and Galeno
(2023): a drift is signalled when the p-value of the estimated statistic
falls to `alpha` or below. The detector is used for **virtual concept
drift** on a numeric feature, or on the error of a model.

## Usage

``` r
dfr_greedyks(
  cdf = "norm",
  m = 100,
  alpha = 0.01,
  grace_period = m,
  reference_size = 1000,
  exact = TRUE,
  min_instances = 0,
  target_feat = NULL
)
```

## Arguments

- cdf:

  The reference distribution: a CDF (a function of a numeric vector,
  kept for the whole stream), or one of `"norm"`, `"exp"` and `"unif"`,
  whose parameters are fitted by maximum likelihood (as `scipy.stats`
  [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html): mean and
  standard deviation with divisor `n`; minimum and mean minus minimum;
  minimum and range) to the first `reference_size` observations, and
  again after every drift.

- m:

  Number of buckets of the sketch (the memory units of the paper).

- alpha:

  Significance level: drift when the p-value is `<= alpha`.

- grace_period:

  Number of observations processed before the test is applied (the paper
  tests once more observations than memory units were seen; the default
  is `m`). It counts from the start of the sketch, so with a fitted
  distribution it includes the reference observations.

- reference_size:

  With a fitted distribution (`cdf` a name), the number of observations
  it is fitted to. They are also the first observations of the sketch,
  as in the call-center experiment of the paper.

- exact:

  Exact distribution of the statistic (`TRUE`, the default, as the
  reference code) or the limiting distribution (`FALSE`); see
  [`ks_sf()`](https://cefet-rj-dal.github.io/heimdall/reference/ks_sf.md).

- min_instances:

  Passed to
  [`greedyks()`](https://cefet-rj-dal.github.io/heimdall/reference/greedyks.md).

- target_feat:

  Feature to be monitored.

## Value

`dfr_greedyks` object

## Details

After a drift, the sketch restarts empty (and, with a fitted
distribution, the reference is fitted again to the next `reference_size`
observations).

The output of each observation (`drifter_output` after
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html)) has the
estimated statistic `D` and its p-value `p_value`. For speed, when more
than 140 observations were processed and `n D^2 < 2.2`, the p-value is
the Pelz-Good approximation instead of the exact Durbin matrix (its
p-value there is above 0.02); with `alpha >= 0.02` the exact value is
used. The decisions are those of the exact test.

## References

Cardoso, D. O. and Galeno, T. D. (2023). Online evaluation of the
Kolmogorov-Smirnov test on arbitrarily large samples. *Journal of
Computational Science*, 67, 101959. <doi:10.1016/j.jocs.2023.101959>

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
x <- c(rnorm(1000, 1, 1), rnorm(4000, 1.3, 1))
detector <- fit(dfr_greedyks(cdf = function(q) pnorm(q, 1, 1)), x)
which(detector$drifter_output$drift)
#>  [1] 1534 1636 1742 1846 2169 2291 2392 2493 2689 2790 2891 2992 3093 3194 3296
#> [16] 3397 3512 3704 3855 3962 4094 4214 4338 4439 4622 4723 4824
# reference fitted to the first 300 observations
detector <- fit(dfr_greedyks(cdf = "norm", reference_size = 300), x)
which(detector$drifter_output$drift)
#> [1] 1534 4654
```
