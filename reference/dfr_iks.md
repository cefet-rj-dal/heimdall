# Incremental Kolmogorov-Smirnov drift detector

Unsupervised drift detection of dos Reis et al. (2016): for every
monitored feature, a two-sample Kolmogorov-Smirnov test between a
reference sample and the sliding window of the last `window_size`
observations, computed incrementally with
[`iks()`](https://cefet-rj-dal.github.io/heimdall/reference/iks.md). A
drift is signalled when the test rejects the null hypothesis for at
least one feature. The detector monitors **virtual concept drift** (the
feature distribution), without labels.

## Usage

``` r
dfr_iks(
  target_feat = NULL,
  window_size = 100,
  alpha = 0.001,
  ca = iks_ca(alpha),
  reference = c("fixed", "consecutive"),
  tie_break = TRUE,
  seed = NULL
)
```

## Arguments

- target_feat:

  Features to be monitored (by default every numeric column).

- window_size:

  Size `W` of the reference sample and of the sliding window.

- alpha:

  Significance level of the test.

- ca:

  Coefficient of the critical value `ca sqrt(2 / W)`; by default
  `iks_ca(alpha)`, `1.9495` for `alpha = 0.001` (the paper and the
  reference code use the table value `1.95`).

- reference:

  `"fixed"` (the default, the paper's method): the reference sample is
  the first `W` observations (the data the model was trained on) and the
  window starts as a copy of it; after a drift the reference becomes the
  current window. `"consecutive"`: the reference is the window of the
  `W` observations before the current window (the consecutive sliding
  windows of Zliobaite, 2010, the CD baseline of the paper).

- tie_break:

  Pair every observation with a random value to break ties (`TRUE`, the
  default, as `IKSSW` of the reference code); otherwise the statistic
  can be overestimated when values repeat, in particular while the
  window overlaps the reference.

- seed:

  Seed of the private generator of the sketches.

## Value

`dfr_iks` object

## Details

The output of each observation (`drifter_output` after
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html)) has the
statistic of every monitored feature (`D_<feature>`) and `drift`. After
a drift with `reference = "consecutive"`, both windows restart.

## References

dos Reis, D. M., Flach, P., Matwin, S. and Batista, G. (2016). Fast
unsupervised online drift detection using incremental Kolmogorov-Smirnov
test. In *Proceedings of the 22nd ACM SIGKDD International Conference on
Knowledge Discovery and Data Mining*, 1545-1554.
<doi:10.1145/2939672.2939836>

Zliobaite, I. (2010). Change with delayed labeling: when is it
detectable? In *2010 IEEE International Conference on Data Mining
Workshops*, 843-850. <doi:10.1109/ICDMW.2010.49>

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
data <- data.frame(a = c(rnorm(600), rnorm(600, 1)), b = rnorm(1200))
detector <- fit(dfr_iks(window_size = 100, seed = 1), data)
which(detector$drifter_output$drift)
#> [1] 662
```
