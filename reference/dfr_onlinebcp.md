# onlineBcp stream detector

Monitors a univariate stream with the online Bayesian change point
detection of
[`online_bcp()`](https://cefet-rj-dal.github.io/heimdall/reference/online_bcp.md):
every observation updates the posterior of the location of the last
change point, and a drift is reported when the largest posterior exceeds
`threshold` at a new location. Since it monitors the distribution of a
feature, it is used for **virtual concept drift**.

## Usage

``` r
dfr_onlinebcp(
  target_feat = NULL,
  theta = 0.9,
  alpha = 1,
  beta = 1,
  threshold = 0.5,
  numerics = c("stable", "reference"),
  max_run = NULL
)
```

## Arguments

- target_feat:

  Feature to be monitored.

- theta:

  Probability that no change occurs at a step.

- alpha, beta:

  Hyperparameters of the inverse-gamma prior of the variance.

- threshold:

  Threshold of the posterior probability (`th_cp`).

- numerics:

  `"stable"` (default) computes the weights in log scale with
  [`lgamma()`](https://rdrr.io/r/base/Special.html). `"reference"`
  computes them as onlineBcp 0.1.4 does, with
  [`gamma()`](https://rdrr.io/r/base/Special.html) and powers that
  overflow for segments longer than about 340 observations; the
  overflowing weights are set to 0, which forces change points at
  multiples of about 341 observations (see Details).

- max_run:

  If set, the posterior is kept for at most the last `max_run` candidate
  locations (the oldest are dropped), which bounds the cost of a step on
  long streams without changes. `NULL` (default) keeps them all, as the
  reference.

## Value

`dfr_onlinebcp` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds the largest posterior probability (`max_prob`) and the location
found (`location`, 0 when below the threshold) for every observation.

## Details

The detector reports a drift on the observation at which a new change
point location is found; that location (the last observation before the
change) is kept in `obj$changepoints` with its posterior probability.
The data are assumed approximately normal within segments; transform
them first otherwise (for counts, `sqrt(y + 3/8)`, as in the paper).
Missing values are skipped.

## References

Yigiter, A., Chen, J., An, L. and Danacioglu, N. (2015). An on-line CNV
detection method for short sequencing reads. *Journal of Applied
Statistics*, 42(7), 1556-1571. <doi:10.1080/02664763.2014.1001330>

Xu, H., Yigiter, A. and Chen, J. (2022). onlineBcp: An R package for
online change point detection using a Bayesian approach. *SoftwareX*,
17, 100999. <doi:10.1016/j.softx.2022.100999>

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
data <- data.frame(serie = c(stats::rnorm(100, 0), stats::rnorm(100, 3)))
model <- fit(dfr_onlinebcp(theta = 0.95, threshold = 0.7), data)
model$changepoints
#>   location post.prob detected_at
#> 1      100 0.7204097         105
```
