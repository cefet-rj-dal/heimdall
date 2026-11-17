# Online Bayesian change point detection (onlineBcp)

Online Bayesian change point detection for normal data of Yigiter et al.
(2015), as implemented in the R package onlineBcp (Xu, Yigiter and Chen,
2022). The time since the last change (run length) follows a Markov
chain with geometric hazard (`1 - theta` is the probability of a change
at each step), and the segments are normal with a normal-inverse-gamma
prior on their mean and variance. At every step the posterior of the
last change point location is updated; when its largest value exceeds
`threshold`, that location becomes the last change point and the search
restarts from it.

## Usage

``` r
online_bcp(
  x,
  theta = 0.9,
  alpha = 1,
  beta = 1,
  threshold = 0.5,
  numerics = c("stable", "reference"),
  normality = FALSE
)
```

## Arguments

- x:

  A numeric vector without missing values.

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

- normality:

  `TRUE` adds the Kolmogorov-Smirnov normality test of each segment.

## Value

An `online_bcp` object: `changepoints` (location, the last observation
before the change, and its posterior probability), `segments` (begin,
end, mean, SD, interval limits and, optionally, the normality p-value),
and `trace`, one row per step with the largest posterior probability
(`max_prob`), the location (`location`, 0 when below the threshold) and
the run of equal locations (`run`), as `max_p` of onlineBcp.

## Details

The update follows `online_cp()` of onlineBcp 0.1.4
(<https://github.com/ElsevierSoftwareX/SOFTX-D-21-00221>). At time `t`,
for every candidate location `j` from the last change point `tau` to
`t`,

- `j < t`:
  `w = Gamma(u + 1/2) / Gamma(u) * sqrt((t - j + 1) / (t - j + 2)) * (A + 2 beta)^u / (B + 2 beta)^(u + 1/2) / sqrt(pi)`,
  `u = (t - j + 2 alpha) / 2`, with `A` and `B` the centred sums of
  squares of `x[(j + 1):t]` and `x[(j + 1):(t + 1)]` (with denominators
  `t - j + 1` and `t - j + 2`), and `P(R = j) = w theta P_prev(R = j)`;

- `j = t`:
  `w = 2^alpha Gamma(alpha + 1/2) / (sqrt(2 pi) (x[t + 1]^2 / 2 + beta)^(alpha + 1/2))`,
  and `P(R = t) = w (1 - theta) sum(P_prev)`.

The code differs from Eq. 3 of the paper in the weight of a new segment,
`(x^2 / 2 + beta)` where the paper writes `(x^2 / 2 + 2 beta)`; the code
is followed. With `numerics = "reference"`,
[`gamma()`](https://rdrr.io/r/base/Special.html) overflows when
`(t - j + 2 alpha + 1) / 2` exceeds about 171.6, and the weight, then
`NaN` or `Inf`, is set to 0 as in the code. The change points of the
paper's aCGH example at 341, 682 and 1023 come from this overflow;
`"stable"` avoids it.

The segments are built as `summary.BayesCP()` does. A change point is a
location whose posterior exceeds `threshold` at a step where the
location differs from that of the previous step. Segments of one
observation are merged with the previous segment of more than one, or
with the next one at the start. The interval of each segment mean is
`mean +- qnorm(0.95) sd / sqrt(n)` (a 90% interval; the paper calls it
95%). With `normality = TRUE`, the p-value of the Kolmogorov-Smirnov
test of each segment against the normal distribution with its mean and
standard deviation is added.

The cost of a step grows with the time since the last change point, so a
long sequence without changes takes quadratic time.

## References

Yigiter, A., Chen, J., An, L. and Danacioglu, N. (2015). An on-line CNV
detection method for short sequencing reads. *Journal of Applied
Statistics*, 42(7), 1556-1571. <doi:10.1080/02664763.2014.1001330>

Xu, H., Yigiter, A. and Chen, J. (2022). onlineBcp: An R package for
online change point detection using a Bayesian approach. *SoftwareX*,
17, 100999. <doi:10.1016/j.softx.2022.100999>

## See also

[`dfr_onlinebcp()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_onlinebcp.md)
to monitor a stream.

## Examples

``` r
set.seed(1)
x <- c(stats::rnorm(100, 0), stats::rnorm(100, 3), stats::rnorm(100, 0))
result <- online_bcp(x, theta = 0.95, threshold = 0.7)
result$changepoints
#>   location post.prob
#> 1      100 0.7204097
result$segments
#>   begin end      mean        SD    LL of CI UL of CI
#> 1     1 100 0.1088874 0.8981994 -0.03885328 0.256628
#> 2   101 300 1.4959327 1.7746395  1.28952669 1.702339
```
