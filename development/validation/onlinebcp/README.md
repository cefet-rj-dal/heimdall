# Validation: onlineBcp online Bayesian change point detection

Checks `online_bcp()` and `dfr_onlinebcp()` against Xu, Yigiter and Chen
(2022), *onlineBcp: An R package for online change point detection using a
Bayesian approach* (SoftwareX 17, 100999), and its code
(<https://github.com/ElsevierSoftwareX/SOFTX-D-21-00221>, onlineBcp 0.1.4).
The method is that of Yigiter et al. (2015). It assumes a normal model with
a normal-inverse-gamma prior and a geometric prior (`theta`) on the segment
lengths, and updates the posterior of the location of the last change point
at each observation.

```sh
# from the package root
git clone https://github.com/ElsevierSoftwareX/SOFTX-D-21-00221 data-real/onlinebcp/reference
# the package sources are in data-real/onlinebcp/reference/onlineBcp
# inside the devcontainer, about 4 minutes (almost all of it the reference on cnv_H2347)
Rscript development/validation/onlinebcp/run_onlinebcp.R > development/validation/results/onlinebcp/run.log
```

`data-real/` is ignored by git. Results go to
`development/validation/results/onlinebcp/`.

The script runs the reference `R/online_cp.R` unchanged (via `sys.source`).
It uses the three data sets of the paper, shipped with the package, with the
calls of Section 4:

| data | n | transform | theta | alpha | beta | threshold |
|---|---|---|---|---|---|---|
| aCGH (`transNorm`) | 2046 | none | 0.95 | 1 | 1 | 0.7 |
| cnv_H2347 (column 2) | 17595 | / 1000 | 0.9 | 1 | 1 | 0.7 |
| covid (`New_cases`) | 219 | sqrt(y + 3/8) | 0.7 | 1.1 | 1.2 | 0.3 |

## 1. Equivalence with the reference (`equivalence.csv`)

This compares `online_bcp(numerics = "reference")` with `online_cp()`
followed by `summary.BayesCP(norm.test = TRUE)`.

| data | max_p trace | locations and runs | change points | posterior probabilities | segments table | time (ours / reference) |
|---|---|---|---|---|---|---|
| aCGH | 6.3e-14 | identical | 7 = 7, same | 4.6e-15 | identical | 0.36 s / 23.2 s |
| cnv_H2347 | 3.4e-13 | identical | 51 = 51, same | 4.2e-14 | identical | 6.2 s / 195.8 s |
| covid | 8.4e-14 | identical | 17 = 17, same | 6.1e-16 | identical | 0.02 s / 0.06 s |

The segments table holds the begin, end, mean, SD, CI and KS normality
p-value of each segment. `online_bcp()` is 30-60 times faster because it uses
prefix sums instead of recomputing the segment sums at every step.

## 2. Outputs printed in the paper (`paper_outputs.csv`)

| check | online_bcp() | paper |
|---|---|---|
| aCGH change points | 341 682 1023 1463 1513 1517 1519 | the same |
| aCGH posterior probabilities | 0.9178 0.8638 0.8093 0.7002 0.7049 0.7600 0.7394 | the same |
| aCGH segments (begin, end, mean, SD, CI) | largest difference 4.7e-6 | printed with 4-6 digits |
| aCGH normality p-values | largest difference 3.4e-6 | printed with 5 digits |
| cnv_H2347 change points | 51 | 51 |
| COVID-19 change points | 17 (18 segments) | 17 (18 segments) |
| COVID-19 segments (begin, end, mean, SD, normality) | largest difference 5.0e-5 | printed with 4-5 digits |

All the differences are within the rounding of the printed values.

## 3. Numerical overflow in the reference (`acgh_stable_changepoints.csv`)

The reference computes the predictive weights with `gamma()` and with powers
of the sums of squares. `gamma(us)` overflows once `us > 171`, that is, for
runs longer than about 340 observations. The weights then become `NaN` or
`Inf` and are set to 0. All the long-run candidates disappear together, and
when the variance is small the posterior jumps to a change point about 341
observations after the previous one.

`numerics = "stable"` (the default) computes the same weights on the log
scale with `lgamma()`. On short segments it agrees with `"reference"` to
1e-8 (unit test).

| data | reference | stable | only in reference | only in stable |
|---|---|---|---|---|
| aCGH | 7 | 3 | 341 682 1023 1517 1519 | 1992 |
| cnv_H2347 | 51 | 0 | all 51 | — |
| covid | 17 | 17 | — | — |

* **aCGH:** 341, 682 and 1023 are the overflow. 1517 and 1519 disappear
  as well, because the posterior after 1513 is different without the
  earlier spurious resets.
  * The stable change points are 1463, 1513 and 1992 (posterior 0.71, 0.76
    and 0.71).
  * The segments are 1-1463 (mean -0.016), 1464-1513 (the amplification,
    mean 0.437), 1514-1992 and 1993-2046.
* **cnv_H2347:** all 51 change points printed in the paper are produced by
  the overflow. They are spaced 341 apart, or multiples of it, apart from a
  few short gaps of 2-5 observations right after a reset. With the stable
  numerics the posterior of "no change since the start" stays around 0.99
  over the whole series (sd 0.0175), and no change point is found even at
  threshold 0.5.
* **covid:** no segment is long enough to overflow, so both modes give the
  same 17 change points.

## 4. Stream detector (`stream.csv`)

`dfr_onlinebcp()` runs the same recursion one observation at a time. It
reports a drift whenever the location of the last change point moves to a
new value above the threshold.

The locations it reports are exactly the distinct locations in the trace of
`online_bcp()`. The `max_prob` output is the same trace (unit test).

| data | drifts | mean delay | change points in summary |
|---|---|---|---|
| aCGH (reference numerics) | 7 | 259 | 7 |
| covid | 39 | 1.6 | 17 |

* **Covid: drifts vs change points.** The stream detector reports 39 drifts
  and the summary 17 change points. `summary.BayesCP()` builds the final
  segmentation after the whole series has been seen, and drops locations
  that later observations revise. The stream detector cannot wait for that,
  so it reports every location at the time it is found.
* **aCGH: the long delay.** The mean delay of 259 comes from the overflow
  change points, which are only confirmed hundreds of observations later.

`max_run` bounds the number of candidate locations kept, and so the cost of
a step. In the unit test it gives the same change points.

## Deviations from the reference

* **Default numerics:** `numerics = "stable"` is the default. The reference
  behaviour, including the overflow, is available with
  `numerics = "reference"`.
* **Paper and code differ:** the formula follows the code, which uses
  `(x^2 / 2 + beta)`; the paper writes `2 * beta`.
* **The "95%" confidence interval:** `summary.BayesCP()` labels it 95% but
  computes it with `qnorm(0.95)`, which makes it a 90% interval. It is kept
  as is, so that the segments match the reference.
