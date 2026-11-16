# Domino drift effect (DDE) detector

Feature drift detector that monitors only a subset of the features and
estimates the probability that each of the other features drifts, from
the correlation between the features (Szucs and Nemeth, 2025). The
method relies on the *domino drift effect*: when features drift
together, the neighbours (strongly correlated features) of a drifting
feature are more likely to drift as well.

## Usage

``` r
dfr_dde(
  target_feat = NULL,
  monitored = NULL,
  n_monitored = NULL,
  test = "js",
  threshold = NULL,
  window_size = 500,
  monitoring_step = 1,
  confirmation_delay = NULL,
  thresholds = seq(0, 0.9, by = 0.1),
  score_threshold = 0.5,
  update_curve = TRUE,
  delta = 2e-05,
  seed = NULL
)
```

## Arguments

- target_feat:

  Names of the features. `NULL` (the default) uses every numeric column.

- monitored:

  Names of the monitored features, \\S\_{check}\\.

- n_monitored:

  Number of monitored features, drawn at random when `monitored` is
  `NULL`. When both are `NULL` every feature is monitored and no score
  is predicted.

- test:

  Univariate drift test applied to each monitored feature: one of `"js"`
  (Jensen-Shannon distance), `"ks"` (Kolmogorov-Smirnov),
  `"wasserstein"` (normalized Wasserstein distance) or `"adwin"`. See
  [`dde_feature_drift()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_feature_drift.md).

- threshold:

  Drift threshold of the test. `NULL` uses the default of
  [`dde_feature_drift()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_feature_drift.md).

- window_size:

  Size of the reference and of the analysis windows (\\r = q\\).

- monitoring_step:

  Number of observations between two consecutive tests of the
  window-based tests.

- confirmation_delay:

  Number of observations between the first drift signal and the decision
  on which features drift. `NULL` (the default) uses half a window. See
  Details.

- thresholds:

  Correlation thresholds \\z\\ at which the DDE curve is evaluated.

- score_threshold:

  Score from which an unmonitored feature is predicted to drift.

- update_curve:

  Whether to refine the DDE curve with the drifts detected after the
  initialization, by averaging it with the curves of the monitored
  features (Algorithm 1, line 29).

- delta:

  Confidence of ADWIN, used when `test = "adwin"`.

- seed:

  Seed of the random draw of the monitored features.

## Value

`dfr_dde` object

## Details

The detector compares, for each feature, a reference window with the
following analysis window, both of `window_size` observations (Algorithm
1). It starts in an initialization phase in which every feature is
monitored. The first drift with at least two drifting features fixes the
DDE curve, computed with
[`dde_curve()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_curve.md)
from the Pearson correlations in the reference window. From then on only
the monitored features are tested. When some of them drift, every
unmonitored feature gets a drift score with
[`dde_score()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_score.md),
the mean of the DDE curve over its correlations with the drifting
monitored features (Eq. 25).

A drift is reported when at least one monitored feature drifts. The
features do not cross their thresholds at the same observation, so the
first signal opens a confirmation period of `confirmation_delay`
observations, after which the windows are tested again: the drifting
features are those of this confirmation test (with ADWIN, every feature
signalled during the period), and no drift is reported if none remains.
The correlations are measured in the reference window of the first
signal. After a drift the analysis window becomes the new reference
window.

The fitted object exposes:

- `curve`: the current DDE curve (see
  [`dde_curve()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_curve.md));

- `feature_drift`: per feature, the last decision for the monitored
  features (0 or 1) and the predicted score for the others;

- `predicted_drift`: per feature, whether it is detected or predicted to
  drift in the last drift;

- `detections`: one entry per reported drift, with the observation, the
  drifting monitored features and the predicted scores.

The paper's code (<https://github.com/marcellnemeth/DominoDriftEffect>)
is not public, so the implementation follows the paper. The window tests
use the definitions and default thresholds of the Evidently library.

## References

Szucs, G. and Nemeth, M. (2025). Domino drift effect approach for
probability estimation of feature drift in high-dimensional data.
*Knowledge and Information Systems*, 67, 4597-4621.
<doi:10.1007/s10115-025-02362-0>

## Examples

``` r
library(daltoolbox)
library(heimdall)

# Six features in two correlated groups; the first group drifts.
set.seed(1)
n <- 600
make <- function(n, shift) {
  a <- stats::rnorm(n, shift)
  b <- stats::rnorm(n)
  data.frame(a1 = a + stats::rnorm(n, sd = 0.3), a2 = a + stats::rnorm(n, sd = 0.3),
             a3 = a + stats::rnorm(n, sd = 0.3), b1 = b + stats::rnorm(n, sd = 0.3),
             b2 = b + stats::rnorm(n, sd = 0.3), b3 = b + stats::rnorm(n, sd = 0.3))
}
stream <- rbind(make(n, 0), make(n, 3), make(n, 3), make(n, 6))

model <- dfr_dde(monitored = c("a1", "b1"), test = "ks", threshold = 1e-3,
                 window_size = 300, monitoring_step = 50)
model <- fit(model, stream)
model$curve
#>    threshold pairs drifting_pairs probability lift
#> 1        0.0    15              3         0.4  1.0
#> 2        0.1     6              3         1.0  2.5
#> 3        0.2     6              3         1.0  2.5
#> 4        0.3     6              3         1.0  2.5
#> 5        0.4     6              3         1.0  2.5
#> 6        0.5     6              3         1.0  2.5
#> 7        0.6     6              3         1.0  2.5
#> 8        0.7     6              3         1.0  2.5
#> 9        0.8     6              3         1.0  2.5
#> 10       0.9     6              3         1.0  2.5
model$feature_drift
#>  a1  a2  a3  b1  b2  b3 
#> 1.0 1.0 1.0 0.0 0.4 0.4 
```
