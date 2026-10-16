
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src='https://raw.githubusercontent.com/cefet-rj-dal/heimdall/main/inst/logo.png' alt='Logo do pacote heimdall' align='centre' height='150' width='129'/> Heimdall

<!-- badges: start -->

[![R-CMD-check](https://github.com/cefet-rj-dal/heimdall/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cefet-rj-dal/heimdall/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/cefet-rj-dal/heimdall/graph/badge.svg)](https://app.codecov.io/gh/cefet-rj-dal/heimdall)
[![CRAN
status](https://www.r-pkg.org/badges/version/heimdall)](https://CRAN.R-project.org/package=heimdall)
![CRAN Downloads](https://cranlogs.r-pkg.org/badges/heimdall) ![GitHub
Stars](https://img.shields.io/github/stars/cefet-rj-dal/heimdall?logo=Github)
<!-- badges: end -->

Heimdall is a tool designed for concept drift detection in continuous
data streams. When analyzing time series data, it is possible to observe
significant changes in the data distribution or in the accuracy of
predictive models during the forecasting process. These changes are
known as concept drift.

The goal of the Heimdall package is to detect when concept drift occurs,
enabling the detection and adaptation of models in non-stationary
contexts. The package offers several state-of-the-art methods to
identify and handle concept drift, as well as techniques for efficient
model adaptation.

Heimdall is particularly useful when you need to move quickly from the
theory of concept drift to a working streaming implementation. The
package includes detectors for both **real concept drift** and **virtual
concept drift**, and the repository examples are designed to help users
understand that distinction through small, reproducible workflows.

------------------------------------------------------------------------

## Getting started

The package ships with three vignettes, also available on the [package
website](https://cefet-rj-dal.github.io/heimdall/):

- `vignette("heimdall")` introduces the streaming workflow shared by
  every detector and shows how to monitor real and virtual concept
  drift;
- `vignette("adaptive-models")` shows how `stealthy()` retrains a model
  automatically when a drift is detected;
- `vignette("configuring-detectors")` shows how to read
  `drifter_output`, calibrate a threshold for your data, and choose
  between sliding and anchored windows.

------------------------------------------------------------------------

## Drifter outputs

Besides the drift decision, every drifter exposes the statistic it
monitors. After `fit()`, `obj$drifter_output` is a data frame with one
row per observation, and `obj$last_drifter_output` holds the value for
the last observation processed by `update_state()`. Rows are `NA` when
the detector reported nothing for that observation, for example while
its window is still filling or when it skipped a missing value.

`stealthy()` exposes the output of its drifter in `obj$drifter_output`
as well, with one row per row of `x` across every call to `fit()`. Rows
are `NA` while the drifter is not monitoring (during the warmup, or
after a drift until the model is retrained).

| Drifter | Type | `drifter_output` columns |
|----|----|----|
| `dfr_ddm()` | error-based | `miss_prob`: running error rate |
| `dfr_eddm()` | error-based | `mean_distance`: mean distance between two consecutive errors |
| `dfr_hddm()` | error-based | `estimation`: running error rate |
| `dfr_ecdd()` | error-based | `Z`: exponentially weighted moving average of the errors |
| `dfr_cusum()` | error-based | `g`: cumulative sum compared with `lambda` |
| `dfr_adwin()` | univariate | `width`: current length of the adaptive window |
| `dfr_page_hinkley()` | univariate | `ph_statistic`: Page-Hinkley statistic compared with `threshold` (the larger of the two directions when `direction = "both"`) |
| `dfr_kswin()` | univariate | `D`: Kolmogorov-Smirnov statistic; `p`: its p-value |
| `dfr_lbdd()` | univariate | `levene_p`: p-value of Levene’s test |
| `dfr_mcdd()` | univariate | `old_shapiro_p`, `new_shapiro_p`: normality p-values of the reference and recent halves; `comparison_p`: p-value of the t-test or Wilcoxon test |
| `dfr_kldist()` | multivariate | `divergence`: divergence between the reference and recent halves, compared with `threshold` |
| `dfr_aedd()` | multivariate | one column per feature: reconstruction error of the most recent row (encoder-decoder), or its encoding (encoder only) |
| `dfr_multi_criteria()` | combined | one logical column per drifter in `drifter_list`, with its decision on that observation |
| `dfr_inactive()`, `dfr_passive()` | baseline | no columns: they monitor nothing |

------------------------------------------------------------------------

## Choosing what a detector monitors

Every distribution-based detector selects its input through
`target_feat`:

- univariate detectors (`dfr_adwin()`, `dfr_page_hinkley()`,
  `dfr_kswin()`, `dfr_lbdd()`, `dfr_mcdd()`) take a single column name;
- multivariate detectors (`dfr_kldist()`, `dfr_aedd()`) take one or more
  column names, and monitor every numeric column when `target_feat` is
  `NULL`.

Inside `stealthy()` and `dfr_multi_criteria()`, a univariate detector
without a `target_feat` receives the mean of each row. In `stealthy()`,
column names refer to the one-hot encoded, normalized columns, so
numeric columns keep their names.

------------------------------------------------------------------------

## What each detector cannot see

Each detector is blind to some kinds of change. Combine detectors with
`dfr_multi_criteria()` when several kinds of change matter.

| Drifter | Reacts to | Does not see |
|----|----|----|
| `dfr_ddm()`, `dfr_hddm()` | increases of the error rate | decreases of the error rate, and any change that leaves the error rate unchanged |
| `dfr_eddm()` | errors becoming closer together | changes that do not affect the distance between errors |
| `dfr_ecdd()`, `dfr_cusum()` | increases of the error rate | decreases of the error rate |
| `dfr_page_hinkley()` | increases of the mean (decreases or both with `direction`) | changes of spread or shape with the same mean |
| `dfr_adwin()` | changes of the mean | changes of spread or shape with the same mean |
| `dfr_mcdd()` | changes of location (mean or median) | changes of spread with the same centre |
| `dfr_lbdd()` | changes of spread | shifts of the mean with the same spread |
| `dfr_kswin()` | any change of the distribution of one feature | changes in the relation between features |
| `dfr_kldist()` | changes of the marginal distribution of each feature; with the default 100-row window, shifts of about two standard deviations | small shifts (below about one standard deviation with the default window) and changes in the relation between features |
| `dfr_aedd()` | changes of the joint distribution, through the reconstruction error | changes that the autoencoder reconstructs equally well |

Distribution-based detectors only see **virtual concept drift** (changes
in the inputs); error-based detectors only see **real concept drift**
that degrades the predictions.

------------------------------------------------------------------------

## Long streams

- The window-based detectors (`dfr_kswin()`, `dfr_lbdd()`, `dfr_mcdd()`,
  `dfr_kldist()`, `dfr_aedd()`) run a statistical test or a comparison
  at every evaluation. `monitoring_step` is the main speed control: with
  `monitoring_step = 10`, the test runs once every ten observations.
- `stealthy(history_size = )` bounds the rows kept in `drifter_output`
  and `drifter_input`.
- `nrm_memory(max_size = )` bounds the history used to estimate the
  normalization.

------------------------------------------------------------------------

## Examples

Heimdall usage examples are organized to help users learn the package
progressively, not only to list detector names. Every drifter has two
examples:

- a **detection** example, where the detector monitors a stream on its
  own through the `update_state()` loop;
- an **online prediction** example, where the detector is plugged into
  `stealthy()` to retrain a classifier whenever a drift is reported.

Each example is available as a rendered walkthrough, an extracted R
script and a Word document.

Recommended starting points:

- `dfr_adwin` for a simple univariate distribution-monitoring example;
- `dfr_ddm` for an error-based supervised example;
- `dfr_aedd` for a multivariate unsupervised example;
- `dfr_inactive` and `dfr_passive` as baselines to compare any detector
  against.

Browse the materials here:

- [Examples
  directory](https://github.com/cefet-rj-dal/heimdall/tree/main/examples)

------------------------------------------------------------------------

## Installation

The latest version of Heimdall is available on CRAN:

``` r
install.packages("heimdall")
```

You can install the development version directly from GitHub:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/heimdall", force = TRUE, upgrade = "never")
```

------------------------------------------------------------------------

## Bugs and Feature Requests

If you encounter any issues or would like to suggest new features,
please open an issue:

- [GitHub Issues](https://github.com/cefet-rj-dal/heimdall/issues)

------------------------------------------------------------------------

## Contributing

See
[CONTRIBUTING.md](https://github.com/cefet-rj-dal/heimdall/blob/main/CONTRIBUTING.md)
for the development container, the Git hook, the code style and how the
examples are generated.
