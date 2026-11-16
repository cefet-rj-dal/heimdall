# PSI stream detector

Monitors a stream with the Population Stability Index
([`psi()`](https://cefet-rj-dal.github.io/heimdall/reference/psi.md)):
the first `n_reference` observations form the baseline, and every later
window of `window_size` observations is compared with it, feature by
feature. Since it monitors the distribution of the observed features, it
is primarily used for **virtual concept drift**, and fits the paper's
setting of validating the fields of incoming data files against a
baseline file.

## Usage

``` r
dfr_psi(
  target_feat = NULL,
  n_reference = 1000,
  window_size = 1000,
  bins = 10,
  threshold = 0.25
)
```

## Arguments

- target_feat:

  Names of the columns to be monitored. `NULL` (the default) monitors
  every numeric column.

- n_reference:

  Size of the baseline.

- window_size:

  Size of the compared windows (non-overlapping).

- bins:

  Number of quantile groups of the baseline (10 for deciles, 20 for
  demi-deciles).

- threshold:

  A drift is reported when the PSI of some feature exceeds it. The
  default, 0.25, is the paper's limit of a significant drift; 0.1 also
  flags the moderate ones. See
  [`psi()`](https://cefet-rj-dal.github.io/heimdall/reference/psi.md)
  for the PSI of samples without drift, which grows as the windows get
  smaller.

## Value

`dfr_psi` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds the PSI of every feature (`psi_<feature>`) on the last observation
of every window, and `obj$reference` the baseline.

## Details

A drift is reported on the last observation of every window whose
largest PSI exceeds `threshold`.
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
forgets the baseline, and the next `n_reference` observations form a new
one. Missing values are kept: in the windows they count as observations
in no bin (see
[`psi()`](https://cefet-rj-dal.github.io/heimdall/reference/psi.md)).

## References

Kurian, J. F. and Allali, M. (2024). Detecting drifts in data streams
using Kullback-Leibler (KL) divergence measure for data engineering
applications. *Journal of Data, Information and Management*, 6, 207-216.
<doi:10.1007/s42488-024-00119-y>

## Examples

``` r
library(daltoolbox)
library(heimdall)
# Scores around 610; from row 3001 half of those above 643 drop by 200.
set.seed(1)
score <- stats::rnorm(5000, 610, 50)
late <- 3001:5000
drop <- late[score[late] > 643 & stats::runif(2000) < 0.5]
score[drop] <- score[drop] - 200
# The PSI reaches about 0.2: a moderate drift, flagged with threshold 0.1.
detector <- dfr_psi(n_reference = 2000, window_size = 500, threshold = 0.1)
model <- fit(detector, data.frame(cbm = score))
model$drifter_output[!is.na(model$drifter_output$psi_cbm), , drop = FALSE]
#>         psi_cbm
#> 2500 0.01488661
#> 3000 0.01372769
#> 3500 0.18216773
#> 4000 0.20573507
#> 4500 0.21753113
#> 5000 0.21881182
model$drifted
#> [1] TRUE
```
