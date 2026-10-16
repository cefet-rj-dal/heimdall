# Levene Based Drift Detection Method method

LBDD is a window-based detector that compares the variability of a
reference and a recent sample using Levene's test. Because it monitors
changes in the distribution of an observed feature rather than model
performance, it is primarily aimed at **virtual concept drift**. In this
package, the detector follows the statistical-testing approach discussed
by Giusti et al. (2021) for drift analysis, using Levene's variance test
as its core mechanism.

## Usage

``` r
dfr_lbdd(
  target_feat = NULL,
  alpha = 0.001,
  window_size = 300,
  window_type = "sliding",
  monitoring_step = 1,
  data = NULL
)
```

## Arguments

- target_feat:

  Feature to be monitored

- alpha:

  Probability threshold for the test statistic

- window_size:

  Size of the window

- window_type:

  Window policy. `"sliding"` (default) keeps at most `window_size`
  observations, so both halves move with the stream. `"anchored"` keeps
  every observation, which pins the reference half to the beginning of
  the stream and makes memory grow with the stream length.

- monitoring_step:

  Number of observations between two consecutive tests. The default
  (`1`) tests at every observation; larger values reduce the
  computational cost on long streams.

- data:

  Already collected data to avoid cold start.

## Value

`dfr_lbdd` object

## Details

LBDD only reacts to changes of spread (variance); a shift of the mean
with the same spread is not detected, so combine it with
[`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
when both kinds of change matter. The test runs at every observation, so
`alpha` should be small to keep false alarms rare.

Levene's test is computed with the group spread centred on the median
(the Brown-Forsythe variant).

Missing observations are skipped instead of being imputed. When a drift
is reported the window is trimmed to its most recent half;
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
empties it.

## References

Giusti, L., Carvalho, L., Gomes, A. T., Coutinho, R., Soares, J., and
Ogasawara, E. (2021). Analysing flight delay under concept drift.
*Evolving Systems*. <doi:10.1007/s12530-021-09415-z>

## Examples

``` r
# Load Heimdall and the synthetic univariate stream.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the numeric stream monitored by the detector.
data(st_drift_examples)
serie <- st_drift_examples$uv_virtual_drift

# Plot the series before starting drift detection.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate the Levene-based detector.
model <- dfr_lbdd(target_feat='serie', window_size=100)

# Update the detector over the stream and record each drift alarm.
detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_len(nrow(serie))){
  output <- update_state(output$obj, serie$serie[i])
  if (output$drift){
    type <- 'drift'
    output$obj <- reset_state(output$obj)
  } else {
    type <- ''
  }
  detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}

# Print the positions where LBDD detected drift.
detection[detection$type == 'drift',]
#>     idx event  type
#> 126 126  TRUE drift
#> 226 226  TRUE drift
#> 328 328  TRUE drift
#> 428 428  TRUE drift

# Overlay those drift points on the original numeric stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```
