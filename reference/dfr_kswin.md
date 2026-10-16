# KSWIN method

KSWIN applies a Kolmogorov-Smirnov test between a recent window and a
reference sample drawn from older observations. In this package, the
method is primarily used for **virtual concept drift**, because it
monitors distributional changes in a numeric feature stream. The method
follows Raab et al. (2020) <doi:10.1016/j.neucom.2019.11.111>.

## Usage

``` r
dfr_kswin(
  target_feat = NULL,
  window_size = 300,
  stat_size = 100,
  alpha = 1e-04,
  window_type = "sliding",
  monitoring_step = 1,
  exact = NULL,
  data = NULL,
  seed = NULL
)
```

## Arguments

- target_feat:

  Feature to be monitored.

- window_size:

  Size of the sliding window. It must be greater than `stat_size`, and
  at least `2 * stat_size` is recommended so that the reference sample
  is as large as the recent one.

- stat_size:

  Size of the statistic window

- alpha:

  Probability for the test statistic of the Kolmogorov-Smirnov-Test The
  alpha parameter is very sensitive, therefore should be set below 0.01.

- window_type:

  Window policy. `"sliding"` (default) keeps at most `window_size`
  observations, so both the reference and the statistic halves move with
  the stream. `"anchored"` keeps every observation, which pins the
  reference sample to the beginning of the stream and makes memory grow
  with the stream length.

- monitoring_step:

  Number of observations between two consecutive tests. The default
  (`1`) tests at every observation; larger values reduce the
  computational cost on long streams.

- exact:

  Passed to [`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html).
  The default (`NULL`) lets R choose between the exact and the
  asymptotic test, which is much faster for windows of this size. Up to
  version 1.3.0 the default was `TRUE`.

- data:

  Already collected data to avoid cold start.

- seed:

  Seed of the random number stream used to subsample the reference
  window. When `NULL` (the default) it is drawn from the global random
  number generator once, when the detector is created.

## Value

`dfr_kswin` object

## Details

The reference window is randomly subsampled to `stat_size` observations.
The detector keeps its own random number stream, so it does not change
the global random state while it runs; set `seed` (or call
[`base::set.seed()`](https://rdrr.io/r/base/Random.html) before creating
the detector) for reproducible results.

A drift requires both a significant p-value and a KS statistic above
`sqrt(-log(alpha) / stat_size)`, as defined by Raab et al. Missing
observations are skipped instead of being imputed.

## References

Raab, C., Heusinger, M., and Schleif, F.-M. (2020). Reactive soft
prototype computing for concept drift streams. *Neurocomputing*, 416,
340-351. <doi:10.1016/j.neucom.2019.11.111>

## Examples

``` r
# Load Heimdall and the synthetic univariate stream.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the numeric stream used by the detector.
data(st_drift_examples)
serie <- st_drift_examples$uv_virtual_drift

# Plot the monitored variable before detection.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate KSWIN with explicit window parameters.
model <- dfr_kswin(target_feat='serie', window_size=100, stat_size=50)

# Update the detector sequentially and store the alarm positions.
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

# Print the points where KSWIN raised drift alarms.
detection[detection$type == 'drift',]
#>     idx event  type
#> 221 221  TRUE drift
#> 271 271  TRUE drift
#> 331 331  TRUE drift
#> 381 381  TRUE drift
#> 431 431  TRUE drift

# Overlay the detected drifts on the original stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```
