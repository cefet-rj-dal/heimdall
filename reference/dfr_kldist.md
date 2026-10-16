# KL Distance method

This detector compares a reference and a recent window through the
symmetrised Kullback-Leibler divergence of their binned empirical
distributions, `sum((p - q) * log(p / q))`, which is the quantity also
known as the Jeffreys divergence or, in the credit-risk literature, the
Population Stability Index. It is multivariate: every monitored column
contributes to the score. Because it monitors the distribution of the
observed features rather than predictive error, it is primarily used for
**virtual concept drift**. The statistical foundation is the divergence
introduced by Kullback and Leibler (1951).

## Usage

``` r
dfr_kldist(
  target_feat = NULL,
  window_size = 100,
  threshold = 0.12,
  window_type = "sliding",
  breaks = 20,
  monitoring_step = 1,
  data = NULL,
  p_th = NULL
)
```

## Arguments

- target_feat:

  Names of the columns to be monitored. `NULL` (the default) monitors
  every numeric column.

- window_size:

  Size of the window

- threshold:

  Drift threshold applied to the divergence. The default (`0.12`) was
  calibrated by simulation for the default window: on stationary normal
  data the divergence stayed below 0.1, while a shift of two standard
  deviations exceeded 0.13 in 90% of the runs. Shifts smaller than about
  one standard deviation are not separable from noise with 50-row
  halves; use a larger window to detect them.

- window_type:

  Window policy. `"sliding"` (default) keeps at most `window_size` rows,
  so both halves move with the stream. `"anchored"` keeps every row,
  which pins the reference half to the beginning of the stream and makes
  memory grow with the stream length.

- breaks:

  Number of bins used to discretise each feature before the divergence
  is computed.

- monitoring_step:

  Number of rows between two consecutive evaluations. The default (`1`)
  evaluates at every row; larger values reduce the computational cost on
  long streams.

- data:

  Already collected data to avoid cold start.

- p_th:

  Deprecated name of `threshold`.

## Value

`dfr_kldist` object

## Details

Bins are built over the reference and recent halves together, so the two
histograms are always comparable. Empty bins are smoothed before the
logarithm is taken, and the score is averaged over bins and over
features.

The detector consumes one row of a data frame at a time (a single value
is also accepted). Rows whose monitored values are all missing are
skipped instead of being imputed.

## References

Kullback, S., and Leibler, R. A. (1951). On information and sufficiency.
*The Annals of Mathematical Statistics*, 22(1), 79-86.
<doi:10.1214/aoms/1177729694>

## Examples

``` r
# Load Heimdall and the example data stream.
library(heimdall)

# Fix the seed to keep the example reproducible.
seed <- 1
set.seed(seed)

# Load the univariate stream monitored in this walkthrough.
data(st_drift_examples)
serie <- st_drift_examples$uv_virtual_drift

# Plot the monitored numeric signal before detection.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate the KL-divergence-based detector.
model <- dfr_kldist(window_size=100, threshold=0.10)

# Update the detector sequentially and store drift alarms.
detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_len(nrow(serie))){
  output <- update_state(output$obj, serie[i, 'serie', drop=FALSE])
  if (output$drift){
    type <- 'drift'
    output$obj <- reset_state(output$obj)
  } else {
    type <- ''
  }
  detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}

# Print the points where KLDIST detected drift.
detection[detection$type == 'drift',]
#>     idx event  type
#> 145 145  TRUE drift
#> 245 245  TRUE drift
#> 345 345  TRUE drift
#> 445 445  TRUE drift
# Overlay the detected drifts on the original numeric stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```
