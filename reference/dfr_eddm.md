# Adapted Early Drift Detection Method (EDDM) method

EDDM extends DDM by monitoring the distance between classification
errors instead of only the error rate, which makes it more sensitive to
gradual degradation. Because it operates on the model error stream, it
is primarily intended for **real concept drift**. The method follows
Baena-Garcia et al. (2006), who proposed EDDM for improved detection of
gradual drift.

## Usage

``` r
dfr_eddm(
  min_instances = 30,
  min_num_errors = 30,
  warning_level = 0.95,
  out_control_level = 0.9
)
```

## Arguments

- min_instances:

  The minimum number of instances before detecting change

- min_num_errors:

  The minimum number of errors before detecting change

- warning_level:

  Necessary level for warning zone

- out_control_level:

  Necessary level for a positive drift detection

## Value

`dfr_eddm` object

## Details

EDDM monitors the distance between consecutive errors, which makes it
sensitive to gradual drifts but also prone to false alarms when the
error rate is high. `warning_level` must not be lower than
`out_control_level`. Values must be `1` (error) or `0` (correct
prediction), and missing values are treated as correct predictions
(`0`).

## References

Baena-Garcia, M., del Campo-Avila, J., Fidalgo, R., Bifet, A., Gavalda,
R., and Morales-Bueno, R. (2006). Early drift detection method. In
*Fourth International Workshop on Knowledge Discovery from Data
Streams*.

## Examples

``` r
# Load Heimdall and the built-in synthetic stream.
library(heimdall)

# Fix the seed so the results are reproducible.
seed <- 1
set.seed(seed)

# Load the stream and derive the binary monitored signal.
data(st_drift_examples)
data <- st_drift_examples$uv_virtual_drift
data$prediction <- st_drift_examples$uv_virtual_drift$serie > 4

# Visualize the binary stream that EDDM will monitor.
plot(x=seq_len(nrow(data)), y=data$prediction)


# Instantiate the EDDM detector.
model <- dfr_eddm()

# Process the stream sequentially and store each detected alarm.
detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_len(nrow(data))){
  output <- update_state(output$obj, data$prediction[i])
  if (output$drift){
    type <- 'drift'
    output$obj <- reset_state(output$obj)
  } else {
    type <- ''
  }
  detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}

# Print the drift alarms returned by EDDM.
detection[detection$type == 'drift',]
#>     idx event  type
#> 231 231  TRUE drift

# Show those alarms on top of the original numeric series.
plot(x=seq_len(nrow(data)), y=data$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```
