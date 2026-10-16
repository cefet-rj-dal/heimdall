# Adapted EWMA for Concept Drift Detection (ECDD) method

ECDD applies an exponentially weighted moving average (EWMA) control
chart to the online classification error stream. Since it monitors
predictive errors directly, it is primarily designed to detect **real
concept drift**. The method follows Ross et al. (2012), who adapted EWMA
charts for concept-drift detection in streaming classifiers
<doi:10.1016/j.patrec.2011.08.019>.

## Usage

``` r
dfr_ecdd(lambda = 0.2, min_run_instances = 30, average_run_length = 400)
```

## Arguments

- lambda:

  EWMA smoothing parameter

- min_run_instances:

  The minimum number of instances before detecting change

- average_run_length:

  Desired Average Run Length (ARL), i.e. the expected number of
  observations between two false alarms. The published control-limit
  polynomials exist for 100, 400 and 1000; other values use the next one
  up, and values above 1000 are rejected. The default changed from 100
  to 400 in the development version, which reduced false alarms about
  sevenfold.

## Value

`dfr_ecdd` object

## Details

Missing values are treated as correct predictions (`0`).

## References

Ross, G. J., Adams, N. M., Tasoulis, D. K., and Hand, D. J. (2012).
Exponentially weighted moving average charts for detecting concept
drift. *Pattern Recognition Letters*, 33(2), 191-198.
<doi:10.1016/j.patrec.2011.08.019>

## Examples

``` r
# Load Heimdall and the synthetic example stream.
library(heimdall)

# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)

# Load the stream and derive the binary monitored signal.
data(st_drift_examples)
data <- st_drift_examples$uv_virtual_drift
data$prediction <- st_drift_examples$uv_virtual_drift$serie > 4

# Plot the binary stream that ECDD will monitor.
plot(x=seq_len(nrow(data)), y=data$prediction)


# Instantiate the ECDD detector.
model <- dfr_ecdd()

# Update the detector sequentially and store the detected alarms.
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

# Print the drift points signaled by ECDD.
detection[detection$type == 'drift',]
#>     idx event  type
#> 201 201  TRUE drift

# Overlay the alarms on the original numeric signal.
plot(x=seq_len(nrow(data)), y=data$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```
