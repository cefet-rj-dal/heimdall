# Adapted Drift Detection Method (DDM) method

DDM monitors the online error rate of a predictive model under the
PAC-learning assumption that, in a stationary environment, the error
should decrease or remain stable as more samples are observed. Because
it operates on the classifier error stream, it is primarily a detector
of **real concept drift**. The method follows Gama et al. (2004)
<doi:10.1007/978-3-540-28645-5_29>.

## Usage

``` r
dfr_ddm(min_instances = 30, warning_level = 2, out_control_level = 3)
```

## Arguments

- min_instances:

  The minimum number of instances before detecting change

- warning_level:

  Necessary level for warning zone (2 standard deviation)

- out_control_level:

  Necessary level for a positive drift detection

## Value

`dfr_ddm` object

## Details

The detector only reacts to an increase of the error rate (a degradation
of the model); improvements are not reported. Values must be `1` (error)
or `0` (correct prediction), and missing values are treated as correct
predictions (`0`).

## References

Gama, J., Medas, P., Castillo, G., and Rodrigues, P. P. (2004). Learning
with drift detection. In *Advances in Artificial Intelligence - SBIA
2004*, 286-295. <doi:10.1007/978-3-540-28645-5_29>

## Examples

``` r
# Load Heimdall and the built-in synthetic data stream.
library(heimdall)

# Fix the seed to make the example reproducible.
seed <- 1
set.seed(seed)

# Load the univariate stream and derive the binary monitored signal.
data(st_drift_examples)
data <- st_drift_examples$uv_virtual_drift
data$prediction <- st_drift_examples$uv_virtual_drift$serie > 4

# Plot the binary stream monitored by DDM.
plot(x=seq_len(nrow(data)), y=data$prediction)


# Instantiate the DDM detector.
model <- dfr_ddm()

# Process the stream sequentially and store the drift alarms.
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

# Print the positions where DDM detected drift.
detection[detection$type == 'drift',]
#>     idx event  type
#> 201 201  TRUE drift

# Show the detected drifts over the original numeric signal.
plot(x=seq_len(nrow(data)), y=data$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```
