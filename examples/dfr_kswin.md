# KSWIN Example

KSWIN applies a Kolmogorov-Smirnov test between a recent window and a reference sample from older observations. It is a practical detector when you want to identify changes in the distribution of a numeric stream.

In this example, KSWIN is used for **virtual concept drift** detection.

Reference: Raab, C., Heusinger, M., and Schleif, F.-M. (2020). *Reactive soft prototype computing for concept drift streams*. Neurocomputing, 416, 340-351. <doi:10.1016/j.neucom.2019.11.111>

## Learning goal

This example demonstrates how to use a classical window-based statistical test inside the Heimdall update loop.


``` r
# Load Heimdall and the synthetic univariate stream.
library(heimdall)
```


``` r
# Fix the seed for reproducibility.
seed <- 1
set.seed(seed)
```


``` r
# Load the numeric stream used by the detector.
data(st_drift_examples)
serie <- st_drift_examples$univariate
```


``` r
# Plot the monitored variable before detection.
plot(x=seq_len(nrow(serie)), y=serie$serie)
```

![plot of chunk unnamed-chunk-4](fig/dfr_kswin/unnamed-chunk-4-1.png)


``` r
# Instantiate KSWIN with explicit window parameters.
model <- dfr_kswin(target_feat='serie', window_size=100, stat_size=50)
```


``` r
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
```


``` r
# Print the points where KSWIN raised drift alarms.
detection[detection$type == 'drift',]
```

```
##     idx event  type
## 228 228  TRUE drift
## 338 338  TRUE drift
## 389 389  TRUE drift
## 440 440  TRUE drift
```


``` r
# Overlay the detected drifts on the original stream.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```

![plot of chunk unnamed-chunk-8](fig/dfr_kswin/unnamed-chunk-8-1.png)
