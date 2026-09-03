# Cumulative Sum for Concept Drift Detection (CUSUM) method

CUSUM is a sequential analysis procedure that accumulates deviations in
a monitored signal and raises an alarm when the cumulative evidence
exceeds a threshold. In this package, the detector is implemented as an
error-based monitor, so it is primarily intended for **real concept
drift** affecting predictive performance. The concept-drift adaptation
follows the sequential change-detection literature discussed by
Muthukrishnan, Berg, and Wu (2007) <doi:10.1109/ICDMW.2007.89>.

## Usage

``` r
dfr_cusum(lambda = 100)
```

## Arguments

- lambda:

  Necessary level for warning zone (2 standard deviation)

## Value

`dfr_cusum` object

## Details

Missing values are treated as correct predictions (`0`).

## References

Muthukrishnan, S., Berg, E., and Wu, Y. (2007). Sequential change
detection on data streams. In *Seventh IEEE International Conference on
Data Mining Workshops (ICDMW 2007)*. <doi:10.1109/ICDMW.2007.89>

## Examples

``` r
library(daltoolbox)
library(heimdall)

# This example uses an error-based drift detector with a synthetic
# model residual where 1 is an error and 0 is a correct prediction.

data(st_drift_examples)
data <- st_drift_examples$univariate
data$event <- NULL
data$prediction <- st_drift_examples$univariate$serie > 4

model <- dfr_cusum()

detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_along(data$prediction)){
 output <- update_state(output$obj, data$prediction[i])
 if (output$drift){
   type <- 'drift'
   output$obj <- reset_state(output$obj)
 }else{
   type <- ''
 }
 detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}

detection[detection$type == 'drift',]
#>     idx event  type
#> 301 301  TRUE drift
```
