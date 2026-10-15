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
dfr_cusum(lambda = 50, delta = 0.005, min_instances = 30)
```

## Arguments

- lambda:

  Detection threshold: a drift is reported when the cumulative sum
  exceeds it. Lower values react faster at the cost of more false
  alarms.

- delta:

  Tolerance subtracted at every step, i.e. the increase of the error
  rate that is still considered acceptable.

- min_instances:

  Minimum number of observations before a drift can be reported.

## Value

`dfr_cusum` object

## Details

The detector follows the `CusumDM` detector of MOA: it accumulates
`max(0, sum + x - mean - delta)`, where `mean` is the running error
rate, so it reacts to an increase of the error rate whatever its level.
After a drift is reported the statistics restart from scratch. Missing
values are treated as correct predictions (`0`).

Up to version 1.3.0, `dfr_cusum()` accumulated `+1` for every error and
`-1` for every correct prediction, so it could only detect error rates
above 50%.

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
#> 258 258  TRUE drift
```
