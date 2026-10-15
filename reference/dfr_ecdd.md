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
library(daltoolbox)
library(heimdall)

# This example uses an error-based drift detector with a synthetic
# model residual where 1 is an error and 0 is a correct prediction.

data(st_drift_examples)
data <- st_drift_examples$univariate
data$event <- NULL
data$prediction <- st_drift_examples$univariate$serie > 4

model <- dfr_ecdd()

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
#> 201 201  TRUE drift
```
