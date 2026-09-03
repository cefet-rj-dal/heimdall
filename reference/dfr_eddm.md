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

Missing values are treated as correct predictions (`0`).

## References

Baena-Garcia, M., del Campo-Avila, J., Fidalgo, R., Bifet, A., Gavalda,
R., and Morales-Bueno, R. (2006). Early drift detection method. In
*Fourth International Workshop on Knowledge Discovery from Data
Streams*.

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

model <- dfr_eddm()

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
#> 231 231  TRUE drift
```
