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

Missing values are treated as correct predictions (`0`).

## References

Gama, J., Medas, P., Castillo, G., and Rodrigues, P. P. (2004). Learning
with drift detection. In *Advances in Artificial Intelligence - SBIA
2004*, 286-295. <doi:10.1007/978-3-540-28645-5_29>

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

model <- dfr_ddm()

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
