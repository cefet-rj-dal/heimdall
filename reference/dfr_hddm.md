# Adapted Hoeffding Drift Detection Method (HDDM) method

HDDM_A is a sequential detector based on Hoeffding's inequality that
tests whether the mean of the monitored error stream has increased
beyond statistically expected fluctuations. Because this implementation
is error-based, it is primarily targeted at **real concept drift**. The
theoretical basis follows Frias-Blanco et al. (2015)
<doi:10.1109/TKDE.2014.2345382>.

## Usage

``` r
dfr_hddm(
  drift_confidence = 0.001,
  warning_confidence = 0.005,
  two_side_option = TRUE
)
```

## Arguments

- drift_confidence:

  Confidence to the drift

- warning_confidence:

  Confidence to the warning

- two_side_option:

  Option to monitor error increments and decrements (two-sided) or only
  increments (one-sided)

## Value

`dfr_hddm` object

## Details

Missing values are treated as correct predictions (`0`).

## References

Frias-Blanco, I., del Campo-Avila, J., Ramos-Jimenez, G., Morales-Bueno,
R., Ortiz-Diaz, A., and Caballero-Mota, Y. (2015). Online and
nonparametric drift detection methods based on Hoeffding's bounds. *IEEE
Transactions on Knowledge and Data Engineering*, 27(3), 810-823.
<doi:10.1109/TKDE.2014.2345382>

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

model <- dfr_hddm()

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
#> 204 204  TRUE drift
```
