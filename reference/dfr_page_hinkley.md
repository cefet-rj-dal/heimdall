# Adapted Page Hinkley method

The Page-Hinkley test is a sequential change-point detector that
monitors cumulative deviations from a running mean and signals a change
when those deviations grow persistently. In this package, the
implementation is primarily used for **virtual concept drift** when it
monitors a numeric feature stream, although the same statistic can also
be applied to error streams to detect **real concept drift**. The method
is based on Page (1954) and the later streaming adaptation popularized
in data-stream mining.

## Usage

``` r
dfr_page_hinkley(
  target_feat = NULL,
  min_instances = 30,
  delta = 0.005,
  threshold = 50,
  alpha = 1 - 1e-04
)
```

## Arguments

- target_feat:

  Feature to be monitored.

- min_instances:

  The minimum number of instances before detecting change

- delta:

  The delta factor for the Page Hinkley test

- threshold:

  The change detection threshold (lambda)

- alpha:

  The forgetting factor, used to weight the observed value and the mean

## Value

`dfr_page_hinkley` object

## Details

Missing observations are skipped instead of being imputed.

## References

Page, E. S. (1954). Continuous inspection schemes. *Biometrika*,
41(1/2), 100-115. <doi:10.2307/2333009>

## Examples

``` r
library(daltoolbox)
library(heimdall)

# This example assumes a model residual where 1 is an error and 0 is a
# correct prediction.

data(st_drift_examples)
data <- st_drift_examples$univariate
data$event <- NULL

model <- dfr_page_hinkley(target_feat='serie')

detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_along(data$serie)){
 output <- update_state(output$obj, data$serie[i])
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
#> 205 205  TRUE drift
```
