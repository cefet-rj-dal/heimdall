# KL Distance method

This detector compares consecutive reference and recent windows through
the Kullback-Leibler divergence estimated from their empirical
distributions. In this package, it is primarily used for **virtual
concept drift**, since it monitors changes in the distribution of a
numeric feature stream rather than predictive error. The statistical
foundation is the Kullback-Leibler divergence introduced by Kullback and
Leibler (1951).

## Usage

``` r
dfr_kldist(
  target_feat = NULL,
  window_size = 100,
  p_th = 0.5,
  monitoring_step = 1,
  data = NULL
)
```

## Arguments

- target_feat:

  Feature to be monitored.

- window_size:

  Size of the sliding window

- p_th:

  Drift threshold applied to the KL divergence, in bits. Despite the
  name, this is not a p-value. The default (0.5) was calibrated for the
  default window size; larger windows estimate the divergence more
  precisely and can use lower thresholds.

- monitoring_step:

  Number of observations between two consecutive tests. The default
  (`1`) tests at every observation; larger values reduce the
  computational cost on long streams.

- data:

  Already collected data to avoid cold start.

## Value

`dfr_kldist` object

## Details

Missing observations are skipped instead of being imputed. When a drift
is reported the window is trimmed to its most recent half, and
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
preserves that window so the detector does not restart cold.

## References

Kullback, S., and Leibler, R. A. (1951). On information and sufficiency.
*The Annals of Mathematical Statistics*, 22(1), 79-86.
<doi:10.1214/aoms/1177729694>

## Examples

``` r
library(daltoolbox)
library(heimdall)

# This example assumes a model residual where 1 is an error and 0 is a
# correct prediction.

data(st_drift_examples)
data <- st_drift_examples$univariate
data$event <- NULL

model <- dfr_kldist(target_feat='serie')

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
#> 127 127  TRUE drift
#> 219 219  TRUE drift
#> 270 270  TRUE drift
#> 329 329  TRUE drift
#> 380 380  TRUE drift
#> 431 431  TRUE drift
```
