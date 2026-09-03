# KSWIN method

KSWIN applies a Kolmogorov-Smirnov test between a recent window and a
reference sample drawn from older observations. In this package, the
method is primarily used for **virtual concept drift**, because it
monitors distributional changes in a numeric feature stream. The method
follows Raab et al. (2020) <doi:10.1016/j.neucom.2019.11.111>.

## Usage

``` r
dfr_kswin(
  target_feat = NULL,
  window_size = 1500,
  stat_size = 500,
  alpha = 1e-07,
  monitoring_step = 1,
  exact = TRUE,
  data = NULL
)
```

## Arguments

- target_feat:

  Feature to be monitored.

- window_size:

  Size of the sliding window (must be \> 2\*stat_size)

- stat_size:

  Size of the statistic window

- alpha:

  Probability for the test statistic of the Kolmogorov-Smirnov-Test The
  alpha parameter is very sensitive, therefore should be set below 0.01.

- monitoring_step:

  Number of observations between two consecutive tests. The default
  (`1`) tests at every observation; larger values reduce the
  computational cost on long streams.

- exact:

  Passed to [`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html).
  The default (`TRUE`) preserves the behaviour of previous versions;
  setting it to `NULL` lets R choose, which is considerably faster for
  the default window sizes.

- data:

  Already collected data to avoid cold start.

## Value

`dfr_kswin` object

## Details

The reference window is randomly subsampled, so results depend on the
state of the random number generator. Call
[`base::set.seed()`](https://rdrr.io/r/base/Random.html) before the
stream loop if reproducibility is required.

Missing observations are skipped instead of being imputed. When a drift
is reported the window is trimmed to its most recent `stat_size`
observations, and
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
preserves that window so the detector does not restart cold.

## References

Raab, C., Heusinger, M., and Schleif, F.-M. (2020). Reactive soft
prototype computing for concept drift streams. *Neurocomputing*, 416,
340-351. <doi:10.1016/j.neucom.2019.11.111>

## Examples

``` r
library(daltoolbox)
library(heimdall)

# This example assumes a model residual where 1 is an error and 0 is a
# correct prediction.

data(st_drift_examples)
data <- st_drift_examples$univariate
data$event <- NULL

model <- dfr_kswin(target_feat='serie')

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
#> [1] idx   event type 
#> <0 rows> (or 0-length row.names)
```
