# Levene Based Drift Detection Method method

LBDD is a window-based detector that compares the variability of
reference and recent samples using Levene's test. Because it monitors
changes in the distribution of an observed feature rather than model
performance, it is primarily aimed at **virtual concept drift**. In this
package, the detector follows the statistical-testing approach discussed
by Giusti et al. (2021) for drift analysis, using Levene's variance test
as its core mechanism.

## Usage

``` r
dfr_lbdd(
  target_feat = NULL,
  alpha = 0.01,
  window_size = 1500,
  monitoring_step = 1,
  data = NULL
)
```

## Arguments

- target_feat:

  Feature to be monitored

- alpha:

  Probability threshold for the test statistic

- window_size:

  Size of the sliding window

- monitoring_step:

  Number of observations between two consecutive tests. The default
  (`1`) tests at every observation; larger values reduce the
  computational cost on long streams.

- data:

  Already collected data to avoid cold start.

## Value

`dfr_lbdd` object

## Details

Levene's test is computed with the group spread centred on the median
(the Brown-Forsythe variant).

Missing observations are skipped instead of being imputed. When a drift
is reported the window is trimmed to its most recent half, and
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
preserves that window so the detector does not restart cold.

## References

Giusti, L., Carvalho, L., Gomes, A. T., Coutinho, R., Soares, J., and
Ogasawara, E. (2021). Analysing flight delay under concept drift.
*Evolving Systems*. <doi:10.1007/s12530-021-09415-z>

## Examples

``` r
library(daltoolbox)
library(heimdall)

# This example assumes a model residual where 1 is an error and 0 is a
# correct prediction.

data(st_drift_examples)
data <- st_drift_examples$univariate
data$event <- NULL

model <- dfr_lbdd(target_feat='serie', window_size=100, alpha=0.05)

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
#> 115 115  TRUE drift
#> 166 166  TRUE drift
#> 217 217  TRUE drift
#> 268 268  TRUE drift
#> 325 325  TRUE drift
#> 376 376  TRUE drift
#> 427 427  TRUE drift
#> 478 478  TRUE drift
```
