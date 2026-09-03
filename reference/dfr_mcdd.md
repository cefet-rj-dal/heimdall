# Mean Comparison Distance method

MCDD is a window-based detector that compares the location of reference
and recent samples by means of hypothesis tests on their central
tendency. Because it monitors the distribution of observed features
rather than predictive errors, it is primarily intended for **virtual
concept drift**. In this package, the detector follows the
statistical-testing perspective adopted by Giusti et al. (2021) for
drift analysis.

## Usage

``` r
dfr_mcdd(
  target_feat = NULL,
  alpha = 1e-08,
  window_size = 1500,
  monitoring_step = 1,
  data = NULL
)
```

## Arguments

- target_feat:

  Feature to be monitored

- alpha:

  Probability threshold for all test statistics

- window_size:

  Size of the sliding window

- monitoring_step:

  Number of observations between two consecutive tests. The default
  (`1`) tests at every observation; larger values reduce the
  computational cost on long streams.

- data:

  Already collected data to avoid cold start.

## Value

`dfr_mcdd` object

## Details

Normality of both halves of the window is checked with
[`stats::shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html);
when it holds, a t-test is applied, otherwise a Wilcoxon rank-sum test
is used. Shapiro-Wilk is only defined for samples of at most 5000
observations, so larger windows always fall back to the nonparametric
test.

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

model <- dfr_mcdd(target_feat='serie', window_size=100, alpha=0.05)

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
#> 108 108  TRUE drift
#> 159 159  TRUE drift
#> 210 210  TRUE drift
#> 261 261  TRUE drift
#> 315 315  TRUE drift
#> 366 366  TRUE drift
#> 417 417  TRUE drift
#> 468 468  TRUE drift
```
