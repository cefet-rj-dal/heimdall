# Mean Comparison Distance method

MCDD is a window-based detector that compares the location of a
reference and a recent sample by means of a hypothesis test on their
central tendency. Because it monitors the distribution of an observed
feature rather than predictive errors, it is primarily intended for
**virtual concept drift**. In this package, the detector follows the
statistical-testing perspective adopted by Giusti et al. (2021) for
drift analysis.

## Usage

``` r
dfr_mcdd(
  target_feat = NULL,
  alpha = 1e-04,
  normality_alpha = 0.05,
  window_size = 300,
  window_type = "sliding",
  monitoring_step = 1,
  data = NULL
)
```

## Arguments

- target_feat:

  Feature to be monitored

- alpha:

  Probability threshold for the comparison test

- normality_alpha:

  Significance level of the Shapiro-Wilk normality test used to decide
  between the parametric and the nonparametric comparison. It is
  deliberately separate from `alpha`, since the two thresholds answer
  different questions.

- window_size:

  Size of the window

- window_type:

  Window policy. `"sliding"` (default) keeps at most `window_size`
  observations, so both halves move with the stream. `"anchored"` keeps
  every observation, which pins the reference half to the beginning of
  the stream and makes memory grow with the stream length.

- monitoring_step:

  Number of observations between two consecutive tests. The default
  (`1`) tests at every observation; larger values reduce the
  computational cost on long streams.

- data:

  Already collected data to avoid cold start.

## Value

`dfr_mcdd` object

## Details

MCDD only reacts to changes of location (mean or median); a change of
spread with the same centre is not detected, so combine it with
[`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md)
when both kinds of change matter.

Normality of both halves of the window is checked with
[`stats::shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html);
when it holds, a t-test is applied, otherwise a Wilcoxon rank-sum test
is used. Shapiro-Wilk is only defined for samples of at most 5000
observations, so larger windows always fall back to the nonparametric
test.

Missing observations are skipped instead of being imputed.

## References

Giusti, L., Carvalho, L., Gomes, A. T., Coutinho, R., Soares, J., and
Ogasawara, E. (2021). Analysing flight delay under concept drift.
*Evolving Systems*. <doi:10.1007/s12530-021-09415-z>

## Examples

``` r
# Load Heimdall and the synthetic stream.
library(heimdall)

# Fix the seed to keep the walkthrough reproducible.
seed <- 1
set.seed(seed)

# Load the univariate series monitored in this example.
data(st_drift_examples)
serie <- st_drift_examples$uv_virtual_drift

# Plot the monitored signal before running the detector.
plot(x=seq_len(nrow(serie)), y=serie$serie)

# Instantiate the mean-comparison detector.
model <- dfr_mcdd(target_feat='serie', window_size=100)

# Update the detector sequentially and collect the alarm positions.
detection <- NULL
output <- list(obj=model, drift=FALSE)
for (i in seq_len(nrow(serie))){
  output <- update_state(output$obj, serie$serie[i])
  if (output$drift){
    type <- 'drift'
    output$obj <- reset_state(output$obj)
  } else {
    type <- ''
  }
  detection <- rbind(detection, data.frame(idx=i, event=output$drift, type=type))
}

# Print the detected drift points.
detection[detection$type == 'drift',]
#>     idx event  type
#> 225 225  TRUE drift
#> 332 332  TRUE drift
#> 432 432  TRUE drift

# Overlay the detected drifts on the original series.
plot(x=seq_len(nrow(serie)), y=serie$serie)
for (drift_index in detection[detection$type == 'drift', 'idx']) {
  abline(v=drift_index, col='red', lty=2)
}
```
