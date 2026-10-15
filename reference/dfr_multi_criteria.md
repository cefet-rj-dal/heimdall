# Multi Criteria Drifter sub-class

Combines the output of several drift detectors into a single decision.

## Usage

``` r
dfr_multi_criteria(drifter_list, combination = "or", fuzzy_window = 10)
```

## Arguments

- drifter_list:

  Named list of drifters to combine. Every element must inherit from
  either `dist_based` or `mv_dist_based`.

- combination:

  How the drifters will be combined. One of `'or'` (default), `'and'` or
  `'fuzzy'`.

- fuzzy_window:

  Number of observations during which a detection is still taken into
  account by the `'and'` and `'fuzzy'` combinations.

## Value

Drifter object

## Details

- `'or'` reports a drift as soon as any detector does;

- `'and'` reports a drift when every detector has reported one within
  the last `fuzzy_window` observations;

- `'fuzzy'` gives each detection a weight that decays linearly from 1 to
  0 over `fuzzy_window` observations, and reports a drift when the sum
  of the weights exceeds half the number of detectors.

After a combined drift is reported, the past detections are forgotten,
so a single change is reported once. Detectors with a `target_feat`
receive that column of each row; the other univariate detectors receive
the row mean.

`obj$detections` counts the drifts reported by each detector and
`obj$last_drifts` holds their decisions on the last observation.

## Examples

``` r
library(daltoolbox)
library(heimdall)

set.seed(1)
data <- data.frame(serie = c(stats::rnorm(100), stats::rnorm(100, mean = 10)))

model <- dfr_multi_criteria(
 drifter_list = list(
   ph = dfr_page_hinkley(),
   ph_sensitive = dfr_page_hinkley(threshold = 10)
 ),
 combination = 'or'
)

model <- fit(model, data)
model$drifted
#> [1] TRUE
model$detections
#>           ph ph_sensitive 
#>            1            2 
```
