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

  Sets the fuzzy window size. Only used when `combination = 'fuzzy'`.

## Value

Drifter object

## Note

The `'fuzzy'` combination rebuilds the whole membership matrix on every
update, so its cost grows quadratically with the length of the stream.
Prefer `'or'` or `'and'` for long streams.

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
```
