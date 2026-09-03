# Inactive dummy detector

Implements a dummy detector that never reports a drift. Useful as a
baseline when evaluating adaptation strategies.

## Usage

``` r
dfr_inactive()
```

## Value

Drifter object

## Examples

``` r
model <- dfr_inactive()
output <- update_state(model, 1)
output$drift
#> [1] FALSE
```
