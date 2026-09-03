# Passive dummy detector

Implements a dummy detector that always reports a drift. Useful as an
upper baseline, since it forces the model to be retrained at every
batch.

## Usage

``` r
dfr_passive()
```

## Value

Drifter object

## Examples

``` r
model <- dfr_passive()
output <- update_state(model, 1)
output$drift
#> [1] TRUE
```
