# Drifter

Ancestor class for drift detection.

Every detector in the package follows the same contract:

- `update_state(obj, value)` returns a list with the updated object
  (`obj`) and a logical flag (`drift`) telling whether a drift was
  detected **in that call**;

- `obj$drifted` is sticky: it stays `TRUE` after the first detection
  until
  [`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
  is called;

- `reset_state(obj)` returns the detector to its initial state.

## Usage

``` r
drifter()
```

## Value

Drifter object

## Examples

``` r
model <- drifter()
model$drifted
#> [1] FALSE
```
