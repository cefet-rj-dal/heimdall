# Drifter

Ancestor class for drift detection.

Every detector in the package follows the same contract:

- `update_state(obj, value)` returns a list with the updated object
  (`obj`) and a logical flag (`drift`) telling whether a drift was
  detected \*\* in that call\*\*;

- `obj$drifted` is sticky: it stays `TRUE` after the first detection
  until
  [`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
  is called;

- `reset_state(obj)` returns the detector to its initial state;

- `obj$drifter_output` holds the per-observation diagnostics produced by
  the last call to
  [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), one row per
  observation; rows are `NA` when the detector reported nothing for that
  observation (for example while its window is filling, or for a skipped
  missing value).

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
