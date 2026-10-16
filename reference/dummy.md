# Dummy Drifter sub-class

Ancestor class for the baseline detectors that ignore the monitored
signal,
[`dfr_inactive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_inactive.md)
and
[`dfr_passive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_passive.md).

## Usage

``` r
dummy()
```

## Value

Drifter object

## Examples

``` r
obj <- dummy()
class(obj)
#> [1] "dummy"    "drifter"  "dal_base"
```
