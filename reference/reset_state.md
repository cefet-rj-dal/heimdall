# Reset State

Reset Drifter State

## Usage

``` r
reset_state(obj)
```

## Arguments

- obj:

  Drifter object

## Value

updated Drifter object

## Examples

``` r
model <- dfr_ddm()
model <- reset_state(model)
model$drifted
#> [1] FALSE
```
