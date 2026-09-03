# Update State

Update Drifter State with a new observation.

## Usage

``` r
update_state(obj, value, ...)
```

## Arguments

- obj:

  Drifter object

- value:

  a value that represents a processed batch

- ...:

  optional arguments

## Value

a list with two elements: `obj`, the updated Drifter object, and
`drift`, a logical flag indicating whether a drift was detected in this
call

## Examples

``` r
model <- dfr_ddm()
output <- update_state(model, 0)
output$drift
#> [1] FALSE
```
