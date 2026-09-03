# Process Batch

Process Batch

## Usage

``` r
# S3 method for class 'drifter'
fit(obj, data, prediction = NULL, ...)
```

## Arguments

- obj:

  Drifter object

- data:

  data batch in data frame format

- prediction:

  prediction batch as vector format. Optional, and unused by the default
  method; it is kept for compatibility with detectors that need the
  model output.

- ...:

  optional arguments

## Value

updated Drifter object

## Examples

``` r
library(daltoolbox)
model <- dfr_inactive()
model <- fit(model, data.frame(serie = 1:10))
model$drifted
#> [1] FALSE
```
