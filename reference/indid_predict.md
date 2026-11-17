# Probabilities of change of an InDiD model

Runs an
[`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md)
model on sequences and returns, for every sequence and time step, the
probability that a change has happened.

## Usage

``` r
indid_predict(model, x)
```

## Arguments

- model:

  An `indid` object.

- x:

  Sequences, in the formats accepted by
  [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md).

## Value

A matrix `[sequence, time]` of probabilities.

## Examples

``` r
set.seed(1)
x <- matrix(stats::rnorm(20 * 16, 1), 20)
labels <- matrix(0, 20, 16)
model <- indid(x, labels, T = 4, epochs = 2, seed = 1)
dim(indid_predict(model, x))
#> [1] 20 16
```
