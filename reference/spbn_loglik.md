# Log-likelihood of each node of a semiparametric Bayesian network

Evaluates `log f(x_i | pa_i)` for every observation and node of an
[`spbn()`](https://cefet-rj-dal.github.io/heimdall/reference/spbn.md)
model.

## Usage

``` r
spbn_loglik(model, data)
```

## Arguments

- model:

  An `spbn` object.

- data:

  A data frame or matrix with the model's variables.

## Value

A matrix with one row per observation and one column per node.

## Examples

``` r
set.seed(1)
x <- data.frame(a = stats::rnorm(200), b = stats::rnorm(200))
head(spbn_loglik(spbn(x), x))
#>               a         b
#> [1,] -1.0980030 -1.028712
#> [2,] -0.8556594 -2.120772
#> [3,] -1.2846934 -1.969324
#> [4,] -2.2591068 -1.012534
#> [5,] -0.8931970 -2.986821
#> [6,] -1.2694505 -3.946794
```
