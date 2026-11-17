# Semiparametric Bayesian network

Fits a semiparametric Bayesian network (Atienza et al., 2022) to
continuous data: every node has either a linear Gaussian conditional
density or a conditional kernel density estimate (CKDE) given its
parents. It is the density model of
[`dfr_llr2()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_llr2.md).

## Usage

``` r
spbn(data, structure = NULL, node_types = NULL, max_parents = 4, folds = 5)
```

## Arguments

- data:

  A data frame or matrix of numeric variables (no missing values).

- structure:

  Optional structure: a named list giving the parents of every variable
  (`list(B = "A", C = c("A", "B"), A = character(0))`) or a two-column
  matrix of arcs (`from`, `to`). `NULL` (default) learns it by greedy
  hill climbing.

- node_types:

  Optional named character vector of node types (`"lg"` for linear
  Gaussian, `"ckde"` for conditional KDE). `NULL` (default) chooses each
  node's type by cross-validated log-likelihood.

- max_parents:

  Largest number of parents of a node in the learned structure.

- folds:

  Number of folds of the cross-validation used to choose the node types.

## Value

An `spbn` object: the variables, their parents, node types and the
fitted CPDs.

## Details

Structure learning is a greedy hill climbing (arc addition, removal and
reversal) with the BIC score of a Gaussian Bayesian network. The type of
each node is then chosen given its parents: the type with the larger
log-likelihood over `folds` contiguous folds (each fold scored by the
CPD fitted on the other folds).

A linear Gaussian CPD is the least-squares regression of the node on its
parents with the maximum likelihood variance. A CKDE CPD is
`f(x, pa) / f(pa)`, the ratio of Gaussian kernel density estimates with
the bandwidth matrix of the normal reference rule,
`H = (4 / (d + 2))^(2 / (d + 4)) S N^(-2 / (d + 4))` (`S` the sample
covariance of the node and its parents, `d` their number, `N` the sample
size); the density of the parents uses the corresponding block of `H`,
so the conditional density integrates to one.

The paper uses the binned version (B-SPBN), which speeds up the kernel
sums by binning the data on a grid, and learns the structure and the
node types together by hill climbing with a cross-validated score. Here
the kernel sums are exact and the node types are chosen after the
structure.

## References

Atienza, D., Bielza, C. and Larrañaga, P. (2022). Semiparametric
Bayesian networks. *Information Sciences*, 584, 564-582.
<doi:10.1016/j.ins.2021.10.074>

## See also

[`spbn_loglik()`](https://cefet-rj-dal.github.io/heimdall/reference/spbn_loglik.md),
[`dfr_llr2()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_llr2.md).

## Examples

``` r
set.seed(1)
a <- stats::rnorm(500)
b <- 2 * a + stats::rnorm(500, sd = 0.5)
c <- ifelse(stats::runif(500) < 0.5, b - 3, b + 3) + stats::rnorm(500)
model <- spbn(data.frame(a, b, c))
model
#> Semiparametric Bayesian network: 3 nodes (1 linear Gaussian, 2 CKDE), 2 arcs
#>   a [lg] <- b
#>   b [ckde] <- c
#>   c [ckde] <- (none)
colMeans(spbn_loglik(model, data.frame(a, b, c)))
#>           a           b           c 
#> -0.06369788 -1.66472082 -2.70041599 
```
