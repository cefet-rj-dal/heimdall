# Kernel two-sample test (MMD)

Permutation test of whether two samples come from the same distribution,
using the maximum mean discrepancy (Gretton et al., 2012) with a
random-trees kernel: the similarity of two points is the share of
totally random trees in which they fall in the same leaf. This is the
test used to verify virtual drift in the reference implementation of
Hinder et al. (2026).

## Usage

``` r
dsg_mmd_test(x1, x2, n_perm = 2500, n_trees = 100, max_depth = 5)
```

## Arguments

- x1, x2:

  Data frames or matrices with the same numeric columns.

- n_perm:

  Number of permutations.

- n_trees, max_depth:

  Size of the random-trees embedding.

## Value

list with the MMD `statistic` and the permutation `p_value` (share of
permutations with a larger statistic)

## References

Gretton, A., Borgwardt, K. M., Rasch, M. J., Scholkopf, B., and Smola,
A. (2012). A kernel two-sample test. *Journal of Machine Learning
Research*, 13, 723-773.

## Examples

``` r
set.seed(1)
same <- dsg_mmd_test(matrix(stats::rnorm(200), 100), matrix(stats::rnorm(200), 100), n_perm = 200)
shifted <- dsg_mmd_test(matrix(stats::rnorm(200), 100), matrix(stats::rnorm(200, 1), 100),
                        n_perm = 200)
c(same$p_value, shifted$p_value)
#> [1] 0.45 0.00
```
