# Likelihood-ratio change point of a binary sequence

Finds the split of a binary sequence that maximises the log-likelihood
ratio between two Bernoulli segments and a single one (problem *Change*
of Tatti, 2019), searching only the border indices, with the \\(1 -
\epsilon)\\ approximation of Algorithm 3 when `eps > 0`.

## Usage

``` r
fastlr_split(x, eps = 0)
```

## Arguments

- x:

  Binary vector (0/1 or logical).

- eps:

  Approximation guarantee, in \[0, 1): the returned score is at least
  \\1 - \epsilon\\ times the optimum. `0` tests every border index.

## Value

A list with `score` (log-likelihood ratio, natural logarithm), `index`
(the first entry of the second segment; `length(x) + 1` or `1` when no
split improves the likelihood), `tests` (number of evaluated change
points), `candidates` (candidates of FindCands, without the FindSegment
points) and `blocks` (number of border indices of the sequence and of
the flipped sequence).

## References

Tatti, N. (2019). Fast likelihood-based change point detection. In
*Machine Learning and Knowledge Discovery in Databases (ECML PKDD
2019)*, 662-677. <doi:10.1007/978-3-030-46150-8_39>. arXiv:2301.08892.

## Examples

``` r
set.seed(1)
x <- c(rbinom(300, 1, 0.2), rbinom(200, 1, 0.6))
fastlr_split(x)
#> $score
#> [1] 56.32389
#> 
#> $index
#> [1] 300
#> 
#> $tests
#> [1] 13
#> 
#> $candidates
#> [1] 13
#> 
#> $blocks
#> [1] 13
#> 
fastlr_split(x, eps = 0.5)$score
#> [1] 56.32389
```
