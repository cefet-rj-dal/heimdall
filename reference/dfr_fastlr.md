# Fast likelihood-based change point detector for binary streams

Error-based detector of Tatti (2019). The entries since the last change
are modelled by one Bernoulli variable (M1) or split in two segments
with their own Bernoulli variables (M2). After every entry the split
that maximises the log-likelihood ratio of M2 over M1 is found among the
border indices (blocks maintained by the pool adjacent violators update)
with a \\(1 - \epsilon)\\ approximation in \\O(\epsilon^{-1} \log^2 n)\\
time; a change is declared when the ratio exceeds \\\sigma = \tau + \log
n\\ (BIC penalty plus `tau`), and the window then restarts from scratch.

## Usage

``` r
dfr_fastlr(tau = 6, eps = 0)
```

## Arguments

- tau:

  Threshold parameter \\\tau\\ (the paper uses 6).

- eps:

  Approximation guarantee \\\epsilon\\ in \[0, 1); `0` tests every
  border index (exact).

## Value

`dfr_fastlr` object

## Details

Values must be `1` (error) or `0` (correct prediction); missing values
are treated as `0`, as in the other error-based detectors. Unlike DDM,
changes in both directions are detected. `drifter_output` has the score
(log-likelihood ratio, natural logarithm) after every entry and, when a
change is declared, the stream index of the estimated change point (the
first entry of the second segment).

## References

Tatti, N. (2019). Fast likelihood-based change point detection. In
*Machine Learning and Knowledge Discovery in Databases (ECML PKDD
2019)*, 662-677. <doi:10.1007/978-3-030-46150-8_39>. arXiv:2301.08892.

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
x <- c(rbinom(1000, 1, 0.25), rbinom(1000, 1, 0.75), rbinom(1000, 1, 0.25))
detector <- fit(dfr_fastlr(tau = 6, eps = 0.1), x)
out <- detector$drifter_output
which(!is.na(out$change_point))     # when the changes were declared
#> [1] 1030 2017
out$change_point[!is.na(out$change_point)]   # estimated change points
#> [1] 1001 2004
```
