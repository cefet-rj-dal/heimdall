# Population Stability Index (PSI)

Compares the distribution of a new sample with a baseline sample,
feature by feature, through the Population Stability Index, the
symmetrised Kullback-Leibler divergence of their binned distributions
(Kurian and Allali, 2024): \$\$PSI = \sum\_{i=1}^{B} (P_i - Q_i) \ln(P_i
/ Q_i),\$\$ where \\Q_i\\ and \\P_i\\ are the shares of the baseline and
of the new sample in bin \\i\\. The bins are quantile groups of the
baseline (deciles for `bins = 10`, demi-deciles for `bins = 20`).

## Usage

``` r
psi(expected, actual, bins = 10, thresholds = c(0.1, 0.25))
```

## Arguments

- expected:

  The baseline sample: a numeric vector, or a data frame or matrix with
  one column per feature.

- actual:

  The new sample, with the same features as `expected`.

- bins:

  Number of quantile groups of the baseline.

- thresholds:

  Limits between the drift levels: PSI below the first is "little"
  drift, between the two "moderate" drift (to be reviewed), and above
  the second "significant" drift.

## Value

A `psi` object: a list with `summary`, a data frame with one row per
feature (`feature`, `psi`, `level`, number of `bins`), and `bins`, a
data frame with one row per feature and bin (`feature`, `bin`, `lower`
and `upper` baseline values, `expected` and `actual` shares, `part`, the
contribution of the bin).

## Details

The computation follows the SAS macro of the reference code
(<https://github.com/FrancisKurian/Kullback>, `PSI_computation.sas`):

1.  The baseline is ranked (ties get their mean rank) and split into
    `bins` groups as `PROC RANK GROUPS=`: group
    `floor(rank * bins / (n + 1))`. Tied values always share a group, so
    heavily tied data may give fewer bins. Bin `i` covers the baseline
    values from its smallest to its largest.

2.  New values below the smallest baseline value or above the largest
    are moved to it (they fall in the first or the last bin).

3.  The shares are the bin counts over the sample sizes. Missing new
    values count in the size of the new sample but in no bin, so a
    stream that starts sending missing values lowers every share.
    Missing baseline values are left out. A new sample whose values are
    all missing has every share 0, and so a PSI of 0 (step 4): missing
    data must be checked separately.

4.  A bin without new values contributes 0, as in the reference (where
    the logarithm of 0 is missing and the missing part is set to 0).

The reference assigns a value through a SAS format built from the bin
ranges; a new value that falls strictly between the largest value of a
bin and the smallest of the next one matches no range and is dropped
from the counts (but kept in the sample size). Here such a value goes to
the lower bin. The paper's thresholds are 0.1 and 0.25 (Section 5;
Section 3.1 uses 0.2 for the second). The PSI of two samples from the
same distribution is not 0: it is about
`(bins - 1) (1 / n_expected + 1 / n_actual)`, which matters for small
samples.

## References

Kurian, J. F. and Allali, M. (2024). Detecting drifts in data streams
using Kullback-Leibler (KL) divergence measure for data engineering
applications. *Journal of Data, Information and Management*, 6, 207-216.
<doi:10.1007/s42488-024-00119-y>

## See also

[`dfr_psi()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_psi.md)
to monitor a stream, and
[`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
for the same divergence over a sliding window with equal-width bins.

## Examples

``` r
set.seed(1)
base <- stats::rnorm(10000, 610, 50)
new <- base
top <- new > 643 & stats::runif(10000) < 0.5
new[top] <- new[top] - 200
result <- psi(base, new)
result$summary
#>   feature       psi    level bins
#> 1   value 0.1858564 moderate   10
head(result$bins)
#>   feature bin    lower    upper expected actual         part
#> 1   value   1 426.4350 544.6399      0.1 0.2286 1.063269e-01
#> 2   value   2 544.6916 567.6745      0.1 0.1010 9.950331e-06
#> 3   value   3 567.7063 583.5134      0.1 0.1000 0.000000e+00
#> 4   value   4 583.5145 596.8759      0.1 0.1001 9.995003e-08
#> 5   value   5 596.9027 609.1905      0.1 0.1001 9.995003e-08
#> 6   value   6 609.2166 621.9483      0.1 0.1000 0.000000e+00
psi(base, new, bins = 20)$summary
#>   feature       psi    level bins
#> 1   value 0.2461855 moderate   20
```
