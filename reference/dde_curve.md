# DDE probability curve

Computes the DDE probability curve and the DDE probability lift curve of
a drift (Szucs and Nemeth, 2025, Section 2).

## Usage

``` r
dde_curve(data, drifting, thresholds = seq(0, 0.9, by = 0.1))
```

## Arguments

- data:

  Data frame or matrix with the features in the reference window, where
  the Pearson correlations are measured.

- drifting:

  Names (or a logical vector over the columns of `data`) of the features
  that drift.

- thresholds:

  Correlation thresholds \\z\\.

## Value

data frame with one row per threshold and the columns `threshold`,
`pairs` (\\B(z)\\), `drifting_pairs` (\\b(z)\\), `probability`
(\\r\_{n/N}(z)\\, `NA` when no pair reaches the threshold) and `lift`.

## Details

For each threshold \\z\\, \\B(z)\\ is the number of feature pairs with
\\\|\rho\_{ij}\| \ge z\\ (Eq. 14) and \\b(z)\\ the number of those pairs
in which both features drift (Eq. 15). With \\m\\ features of which
\\k\\ drift, the DDE probability is \$\$r\_{n/N}(z) = \frac{m}{k}
\frac{b(z)}{B(z)}\$\$ (Eqs. 16 and 20), the probability that a neighbour
of a drifting feature drifts too, and the lift divides it by its value
at \\z = 0\\, \\(k - 1)/(m - 1)\\ (Eqs. 21 to 24). A flat lift curve
means that the features drift independently of their correlation; an
increasing one, that the drift affects groups of correlated features
(the domino drift effect). Features without variance have no correlation
with the others and are treated as uncorrelated.

## Examples

``` r
set.seed(1)
a <- stats::rnorm(200)
b <- stats::rnorm(200)
data <- data.frame(a1 = a, a2 = a + stats::rnorm(200, sd = 0.2),
                   a3 = a + stats::rnorm(200, sd = 0.5),
                   b1 = b, b2 = b + stats::rnorm(200, sd = 0.2),
                   b3 = b + stats::rnorm(200, sd = 0.5))
# The correlated group "a" drifts: the lift grows with the correlation.
dde_curve(data, c("a1", "a2", "a3"))
#>    threshold pairs drifting_pairs probability lift
#> 1        0.0    15              3         0.4  1.0
#> 2        0.1     6              3         1.0  2.5
#> 3        0.2     6              3         1.0  2.5
#> 4        0.3     6              3         1.0  2.5
#> 5        0.4     6              3         1.0  2.5
#> 6        0.5     6              3         1.0  2.5
#> 7        0.6     6              3         1.0  2.5
#> 8        0.7     6              3         1.0  2.5
#> 9        0.8     6              3         1.0  2.5
#> 10       0.9     2              1         1.0  2.5
```
