# EI-kMeans stream detector

Monitors a stream with an EI-kMeans partitioning
([`eikmeans()`](https://cefet-rj-dal.github.io/heimdall/reference/eikmeans.md)):
the first `n_train` observations form the training sample, and every
later window of `window_size` observations is tested against it. Since
it monitors the distribution of the observed features, it is primarily
used for **virtual concept drift**.

## Usage

``` r
dfr_eikmeans(target_feat = NULL, n_train = 2000, window_size = 200, ...)
```

## Arguments

- target_feat:

  Names of the columns to be monitored. `NULL` (the default) monitors
  every numeric column.

- n_train:

  Size of the training sample.

- window_size:

  Size of the tested windows (non-overlapping).

- ...:

  Further arguments for
  [`eikmeans()`](https://cefet-rj-dal.github.io/heimdall/reference/eikmeans.md)
  (`k`, `alpha`, `min_points`, `n_build`, `thetas`).

## Value

`dfr_eikmeans` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds the chi-square statistic and the p-value of every tested window
(on its last observation), and `obj$model` the partitioning.

## Details

A drift is reported on the last observation of every window whose
p-value is below `alpha`.
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
forgets the partitioning, and the next `n_train` observations train a
new one, which is the adaptation the paper recommends after a drift.
Rows with missing values are skipped.

## References

Liu, A., Lu, J. and Zhang, G. (2021). Concept drift detection via equal
intensity k-means space partitioning. *IEEE Transactions on
Cybernetics*, 51(6), 3198-3211. <doi:10.1109/TCYB.2020.2983962>

## Examples

``` r
library(daltoolbox)
library(heimdall)
# Two clusters; after 3000 rows the right one moves.
set.seed(1)
cluster <- function(n, x) data.frame(x1 = stats::rnorm(n, x), x2 = stats::rnorm(n))
stream <- rbind(cluster(1500, 0), cluster(1500, 5))[sample(3000), ]
drifted <- rbind(cluster(500, 0), cluster(500, 6))[sample(1000), ]
model <- fit(dfr_eikmeans(n_train = 2000, window_size = 200), rbind(stream, drifted))
which(model$drifter_output$p_value < 0.05)
#> [1] 3200 3400 3600 3800 4000
```
