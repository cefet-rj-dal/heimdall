# WWAggr and naive aggregations of ensemble scores

Aggregates the change point scores of the members of an ensemble
([`cpd_scores()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_scores.md)
of a
[`cpd_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md))
into one score per time step. WWAggr (Stepikin et al., 2025) compares,
at every step, the scores of all members in a "future" window of the
last `window_size` steps with those in an anchor ("history") window, by
the Wasserstein distance of the two samples (the window flattened over
steps and members) or by the maximum mean discrepancy; the naive
aggregations take the mean, minimum, maximum, median or a quantile of
the members at every step.

## Usage

``` r
wwaggr(
  scores,
  method = c("wasserstein", "mmd", "mean", "min", "max", "median", "quantile"),
  window_size = 3,
  anchor = c("prev", "start", "combined"),
  p = 1,
  kernel = c("rbf", "multiscale"),
  bandwidth = NULL,
  q = 0.5
)
```

## Arguments

- scores:

  Scores of the members: an array `[member, sequence, time]` or a matrix
  `[member, time]` (one sequence).

- method:

  `"wasserstein"` (WWAggr, default), `"mmd"`, `"mean"`, `"min"`,
  `"max"`, `"median"` or `"quantile"`.

- window_size:

  Size of the windows (distance methods).

- anchor:

  Anchor window: `"prev"` (default, the `window_size` steps before the
  future window, as Algorithm 1 of the paper), `"start"` (the first
  `window_size` steps of the sequence) or `"combined"` (the first
  `window_size %/% 2` steps followed by the `window_size %/% 2` steps
  before the last ones; the future window then has
  `2 * (window_size %/% 2)` steps).

- p:

  Order of the Wasserstein distance.

- kernel:

  Kernel of the MMD: `"rbf"` or `"multiscale"`.

- bandwidth:

  Bandwidths of the MMD kernel. `NULL` uses those of the reference: 10,
  15, 20 and 50 (rbf) or 0.2, 0.5, 0.9 and 1.3 (multiscale).

- q:

  Quantile of the `"quantile"` method (the median is 0.5, the minimum 0
  and the maximum 1; linear interpolation as `torch.quantile`).

## Value

A matrix `[sequence, time]` of aggregated scores, or a vector for one
sequence.

## Details

The steps where some member has no score (the first steps of the
representation models) are left out, as in the reference code, and get
`NA`. For the distance methods, the score of the `i`-th remaining step
(`i > window_size`) compares the future window of steps
`i - window_size` to `i - 1` with the anchor window, and the first
`window_size` scores are 0 (`anchor_window_detector_batch` of the
reference, `src/ensembles/ distances.py`). For `"prev"`, the anchor
starts at `max(1, i - 2 * window_size)`.

The 1-Wasserstein distance of two samples of the same size is the mean
absolute difference of their order statistics, and the p-Wasserstein
distance the p-th root of the mean of their p-th powers. The MMD treats
the steps of a window as samples and the members as dimensions; it is
the mean of `k(x, x') + k(y, y') - 2 k(x, y')` over all pairs, with the
sum of the kernels of all bandwidths: `exp(-d / (2 a))` (rbf) or
`a^2 / (a^2 + d)` (multiscale), `d` the squared distance. With
calibrated members, the scores are in `[0, 1]` and so is the
1-Wasserstein distance, so that a threshold around 0.5 works without
tuning (Section IV of the paper).

## References

Stepikin, A., Romanenkova, E. and Zaytsev, A. (2025). WWAggr: A window
Wasserstein-based aggregation for ensemble change point detection. *2025
IEEE International Conference on Data Mining (ICDM)*, 1535-1544.
<doi:10.1109/ICDM65498.2025.00163>

## Examples

``` r
set.seed(1)
# 10 members, 2 sequences of 30 steps; the change is at step 16.
scores <- array(stats::runif(10 * 2 * 30, 0, 0.2), c(10, 2, 30))
scores[, , 16:30] <- scores[, , 16:30] + 0.7
round(wwaggr(scores, window_size = 3)[1, 12:22], 2)
#>  [1] 0.01 0.01 0.02 0.01 0.01 0.23 0.47 0.73 0.50 0.24 0.01
round(wwaggr(scores, method = "mean")[1, 12:22], 2)
#>  [1] 0.09 0.09 0.09 0.06 0.79 0.84 0.80 0.80 0.82 0.83 0.80
```
