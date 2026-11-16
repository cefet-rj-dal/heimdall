# Maximum Concept Discrepancy drift detector (MCD-DD)

Unsupervised online drift detector of Wan, Liang and Yoon (2024). A
sliding window of `window_size` observations is split into `n_sub`
sub-windows. Sets of `m` points sampled from a sub-window are embedded
by a small neural network as the mean of the point encodings (the
concept representation), and the maximum concept discrepancy (MCD)
between two sub-windows is the distance between their representations.
Every time a new sub-window arrives, its MCD to the previous sub-window
is compared with a threshold; then the encoder is trained on the window
by contrastive learning: sets from the same sub-window are pulled
together, and sets with noise added (weak negatives, same sub-window;
strong negatives, first and last sub-windows) pushed apart. The
threshold is the `percentile` quantile of the MCD between sets of the
same sub-window. Since it monitors the distribution of the observed
features, it is used for **virtual concept drift**.

## Usage

``` r
dfr_mcd(
  target_feat = NULL,
  window_size = 3000,
  n_sub = 10,
  m = 30,
  k = 10,
  hidden_size = 100,
  output_size = 100,
  eps_small = 1,
  eps_big = 10,
  learning_rate = 0.005,
  epochs = 1,
  temperature = 0.1,
  lambda = 1,
  percentile = 0.95,
  seed = NULL
)
```

## Arguments

- target_feat:

  Names of the columns to be monitored. `NULL` (the default) monitors
  every numeric column.

- window_size:

  Size of the sliding window (a multiple of `n_sub`).

- n_sub:

  Number of sub-windows in a window; the window slides by one sub-window
  (`window_size / n_sub` observations).

- m:

  Number of points in a sample set.

- k:

  Number of sample sets drawn from a sub-window (even).

- hidden_size, output_size:

  Sizes of the hidden and output layers of the encoder.

- eps_small, eps_big:

  Standard deviations of the Gaussian noise of the weak and strong
  negative sets.

- learning_rate:

  Learning rate of the Adam optimiser.

- epochs:

  Optimisation steps per window.

- temperature:

  Temperature of the InfoNCE-like loss.

- lambda:

  Weight of the gradient penalty that keeps the encoder close to
  1-Lipschitz.

- percentile:

  Quantile of the within-sub-window MCD used as threshold.

- seed:

  Seed of the private random stream (initial weights, sampling and
  noise). `NULL` draws one from the session.

## Value

`dfr_mcd` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds, on the last observation of every tested sub-window, its MCD to
the previous sub-window (`mcd`) and the threshold (`threshold`);
`obj$distances` has one row per tested sub-window with its MCD to each
earlier sub-window of the window (the heatmaps of the paper).

## Details

The detector follows the reference code
(<https://github.com/LiangYiAnita/mcd-dd>, `mcd.py`, `encoder.py` and
`main.py`), which differs from the paper in some points:

- The encoder is `Linear - ReLU - Linear - Sigmoid`.

- Sample sets are drawn with replacement. The positive term of a
  sub-window is the mean MCD over all pairs of its `k` sets.

- The weak negative term compares the first `k / 2` sets with the other
  `k / 2` plus noise, and the strong one the sets of the first
  sub-window with those of the last one plus noise. Each is the mean MCD
  over the pairs of different indices.

- The loss is
  `log(sum(exp(pos / T)) / (sum(exp(pos / T)) + sum(exp(neg / T))))`
  plus `lambda` times the gradient penalty.

- The gradient penalty is computed on the sets of the last sub-window.
  The norm of the input gradient of the summed encodings is taken over
  the points of a set, for every set and feature, as in the reference.

- The MCD of two sub-windows in the test is the mean distance between
  the representations of their `k` sets, over pairs of different
  indices.

- The first window only trains. From then on every window is tested with
  the encoder and threshold of the previous one, and then trained.

The data are not scaled, as in the reference; the noise levels refer to
the units of the data. The network, its gradients (including the second
derivatives of the gradient penalty) and Adam are written in R.

## References

Wan, K., Liang, Y. and Yoon, S. (2024). Online drift detection with
maximum concept discrepancy. *Proceedings of the 30th ACM SIGKDD
Conference on Knowledge Discovery and Data Mining*, 2924-2935.
<doi:10.1145/3637528.3672016>

## See also

[`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
is a different detector (mean comparison).

## Examples

``` r
# \donttest{
library(daltoolbox)
library(heimdall)
# Mixture of two Gaussians whose weight changes at row 4001.
set.seed(1)
mixture <- function(n, p) {
  wide <- stats::runif(n) > p
  matrix(stats::rnorm(n * 3, 20, ifelse(wide, 50, 10)), n)
}
stream <- as.data.frame(rbind(mixture(4000, 0.2), mixture(2000, 0.8)))
detector <- dfr_mcd(window_size = 1000, m = 30, seed = 1)
model <- fit(detector, stream)
output <- model$drifter_output
which(output$mcd > output$threshold)
#> [1] 4100
# }
```
