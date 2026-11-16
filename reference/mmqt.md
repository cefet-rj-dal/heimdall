# Multimodal QuantTree (MMQT)

Batch-wise change detector for stationary conditions made of several
*modalities* (Stucchi et al., 2023). Every stationary batch is drawn
from one of a set of unknown distributions, for instance the operating
modes of a machine, and a change is any batch that comes from none of
them. MMQT builds a single QuantTree histogram over the training
batches, clusters their bin-probability vectors to find the modalities,
and tests each new batch against the modality closest to it with a
modality-specific threshold that controls the false positive rate.

## Usage

``` r
mmqt(
  training,
  nu = NULL,
  bins = 16,
  alpha = 0.05,
  n_modalities = NULL,
  alpha_modalities = 0.05,
  max_modalities = 10,
  calibration = TRUE,
  n_repeats = 10,
  n_sim = 10000,
  partition = NULL,
  seed = NULL
)
```

## Arguments

- training:

  The training set: a list of batches (matrices or data frames with the
  same columns and `nu` rows each), or a single matrix or data frame
  that is cut into consecutive batches of `nu` rows (an incomplete last
  batch is dropped).

- nu:

  Number of observations per batch. Taken from the batches when
  `training` is a list.

- bins:

  Number of bins `K` of the histogram, built with uniform target
  probabilities `1 / K`.

- alpha:

  Target false positive rate of the test.

- n_modalities:

  Number of modalities `M`. `NULL` (the default) estimates it with
  Algorithm 1 of the paper: the smallest `M` whose detector has an
  empirical false positive rate on the training batches not above
  `alpha_modalities`.

- alpha_modalities:

  Target false positive rate `alpha'` used to estimate the number of
  modalities.

- max_modalities:

  Largest number of modalities tried by the estimation.

- calibration:

  Whether to calibrate the bin probabilities of each modality (Eqs.
  12-13), which corrects for a histogram built on the data of all
  modalities.

- n_repeats:

  Number of times the estimation is repeated, each with a new histogram.
  The number of modalities is the most frequent estimate, and the
  histogram the one whose training false positive rate is closest to
  `alpha_modalities` among those that gave it. Ignored when
  `n_modalities` is given.

- n_sim:

  Number of Monte Carlo simulations used to compute each threshold.

- partition:

  A partitioning from another `mmqt` model (`model$partition`) to reuse
  instead of building a new one.

- seed:

  Seed of a private random number stream, so that training does not
  change the global one. `NULL` uses the global generator.

## Value

An `mmqt` object with the partitioning (`partition`), the reference bin
probabilities of each modality (`reference`, one row per modality),
their thresholds (`thresholds`), the number of training batches of each
modality (`n_batches`), the false positive rate on the training batches
(`training_fpr`) and, when the number of modalities was estimated, the
estimates of every repetition (`estimation`).

## Details

**Histogram.** The partitioning is a QuantTree (Boracchi et al., 2018),
ported from the reference code: each bin is cut from the remaining space
along a random coordinate, at the sample quantile that leaves the target
share of the remaining training points in the bin. Its bin probabilities
have a distribution that does not depend on the data, so the threshold
of the total variation statistic `T = 0.5 * sum(|p - pi|)` can be
computed by simulation for any data (Frittoli et al., 2022): the bin
probabilities of the histogram are drawn from their Dirichlet
distribution and the counts of a batch from a multinomial, `n_sim`
times, and the threshold is the `1 - alpha` quantile of the simulated
statistic.

**Modalities.** The bin-probability vectors of the training batches are
clustered by k-means (k-means++ initialization, ten starts). The
centroid of a cluster estimates the bin probabilities `pi^m` of a
modality, which are calibrated as in Eq. 12, and the modality gets the
threshold of a QuantTree with probabilities `pi^m` built on the `nu N_m`
points of its `N_m` batches.

**Monitoring.** A batch is assigned to the modality whose (calibrated)
bin probabilities are closest in Euclidean distance (Eq. 7), and a
change is detected when its statistic against that modality exceeds the
modality's threshold (Eqs. 8-9).

The calibration needs, for every bin, the training points of the
modality closest to the bin boundary inside and outside the bin. When
the modality has no point on one of the two sides, `g` is set to 0.

The reference repository (<https://github.com/diegocarrera89/quantTree>)
implements QuantTree, but its MMQT is announced as "coming soon"; MMQT
is therefore implemented from the paper.

## References

Stucchi, D., Magri, L., Carrera, D. and Boracchi, G. (2023). Multimodal
batch-wise change detection. *IEEE Transactions on Neural Networks and
Learning Systems*, 34(10), 6783-6797. <doi:10.1109/TNNLS.2023.3294846>

Boracchi, G., Carrera, D., Cervellera, C. and Maccio, D. (2018).
QuantTree: histograms for change detection in multivariate data streams.
*Proceedings of the 35th International Conference on Machine Learning*,
PMLR 80, 639-648.

## See also

[`mmqt_test()`](https://cefet-rj-dal.github.io/heimdall/reference/mmqt_test.md)
to test batches and
[`dfr_mmqt()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mmqt.md)
to monitor a stream.

## Examples

``` r
# Two operating modes; a change moves the first one.
set.seed(1)
mode <- function(n, center) matrix(stats::rnorm(2 * n, center), ncol = 2)
training <- c(lapply(1:20, function(i) mode(64, 0)), lapply(1:20, function(i) mode(64, 4)))
model <- mmqt(training, bins = 8, n_repeats = 2, n_sim = 2000, seed = 1)
model$n_modalities
#> [1] 2
mmqt_test(model, list(mode(64, 0), mode(64, 4), mode(64, 1.5)))
#>   modality  statistic threshold drift
#> 1        2 0.06758636 0.1603606 FALSE
#> 2        1 0.03124695 0.1604548 FALSE
#> 3        2 0.62441452 0.1603606  TRUE
```
