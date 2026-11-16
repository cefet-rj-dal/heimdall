# Equal intensity k-means space partitioning (EI-kMeans)

Histogram-based two-sample test for drift detection (Liu et al., 2021).
The feature space is partitioned by k-means clusters of (nearly) equal
intensity, so that no bin straddles a gap between clusters of the data,
and a new sample is compared with the training sample by Pearson's
chi-square test on the counts of the two samples in every bin.

## Usage

``` r
eikmeans(
  training,
  n_test,
  k = 40,
  alpha = 0.05,
  min_points = 50,
  n_build = 2000,
  thetas = seq(0, 0.95, by = 0.05)
)
```

## Arguments

- training:

  The training sample (matrix or data frame of numeric features).

- n_test:

  Size of the samples that will be tested. The number of bins is at most
  `n_test / 5`, so that the expected count of every bin in a test sample
  is at least 5.

- k:

  Largest number of bins.

- alpha:

  Significance level of the chi-square test.

- min_points:

  Smallest number of training points per bin (`beta`).

- n_build:

  Number of training points used to build the partitioning: the first
  `n_build` rows. The counts of the bins are then taken from the whole
  training sample.

- thetas:

  Values of the shape parameter `theta` of the amplify-shrink
  coefficients `exp(theta (r - 1))`, tried in order.

## Value

An `eikmeans` object with the centroids (`centers`), the amplify-shrink
coefficients (`amplify`), the training counts of every bin (`counts`)
and the settings.

## Details

The partitioning follows the reference code
(<https://github.com/Anjin-Liu/TCYB2019-EIkMeansDriftDetection>):

1.  The number of bins starts at `min(k, n_test / 5, n / 50)` (`n` is
    the training size).

2.  Greedy equal intensity initialization (Algorithm 1): `K - 1` times,
    the point whose `n_build / K`-th nearest neighbour is farthest is
    taken together with its `n_build / K` nearest neighbours as a group,
    and the group is removed; the remaining points are the last group.
    The first point of every group starts k-means (Lloyd's algorithm,
    with the stopping rule of scikit-learn).

3.  Amplify-shrink (Algorithm 2): with intensity ratios `r` (points per
    cluster over `n_build / K`), a point goes to the cluster that
    minimises its distance times `exp(theta (r - 1))`, so crowded
    clusters shrink and sparse ones grow. The ratios are updated after
    every `theta`, and the search stops when every cluster has more than
    `min_points` points. If no `theta` is enough, the number of bins is
    lowered by one and the search repeated.

4.  A test sample is assigned to the bins in the same way, and the 2 x K
    table of training and test counts is tested with Pearson's
    chi-square test (Algorithm 3); bins empty in both samples are left
    out, and the Yates correction is used with one degree of freedom, as
    in `scipy.stats.chi2_contingency`.

The reference code differs from the paper in a few points, and the code
is followed: `theta` ranges over 0, 0.05, ..., 0.95 (the paper gives up
to 1.5), the initialization uses the distance to the `n / K`-th
neighbour (the paper describes the nearest one) and starts k-means from
a point of each group (the paper, from the group mean), the starting
number of bins depends on the test size, and the partitioning is built
on the first 2000 training points. If the number of bins reaches 1, the
reference falls back to `k` plain k-means clusters; so does this
implementation.

## References

Liu, A., Lu, J. and Zhang, G. (2021). Concept drift detection via equal
intensity k-means space partitioning. *IEEE Transactions on
Cybernetics*, 51(6), 3198-3211. <doi:10.1109/TCYB.2020.2983962>

## See also

[`eikmeans_test()`](https://cefet-rj-dal.github.io/heimdall/reference/eikmeans_test.md)
to test a sample and
[`dfr_eikmeans()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_eikmeans.md)
to monitor a stream.

## Examples

``` r
# Two clusters; the right one moves.
set.seed(1)
cluster <- function(n, x) cbind(stats::rnorm(n, x), stats::rnorm(n))
training <- rbind(cluster(1000, 0), cluster(1000, 5))
model <- eikmeans(training, n_test = 200)
nrow(model$centers)
#> [1] 22
eikmeans_test(model, rbind(cluster(100, 0), cluster(100, 5)))
#>   statistic df   p_value drift
#> 1  16.25849 21 0.7549563 FALSE
eikmeans_test(model, rbind(cluster(100, 0), cluster(100, 5.8)))
#>   statistic df      p_value drift
#> 1  75.42494 21 4.626624e-08  TRUE
```
