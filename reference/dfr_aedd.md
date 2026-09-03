# Autoencoder-Based Drift Detection method

AEDD is an unsupervised multivariate detector that compares
reconstruction errors produced by an autoencoder on reference and recent
windows. Because it monitors changes in the input distribution rather
than classifier performance, this implementation is primarily aimed at
**virtual concept drift**. The method follows Kaminskyi, Li, and Muller
(2022) <doi:10.1109/ICDMW58026.2022.00109>.

## Usage

``` r
dfr_aedd(
  encoding_size,
  ae_class = NULL,
  batch_size = 32,
  num_epochs = 1000,
  learning_rate = 0.001,
  window_size = 100,
  monitoring_step = 1700,
  criteria = "mann_whitney",
  alpha = 0.01,
  reporting = FALSE
)
```

## Arguments

- encoding_size:

  Encoding Size

- ae_class:

  Autoencoder Class. When `NULL` (the default),
  `daltoolbox::autoenc_encode_decode` is used.

- batch_size:

  Batch Size for batch learning

- num_epochs:

  Number of Epochs for training

- learning_rate:

  Learning Rate

- window_size:

  Size of the most recent data to be used

- monitoring_step:

  The number of rows that the drifter waits to be is updated

- criteria:

  The method to be used to check if there is a drift. One of
  `'mann_whitney'` (default), `'kolmogorov_smirnov'`, `'levene'`,
  `'parametric_threshold'` or `'nonparametric_threshold'`

- alpha:

  The significance threshold for the statistical test used in criteria

- reporting:

  If TRUE, some data are returned as norm_x_oh, drift_input, hist_proj,
  and recent_proj.

## Value

`dfr_aedd` object

## References

Kaminskyi, D., Li, B., and Muller, E. (2022). Reconstruction-based
unsupervised drift detection over multivariate streaming data. In *2022
IEEE International Conference on Data Mining Workshops (ICDMW)*.
<doi:10.1109/ICDMW58026.2022.00109>

## Examples

``` r
# A rendered walkthrough is available at
# https://github.com/cefet-rj-dal/heimdall/blob/main/examples/dfr_aedd.md
```
