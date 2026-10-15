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
  monitoring_step = floor(window_size/2),
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
  [`daltoolboxdp::autoenc_ed`](https://rdrr.io/pkg/daltoolboxdp/man/autoenc_ed.html)
  is used, which requires the suggested package `daltoolboxdp` and its
  Python backend.

- batch_size:

  Batch Size for batch learning

- num_epochs:

  Number of Epochs for training

- learning_rate:

  Learning Rate

- window_size:

  Number of most recent rows kept by the detector. The oldest half is
  the reference window and the newest half the recent window.

- monitoring_step:

  Number of rows between two consecutive checks. The default checks
  twice per window.

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

## Details

The autoencoder expects inputs in the `[0, 1]` range. Values outside
that range are clipped to it, and the detector warns the first time this
happens, so the data should be normalized beforehand (for example with
[`daltoolbox::minmax()`](https://rdrr.io/pkg/daltoolbox/man/minmax.html)).
`ae_class` must be a constructor accepting the arguments `input_size`,
`encoding_size`, `batch_size`, `num_epochs` and `learning_rate`, whose
objects implement [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html)
and [`transform()`](https://rdrr.io/pkg/daltoolbox/man/transform.html).

The autoencoder is trained on the reference window the first time the
window is full, and again after every drift or whenever new columns
appear.

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
