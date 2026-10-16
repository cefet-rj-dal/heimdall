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
  ae_params = list(batch_size = 32, num_epochs = 1000, learning_rate = 0.001,
    error_function = "mae"),
  window_size = 100,
  window_type = "sliding",
  monitoring_step = floor(window_size/2),
  criteria = "mann_whitney",
  alpha = 0.01,
  threshold = 0.01,
  target_feat = NULL
)
```

## Arguments

- encoding_size:

  Encoding Size

- ae_class:

  Autoencoder constructor. When `NULL` (the default),
  [`daltoolboxdp::autoenc_ed`](https://rdrr.io/pkg/daltoolboxdp/man/autoenc_ed.html)
  is used, which requires the suggested package `daltoolboxdp` and its
  Python backend.

- ae_params:

  Named list of arguments forwarded to `ae_class`, plus
  `error_function`, which selects how the reconstruction error is
  summarised. One of `"mae"` (default) or `"rmse"`.

- window_size:

  Number of most recent rows kept by the detector. The oldest half is
  the reference window and the newest half the recent window.

- window_type:

  Window policy. `"sliding"` (default) keeps at most `window_size` rows,
  so both halves move with the stream. `"anchored"` keeps every row,
  which pins the reference half to the beginning of the stream and makes
  memory grow with the stream length.

- monitoring_step:

  Number of rows between two consecutive checks. The default checks
  twice per window.

- criteria:

  The method used to check for a drift. One of `"mann_whitney"`
  (default), `"kolmogorov_smirnov"`, `"levene"`, `"psi"`,
  `"parametric_threshold"`, `"nonparametric_threshold"` or
  `"diff_threshold"`.

- alpha:

  Significance level of the hypothesis-test criteria (`"mann_whitney"`,
  `"kolmogorov_smirnov"` and `"levene"`).

- threshold:

  Drift threshold of the `"psi"` and `"diff_threshold"` criteria,
  compared directly against the divergence or the median error
  difference. It is a raw threshold, not a probability.

- target_feat:

  Names of the columns to be monitored. `NULL` (the default) monitors
  every numeric column.

## Value

`dfr_aedd` object

## Details

The autoencoder expects inputs in the `[0, 1]` range. Values outside
that range are clipped to it, and the detector warns the first time this
happens, so the data should be normalized beforehand (for example with
[`daltoolbox::minmax()`](https://rdrr.io/pkg/daltoolbox/man/minmax.html)).
`ae_class` must be a constructor accepting the arguments `input_size`
and `encoding_size` plus the entries of `ae_params`, whose objects
implement [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html) and
[`transform()`](https://rdrr.io/pkg/daltoolbox/man/transform.html).

The autoencoder is trained on the reference window the first time the
window is full, and again after every drift or whenever new columns
appear.

The `"levene"` criterion tests every reconstructed feature and combines
the per-feature p-values with a Bonferroni correction, instead of
looking at the first feature only.

## References

Kaminskyi, D., Li, B., and Muller, E. (2022). Reconstruction-based
unsupervised drift detection over multivariate streaming data. In *2022
IEEE International Conference on Data Mining Workshops (ICDMW)*.
<doi:10.1109/ICDMW58026.2022.00109>

## Examples

``` r
# Training an autoencoder is too slow for an inline example. A rendered
# walkthrough is available at
# https://github.com/cefet-rj-dal/heimdall/blob/main/examples/1_detection/dfr_aedd.md
```
