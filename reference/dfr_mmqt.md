# Multimodal QuantTree stream detector

Monitors a stream with a Multimodal QuantTree
([`mmqt()`](https://cefet-rj-dal.github.io/heimdall/reference/mmqt.md)):
the observations are grouped in consecutive batches of `nu` rows, the
first `n_train_batches` batches train the model, and every later batch
is tested. Since it monitors the distribution of the observed features,
it is primarily used for **virtual concept drift**.

## Usage

``` r
dfr_mmqt(target_feat = NULL, nu = 128, n_train_batches = 64, model = NULL, ...)
```

## Arguments

- target_feat:

  Names of the columns to be monitored. `NULL` (the default) monitors
  every numeric column.

- nu:

  Number of observations per batch.

- n_train_batches:

  Number of batches used for training. Ignored when `model` is given.

- model:

  A trained
  [`mmqt()`](https://cefet-rj-dal.github.io/heimdall/reference/mmqt.md)
  model. When given, every batch is tested and
  [`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
  keeps the model.

- ...:

  Further arguments for
  [`mmqt()`](https://cefet-rj-dal.github.io/heimdall/reference/mmqt.md)
  (`bins`, `alpha`, `n_modalities`, `alpha_modalities`,
  `max_modalities`, `calibration`, `n_repeats`, `n_sim`, `seed`).

## Value

`dfr_mmqt` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds, on the last observation of every tested batch, the assigned
modality, the statistic and the threshold, and `obj$model` the trained
model.

## Details

Each batch is tested on its own (MMQT is a one-shot test, with a false
positive rate `alpha` per batch), so the detector reports a drift on the
last observation of every batch whose statistic exceeds the threshold of
its modality.
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
forgets the model it trained and trains a new one on the next
`n_train_batches` batches. Rows with missing values are skipped.

## References

Stucchi, D., Magri, L., Carrera, D. and Boracchi, G. (2023). Multimodal
batch-wise change detection. *IEEE Transactions on Neural Networks and
Learning Systems*, 34(10), 6783-6797. <doi:10.1109/TNNLS.2023.3294846>

Boracchi, G., Carrera, D., Cervellera, C. and Maccio, D. (2018).
QuantTree: histograms for change detection in multivariate data streams.
*Proceedings of the 35th International Conference on Machine Learning*,
PMLR 80, 639-648.

## Examples

``` r
library(daltoolbox)
library(heimdall)
# Batches alternate between two modes; from batch 50 the first mode moves.
set.seed(1)
batch <- function(center) data.frame(x = stats::rnorm(64, center), y = stats::rnorm(64, center))
centers <- c(rep(c(0, 4), 25), rep(c(1.5, 4), 10))
stream <- do.call(rbind, lapply(centers, batch))
detector <- dfr_mmqt(nu = 64, n_train_batches = 40, bins = 8, n_repeats = 2, n_sim = 2000, seed = 1)
model <- fit(detector, stream)
model$model$n_modalities
#> [1] 2
which(model$drifter_output$statistic > model$drifter_output$threshold) %/% 64
#>  [1] 51 53 55 57 59 61 63 65 67 69
```
