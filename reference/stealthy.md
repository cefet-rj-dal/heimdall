# Stealthy

Ancestor class for drift adaptive models. It wraps a predictive model
and a drift detector, retraining the model whenever a drift is reported.

## Usage

``` r
stealthy(
  model,
  drift_method,
  norm_class = daltoolbox::zscore(),
  warmup_size = 100,
  th = 0.5,
  target_uni_drifter = FALSE,
  incremental_memory = FALSE,
  active_warmup = FALSE,
  class_balance = "inactive",
  obsolete_model = "null",
  reporting = FALSE,
  history_size = NULL,
  verbose = FALSE
)
```

## Arguments

- model:

  The algorithm object to be used for predictions

- drift_method:

  The algorithm object to detect drifts

- norm_class:

  Class used to perform normalization

- warmup_size:

  Number of rows used to warmup the drifter. No drift will be detected
  during this phase

- th:

  The threshold to be used with classification algorithms

- target_uni_drifter:

  Passes the prediction target to the drifts as the target feat when the
  drifter is univariate and dist_based. The target must be binary
  (logical, 0/1 or a factor with those labels).

- incremental_memory:

  If true, the model will retrain with all available data whenever the
  fit is called. If false, it only retrains when a drift is detected.

- active_warmup:

  If true, the models will be retrained for each batch while the number
  of rows for the warmup_size is not met.

- class_balance:

  Class balancing strategy. `"buffer"` sets a buffer with the last
  `warmup_size/2` rows for each class. `"inactive"` does nothing.

- obsolete_model:

  Strategy used by [`predict()`](https://rdrr.io/r/stats/predict.html)
  while no model is fitted. One of `"null"` (default, returns all
  `FALSE`), `"majority"` (majority class classifier) or `"last_model"`
  (keeps using the previous model).

- reporting:

  If TRUE, every batch given to
  [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html) (`x` and `y`)
  is accumulated in `drifter_input`.

- history_size:

  Maximum number of rows kept in `drifter_output` and `drifter_input`;
  the oldest rows are dropped first. `NULL` (the default) keeps the
  whole stream, so set it on long streams to bound memory.

- verbose:

  if TRUE shows drift messages

## Value

Stealthy object

## Details

The drifter decides what it monitors through its own `target_feat`,
given in terms of the one-hot encoded and normalized columns: a
univariate drifter with a `target_feat` receives that column, a
univariate drifter without one receives the row mean of every column,
and multivariate, multi-criteria and dummy drifters receive the selected
columns (every column when their `target_feat` is `NULL`).

The batch is evaluated before the model is updated: error-based
detectors see prequential residuals, computed with the model as it was
at the beginning of the call, and the detector is fed the incoming batch
exactly once per call.

`obj$drifter_output` holds the diagnostics reported by the drifter (for
example the window width of ADWIN or the p-values of KSWIN; see the
`drifter_output` of each detector), with one row per row of `x` across
every call to [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html).
Rows are `NA` while the drifter is not monitoring (during the warmup, or
after a drift until the model is retrained) and for observations it
skipped, such as error-based rows without a prediction.

## Examples

``` r
# See ?dfr_ddm for a drift detector that can be plugged here, and the
# online prediction walkthroughs at
# https://github.com/cefet-rj-dal/heimdall/tree/main/examples/2_online_prediction
```
