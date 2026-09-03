# Stealthy

Ancestor class for drift adaptive models. It wraps a predictive model
and a drift detector, retraining the model whenever a drift is reported.

## Usage

``` r
stealthy(
  model,
  drift_method,
  monitored_features = NULL,
  norm_class = daltoolbox::zscore(),
  warmup_size = 100,
  th = 0.5,
  target_uni_drifter = FALSE,
  incremental_memory = TRUE,
  verbose = FALSE,
  reporting = FALSE
)
```

## Arguments

- model:

  The algorithm object to be used for predictions

- drift_method:

  The algorithm object to detect drifts

- monitored_features:

  List of features that will be monitored by the drifter

- norm_class:

  Class used to perform normalization

- warmup_size:

  Number of rows used to warmup the drifter. No drift will be detected
  during this phase

- th:

  The threshold to be used with classification algorithms

- target_uni_drifter:

  Passes the prediction target to the drifts as the target feat when the
  drifter is univariate and dist_based.

- incremental_memory:

  If true, the model will retrain with all available data whenever the
  fit is called. If false, it only retrains when a drift is detected.

- verbose:

  if TRUE shows drift messages

- reporting:

  If TRUE, some data are returned as norm_x_oh, drift_input, hist_proj,
  and recent_proj.

## Value

Stealthy object

## Examples

``` r
# See ?dfr_ddm for an example of a drift detector that can be plugged here.
```
