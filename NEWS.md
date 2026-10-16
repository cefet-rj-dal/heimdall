# heimdall 1.4.0

## Breaking changes

* `update_state()` now returns a `list(obj =, drift =)` for *every* detector,
  including the base `drifter` method and `dfr_inactive()`. Previously those
  returned the object itself, which broke the streaming loop shown in the
  examples.
* `dfr_kswin()`, `dfr_mcdd()`, `dfr_lbdd()`, `dfr_kldist()` and `dfr_aedd()`
  gained a `window_type` argument. `"sliding"` (the new default) keeps at most
  `window_size` observations; `"anchored"` keeps every observation, pinning the
  reference half to the beginning of the stream. The 1.3.x behaviour of these
  four detectors corresponds to `"anchored"`.
* The `dfr_aedd()` values `window_type = "fixed"` and `"moving"` are deprecated
  aliases for `"sliding"` and `"anchored"`. They still work and warn.
* `dfr_aedd()` takes the autoencoder settings through `ae_params` (a named
  list forwarded to `ae_class`, plus `error_function`), gained the `"psi"` and
  `"diff_threshold"` criteria, and no longer has a `reporting` argument.
* `dfr_aedd()` no longer accepts an autoencoder class that is neither an
  encoder nor an encoder-decoder; it fails with an explicit message instead of
  reaching an undefined variable.
* `dfr_kldist()` is now multivariate: it consumes a data frame one row at a
  time, every numeric column contributes to the score, and it no longer takes
  a `target_feat` argument. It gained a `breaks` argument.
* `dfr_mcdd()` gained `normality_alpha` (default `0.05`). The Shapiro-Wilk
  threshold no longer reuses `alpha`, which at its default of `1e-08` made the
  normality gate a no-op.
* `reset_state()` on the window-based detectors restarts with an empty window.
  Together with the sliding policy, which tests as soon as the window is full
  (one observation earlier than 1.3.x), this moves the detection positions of
  `dfr_kswin()`, `dfr_lbdd()` and `dfr_mcdd()`.
* `stealthy()` gained back a `reporting` argument (default `FALSE`). The
  accumulation of `drifter_input` and `drifter_output` across every batch is
  now opt-in, since those frames grow with the whole stream.
* `fit()` on a univariate detector now monitors only `target_feat` when it is
  given a data frame. It used to concatenate every column into a single
  stream. A data frame with several columns and no `target_feat` is now an
  error instead of being silently concatenated.
* `update_state()` rejects ambiguous input instead of guessing: vectors with
  more than one value, text that is not a number, and, for error-based
  detectors, values other than `0`, `1`, `TRUE` or `FALSE`. Factors are read
  through their labels (previously through their internal codes), and
  infinite values are treated as missing, with a warning.
* `dfr_cusum()` was reimplemented after MOA's `CusumDM`, with the arguments
  `lambda` (now 50), `delta` and `min_instances`. The previous version added
  `+1` per error and `-1` per correct prediction, so it could only detect
  error rates above 50%.
* `dfr_multi_criteria()` no longer keeps `obj$drifts`, which grew with every
  observation. Use `obj$detections` (alarms per detector) and
  `obj$last_drifts` (decisions on the last observation) instead. `"and"` now
  accepts detections up to `fuzzy_window` observations apart (it used to
  require them on the same observation, which almost never happens), and
  both `"and"` and `"fuzzy"` report a change once instead of on several
  consecutive observations. The `"fuzzy"` combination now costs the same per
  observation regardless of the length of the stream.
* New defaults, calibrated by simulation to keep false alarms rare:
  `dfr_kswin(window_size = 300, stat_size = 100, alpha = 1e-4, exact = NULL)`,
  `dfr_lbdd(window_size = 300, alpha = 0.001)`,
  `dfr_mcdd(window_size = 300, alpha = 1e-4)`,
  `dfr_ecdd(average_run_length = 400)` and
  `dfr_aedd(monitoring_step = window_size / 2)`. The previous windows of 1500
  observations were three times longer than the bundled example data.
* `dfr_aedd()` now uses `daltoolboxdp::autoenc_ed` as its default
  autoencoder; the previous default no longer exists in `daltoolbox`.
* `st_drift_examples` now holds six synthetic datasets (`uv_virtual_drift`,
  `uv_vct_real_drift`, `uv_vdp_real_drift`, `mv_real_drift`,
  `mv_vct_real_drift` and `mv_vdp_real_drift`). The former `univariate` and
  `dataset1` are available as `uv_virtual_drift` and `mv_real_drift`.

## New features

* Every detector stores the per-observation diagnostics of its last `fit()`
  call in `obj$drifter_output` (for example the window width of ADWIN, the
  p-values of KSWIN and MCDD, or the divergence of `dfr_kldist()`).
* `dfr_page_hinkley()` gains `direction = c("increase", "decrease", "both")`.
  The default keeps the previous one-sided behaviour.
* New `dummy()` ancestor class for the baseline detectors `dfr_inactive()` and
  `dfr_passive()`.

## Bug fixes

### Adaptive model

* `fit.stealthy()` computed the error-based residuals against `norm_x_oh`,
  a variable that had been set to `NULL` a few lines earlier (or never
  defined). Every error-based detector received a null input. The batch is now
  projected once, through `.stealthy_project_batch()`, and used directly.
* The drift check now runs *before* the model is updated, so error-based
  detectors observe prequential residuals instead of training error.
* The detector is fed the incoming batch exactly once per call. The previous
  version fitted it with the new batch and then again with the whole
  accumulated `x_train`, and finally cleared `drift_method$drifted`, discarding
  the second signal.
* `predict.stealthy()` with `obsolete_model = "majority"` referenced an
  undefined `model` variable and a `norm_data_oh` that had not been computed
  yet, and called an unimported `cla_majority`. `obsolete_model` is now
  validated with `match.arg()`.
* `stealthy()` initialised `train_model` only when `class_balance = "buffer"`.
* `stealthy(target_uni_drifter = TRUE)` works with factor targets; it used to
  feed `NA` to the detector, which then never detected anything.
* `stealthy()` now honours `monitored_features`, which was ignored.

### Detectors

* The sliding windows slide again. In 1.3.x `dfr_kswin()`, `dfr_mcdd()`,
  `dfr_kldist()` and `dfr_lbdd()` appended every observation without ever
  trimming, so memory grew without bound and the reference half stayed frozen
  on the first observations of the stream.
* `dfr_hddm()` now detects increases of the error rate. A misplaced
  parenthesis in the bound that tracks the minimum error rate made the
  detector update that minimum on almost every observation, so it practically
  never reported a drift.
* `dfr_page_hinkley()` compares `sum - min_sum` against the threshold again.
  1.3.x compared `sum` alone, which is not the Page-Hinkley test, and left
  `min_sum` computed but unused.
* `dfr_page_hinkley()` skips missing observations again. The `tryCatch` that
  replaced the `NA` guard let `NA` into the running mean and returned `NULL`
  from `update_state()` on any error, breaking the caller's loop.
* `dfr_mcdd()` applies exactly one comparison test per evaluation. 1.3.x fell
  through from the t-test to the Wilcoxon test, giving two uncorrected chances
  to reject.
* `dfr_kswin()` requires both a significant p-value and a KS statistic above
  `sqrt(-log(alpha) / stat_size)`, as in Raab et al., and subsamples its
  reference window again.
* `dfr_kswin()` rejects `window_size == stat_size`, which crashed at the first
  test.
* `dfr_kldist()` divided its score by the number of features *plus one*,
  because the temporary `bin` column was counted as a feature.
* `dfr_ecdd()` used the last coefficient of the ARL 1000 polynomial (987.23)
  in the ARL 400 control limit; it now uses 848.18, as in the reference
  implementation.
* `dfr_ecdd()` no longer fails with `object 'control_limit' not found` for
  average run lengths above 1000; the argument is validated up front.
* `dfr_cusum()` and `dfr_ecdd()` restart after a drift, so they report each
  change once instead of on every following observation when `reset_state()`
  is not called.
* `dfr_aedd()`, criterion `parametric_threshold`, compared the history mean
  against itself and could never fire.
* `dfr_aedd()`, criterion `psi`, trimmed a non-existent `state$window` field.
* `dfr_aedd()`, criterion `levene`, tested only the first reconstructed
  feature. It now tests every feature and combines the p-values with a
  Bonferroni correction.
* `dfr_aedd()` no longer builds a throwaway autoencoder with `input_size = 1`
  in the constructor, and no longer keeps its whole history in `state$data`
  under the sliding policy. It retrains the autoencoder when new columns
  appear, warns the first time it has to clip values outside `[0, 1]` and no
  longer fails on missing values.
* `dfr_aedd()` and `dfr_hddm()` no longer latch the per-call drift flag. The
  `drift` element refers to the current call; `obj$drifted` stays sticky until
  `reset_state()`.
* `fit.dfr_passive()` iterated over `2:length(data)`, which is the column count
  for a data frame. All `fit()` methods now walk the stream through shared
  helpers.
* `fit()` no longer breaks on a stream with a single observation, and rejects
  an empty one with a clear message.
* Missing values are handled consistently: error-based detectors treat `NA` as
  a correct prediction, distribution-based detectors skip the observation.
* Warm-start data (`data =`) longer than `window_size` is truncated to the
  most recent observations under the sliding policy; it used to make the
  window permanently longer.
* `dfr_multi_criteria()` fails with a clear message when a detector's
  `target_feat` is missing from the data, instead of never detecting.

### Metrics and normalization

* `mt_fscore()` uses its `f` argument as the beta of the F-beta score. It
  previously stored the parameter and always computed F1. The same formula was
  wrong in the twelve online-prediction example scripts, which have been fixed.
* Precision, recall, F-score and accuracy accept factor and character labels.
  With a factor `y_true`, precision returned 1.5 and recall `NaN`. The metrics
  now also refuse vectors of different lengths instead of recycling them.
* `mt_rocauc()` drops observations with missing scores, with a warning,
  instead of scoring them as zero.
* `inverse_transform.nrm_memory()` inverts the data it is given instead of the
  internal history.
* `nrm_memory()` no longer drops repeated rows from its history, which
  distorted the normalization (a z-score of 1.79 came out as 0.71).

## Validation and documentation

* Count parameters must be whole numbers, `dfr_eddm()` requires
  `warning_level >= out_control_level`, and `target_feat` must be a single
  column name.
* `nrm_base()` objects keep the `dal_base` class.
* The help pages state which changes each detector cannot see: DDM and HDDM
  only report increases of the error rate, LBDD only changes of spread, MCDD
  only changes of location.
* `?st_drift_examples` documents every bundled dataset.
* `dfr_adwin()` is now implemented in pure R. It no longer needs `reticulate`,
  Python or `numpy`, and a `dfr_adwin` object can now be saved with
  `saveRDS()`. The port follows the scikit-multiflow implementation the
  previous version wrapped, so detections are expected to be the same.

## Dependencies

* `caret` and `Metrics` were dropped from `Imports`. One-hot encoding in
  `stealthy()` is now done internally with the same column names produced by
  `caret::dummyVars()`, and precision and recall are computed internally.
* `reticulate` was dropped from `Suggests`, together with the bundled
  `adwin.py` module. `daltoolboxdp` was added to `Suggests` for the default
  autoencoder of `dfr_aedd()`.

## Documentation and infrastructure

* New vignettes: `vignette("heimdall")` and `vignette("adaptive-models")`.
* Examples are organised in `examples/1_detection` and
  `examples/2_online_prediction`, with synthetic datasets and online
  prediction walkthroughs for every detector.
* New tests cover every detector, `stealthy()`, `dfr_multi_criteria()`, the
  metrics and the internal helpers, plus regression tests that pin the
  detection positions of every detector on fixed streams.
* New GitHub Actions workflows for test coverage and linting.
