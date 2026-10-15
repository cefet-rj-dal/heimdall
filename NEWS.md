# heimdall (development version)

## Breaking changes

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
  `dfr_kldist(p_th = 0.5)`, `dfr_kswin(window_size = 300, stat_size = 100,
  alpha = 1e-4, exact = NULL)`, `dfr_lbdd(window_size = 300, alpha = 0.001)`,
  `dfr_mcdd(window_size = 300, alpha = 1e-4)`,
  `dfr_ecdd(average_run_length = 400)` and
  `dfr_aedd(monitoring_step = window_size / 2)`. The previous windows of 1500
  observations were three times longer than the bundled example data.
* `dfr_aedd()` now uses `daltoolboxdp::autoenc_ed` as its default
  autoencoder; the previous default no longer exists in `daltoolbox`.

## Bug fixes

* `dfr_hddm()` now detects increases of the error rate. A misplaced
  parenthesis in the bound that tracks the minimum error rate made the
  detector update that minimum on almost every observation, so it practically
  never reported a drift.
* `dfr_kldist()` smooths the histograms with one pseudo-count per bin. With
  the previous smoothing (`1e-12`), any empty bin made the divergence explode,
  so the detector raised about 190 false alarms per 10,000 stationary
  observations whatever the threshold.
* `dfr_ecdd()` used the last coefficient of the ARL 1000 polynomial (987.23)
  in the ARL 400 control limit; it now uses 848.18, as in the reference
  implementation.
* `dfr_cusum()` and `dfr_ecdd()` restart after a drift, so they report each
  change once instead of on every following observation when `reset_state()`
  is not called.
* `dfr_aedd()` no longer grows its window without bound (with the default
  `monitoring_step`, 4000 rows left 3998 stored for a window of 100), its
  reference and recent windows no longer share a row, and it retrains the
  autoencoder when new columns appear. It
  warns the first time it has to clip values outside `[0, 1]` and no longer
  fails on missing values.
* Precision, recall, F-score and accuracy accept factor and character labels.
  With a factor `y_true`, precision returned 1.5 and recall `NaN`. The metrics
  now also refuse vectors of different lengths instead of recycling them.
* `mt_rocauc()` drops observations with missing scores, with a warning,
  instead of scoring them as zero.
* `nrm_memory()` no longer drops repeated rows from its history, which
  distorted the normalization (a z-score of 1.79 came out as 0.71).
* `stealthy(target_uni_drifter = TRUE)` works with factor targets; it used to
  feed `NA` to the detector, which then never detected anything.
* `stealthy()` now honours `monitored_features`, which was ignored.
* Warm-start data (`data =`) longer than `window_size` is truncated to the
  most recent observations; it used to make the window permanently longer.
* `dfr_kswin()` rejects `window_size == stat_size`, which crashed at the first
  test.
* `dfr_multi_criteria()` fails with a clear message when a detector's
  `target_feat` is missing from the data, instead of never detecting.

## New features

* `dfr_page_hinkley()` gains `direction = c("increase", "decrease", "both")`.
  The default keeps the previous one-sided behaviour.

## Validation and documentation

* Count parameters must be whole numbers, `dfr_eddm()` requires
  `warning_level >= out_control_level`, `target_feat` must be a single column
  name, and `dfr_kldist(p_th)` must be numeric.
* `nrm_base()` objects keep the `dal_base` class.
* The help pages state which changes each detector cannot see: DDM and HDDM
  only report increases of the error rate, LBDD only changes of spread, MCDD
  only changes of location.
* `?st_drift_examples` documents the actual datasets (`dataset1`, `dataset2`,
  `univariate`) instead of a non-existent `example1`.
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
* New tests cover every detector, `stealthy()`, `dfr_multi_criteria()`, the
  metrics and the internal helpers, plus regression tests that pin the
  detection positions of every detector on fixed streams.
* New GitHub Actions workflows for test coverage and linting.

# heimdall 1.3.0

## Breaking changes

* `norm()` was renamed to `nrm_base()` and is no longer exported. The old name
  masked `base::norm()` whenever the package was attached. Code that called
  `heimdall::norm()` must be updated to `nrm_base()`; `nrm_memory()` is
  unaffected.
* `update_state()` now returns a `list(obj =, drift =)` for *every* detector,
  including the base `drifter` method and the `dfr_inactive` / `dfr_passive`
  baselines. Previously those returned the object itself, which broke the
  streaming loop shown in every example.
* `update_state.stealthy()` follows the same contract and now updates the
  correct field (it referenced a non-existent `drift_technique` slot).
* `reticulate` moved from `Imports` to `Suggests`. Only `dfr_adwin()` needs it,
  and it now fails with an actionable message when Python or `numpy` is
  missing. `Config/reticulate` no longer declares `torch`, `scipy`, `pandas`,
  `matplotlib` and `scikit-learn`, which were never used.
* `car` and `ggplot2` were dropped from `Imports`. Levene's test is now
  computed internally (Brown-Forsythe variant, median-centred), and `ggplot2`
  was never used in `R/`.
* `dfr_mcdd()` and `dfr_lbdd()` now trim their window to its most recent half
  when a drift is reported, and `reset_state()` preserves that window. This
  matches what `dfr_kldist()` and `dfr_kswin()` already did.

## Bug fixes

* `fit()` no longer breaks on a stream with a single observation. `dfr_cusum`,
  `dfr_ddm`, `dfr_ecdd`, `dfr_eddm` and `dfr_hddm` used `2:length(data)`, which
  reversed to `c(2, 1)` and processed a non-existent element followed by a
  duplicate of the first one.
* `mt_fscore()` now uses its `f` argument as the beta of the F-beta score. It
  previously stored the parameter and always computed F1.
* `inverse_transform.nrm_memory()` now inverts the data it is given instead of
  the internal history, which it ignored.
* `fit.nrm_memory()` no longer emits a spurious warning and injects a row of
  zeros into the history on its first call.
* `dfr_ecdd()` no longer fails with `object 'control_limit' not found` for
  average run lengths above 1000; the argument is validated up front.
* `dfr_hddm()` and `dfr_aedd()` no longer latch the per-call drift flag. The
  `drift` element returned by `update_state()` refers to the current call;
  `obj$drifted` remains sticky until `reset_state()`.
* `dfr_mcdd()` no longer stores the incoming observation twice when the window
  is full and no drift is detected.
* `dfr_lbdd()` no longer discards the incoming observation when a drift is
  detected.
* The `levene` criterion of `dfr_aedd()` no longer fails on multivariate input;
  the previous implementation referenced a hard-coded `V1` column that was
  never created.
* `dfr_multi_criteria()` validates `drifter_list` and `combination` instead of
  failing later with `object 'has_drift' not found` or silently reusing the
  previous detector's output.
* `dfr_mcdd()` falls back to the nonparametric test for windows larger than
  5000 observations, where `shapiro.test()` is undefined.
* Missing values are now handled consistently: error-based detectors treat `NA`
  as a correct prediction, distribution-based detectors skip the observation.
  `dfr_eddm()` and `dfr_hddm()` previously raised an error on `NA`.

## Documentation

* The duplicated `update_state()` generic was removed. The remaining one keeps
  `...`, matching every method and the documentation.
* Cross-references to `?dd_ddm`, `?hcd_ddm` and `?nrm_mimax`, which are not
  topics of this package, were replaced by runnable examples.
* `dist_based()`, `mv_dist_based()`, `dfr_multi_criteria()`, `fit.drifter()`
  and the dummy detectors gained examples.
* The `dfr_adwin()` example is no longer commented out; it runs when a Python
  runtime with `numpy` is available.
* The `dfr_aedd()` example points at the walkthrough that actually exists in
  the repository.
* `dfr_kldist()` documents that `p_th` is a divergence threshold, not a
  p-value, and `dfr_kswin()` documents that its reference window is randomly
  subsampled.

## New features

* `dfr_kldist()`, `dfr_kswin()`, `dfr_lbdd()` and `dfr_mcdd()` gained a
  `monitoring_step` argument (default `1`, i.e. unchanged behaviour) that runs
  the statistical test every *k* observations, which is the main cost on long
  streams.
* `dfr_kswin()` gained an `exact` argument forwarded to `stats::ks.test()`.
  Setting it to `NULL` is considerably faster for the default window sizes.
* All constructors validate their arguments.

## Infrastructure

* Added a `testthat` suite covering the detector contract, argument validation,
  missing-value handling, window management, metrics and normalizers.
* Added GitHub Actions workflows for `R CMD check --as-cran` (Linux, macOS and
  Windows) and for the `pkgdown` site, plus a versioned `_pkgdown.yml`.
* Added a `BugReports` field and an explicit `Depends: R (>= 3.5.0)`.
* Removed the `globalVariables()` workaround; the ADWIN Python module is loaded
  into its own environment and cached, and `dfr_aedd()` resolves its default
  autoencoder lazily.
