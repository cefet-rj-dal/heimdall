# Changelog

## heimdall 1.4.0

### Breaking changes

- [`update_state()`](https://cefet-rj-dal.github.io/heimdall/reference/update_state.md)
  now returns a `list(obj =, drift =)` for *every* detector, including
  the base `drifter` method and
  [`dfr_inactive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_inactive.md).
  Previously those returned the object itself, which broke the streaming
  loop shown in the examples.
- [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md),
  [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md),
  [`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md),
  [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  and
  [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  gained a `window_type` argument. `"sliding"` (the new default) keeps
  at most `window_size` observations; `"anchored"` keeps every
  observation, pinning the reference half to the beginning of the
  stream. The 1.3.x behaviour of these four detectors corresponds to
  `"anchored"`.
- The
  [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  values `window_type = "fixed"` and `"moving"` are deprecated aliases
  for `"sliding"` and `"anchored"`. They still work and warn.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  takes the autoencoder settings through `ae_params` (a named list
  forwarded to `ae_class`, plus `error_function`), gained the `"psi"`
  and `"diff_threshold"` criteria, and no longer has a `reporting`
  argument.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  no longer accepts an autoencoder class that is neither an encoder nor
  an encoder-decoder; it fails with an explicit message instead of
  reaching an undefined variable.
- [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  is now multivariate: it consumes a data frame one row at a time, and
  every monitored column contributes to the score. Its `target_feat`
  argument now takes one or more column names (by default every numeric
  column), and it gained a `breaks` argument.
- `dfr_kldist(p_th)` was renamed `threshold`, since it is not a p-value;
  `p_th` still works and warns. The default threshold is now `0.12`,
  calibrated by simulation for the default window.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  uses `alpha` only as the significance level of the hypothesis tests
  (it must now be between 0 and 1), and a new `threshold` argument for
  the `"psi"` and `"diff_threshold"` criteria, which compare the
  statistic directly.
- [`stealthy()`](https://cefet-rj-dal.github.io/heimdall/reference/stealthy.md)
  no longer has a `monitored_features` argument. Select the monitored
  columns with the `target_feat` of the drifter instead.
- The `drifter_output` columns of
  [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
  are now `old_shapiro_p`, `new_shapiro_p` and `comparison_p`, and the
  baselines
  [`dfr_inactive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_inactive.md)
  and
  [`dfr_passive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_passive.md)
  report a `drifter_output` without columns.
- [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
  gained `normality_alpha` (default `0.05`). The Shapiro-Wilk threshold
  no longer reuses `alpha`, which at its default of `1e-08` made the
  normality gate a no-op.
- [`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
  on
  [`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md),
  [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md),
  [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  and
  [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  restarts with an empty window
  ([`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md)
  keeps its window). Together with the sliding policy, which tests as
  soon as the window is full (one observation earlier than 1.3.x), this
  moves the detection positions of
  [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md),
  [`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md)
  and
  [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md).
- [`stealthy()`](https://cefet-rj-dal.github.io/heimdall/reference/stealthy.md)
  gained back a `reporting` argument (default `FALSE`). The accumulation
  of `drifter_input` and `drifter_output` across every batch is now
  opt-in, since those frames grow with the whole stream.
- [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html) on a univariate
  detector now monitors only `target_feat` when it is given a data
  frame. It used to concatenate every column into a single stream. A
  data frame with several columns and no `target_feat` is now an error
  instead of being silently concatenated.
- [`update_state()`](https://cefet-rj-dal.github.io/heimdall/reference/update_state.md)
  rejects ambiguous input instead of guessing: vectors with more than
  one value, text that is not a number, and, for error-based detectors,
  values other than `0`, `1`, `TRUE` or `FALSE`. Factors are read
  through their labels (previously through their internal codes), and
  infinite values are treated as missing, with a warning.
- [`dfr_cusum()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_cusum.md)
  was reimplemented after MOA’s `CusumDM`, with the arguments `lambda`
  (now 50), `delta` and `min_instances`. The previous version added `+1`
  per error and `-1` per correct prediction, so it could only detect
  error rates above 50%.
- [`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md)
  no longer keeps `obj$drifts`, which grew with every observation. Use
  `obj$detections` (alarms per detector) and `obj$last_drifts`
  (decisions on the last observation) instead. `"and"` now accepts
  detections up to `fuzzy_window` observations apart (it used to require
  them on the same observation, which almost never happens), and both
  `"and"` and `"fuzzy"` report a change once instead of on several
  consecutive observations. The `"fuzzy"` combination now costs the same
  per observation regardless of the length of the stream.
- New defaults, calibrated by simulation to keep false alarms rare:
  `dfr_kswin(window_size = 300, stat_size = 100, alpha = 1e-4, exact = NULL)`,
  `dfr_lbdd(window_size = 300, alpha = 0.001)`,
  `dfr_mcdd(window_size = 300, alpha = 1e-4)`,
  `dfr_ecdd(average_run_length = 400)` and
  `dfr_aedd(monitoring_step = window_size / 2)`. The previous windows of
  1500 observations were three times longer than the bundled example
  data.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  now uses
  [`daltoolboxdp::autoenc_ed`](https://rdrr.io/pkg/daltoolboxdp/man/autoenc_ed.html)
  as its default autoencoder; the previous default no longer exists in
  `daltoolbox`.
- `st_drift_examples` now holds six synthetic datasets
  (`uv_virtual_drift`, `uv_vct_real_drift`, `uv_vdp_real_drift`,
  `mv_real_drift`, `mv_vct_real_drift` and `mv_vdp_real_drift`). The
  former `univariate` and `dataset1` are available as `uv_virtual_drift`
  and `mv_real_drift`.

### New features

- Every detector stores the per-observation diagnostics of its last
  [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html) call in
  `obj$drifter_output` (for example the window width of ADWIN, the
  p-values of KSWIN and MCDD, or the divergence of
  [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)),
  with one row per observation and `NA` where the detector reported
  nothing.
- [`stealthy()`](https://cefet-rj-dal.github.io/heimdall/reference/stealthy.md)
  always exposes the diagnostics of its drifter in `obj$drifter_output`,
  one row per row of `x` across every call to
  [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), whatever the
  kind of drifter. `reporting = TRUE` now only controls the accumulation
  of the raw batches in `drifter_input`.
- [`dfr_page_hinkley()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_page_hinkley.md)
  gains `direction = c("increase", "decrease", "both")`. The default
  keeps the previous one-sided behaviour.
- New
  [`dummy()`](https://cefet-rj-dal.github.io/heimdall/reference/dummy.md)
  ancestor class for the baseline detectors
  [`dfr_inactive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_inactive.md)
  and
  [`dfr_passive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_passive.md).
- [`mv_dist_based()`](https://cefet-rj-dal.github.io/heimdall/reference/mv_dist_based.md)
  takes a `target_feat` with one or more column names, so every
  distribution-based detector selects what it monitors the same way.
- `stealthy(history_size = )` bounds the rows kept in `drifter_output`
  and `drifter_input`, and `nrm_memory(max_size = )` bounds the history
  used to estimate the normalization.
- [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md)
  keeps its own random number stream, so it no longer changes the global
  random state while it runs, and gains a `seed` argument.
- [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  is about three times faster: the window is kept as a numeric matrix
  and the histograms are built with
  [`tabulate()`](https://rdrr.io/r/base/tabulate.html).

### Bug fixes

#### Adaptive model

- `fit.stealthy()` computed the error-based residuals against
  `norm_x_oh`, a variable that had been set to `NULL` a few lines
  earlier (or never defined). Every error-based detector received a null
  input. The batch is now projected once, through
  `.stealthy_project_batch()`, and used directly.
- The drift check now runs *before* the model is updated, so error-based
  detectors observe prequential residuals instead of training error.
- The detector is fed the incoming batch exactly once per call. The
  previous version fitted it with the new batch and then again with the
  whole accumulated `x_train`, and finally cleared
  `drift_method$drifted`, discarding the second signal.
- `predict.stealthy()` with `obsolete_model = "majority"` referenced an
  undefined `model` variable and a `norm_data_oh` that had not been
  computed yet, and called an unimported `cla_majority`.
  `obsolete_model` is now validated with
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html).
- [`stealthy()`](https://cefet-rj-dal.github.io/heimdall/reference/stealthy.md)
  initialised `train_model` only when `class_balance = "buffer"`.
- `stealthy(target_uni_drifter = TRUE)` works with factor targets; it
  used to feed `NA` to the detector, which then never detected anything.

#### Detectors

- The sliding windows slide again. In 1.3.x
  [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md),
  [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md),
  [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  and
  [`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md)
  appended every observation without ever trimming, so memory grew
  without bound and the reference half stayed frozen on the first
  observations of the stream.
- [`dfr_hddm()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_hddm.md)
  now detects increases of the error rate. A misplaced parenthesis in
  the bound that tracks the minimum error rate made the detector update
  that minimum on almost every observation, so it practically never
  reported a drift.
- [`dfr_page_hinkley()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_page_hinkley.md)
  compares `sum - min_sum` against the threshold again. 1.3.x compared
  `sum` alone, which is not the Page-Hinkley test, and left `min_sum`
  computed but unused.
- [`dfr_page_hinkley()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_page_hinkley.md)
  skips missing observations again. The `tryCatch` that replaced the
  `NA` guard let `NA` into the running mean and returned `NULL` from
  [`update_state()`](https://cefet-rj-dal.github.io/heimdall/reference/update_state.md)
  on any error, breaking the caller’s loop.
- [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
  applies exactly one comparison test per evaluation. 1.3.x fell through
  from the t-test to the Wilcoxon test, giving two uncorrected chances
  to reject.
- [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md)
  requires both a significant p-value and a KS statistic above
  `sqrt(-log(alpha) / stat_size)`, as in Raab et al., and subsamples its
  reference window again.
- [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md)
  rejects `window_size == stat_size`, which crashed at the first test.
- [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  divided its score by the number of features *plus one*, because the
  temporary `bin` column was counted as a feature.
- [`dfr_ecdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_ecdd.md)
  used the last coefficient of the ARL 1000 polynomial (987.23) in the
  ARL 400 control limit; it now uses 848.18, as in the reference
  implementation.
- [`dfr_ecdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_ecdd.md)
  no longer fails with `object 'control_limit' not found` for average
  run lengths above 1000; the argument is validated up front.
- [`dfr_cusum()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_cusum.md)
  and
  [`dfr_ecdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_ecdd.md)
  restart after a drift, so they report each change once instead of on
  every following observation when
  [`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
  is not called.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md),
  criterion `parametric_threshold`, compared the history mean against
  itself and could never fire.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md),
  criterion `psi`, trimmed a non-existent `state$window` field.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md),
  criterion `levene`, tested only the first reconstructed feature. It
  now tests every feature and combines the p-values with a Bonferroni
  correction.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  no longer builds a throwaway autoencoder with `input_size = 1` in the
  constructor, and no longer keeps its whole history in `state$data`
  under the sliding policy. It retrains the autoencoder when new columns
  appear, warns the first time it has to clip values outside `[0, 1]`
  and no longer fails on missing values.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  and
  [`dfr_hddm()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_hddm.md)
  no longer latch the per-call drift flag. The `drift` element refers to
  the current call; `obj$drifted` stays sticky until
  [`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md).
- `fit.dfr_passive()` iterated over `2:length(data)`, which is the
  column count for a data frame. All
  [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html) methods now
  walk the stream through shared helpers.
- [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html) no longer
  breaks on a stream with a single observation, and rejects an empty one
  with a clear message.
- Missing values are handled consistently: error-based detectors treat
  `NA` as a correct prediction, distribution-based detectors skip the
  observation.
- Warm-start data (`data =`) longer than `window_size` is truncated to
  the most recent observations under the sliding policy; it used to make
  the window permanently longer.
- [`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md)
  fails with a clear message when a detector’s `target_feat` is missing
  from the data, instead of never detecting.

#### Metrics and normalization

- [`mt_fscore()`](https://cefet-rj-dal.github.io/heimdall/reference/mt_fscore.md)
  uses its `f` argument as the beta of the F-beta score. It previously
  stored the parameter and always computed F1. The same formula was
  wrong in the twelve online-prediction example scripts, which have been
  fixed.
- Precision, recall, F-score and accuracy accept factor and character
  labels. With a factor `y_true`, precision returned 1.5 and recall
  `NaN`. The metrics now also refuse vectors of different lengths
  instead of recycling them.
- [`mt_rocauc()`](https://cefet-rj-dal.github.io/heimdall/reference/mt_rocauc.md)
  drops observations with missing scores, with a warning, instead of
  scoring them as zero.
- `inverse_transform.nrm_memory()` inverts the data it is given instead
  of the internal history.
- [`nrm_memory()`](https://cefet-rj-dal.github.io/heimdall/reference/nrm_memory.md)
  no longer drops repeated rows from its history, which distorted the
  normalization (a z-score of 1.79 came out as 0.71).

### Validation and documentation

- Count parameters must be whole numbers,
  [`dfr_eddm()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_eddm.md)
  requires `warning_level >= out_control_level`, and `target_feat` must
  be a single column name.
- [`nrm_base()`](https://cefet-rj-dal.github.io/heimdall/reference/nrm_base.md)
  objects keep the `dal_base` class.
- The help pages state which changes each detector cannot see: DDM and
  HDDM only report increases of the error rate, LBDD only changes of
  spread, MCDD only changes of location.
- [`?st_drift_examples`](https://cefet-rj-dal.github.io/heimdall/reference/st_drift_examples.md)
  documents every bundled dataset.
- [`dfr_adwin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_adwin.md)
  is now implemented in pure R. It no longer needs `reticulate`, Python
  or `numpy`, and a `dfr_adwin` object can now be saved with
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html). The port follows
  the scikit-multiflow implementation the previous version wrapped, so
  detections are expected to be the same.

### Dependencies

- `caret` and `Metrics` were dropped from `Imports`. One-hot encoding in
  [`stealthy()`](https://cefet-rj-dal.github.io/heimdall/reference/stealthy.md)
  is now done internally with the same column names produced by
  [`caret::dummyVars()`](https://rdrr.io/pkg/caret/man/dummyVars.html),
  and precision and recall are computed internally.
- The bundled `adwin.py` module was removed:
  [`dfr_adwin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_adwin.md)
  no longer needs Python. `reticulate` stays in `Suggests` only for the
  tests that check whether the Python backend of `daltoolboxdp` is
  available, and `daltoolboxdp` was added to `Suggests` for the default
  autoencoder of
  [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md).

### Documentation and infrastructure

- New vignettes:
  [`vignette("heimdall")`](https://cefet-rj-dal.github.io/heimdall/articles/heimdall.md),
  [`vignette("adaptive-models")`](https://cefet-rj-dal.github.io/heimdall/articles/adaptive-models.md)
  and
  [`vignette("configuring-detectors")`](https://cefet-rj-dal.github.io/heimdall/articles/configuring-detectors.md).
- Examples are organised in `examples/1_detection` and
  `examples/2_online_prediction`, with a detection example and an online
  prediction example for every drifter, including
  [`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md)
  and the
  [`dfr_inactive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_inactive.md)
  and
  [`dfr_passive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_passive.md)
  baselines.
- The README documents the output of each drifter, how each one selects
  its input, and which changes each one cannot see.
- New tests cover every detector,
  [`stealthy()`](https://cefet-rj-dal.github.io/heimdall/reference/stealthy.md),
  [`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md),
  the metrics and the internal helpers, plus statistical tests of the
  false-alarm rate and the detection delay of every detector across
  several seeds.
- New GitHub Actions workflows for test coverage, linting (which now
  fails on any lint), R CMD check in the base image of the development
  container, the AEDD tests with the real autoencoder and the rendering
  of every example, and checks that reject file names invalid on Windows
  and committed merge conflict markers. A matching pre-commit hook is in
  `.githooks/`.
- The code was restyled with `styler`, and legacy files, large raw
  datasets and orphaned example outputs were removed from the
  repository.
