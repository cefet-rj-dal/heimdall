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
* `dfr_mcdd()` gained `normality_alpha` (default `0.05`). The Shapiro-Wilk
  threshold no longer reuses `alpha`, which at its default of `1e-08` made the
  normality gate a no-op.
* `stealthy()` gained back a `reporting` argument (default `FALSE`). The
  accumulation of `drifter_input` and `drifter_output` across every batch is
  now opt-in, since those frames grow with the whole stream.
* `dfr_aedd()` no longer accepts an autoencoder class that is neither an
  encoder nor an encoder-decoder; it fails with an explicit message instead of
  reaching an undefined variable.

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

### Detectors

* The sliding windows slide again. In 1.3.x `dfr_kswin()`, `dfr_mcdd()`,
  `dfr_kldist()` and `dfr_lbdd()` appended every observation without ever
  trimming, so memory grew without bound and the reference half stayed frozen
  on the first observations of the stream.
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
* `dfr_kldist()` divided its score by the number of features *plus one*,
  because the temporary `bin` column was counted as a feature.
* `dfr_aedd()`, criterion `parametric_threshold`, compared the history mean
  against itself and could never fire.
* `dfr_aedd()`, criterion `psi`, trimmed a non-existent `state$window` field.
* `dfr_aedd()`, criterion `levene`, tested only the first reconstructed
  feature. It now tests every feature and combines the p-values with a
  Bonferroni correction.
* `dfr_aedd()` no longer builds a throwaway autoencoder with `input_size = 1`
  in the constructor, and no longer keeps its whole history in `state$data`
  under the sliding policy.
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
* `dfr_ecdd()` no longer fails with `object 'control_limit' not found` for
  average run lengths above 1000; the argument is validated up front.

### Metrics and normalization

* `mt_fscore()` uses its `f` argument as the beta of the F-beta score. It
  previously stored the parameter and always computed F1. The same formula was
  wrong in the twelve online-prediction example scripts, which have been fixed.
* `mt_rocauc()` returns `NA` with a warning when the metric is undefined,
  instead of `0`.
* `inverse_transform.nrm_memory()` inverts the data it is given instead of the
  internal history.