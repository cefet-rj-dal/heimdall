# Changelog

## heimdall 1.3.0

### Breaking changes

- [`norm()`](https://rdrr.io/r/base/norm.html) was renamed to
  [`nrm_base()`](https://cefet-rj-dal.github.io/heimdall/reference/nrm_base.md)
  and is no longer exported. The old name masked
  [`base::norm()`](https://rdrr.io/r/base/norm.html) whenever the
  package was attached. Code that called
  [`heimdall::norm()`](https://rdrr.io/r/base/norm.html) must be updated
  to
  [`nrm_base()`](https://cefet-rj-dal.github.io/heimdall/reference/nrm_base.md);
  [`nrm_memory()`](https://cefet-rj-dal.github.io/heimdall/reference/nrm_memory.md)
  is unaffected.
- [`update_state()`](https://cefet-rj-dal.github.io/heimdall/reference/update_state.md)
  now returns a `list(obj =, drift =)` for *every* detector, including
  the base `drifter` method and the `dfr_inactive` / `dfr_passive`
  baselines. Previously those returned the object itself, which broke
  the streaming loop shown in every example.
- `update_state.stealthy()` follows the same contract and now updates
  the correct field (it referenced a non-existent `drift_technique`
  slot).
- `reticulate` moved from `Imports` to `Suggests`. Only
  [`dfr_adwin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_adwin.md)
  needs it, and it now fails with an actionable message when Python or
  `numpy` is missing. `Config/reticulate` no longer declares `torch`,
  `scipy`, `pandas`, `matplotlib` and `scikit-learn`, which were never
  used.
- `car` and `ggplot2` were dropped from `Imports`. Levene’s test is now
  computed internally (Brown-Forsythe variant, median-centred), and
  `ggplot2` was never used in `R/`.
- [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
  and
  [`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md)
  now trim their window to its most recent half when a drift is
  reported, and
  [`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
  preserves that window. This matches what
  [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  and
  [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md)
  already did.

### Bug fixes

- [`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html) no longer
  breaks on a stream with a single observation. `dfr_cusum`, `dfr_ddm`,
  `dfr_ecdd`, `dfr_eddm` and `dfr_hddm` used `2:length(data)`, which
  reversed to `c(2, 1)` and processed a non-existent element followed by
  a duplicate of the first one.
- [`mt_fscore()`](https://cefet-rj-dal.github.io/heimdall/reference/mt_fscore.md)
  now uses its `f` argument as the beta of the F-beta score. It
  previously stored the parameter and always computed F1.
- `inverse_transform.nrm_memory()` now inverts the data it is given
  instead of the internal history, which it ignored.
- `fit.nrm_memory()` no longer emits a spurious warning and injects a
  row of zeros into the history on its first call.
- [`dfr_ecdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_ecdd.md)
  no longer fails with `object 'control_limit' not found` for average
  run lengths above 1000; the argument is validated up front.
- [`dfr_hddm()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_hddm.md)
  and
  [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  no longer latch the per-call drift flag. The `drift` element returned
  by
  [`update_state()`](https://cefet-rj-dal.github.io/heimdall/reference/update_state.md)
  refers to the current call; `obj$drifted` remains sticky until
  [`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md).
- [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
  no longer stores the incoming observation twice when the window is
  full and no drift is detected.
- [`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md)
  no longer discards the incoming observation when a drift is detected.
- The `levene` criterion of
  [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  no longer fails on multivariate input; the previous implementation
  referenced a hard-coded `V1` column that was never created.
- [`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md)
  validates `drifter_list` and `combination` instead of failing later
  with `object 'has_drift' not found` or silently reusing the previous
  detector’s output.
- [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
  falls back to the nonparametric test for windows larger than 5000
  observations, where
  [`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) is
  undefined.
- Missing values are now handled consistently: error-based detectors
  treat `NA` as a correct prediction, distribution-based detectors skip
  the observation.
  [`dfr_eddm()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_eddm.md)
  and
  [`dfr_hddm()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_hddm.md)
  previously raised an error on `NA`.

### Documentation

- The duplicated
  [`update_state()`](https://cefet-rj-dal.github.io/heimdall/reference/update_state.md)
  generic was removed. The remaining one keeps `...`, matching every
  method and the documentation.
- Cross-references to `?dd_ddm`, `?hcd_ddm` and `?nrm_mimax`, which are
  not topics of this package, were replaced by runnable examples.
- [`dist_based()`](https://cefet-rj-dal.github.io/heimdall/reference/dist_based.md),
  [`mv_dist_based()`](https://cefet-rj-dal.github.io/heimdall/reference/mv_dist_based.md),
  [`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md),
  [`fit.drifter()`](https://cefet-rj-dal.github.io/heimdall/reference/fit.drifter.md)
  and the dummy detectors gained examples.
- The
  [`dfr_adwin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_adwin.md)
  example is no longer commented out; it runs when a Python runtime with
  `numpy` is available.
- The
  [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  example points at the walkthrough that actually exists in the
  repository.
- [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  documents that `p_th` is a divergence threshold, not a p-value, and
  [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md)
  documents that its reference window is randomly subsampled.

### New features

- [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md),
  [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md),
  [`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md)
  and
  [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
  gained a `monitoring_step` argument (default `1`, i.e. unchanged
  behaviour) that runs the statistical test every *k* observations,
  which is the main cost on long streams.
- [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md)
  gained an `exact` argument forwarded to
  [`stats::ks.test()`](https://rdrr.io/r/stats/ks.test.html). Setting it
  to `NULL` is considerably faster for the default window sizes.
- All constructors validate their arguments.

### Infrastructure

- Added a `testthat` suite covering the detector contract, argument
  validation, missing-value handling, window management, metrics and
  normalizers.
- Added GitHub Actions workflows for `R CMD check --as-cran` (Linux,
  macOS and Windows) and for the `pkgdown` site, plus a versioned
  `_pkgdown.yml`.
- Added a `BugReports` field and an explicit `Depends: R (>= 3.5.0)`.
- Removed the
  [`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html)
  workaround; the ADWIN Python module is loaded into its own environment
  and cached, and
  [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  resolves its default autoencoder lazily.
