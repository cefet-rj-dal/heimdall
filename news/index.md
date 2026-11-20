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
  `"mann_whitney"` and `"kolmogorov_smirnov"` criteria now test every
  monitored column separately and combine the p-values with a Bonferroni
  correction (as `"levene"` already did), instead of pooling all latent
  dimensions of an encoder into one sample.
- `dfr_aedd(ae_params = list(error_function = "rmse"))` now computes the
  root mean squared reconstruction error. It previously returned the
  mean absolute error.
- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  gained `column_threshold`: when set, the criterion is evaluated on
  every column and a drift is signalled only when the proportion of
  changed columns is greater than `column_threshold`.
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
- New
  [`dfr_proton()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_proton.md)
  detector (PROTON, Das et al., 2026): online out-of-distribution
  detection for the outputs of a frozen model with an embedding space,
  such as a medical vision-language model. It keeps a bank of confident
  embeddings per class and fuses the distance to the nearest class
  prototype with the MCM score, weighted by the running variance of the
  MCM score. The per-sample OOD scores are reported in `drifter_output`,
  and a `threshold` turns them into alarms.
- New benchmark streams with controlled real and virtual drift (Hinder
  et al., 2026).
  [`dsg_source_data()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source_data.md)
  turns a real-world stream into concepts that combine the feature
  distribution of one time segment with labels sampled from a model of
  `P(y | segment, x)` for another segment, so virtual and real drift can
  be induced separately.
  [`dsg_sea()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md),
  [`dsg_agrawal()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md),
  [`dsg_sine()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md)
  and
  [`dsg_mixed()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md)
  build synthetic sources with regional sub-sampling.
  [`dsg_drift()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_drift.md)
  induces and verifies a drift (kernel two-sample test,
  [`dsg_mmd_test()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_mmd_test.md),
  for virtual drift; share of changed labels for real drift), and
  [`dsg_stream()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_stream.md)
  builds a stream from a pattern such as `"--v--r"`, with the true drift
  of every segment.
- New
  [`dfr_dde()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_dde.md)
  feature drift detector (domino drift effect, Szűcs and Németh, 2025).
  After an initialization drift in which every feature is tested, it
  monitors only a subset of the features and predicts a drift score for
  the others from their correlation with the drifting monitored
  features.
  [`dde_curve()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_curve.md)
  computes the DDE probability and lift curves of a drift,
  [`dde_score()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_score.md)
  the scores of unmonitored features, and
  [`dde_feature_drift()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_feature_drift.md)
  the feature-wise Jensen-Shannon, Kolmogorov-Smirnov and Wasserstein
  window tests.
- New
  [`dfr_qts()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_qts.md)
  unsupervised drift detector (quadtree spatial mapping, Ramos et al.,
  2024). It maps a sliding window onto a height-limited quadtree and
  monitors the number of occupied leaves, signalling a drift when its
  moving average leaves the reference band of the concept or when its
  first derivative changes. Detections match the reference
  implementation.
- New
  [`mmqt()`](https://cefet-rj-dal.github.io/heimdall/reference/mmqt.md)
  batch-wise change detector for multimodal stationary conditions
  (Multimodal QuantTree, Stucchi et al., 2023). It builds one QuantTree
  histogram over the training batches, estimates the number of
  modalities by clustering their bin-probability vectors and controlling
  the false positive rate, calibrates the bin probabilities of each
  modality and tests each batch against the closest modality
  ([`mmqt_test()`](https://cefet-rj-dal.github.io/heimdall/reference/mmqt_test.md)).
  [`dfr_mmqt()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mmqt.md)
  monitors a stream batch by batch with it.
- New
  [`eikmeans()`](https://cefet-rj-dal.github.io/heimdall/reference/eikmeans.md)
  histogram two-sample test (equal intensity k-means space partitioning,
  Liu et al., 2021). Its bins are k-means clusters of nearly equal
  intensity, and samples are compared with the training sample by
  Pearson’s chi-square test
  ([`eikmeans_test()`](https://cefet-rj-dal.github.io/heimdall/reference/eikmeans_test.md)).
  [`dfr_eikmeans()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_eikmeans.md)
  tests consecutive windows of a stream against a training sample.
- New
  [`psi()`](https://cefet-rj-dal.github.io/heimdall/reference/psi.md),
  the Population Stability Index of Kurian and Allali (2024): the
  symmetrised Kullback-Leibler divergence between a baseline and a new
  sample, feature by feature, on quantile bins of the baseline (deciles
  or demi-deciles), with the per-bin contributions and the paper’s drift
  levels.
  [`dfr_psi()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_psi.md)
  compares consecutive windows of a stream with a baseline.
- New
  [`dfr_mcd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcd.md),
  the Maximum Concept Discrepancy drift detector (MCD-DD, Wan, Liang and
  Yoon, 2024): sample sets of sub-windows are embedded by a small
  network trained online by contrastive learning, and a drift is
  reported when the distance between the last two sub-windows exceeds a
  quantile of the distances within sub-windows. The network, its
  gradients and Adam are written in R.
- New
  [`dfr_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_ensemble.md),
  an ensemble drifter that runs several detectors and votes on their
  detections: EFER, majority or threshold voting, crisp (CEDD) or with
  fuzzy time tolerance (FEDD, Giusti Tavares et al., 2025), and
  [`ensemble_vote()`](https://cefet-rj-dal.github.io/heimdall/reference/ensemble_vote.md),
  the same vote on detection series computed beforehand.
- New
  [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md),
  the sequence-to-sequence change point detector of Romanenkova et
  al. (2022): an LSTM that outputs the probability of change at every
  step, trained on labelled sequences with the InDiD loss (expected
  detection delay and time to false alarm), BCE or both in turn;
  [`indid_predict()`](https://cefet-rj-dal.github.io/heimdall/reference/indid_predict.md)
  and the stream detector
  [`dfr_indid()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_indid.md).
  The network, backpropagation through time and Adam are written in R.
- New
  [`dfr_llr2()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_llr2.md),
  the LLR2 detector of Sojo et al. (2027): a semiparametric Bayesian
  network
  ([`spbn()`](https://cefet-rj-dal.github.io/heimdall/reference/spbn.md),
  [`spbn_loglik()`](https://cefet-rj-dal.github.io/heimdall/reference/spbn_loglik.md);
  linear Gaussian and conditional KDE nodes) learned on a baseline, and
  a chi-square test per node of the difference of mean log-likelihoods
  between a sliding and a reference window, with optional
  Benjamini-Hochberg correction (LLR2-BH) and update of the network
  after a drift.
- New
  [`online_bcp()`](https://cefet-rj-dal.github.io/heimdall/reference/online_bcp.md),
  the online Bayesian change point detection of onlineBcp (Yigiter et
  al., 2015; Xu et al., 2022) for normal data, with the segment summary
  of the package and stable (log-scale) numerics, and the stream
  detector
  [`dfr_onlinebcp()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_onlinebcp.md).
- New
  [`cosine_score()`](https://cefet-rj-dal.github.io/heimdall/reference/cosine_score.md)
  and
  [`dfr_cosine()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_cosine.md),
  the data drift check for embeddings (e.g. BERT sentence embeddings of
  NLP inputs) of the custom Amazon SageMaker Model Monitor (Nigenda et
  al., 2022): the average cosine similarity of every input with a
  baseline of embeddings, averaged over monitoring windows and compared
  with a threshold.
- New
  [`dfr_padd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_padd.md),
  the Parallel Activations Drift Detector of Komorniczak and Ksieniewicz
  (2024): chunks of the stream pass through a fixed, randomly
  initialised neural network, and replicated t-tests compare the
  activations of each chunk with those stored since the last drift.
- New
  [`dfr_studd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_studd.md),
  the student-teacher unsupervised drift detector (STUDD) of Cerqueira
  et al. (2023): a student classifier is trained to mimic the teacher,
  and its mimicking error, which needs no label, feeds a change detector
  (Page-Hinkley by default). Labels are only requested to retrain the
  teacher after a drift.
- New
  [`dfr_wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_wwaggr.md)
  and
  [`wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/wwaggr.md),
  the window Wasserstein-based aggregation (WWAggr) of ensembles of
  change point detectors of Stepikin et al. (2025), with the MMD and
  p-Wasserstein distances, the start, previous and combined anchor
  windows, and the mean, min, max, median and quantile aggregations. The
  ensembles
  ([`cpd_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md),
  [`cpd_ensemble_fit()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md);
  naive, bootstrap or mixed) take members from
  [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md)
  (BCE and InDiD losses), the new
  [`tscp2()`](https://cefet-rj-dal.github.io/heimdall/reference/tscp2.md)
  (TS-CP2) and
  [`ts2vec()`](https://cefet-rj-dal.github.io/heimdall/reference/ts2vec.md)
  (SN-TS2Vec), scored by
  [`cpd_scores()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_scores.md)
  and calibrated by
  [`cpd_calibrate()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_calibrate.md)
  (beta or temperature). The networks are trained with a reverse-mode
  automatic differentiation written in R.
- [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md)
  gained `n_layers`, `dropout` and `layer_norm` (the LSTM with layer
  normalisation of the WWAggr code);
  [`dfr_indid()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_indid.md)
  streams these models too.
- New
  [`dfr_greedyks()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_greedyks.md)
  and the sketch
  [`greedyks()`](https://cefet-rj-dal.github.io/heimdall/reference/greedyks.md)
  ([`greedyks_update()`](https://cefet-rj-dal.github.io/heimdall/reference/greedyks.md),
  [`greedyks_statistic()`](https://cefet-rj-dal.github.io/heimdall/reference/greedyks.md),
  [`greedyks_pvalue()`](https://cefet-rj-dal.github.io/heimdall/reference/greedyks.md)):
  the online, approximate one-sample Kolmogorov-Smirnov test of Cardoso
  and Galeno (2023), with `m` buckets and a Fenwick tree, against a
  given CDF or a normal, exponential or uniform distribution fitted to
  the start of the stream.
- New
  [`dfr_iks()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_iks.md)
  and the sketch
  [`iks()`](https://cefet-rj-dal.github.io/heimdall/reference/iks.md)
  ([`iks_add()`](https://cefet-rj-dal.github.io/heimdall/reference/iks.md),
  [`iks_remove()`](https://cefet-rj-dal.github.io/heimdall/reference/iks.md),
  [`iks_statistic()`](https://cefet-rj-dal.github.io/heimdall/reference/iks.md),
  [`iks_kuiper()`](https://cefet-rj-dal.github.io/heimdall/reference/iks.md),
  [`iks_test()`](https://cefet-rj-dal.github.io/heimdall/reference/iks.md),
  [`iks_ca()`](https://cefet-rj-dal.github.io/heimdall/reference/iks.md)):
  the incremental two-sample Kolmogorov-Smirnov test of dos Reis et
  al. (2016), a treap with lazy propagation, and its unsupervised drift
  detector (a fixed reference sample or consecutive windows against the
  sliding window, per feature).
- New
  [`dfr_fastlr()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_fastlr.md)
  and
  [`fastlr_split()`](https://cefet-rj-dal.github.io/heimdall/reference/fastlr_split.md):
  the fast likelihood-based change point detector for binary streams of
  Tatti (2019). The split that maximises the Bernoulli log-likelihood
  ratio is searched among the border indices, with the (1 - eps)
  approximation in O(log^2(n) / eps) time, and a change is declared
  above tau + log(n).
- New
  [`ks_sf()`](https://cefet-rj-dal.github.io/heimdall/reference/ks_sf.md):
  the exact distribution of the one-sample Kolmogorov-Smirnov statistic
  for any `n` (the algorithm of Simard and L’Ecuyer, as
  `scipy.stats.kstwo`), and its limiting distribution.

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
