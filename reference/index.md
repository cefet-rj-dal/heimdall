# Package index

## Error-based detectors

Detectors that monitor the residuals of a predictive model (real concept
drift).

- [`dfr_cusum()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_cusum.md)
  : Cumulative Sum for Concept Drift Detection (CUSUM) method
- [`dfr_ddm()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_ddm.md)
  : Adapted Drift Detection Method (DDM) method
- [`dfr_ecdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_ecdd.md)
  : Adapted EWMA for Concept Drift Detection (ECDD) method
- [`dfr_eddm()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_eddm.md)
  : Adapted Early Drift Detection Method (EDDM) method
- [`dfr_hddm()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_hddm.md)
  : Adapted Hoeffding Drift Detection Method (HDDM) method

## Distribution-based detectors

Detectors that monitor the distribution of the observed features
(virtual concept drift).

- [`dfr_adwin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_adwin.md)
  : ADWIN method
- [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  : KL Distance method
- [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md)
  : KSWIN method
- [`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md)
  : Levene Based Drift Detection Method method
- [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
  : Mean Comparison Distance method
- [`dfr_page_hinkley()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_page_hinkley.md)
  : Adapted Page Hinkley method

## Multivariate and combined detectors

- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  : Autoencoder-Based Drift Detection method
- [`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md)
  : Multi Criteria Drifter sub-class

## Baselines

- [`dfr_inactive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_inactive.md)
  : Inactive dummy detector
- [`dfr_passive()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_passive.md)
  : Passive dummy detector

## Adaptive models

- [`stealthy()`](https://cefet-rj-dal.github.io/heimdall/reference/stealthy.md)
  : Stealthy

## Metrics

- [`metric()`](https://cefet-rj-dal.github.io/heimdall/reference/metric.md)
  : Metric
- [`mt_accuracy()`](https://cefet-rj-dal.github.io/heimdall/reference/mt_accuracy.md)
  : Accuracy Calculator
- [`mt_precision()`](https://cefet-rj-dal.github.io/heimdall/reference/mt_precision.md)
  : Precision Calculator
- [`mt_recall()`](https://cefet-rj-dal.github.io/heimdall/reference/mt_recall.md)
  : Recall Calculator
- [`mt_fscore()`](https://cefet-rj-dal.github.io/heimdall/reference/mt_fscore.md)
  : FScore Calculator
- [`mt_rocauc()`](https://cefet-rj-dal.github.io/heimdall/reference/mt_rocauc.md)
  : ROC AUC Calculator

## Normalization

- [`nrm_base()`](https://cefet-rj-dal.github.io/heimdall/reference/nrm_base.md)
  : Normalizer base class
- [`nrm_memory()`](https://cefet-rj-dal.github.io/heimdall/reference/nrm_memory.md)
  : Memory Normalizer

## Class hierarchy and generics

- [`drifter()`](https://cefet-rj-dal.github.io/heimdall/reference/drifter.md)
  : Drifter
- [`error_based()`](https://cefet-rj-dal.github.io/heimdall/reference/error_based.md)
  : Error Based Drifter sub-class
- [`dist_based()`](https://cefet-rj-dal.github.io/heimdall/reference/dist_based.md)
  : Distribution Based Drifter sub-class
- [`mv_dist_based()`](https://cefet-rj-dal.github.io/heimdall/reference/mv_dist_based.md)
  : Multivariate Distribution Based Drifter sub-class
- [`fit(`*`<drifter>`*`)`](https://cefet-rj-dal.github.io/heimdall/reference/fit.drifter.md)
  : Process Batch
- [`update_state()`](https://cefet-rj-dal.github.io/heimdall/reference/update_state.md)
  : Update State
- [`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
  : Reset State

## Data

- [`st_drift_examples`](https://cefet-rj-dal.github.io/heimdall/reference/st_drift_examples.md)
  : Synthetic time series for concept drift detection
