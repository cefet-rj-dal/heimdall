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
- [`dfr_kswin()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kswin.md)
  : KSWIN method
- [`dfr_lbdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_lbdd.md)
  : Levene Based Drift Detection Method method
- [`dfr_mcdd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcdd.md)
  : Mean Comparison Distance method
- [`dfr_onlinebcp()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_onlinebcp.md)
  : onlineBcp stream detector
- [`online_bcp()`](https://cefet-rj-dal.github.io/heimdall/reference/online_bcp.md)
  : Online Bayesian change point detection (onlineBcp)
- [`dfr_page_hinkley()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_page_hinkley.md)
  : Adapted Page Hinkley method

## Multivariate and combined detectors

- [`dfr_aedd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_aedd.md)
  : Autoencoder-Based Drift Detection method
- [`dfr_kldist()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_kldist.md)
  : KL Distance method
- [`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md)
  : Multi Criteria Drifter sub-class
- [`dfr_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_ensemble.md)
  : Ensemble drift detector (CEDD and FEDD)
- [`ensemble_vote()`](https://cefet-rj-dal.github.io/heimdall/reference/ensemble_vote.md)
  : Ensemble vote over detection series
- [`dfr_proton()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_proton.md)
  : PROTON: prototype-based test-time online OOD detection
- [`dfr_qts()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_qts.md)
  : Quadtree spatial mapping drift detector (QTS)

## Batch-wise multimodal change detection

Multimodal QuantTree (Stucchi et al., 2023).

- [`mmqt()`](https://cefet-rj-dal.github.io/heimdall/reference/mmqt.md)
  : Multimodal QuantTree (MMQT)
- [`mmqt_test()`](https://cefet-rj-dal.github.io/heimdall/reference/mmqt_test.md)
  : Test batches with a Multimodal QuantTree
- [`dfr_mmqt()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mmqt.md)
  : Multimodal QuantTree stream detector

## Histogram two-sample tests

Equal intensity k-means space partitioning (Liu et al., 2021).

- [`eikmeans()`](https://cefet-rj-dal.github.io/heimdall/reference/eikmeans.md)
  : Equal intensity k-means space partitioning (EI-kMeans)
- [`eikmeans_test()`](https://cefet-rj-dal.github.io/heimdall/reference/eikmeans_test.md)
  : Test a sample with an EI-kMeans partitioning
- [`dfr_eikmeans()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_eikmeans.md)
  : EI-kMeans stream detector

## Population Stability Index

PSI on quantile bins of a baseline (Kurian and Allali, 2024).

- [`psi()`](https://cefet-rj-dal.github.io/heimdall/reference/psi.md) :
  Population Stability Index (PSI)
- [`dfr_psi()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_psi.md)
  : PSI stream detector

## Cosine similarity of embeddings

Data drift check for embeddings of the custom SageMaker Model Monitor
(Nigenda et al., 2022).

- [`cosine_score()`](https://cefet-rj-dal.github.io/heimdall/reference/cosine_score.md)
  : Average cosine similarity to a baseline of embeddings
- [`dfr_cosine()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_cosine.md)
  : Cosine similarity drift detector for embeddings

## Parallel Activations Drift Detector

Unsupervised batch detector on the outputs of a random neural network
(Komorniczak and Ksieniewicz, 2024).

- [`dfr_padd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_padd.md)
  : Parallel Activations Drift Detector (PADD)

## Student-Teacher Unsupervised Drift Detection

Unsupervised detector on the mimicking error of a student classifier
(Cerqueira et al., 2023).

- [`dfr_studd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_studd.md)
  : Student-Teacher Unsupervised Drift Detection (STUDD)

## Ensembles of change point detectors (WWAggr)

Window Wasserstein-based aggregation of calibrated deep change point
detectors (Stepikin et al., 2025).

- [`wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/wwaggr.md)
  : WWAggr and naive aggregations of ensemble scores
- [`dfr_wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_wwaggr.md)
  : WWAggr ensemble drift detector
- [`cpd_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md)
  [`cpd_ensemble_fit()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md)
  : Ensembles of change point models
- [`cpd_calibrate()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_calibrate.md)
  : Calibration of change point models
- [`cpd_scores()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_scores.md)
  : Change point scores of a sequence model
- [`tscp2()`](https://cefet-rj-dal.github.io/heimdall/reference/tscp2.md)
  : TS-CP2 change point model
- [`ts2vec()`](https://cefet-rj-dal.github.io/heimdall/reference/ts2vec.md)
  : SN-TS2Vec change point model

## Maximum Concept Discrepancy

Unsupervised detector with a contrastively trained set encoder (Wan et
al., 2024).

- [`dfr_mcd()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_mcd.md)
  : Maximum Concept Discrepancy drift detector (MCD-DD)

## Supervised change point detection (InDiD)

Sequence-to-sequence detector trained with the InDiD loss (Romanenkova
et al., 2022).

- [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md)
  : InDiD change point detection model
- [`indid_predict()`](https://cefet-rj-dal.github.io/heimdall/reference/indid_predict.md)
  : Probabilities of change of an InDiD model
- [`dfr_indid()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_indid.md)
  : InDiD stream detector

## Bayesian network log-likelihood ratio (LLR2)

Fully unsupervised detector with a semiparametric Bayesian network (Sojo
et al., 2027).

- [`spbn()`](https://cefet-rj-dal.github.io/heimdall/reference/spbn.md)
  : Semiparametric Bayesian network
- [`spbn_loglik()`](https://cefet-rj-dal.github.io/heimdall/reference/spbn_loglik.md)
  : Log-likelihood of each node of a semiparametric Bayesian network
- [`dfr_llr2()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_llr2.md)
  : LLR2 drift detector

## Feature drift with the domino drift effect

Predicts which unmonitored features drift from their correlation with
the drifting monitored ones (Szűcs and Németh, 2025).

- [`dfr_dde()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_dde.md)
  : Domino drift effect (DDE) detector
- [`dde_curve()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_curve.md)
  : DDE probability curve
- [`dde_score()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_score.md)
  : DDE drift scores
- [`dde_feature_drift()`](https://cefet-rj-dal.github.io/heimdall/reference/dde_feature_drift.md)
  : Feature-wise drift tests between two windows

## Baselines

- [`dummy()`](https://cefet-rj-dal.github.io/heimdall/reference/dummy.md)
  : Dummy Drifter sub-class
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

## Benchmark streams with controlled drift

Realistic streams with verified real and virtual drift (Hinder et al.,
2026).

- [`dsg_source()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source.md)
  : Drift benchmark source
- [`dsg_source_data()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source_data.md)
  : Benchmark source from a real-world stream
- [`dsg_source_regions()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_source_regions.md)
  : Benchmark source from a synthetic generator
- [`dsg_sea()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md)
  [`dsg_agrawal()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md)
  [`dsg_sine()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md)
  [`dsg_mixed()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_synthetic.md)
  : Synthetic benchmark sources
- [`dsg_take()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_take.md)
  : Draw samples from a drift benchmark source
- [`dsg_drift()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_drift.md)
  : Induce a drift in a benchmark source
- [`dsg_stream()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_stream.md)
  : Generate a benchmark stream with controlled drift
- [`dsg_mmd_test()`](https://cefet-rj-dal.github.io/heimdall/reference/dsg_mmd_test.md)
  : Kernel two-sample test (MMD)
