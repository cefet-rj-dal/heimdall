# Validation: Window Wasserstein-based aggregation (`dfr_wwaggr`)

Checks `wwaggr()`, `dfr_wwaggr()` and the ensemble functions against
Stepikin, Romanenkova and Zaytsev (2025), *Ensembles of deep change point
detectors with Window Wasserstein-based aggregation* (ICDM 2025), and its code,
<https://github.com/stalex2902/wwagr-ensemble-cpd> (commit 4b8e31d).

## What is implemented

| reference | heimdall |
|---|---|
| members: BCE / InDiD seq2seq LSTM (`src/models`, `baselines/seq2seq`) | `indid(loss = "bce" / "indid")`, plus `n_layers`, `dropout`, `layer_norm` (LayerNormLSTM) |
| TS-CP2 (TCN encoder, InfoNCE loss) | `tscp2()` |
| SN-TS2Vec (dilated conv encoder, spectral norm, hierarchical contrastive loss) | `ts2vec()` |
| `get_repr_learning_output` + `post_process_output` (cosine score, EMA) | `cpd_scores()` of `tscp2` / `ts2vec` |
| naive and bootstrap ensembles (`EnsembleCPDModel`, `boot_sample_size`) | `cpd_ensemble()`, `cpd_ensemble_fit(boot_sample_size = )` |
| beta calibration (`betacal`, abm/am/ab/a) and temperature scaling | `cpd_calibrate(method = "beta" / "temperature", parameters = )` |
| `anchor_window_detector_batch`: Wasserstein p, MMD rbf / multiscale, anchors start, prev, combined | `wwaggr(method = "wasserstein" / "mmd", p, kernel, anchor, window_size)` |
| mean, min, max, median of the members | `wwaggr(method = "mean" / "min" / "max" / "median" / "quantile")` |
| online detection with a threshold | `dfr_wwaggr()` (stream, same scores as the batch `wwaggr`) |

The networks are written in R on a small reverse-mode automatic
differentiation (`R/nn_autodiff.R`, `R/nn_layers.R`). torch is not a
dependency. `gradcheck.R` compares every gradient with finite differences; the
largest relative error is 5e-6.

Not implemented:

* the SGLD ensembles of the paper, which have no code in the repository;
* the dropout and layer norm options of TS-CP2, which the HAR and paper configurations do not use.

## Commands

```sh
# on the host (the container has no SSL)
git -c http.sslBackend=schannel clone https://github.com/stalex2902/wwagr-ensemble-cpd data-real/wwaggr/reference
git -c http.sslBackend=schannel clone https://github.com/betacal/python data-real/wwaggr/betacal_src/python   # 3a9dbb4
git -c http.sslBackend=schannel clone -b 0.9.5 https://github.com/PythonOT/POT data-real/wwaggr/pot
# UCI HAPT ("Smartphone-Based Recognition of Human Activities and Postural Transitions")
# unzipped in data-real/wwaggr/har/raw

# inside the devcontainer, from the package root
python3 -I development/validation/wwaggr/wwaggr_reference.py data-real/wwaggr/reference \
  data-real/wwaggr/betacal_src/python data-real/wwaggr/pot data-real/wwaggr/work units
Rscript development/validation/wwaggr/check_units.R data-real/wwaggr/work/units.json \
  development/validation/results/wwaggr/units.csv
Rscript development/validation/wwaggr/gradcheck.R

Rscript development/validation/wwaggr/prepare_har.R data-real/wwaggr/har/raw data-real/wwaggr/har
python3 -I development/validation/wwaggr/har_reference.py data-real/wwaggr/reference \
  data-real/wwaggr/betacal_src/python data-real/wwaggr/har/raw data-real/wwaggr/work/har dataset
Rscript development/validation/wwaggr/check_har_data.R data-real/wwaggr/har/har.rds data-real/wwaggr/work/har
sh development/validation/wwaggr/run_har.sh 3 1 > development/validation/results/wwaggr/har_run.log
OPENBLAS_NUM_THREADS=1 Rscript development/validation/wwaggr/evaluate_har.R data-real/wwaggr/har/har.rds \
  data-real/wwaggr/work/har development/validation/results/wwaggr 4
```

`data-real/` is ignored by git. Results go to
`development/validation/results/wwaggr/`.

`run_har.sh` trains 300 members: 150 with heimdall and 150 with the reference. Each set is:

* 3 ensembles × 10 members of BCE, InDiD, TS-CP2 and SN-TS2Vec;
* bootstrap BCE.

On 8 cores and 7.7 GB the whole queue took about a day. Mean time per member:

| member | heimdall (R) | reference (torch) |
|---|---|---|
| BCE | 54 s | 21 s |
| InDiD | 167 s | 1296 s |
| TS-CP2 | 50 min | 17 min |
| SN-TS2Vec | 2.1 h | 29 min |

Memory per job is about 1.7 GB for SN-TS2Vec and TS-CP2. More than about four
parallel jobs runs the container out of memory.

## Reference code

Nothing is installed from PyPI. The reference modules are used unchanged,
with these stand-ins:

* **pytorch_lightning.** A `LightningModule` that is `torch.nn.Module` with a
  no-op `log()`. The `Trainer` loop is rewritten in `har_reference.py`:
  * sanity check of two validation batches;
  * schedulers stepped once per epoch;
  * gradient clipping;
  * `EarlyStopping` with the configured monitor, `min_delta` and `patience`;
  * the last model is kept.
* **POT 0.9.5.** The pure-Python `wasserstein_1d` from the 0.9.5 tag, with an
  empty stand-in for the compiled `emd_wrap`, which it does not use.
* **betacal 1.1.0.** Loaded from its GitHub clone.
* **av, pytorchvideo.** Empty modules; they are only needed for the video data.
* **NumPy and torch versions.** `numpy.trapz` is mapped to `numpy.trapezoid`.
  The `verbose` argument of `CosineAnnealingWarmRestarts`, removed in torch
  2.14, is dropped.

## Checks

### 1. Step-by-step equivalence (`units.csv`)

The reference functions are run on fixed inputs and weights, and heimdall on
the same ones. The networks of the reference run in float32, because the
reference casts its inputs; heimdall runs in float64.

| component | max relative difference |
|---|---|
| aggregations: Wasserstein p = 1, 2, 3; MMD rbf / multiscale; anchors start, prev, combined; windows 1–5; mean, min, max, median | 1.8e-14 |
| metrics: F1, F1 with margins, AUDC, covering, delay, time to FA | 1.8e-8 |
| seq2seq LSTM, all variants: forward, BCE / InDiD losses, gradients | 1.4e-7 |
| SN-TS2Vec: encoder, masks, spectral norm, hierarchical loss, gradients, with the reference's random draws replayed | 1.4e-7 |
| TS-CP2: encoder, loss, gradients | 8.2e-6 |
| representation scores (cosine + EMA) | 3.1e-6 |
| Adam / AdamW steps and learning-rate schedules | 0 |
| beta calibration (abm, am, ab, a), predictions | 3.3e-4 |
| beta calibration, with a tight tolerance in the reference | 1.7e-9 |

The calibration difference comes from scikit-learn's L-BFGS: the reference
fits in float32 with tolerance 1e-4, while heimdall uses `glm.fit`. With
tolerance 1e-12 in float64, the two agree to 2e-9.

### 2. Human Activity data (HAR)

The reference configuration `configs/human_activity_*.yaml` is used.
`prepare_har.R` rebuilds the sequences of `HumanActivityDataset`:

* train: 3580 sequences; test: 1337;
* length 20, 28 features.

They are identical to the reference sequences (difference 1e-16, same labels).

The protocol follows `scripts/train_single_models.py` and `evaluate_ensemble.py`:

* member seeds: s + 10(e − 1);
* calibration split: 30 % of the training set, `random_state = 42`;
* thresholds: 300 of them, `sigmoid(linspace(−5, 5))` for the naive aggregations and `linspace(0, 1)` for WWAggr;
* WWAggr window: the best of 1, 2, 3.

The table gives the F1 of the calibrated ensembles, as the mean over the 3
ensembles. "Single" is the mean F1 of the members alone, uncalibrated. Pairs are heimdall / reference. Full results are in
`har_summary.csv`, `har_runs.csv` and `har_single_summary.csv`.

| members | single | mean | min | max | median | WWAggr prev | WWAggr start |
|---|---|---|---|---|---|---|---|
| BCE | 0.988 / 0.989 | 0.992 / 0.992 | 0.986 / 0.989 | 0.989 / 0.988 | 0.993 / 0.993 | 0.974 / 0.975 | 0.973 / 0.975 |
| BCE bootstrap | 0.990 / 0.989 | 0.993 / 0.992 | 0.990 / 0.988 | 0.986 / 0.986 | 0.994 / 0.993 | 0.974 / 0.975 | 0.974 / 0.974 |
| InDiD | 0.989 / 0.988 | 0.990 / 0.988 | 0.989 / 0.987 | 0.984 / 0.980 | 0.989 / 0.989 | 0.972 / 0.972 | 0.974 / 0.975 |
| TS-CP2 | 0.926 / 0.927 | 0.929 / 0.928 | 0.924 / 0.925 | 0.928 / 0.929 | 0.930 / 0.929 | 0.903 / 0.902 | 0.904 / 0.903 |
| SN-TS2Vec | 0.933 / 0.935 | 0.934 / 0.936 | 0.937 / 0.936 | 0.932 / 0.935 | 0.933 / 0.936 | 0.904 / 0.905 | 0.898 / 0.898 |

Over the 60 cells (5 models × 6 aggregations × with/without calibration):

* the mean difference heimdall − reference is +0.0001;
* the largest is 0.006 (min of uncalibrated BCE);
* this is of the order of the standard deviation between ensembles (up to 0.007).

The other uses of the method agree in the same way (`har_alternatives.csv`):

* MMD rbf, 2-Wasserstein and the combined anchor;
* mixed ensembles, 5 BCE + 5 InDiD and 5 TS-CP2 + 5 SN-TS2Vec;
* few thresholds (3, 5, 9; Figure 2 of the paper) and the fixed threshold 0.5.

For example, few thresholds with calibrated SN-TS2Vec give:

| thresholds | heimdall | reference |
|---|---|---|
| 3 | 0.645 | 0.645 |
| 5 | 0.851 | 0.850 |
| 9 | 0.880 | 0.880 |

Expected calibration error of the heimdall members (`har_ece.csv`), mean
over the members:

| members | none | temperature | beta |
|---|---|---|---|
| BCE | 0.008 | 0.008 | 0.004 |
| InDiD | 0.310 | 0.124 | 0.094 |
| TS-CP2 | 0.396 | 0.178 | 0.078 |
| SN-TS2Vec | 0.379 | 0.241 | 0.075 |

## Notes

* **HAR is not a data set of the paper.** The repository has configurations
  for it, but the paper reports Yahoo A4Benchmark, Explosions and Road
  Accidents.
  * Explosions and Road Accidents are videos.
  * Yahoo requires accepting the Yahoo Webscope license and was not
    downloaded.
  * The HAR check is therefore an implementation-against-reference
    comparison, not a reproduction of the paper's tables.
  * To compare with Table II, place the A4Benchmark in
    `data-real/wwaggr/yahoo/` and use `configs/yahoo_*.yaml`.
* **WWAggr on HAR.** On HAR, WWAggr is below the naive aggregations in both
  implementations. The sequences are short (20 steps), and the score at step
  t uses the window [t − W, t): the method lags one step, and the first W
  steps score 0. This is the reference's behaviour, kept as is.
* **Temperature scaling.** In the reference it does not change the
  predictions: it keeps the order of the scores, and the thresholds are
  searched anyway. `cpd_calibrate(method = "temperature")` is equivalent.
* **Bootstrap seeds.** The reference draws the bootstrap samples of
  ensembles 2 and 3 from the random state left by the previous training.
  `har_reference.py` restarts them from seed 42 + 100(e − 1), and heimdall
  draws them from seed 1000 + e. The samples are therefore different, with the
  same distribution.
* **Other random draws.** The random draws of R and torch differ
  (initialisation, batches, masks), so the HAR members are not identical, only
  equivalent in distribution. Exact equivalence is checked in step 1 with
  replayed draws.
