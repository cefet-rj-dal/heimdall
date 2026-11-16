# Validation: Multimodal QuantTree (MMQT)

Checks `mmqt()`, `mmqt_test()` and `dfr_mmqt()` against Stucchi, Rizzo,
Frittoli, Carrera and Boracchi (2023), *Multimodal batch-wise change
detection* (IEEE TNNLS 34(10)), and the reference repository
<https://github.com/diegocarrera89/quantTree>.

The repository has the QuantTree code (partitioning, thresholds, data
generators) but not MMQT itself (its README says "coming soon"). MMQT is
therefore implemented from the paper (Section IV, Eqs. 12-13 and Algorithm 1),
and the QuantTree pieces it builds on are checked against the reference code.

```sh
# inside the devcontainer, from the package root
git clone https://github.com/diegocarrera89/quantTree data-real/mmqt/reference/quanttree
# CWRU drive-end 12k files (normal 97-99; IR/B/OR faults 0.007/0.014/0.021, loads 0-2)
#   https://engineering.case.edu/bearingdatacenter -> data-real/mmqt/cwru/*.mat

# 1. QuantTree port against the reference
python3 -I development/validation/mmqt/reference_check.py data-real/mmqt/reference/quanttree \
  development/validation/results/mmqt/reference
Rscript development/validation/mmqt/check_reference.R
# 2. data (synthetic modalities with the reference generator, CWRU STFT features)
python3 -I development/validation/mmqt/prepare_data.py data-real/mmqt/reference/quanttree \
  data-real/mmqt/cwru data-real/mmqt/prepared
# 3. experiments (several hours on 6 cores)
OPENBLAS_NUM_THREADS=1 Rscript development/validation/mmqt/run_mmqt.R 6 > \
  development/validation/results/mmqt/run.log
```

`data-real/` is ignored by git. Results go to
`development/validation/results/mmqt/`.

## 1. QuantTree port

| check | result |
|---|---|
| partitions (5 seeds, d = 2-16, K = 4-64): bin boundaries, bins of training and new data | identical (`check_partitions.csv`) |
| thresholds of the TV statistic, uniform bins (16 cases, from the reference `all_distr_quanttree.pkl`) | 15 identical, 1 differs by one count (K = 128, nu = 256, alpha = 0.5%) (`check_thresholds_uniform.csv`) |
| thresholds for non-uniform bin probabilities (`QuantTreeThresholdStrategy`) | within Monte Carlo noise, at most 0.08 (`check_thresholds_nonuniform.csv`) |

## 2. Synthetic data (Figs. 5-6, Table III)

Gaussian modalities from the reference `get_synthetic_modalities` with a
minimum symmetric KL distance of 1 between modalities. N_m = 64 training
batches of nu = 128 per modality, K = 4d, alpha = alpha' = 5%, 500 stationary
and 500 post-change test batches. 20 runs per configuration (the paper uses 200).

| d | M | MMQT FPR | AUC | M estimated correctly | QuantTree FPR |
|---|---|---|---|---|---|
| 4 | 2-10 | 4.6-5.1% | 0.987-0.994 | 19-20 of 20 runs | 81-96% |
| 8 | 4 | 4.8% | 0.988 | 20 of 20 | 81% |
| 16 | 2-8 | 5.1-5.2% | 0.969-0.975 | 20 of 20 | 33-73% |
| 16 | 10 | 6.2% | 0.968 | 15 of 20 (5 runs found 9) | 72% |

Like the paper, MMQT keeps the FPR at the 5% target while a single QuantTree
fitted to the multimodal data loses control of it. The one deviation, d = 16 and
M = 10, comes from k-means merging two modalities, a limitation the paper also
discusses. Table III (d = 8, M = 4): the FPR of the four modalities ranges from
3.7% to 5.9% for MMQT (run means), against 61-96% for QuantTree.

## 3. Calibration (Table IV)

Two modalities in R^8 at sKL 1, 2, 4 and 8; K = 32; nu = 128; M known; N_m
from 4 to 64. The FPR stays between 4.7% and 6.4% with or without calibration
(`calibration_summary.csv`). The calibration lowers the FPR mostly where the
paper says it matters, with few training batches (N_m = 4, sKL 1: 6.50% to
6.23%). Elsewhere the gain is within noise, because with nu = 128 the bin
probabilities of the two modalities are already well estimated.

## 4. CWRU bearing data (Table VI, top)

STFT features (d = 8). nu = 32, K = 16, 100 training and 150 test batches per
speed, 100 changes (one fault per run), scenarios *Single Speed* (one load) and
*Multi Speed* (loads 0-2 as modalities). The thresholds are recomputed for each
target FPR.

| scenario | method | AUC | FPR at 0.5 / 1 / 2 / 5% | delta M |
|---|---|---|---|---|
| Single Speed | MMQT | 1.000 | 0.36 / 0.92 / 1.50 / 4.49% | 0.03 |
| | paper | 1.000 | 0.49 / 0.98 / 1.78 / 4.72% | 0.27 |
| Multi Speed | MMQT | 0.999 | 0.47 / 0.93 / 1.93 / 4.89% | 0.02 |
| | paper | 0.999 | 0.50 / 0.98 / 2.04 / 4.93% | -0.04 |
| Multi Speed | QuantTree | 0.989 | 25.9 / 28.0 / 40.4 / 56.2% | |
| | paper | | 3.39 / 3.84 / 9.33 / 17.48% | |

MMQT matches the paper. QuantTree in the Multi Speed scenario is worse here
than in the paper (FPR 26-56% against 3-17%). Both are far from the target, but
the size of the gap depends on how far apart the speeds are in our STFT
features, which the paper does not fully specify.

## Deviations from the paper

* MMQT is not in the reference repository; it is written from the paper.
* Synthetic data: sKL0 = 1 is our choice. 20 runs instead of 200,
  500 test batches per kind, and thresholds simulated with 10^4 samples.
* CWRU: the window, overlap and number of STFT coefficients, the batch counts
  and the assignment of the fault files to the runs are our reading of
  Section V-F.
* Not reproduced: M-QuantTrees, SPLL and Density Tree (other baselines), and
  the INSECTS experiment, which uses a dataset that is not available in the
  form the paper uses.
