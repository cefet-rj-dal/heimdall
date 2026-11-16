# Validation: Maximum Concept Discrepancy drift detector (MCD-DD)

Checks `dfr_mcd()` against Wan, Liang and Yoon (2024), *Online drift detection
with maximum concept discrepancy* (KDD 2024), and its reference code
(<https://github.com/LiangYiAnita/mcd-dd>: `mcd.py`, `encoder.py`, `main.py`,
`configs/` and the data sets in `data/`). The package already has a
`dfr_mcdd()`, which is a different detector (mean comparison); hence the name
`dfr_mcd()`.

```sh
# inside the devcontainer, from the package root
git clone https://github.com/LiangYiAnita/mcd-dd data-real/mcdd/reference
mkdir -p data-real/mcdd/csv
python3 -I -c "import numpy as np, glob, os
for f in glob.glob('data-real/mcdd/reference/data/*/*.pkl'):
    np.savetxt('data-real/mcdd/csv/' + os.path.basename(f)[:-4] + '.csv',
               np.asarray(np.load(f, allow_pickle=True), dtype=float), delimiter=',', fmt='%.17g')"

# 1. step-by-step comparison (seconds)
python3 -I development/validation/mcd/reference_trace.py data-real/mcdd/reference \
  data-real/mcdd/reference/data/toy_data/GM_Sud.pkl data-real/mcdd/trace_GM_Sud.json 6 3000 30 100 100 float64
#    (also GamGM_SudGrad 4 windows; INSECTS_Sud 3 windows, 5000 100 200 150; EEG 4 windows, 1000 100 150 100;
#     GM_Sud in float32)
Rscript development/validation/mcd/check_trace.R data-real/mcdd/trace_*.json

# 2. Table 2: the reference code, 20 runs per data set (float32 as published, a few minutes each)
for d in GM_Sud GM_Rec GM_Grad GM_Inc GamLog_Sud LogGamWei_Sud GamGM_SudGrad; do
  python3 -I development/validation/mcd/run_reference.py data-real/mcdd/reference \
    data-real/mcdd/reference/data/toy_data/$d.pkl development/validation/results/mcd/reference/$d.csv 20 3000 30 100 100
done
#    and LogGamWei_Sud, GM_Rec in float64 (output <name>_float64.csv, last argument float64)
# 3. dfr_mcd, 20 runs per data set, and the evaluation of all (about 25 minutes on 7 cores)
OPENBLAS_NUM_THREADS=1 Rscript development/validation/mcd/run_mcd.R 7 > development/validation/results/mcd/run.log
```

`data-real/` is ignored by git. Results go to
`development/validation/results/mcd/`.

## 1. Equivalence with the reference code (`trace_comparison.csv`)

`reference_trace.py` runs the loop of `main.py` with the reference `MCD` and
`Encoder` classes, unchanged. It wraps `torch.randint` and `torch.normal` to
record every draw: the sample-set indices, the weak and strong noise, and the
sets of the threshold and of the test. It also records the initial weights,
and after every window the test distances, the threshold and the weights.
`check_trace.R` replays those draws in R from the same initial weights.

| trace (float64) | windows | test distances | thresholds | weights after each Adam step | decisions |
|---|---|---|---|---|---|
| GM_Sud (d = 5, encoder 100-100) | 6 | <= 7e-16 | <= 1e-15 | <= 1e-14 | all equal |
| GamGM_SudGrad (d = 20) | 4 | <= 2e-16 | <= 2e-16 | <= 7e-15 | all equal |
| INSECTS_Sud (d = 33, m = 100, encoder 200-150) | 3 | <= 1e-16 | <= 2e-16 | <= 3e-15 | all equal |
| EEG (d = 14, raw values around 4000) | 4 | <= 4e-11 | <= 2e-12 | <= 4e-9 | all equal |

These agreements cover:

* the encoder and its gradients;
* the contrastive loss;
* the gradient penalty, whose gradient needs second derivatives (written by
  hand in R);
* Adam, the threshold quantile and the test.

Against the published float32 run of the reference, the differences are of
order 1e-7, which is float32 rounding.

## 2. Table 2: synthetic data sets (`table2.csv`)

Settings of `configs/para_simulated_data.yaml`: W = 3000, 10 sub-windows,
m = 30, k = 10, encoder 100-100, 20 runs. The reference repository has no
evaluation code (`main.py` only prints the detections). Following Section
6.1.1, a tested sub-window of 300 observations is "Drift" when a drift starts
in it. For the incremental ramps of GM_Inc, a sub-window is "Drift" when it
overlaps a ramp. Precision, F1 and MCC are computed over the tested
sub-windows. The drift points of Appendix A.1.1 were checked on the data
(`run.log` prints the mixture weight or the mean by blocks), with these
differences from the text:

* GM_Grad also switches back at 24,000.
* GM_Inc starts at p = 0.8, not at 0.2. The ramps are at the stated places.
* The gradual part of GamGM_SudGrad, not given in the paper, switches at
  12,000, 15,000, 18,000, 21,000 and 24,000.

| data set | dfr_mcd F1 | reference F1 | paper F1 | dfr_mcd MCC | reference MCC | paper MCC |
|---|---|---|---|---|---|---|
| GM_Sud | 1.00 | 1.00 | 1.00 | 1.00 | 1.00 | 1.00 |
| GM_Rec | 0.97 | 0.95 | 0.79 | 0.97 | 0.95 | 0.81 |
| GM_Grad | 0.79 | 0.80 | 0.79 | 0.77 | 0.80 | 0.79 |
| GM_Inc | 0.29 | 0.28 | 0.44 | 0.37 | 0.38 | 0.51 |
| GamLog_Sud | 1.00 | 1.00 | 1.00 | 1.00 | 1.00 | 1.00 |
| LogGamWei_Sud | 0.83 | 0.96 (0.92 in float64) | 0.94 | 0.85 | 0.96 (0.93) | 0.95 |
| GamGM_SudGrad | 0.92 | 0.91 | 0.99 | 0.91 | 0.91 | 0.99 |

* **`dfr_mcd` against the reference:** on six data sets they are equivalent,
  and their mean thresholds do not differ (t-tests over the runs, p from 0.34
  to 0.97). On LogGamWei_Sud the published float32 code differs from the same
  code in float64 after the drift at 15,000. In the Gamma part the float32
  threshold is lower (1.23 against 1.49; p = 6e-4 between float32 and float64
  runs), so it detects the drift at 24,000 more often. `dfr_mcd` computes in
  double precision and agrees with the float64 reference (thresholds 1.46 and
  1.49, p = 0.5). Its F1, 0.83 against 0.92, differs by 1.8 standard errors
  of 20 runs.
* **Both implementations against the paper:** they reproduce GM_Sud,
  GamLog_Sud and GM_Grad, and the reference also LogGamWei_Sud (0.96 against
  0.94). They are above the paper on GM_Rec
  and below it on GM_Inc and GamGM_SudGrad. Both give the same numbers, so
  these gaps come from the evaluation protocol, which is not published: how
  sub-windows are labelled around gradual and incremental drifts, and whether
  the sub-window right after a sudden drift counts as a true detection. In
  GamGM_SudGrad, for example, both implementations add on average one
  detection beyond the six labelled ones.

## Not reproduced

* The real-world rows of Table 2 (INSECTS and EEG). The data sets are in the
  repository, but without drift labels, and the drift positions used by the
  paper could not be confirmed from a primary source. The equivalence in
  section 1 covers both data sets with their settings.
* The baselines (KS, MMD-GK, LSDD, MMD-DK), the ablation (Figure 5) and the
  sensitivity analyses (Figures 6-8).
* `dfr_mcd` takes about 70 seconds per run on a 30,000 x 5 stream (one core).
