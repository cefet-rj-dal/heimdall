# Validation: LLR2 drift detection with semiparametric Bayesian networks

Checks `spbn()`, `spbn_loglik()` and `dfr_llr2()` against Sojo, Bielza and
Larrañaga (2027), *A fully unsupervised concept drift detector via Bayesian
networks for machine condition monitoring* (Expert Systems with Applications
332, 133634).

The detector's own code is not public. The authors' evaluation repository
(<https://github.com/rafasj13/LLR2-Evaluation>) has the synthetic streams, the
generating networks, the metric code and the per-configuration result CSVs of
the reference runs. The validation compares against those.

```sh
# from the package root
git clone https://github.com/rafasj13/LLR2-Evaluation data-real/llr2/reference
# inside the devcontainer: 16 streams x 18 configurations, about 4 hours on 7 cores
OPENBLAS_NUM_THREADS=1 Rscript development/validation/llr2/run_llr2.R 7 > development/validation/results/llr2/run.log
```

`data-real/` is ignored by git. Results go to
`development/validation/results/llr2/`.

## Setup

* **Streams:** 16 synthetic streams, Models 1-4 crossed with four drift types:
  sudden (S), incremental (I), incremental-recurring (IR) and gradual (G).
  They are in `datasets/synthetic2`, 16000 observations each. The drifts are
  at the positions in Table 2.
* **Settings (Appendix B):**
  * window 100, 250 or 500;
  * step 10, 50 or 100;
  * alpha 0.05, with and without the Benjamini-Hochberg correction (LLR2-BH);
  * a baseline of 4096 observations;
  * the `"parameter"` update.
* **Metrics:** ported from `util_files/util_plot.py`
  (`calculate_drift_metrics`):
  * MDR, the missed detection rate;
  * MTD, the mean time to detection;
  * FA, the number of false alarms;
  * MTRd.

  The best configuration per stream and method follows `sort_mtrd_with_nan`:
  MDR = 0 first, then MTRd, then MTD.
* **Check of the metric port:** applied to the detections in the reference
  CSVs, it gives their MTD and MDR columns exactly (576 rows, difference 0).

There are two sets of reference runs:

* **reference (local):** `csvs2/.../LLR2_transfer5.csv`, the method of the
  paper. Its best rows reproduce Tables 4-7 exactly.
* **reference (parameter):** `ablation_experiments2/<stream>/LLR2_parameter-transfer.csv`,
  the parameter-transfer ablation. This is the update that `dfr_llr2()`
  implements, so it is the main point of comparison.

## 1. All configurations (`configurations_vs_reference.csv`, `runs.csv`)

The table gives means over the 9 window × step configurations and the 4
models of each drift type, comparing `dfr_llr2()` with the parameter-transfer
reference runs.

| type | method | MDR | MDR ref. | MTD | MTD ref. | FA | FA ref. | drifts | drifts ref. |
|---|---|---|---|---|---|---|---|---|---|
| S | LLR2 | 0.017 | 0.007 | 63.2 | 69.6 | 0.67 | 0.00 | 8.5 | 7.9 |
| S | LLR2-BH | 0.021 | 0.038 | 73.7 | 87.0 | 0.25 | 0.00 | 8.1 | 7.7 |
| I | LLR2 | 0 | 0 | 296.9 | 328.3 | 8.97 | 7.61 | 15.0 | 13.6 |
| I | LLR2-BH | 0 | 0 | 368.5 | 387.2 | 5.58 | 4.69 | 11.6 | 10.7 |
| IR | LLR2 | 0 | 0 | 259.9 | 329.4 | 11.19 | 8.06 | 16.2 | 13.1 |
| IR | LLR2-BH | 0 | 0.033 | 332.9 | 397.2 | 6.78 | 3.56 | 11.8 | 8.4 |
| G | LLR2 | 0 | 0 | 225.6 | 253.2 | 2.44 | 2.36 | 8.4 | 8.4 |
| G | LLR2-BH | 0.005 | 0.014 | 399.4 | 470.2 | 0.47 | 0.58 | 6.4 | 6.5 |

* **Same pattern as the reference:**
  * the incremental drifts take about 300-400 observations to detect, and
    the sudden ones under 90;
  * BH lowers the false alarms and lengthens the delay;
  * the gradual drifts give few false alarms;
  * MDR is close to 0 everywhere.
* **More sensitive:** `dfr_llr2()` detects 10-20% earlier and gives slightly
  more false alarms, mostly on the incremental-recurring streams and on the
  sudden ones with small windows. Once a drift has been detected, an
  incremental drift keeps changing the distribution. The extra detections
  are re-detections during that change.

## 2. Tables 4-7 (`tables_4_7.csv`, `tables_4_7_by_type.csv`)

These are the best configuration per stream and method. The table gives the
mean over the four models of each type.

| type | method | MTD dfr_llr2 | MTD ref. (parameter) | MTD paper | FA dfr_llr2 | FA ref. (parameter) | FA paper |
|---|---|---|---|---|---|---|---|
| S | LLR2 | 14.4 | 15.3 | 24.8 | 2.25 | 0.00 | 0.00 |
| S | LLR2-BH | 21.3 | 21.9 | 30.7 | 0.75 | 0.00 | 0.00 |
| I | LLR2 | 370.8 | 371.5 | 377.1 | 4.50 | 4.00 | 3.75 |
| I | LLR2-BH | 427.9 | 394.4 | 427.9 | 2.50 | 2.50 | 2.75 |
| IR | LLR2 | 337.0 | 379.5 | 369.0 | 4.50 | 3.00 | 3.25 |
| IR | LLR2-BH | 392.0 | 387.0 | 389.0 | 3.25 | 2.75 | 3.00 |
| G | LLR2 | 170.8 | 226.1 | 182.9 | 2.00 | 2.25 | 2.75 |
| G | LLR2-BH | 238.8 | 387.3 | 253.3 | 0.50 | 0.75 | 1.25 |

* **Missed drifts:** `dfr_llr2()` misses none on any stream (MDR = 0 in all
  32 best rows). The paper misses one in Model 3-S with BH.
* **Detection time:** MTD is at the level of the paper on I, IR and G, and
  lower on S.
* **False alarms:** they are the same on I, IR and G, within one per stream.
* **Sudden drifts:** the exception. The reference has no false alarms there;
  `dfr_llr2()` has 2.25 per stream without BH and 0.75 with it. The small
  windows (100, step 10) that give the shortest MTD also give a few
  re-detections right after the change. These come from the differences in
  the update and the reset listed below.
* **Run time:** each run takes about 320 s with the exact KDE.

## Differences from the paper

* **No public detector code:** the following were reconstructed from the
  text and from the reference CSVs.
  * **Reference window:** after the baseline, the network is learned on
    `W_l` (the first `n_baseline - window_size` observations), and `W_r` is
    the last window. The paper does not say how the 4096 observations are
    split.
  * **Reset after a drift:** the reference window is emptied, and the first
    full window after the drift becomes the new `W_r`. Tests resume at the
    next step. This rule was inferred from the gaps between the reference
    detections, which are never shorter than window + step.
* **CKDE without binning:** the B-SPBN binned kernel density estimate was not
  implemented. The conditional KDEs are exact, with the normal reference
  bandwidth. This is slower, and the densities differ slightly.
* **No HC-TL:** structure updates by hill climbing with transfer learning
  were not implemented. The `"parameter"` update follows the
  parameter-transfer ablation of the paper. The CPD fitted on the sliding
  window is mixed with the one on the learning window, with weight
  `eta = |W_s| / |W_l|`, and the source is dropped when the risk check fails.
  This is why the parameter-transfer runs, not the paper's tables, are the
  main reference.
* **Not reproduced:** the industrial data set (Section 6) is not available.
