# Validation: incremental Kolmogorov-Smirnov test (`iks`, `dfr_iks`)

Checks `iks()` and `dfr_iks()` against dos Reis, Flach, Matwin and Batista
(2016), *Fast unsupervised online drift detection using incremental
Kolmogorov-Smirnov test* (KDD), and its code:

* <https://github.com/denismr/incremental-ks> (commit c6bc988), with
  `IKS.py` / `Treap.py` (pure Python), the datasets of the paper
  (`datasets/*.zip`) and the errata for Arabic.

## What is implemented

| reference | heimdall |
|---|---|
| treap with lazy propagation, keys (value, tie-break), `Add`, `Remove`, `KS`, `Kuiper`, `Test`, `CAForPValue` | `iks()`, `iks_add()`, `iks_remove()`, `iks_statistic()`, `iks_test()`, `iks_ca()` |
| `IKSSW` (reference sample and sliding window) | `dfr_iks(window_size, ca, reference = "fixed")` |
| consecutive sliding windows (CD in the paper) | `dfr_iks(reference = "consecutive")` |

## Differences from the paper

* The authors' code for the classification experiments is not available. The
  protocol was rebuilt from Sections 4 to 6 of the paper; see the header of
  `run_iks.R`. Adaptree (AT) is not reproduced, because its description is
  not enough to implement it.
* `c(alpha)`: `CAForPValue(0.001)` of the reference gives 1.95, while the
  two-sided formula `sqrt(-log(alpha / 2) / 2)` gives 1.858. The reference
  value (1.95) is used, and `iks_ca()` returns the same.
* Ties: with tied values, IKS can return a statistic above the exact KS of
  `ks.test`. This happens even with random tie-breaking, and it happens in
  both implementations (the authors mention it). Without ties, the two agree
  to 1e-16.
* Arabic: the paper used a broken version of the data. The repository gives
  corrected numbers (errata) only for the nearest neighbour. They are compared
  with `Arabic_Paper_Fixed`, and the other classifiers on Arabic are not
  comparable.
* CD: after each retraining, the two consecutive windows start empty and are
  filled with 2 W new outputs. Without this, Bike requested more than 100 % of
  the labels.
* The treap is written in pure R, which is about one order of magnitude
  slower than the Python reference. The timing figures are therefore compared
  through their shape (IKS against the standard and optimized KS), not
  through their absolute values.

## Results (`development/validation/results/iks/`)

* `units.csv`. Over 800 random add/remove operations per case, the KS and
  Kuiper statistics are identical to the reference at every check, as are
  all the test decisions. This holds with and without ties and with the
  reference and sliding window of `dfr_iks`.
* `classification_summary.csv` (Tables 2 and 3, 6 datasets x 3 classifiers):
  * MR is more accurate than BL1 and than BL2 in all 18 settings, as in the
    paper.
  * MR reaches on average 96 % of the accuracy of TL.
  * Mean absolute accuracy gap to the paper: Posture 0.6 points, Bike 2.0,
    AbruptInsects 2.3, Insects 3.5, Keystroke 7.0. The largest gaps are on
    Keystroke, where heimdall's accuracy is higher (for example CD 74.9 vs
    48.3), and on the adaptive methods of the nearest neighbour on Bike and
    Insects (MR and AB 5 to 14 points lower).
  * CD on Bike: 43.73 % of the labels and 75 retrainings, against 44.31 % in
    the paper.
  * The fraction of labels of MR matches the paper exactly on Posture and
    Keystroke (NN, NB). On the other datasets the detector fires more often
    than in the paper. The paper does not give the exact retraining policy,
    such as which features are monitored and what happens after a drift.

## Commands

```sh
# on the host (the container has no SSL)
git -c http.sslBackend=schannel clone https://github.com/denismr/incremental-ks data-real/studd/incremental-ks
# unzip data-real/studd/incremental-ks/datasets/*.zip into data-real/studd/datasets

# inside the devcontainer, from the package root
python3 -I development/validation/iks/iks_reference.py data-real/studd/incremental-ks data-real/iks/work units
Rscript development/validation/iks/check_units.R data-real/iks/work development/validation/results/iks/units.csv
Rscript development/validation/iks/run_iks.R classify data-real/studd/datasets data-real/iks/work 6
Rscript development/validation/iks/run_iks.R timing development/validation/results/iks
Rscript development/validation/iks/summarize.R development/validation/results/iks data-real/iks/work
```

`data-real/` is ignored by git.
