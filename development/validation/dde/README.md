# Validation: domino drift effect (DDE)

Checks `dfr_dde()`, `dde_curve()`, `dde_score()` and `dde_feature_drift()`
against Szucs and Nemeth (2025), *Domino drift effect approach for probability
estimation of feature drift in high-dimensional data*, Knowledge and
Information Systems 67:4597-4621.

```sh
# inside the devcontainer, from the package root
OPENBLAS_NUM_THREADS=1 Rscript development/validation/dde/run_dde.R [window=1000] [folds=10] [out dir]
```

Covertype (UCI) is downloaded to `data-real/dde/` (ignored by git). Results go
to `development/validation/results/dde_*` (about 15 minutes on one core).

## Reference code

The repository named in the paper
(<https://github.com/marcellnemeth/DominoDriftEffect>) returns 404, it is not
among the author's public repositories, and no copy was found. The method was
therefore implemented from the paper:

* DDE curve, Eqs. 14-24: B(z) pairs with |rho| >= z, b(z) of them with both
  features drifting, probability (m / k) b / B, lift = probability /
  ((k - 1) / (m - 1)). Checked in the tests against Table 3, whose values
  are reproduced exactly from its pair counts.
* Score of an unmonitored feature, Eq. 25: the mean of the curve at its
  correlation with each drifting monitored feature (the probability at the
  largest threshold not above |rho|).
* Drift tests per feature with the definitions and default thresholds of
  Evidently, which the paper uses: Jensen-Shannon distance (Sturges bins,
  natural log, threshold 0.1), Kolmogorov-Smirnov (p < 0.05), normed
  Wasserstein (threshold 0.1), plus ADWIN. JS and Wasserstein match scipy to
  1e-12 in the tests.

## Data and setup

The paper's Insects (200 features) and Heartbeats sets were not found, so
the validation uses **Covertype**, its third dataset ("top 16 features", two
classes, Table 2). The paper does not say how the 16 features were ranked or
how the classes were merged. Here they are the 16 most important features of
a random forest on 20 000 records, and the label is class 2 against the rest
(the label is not used by DDE). Windows have 1000 records and drift is tested
with JS.

* **A. Induced drift (the paper's setting).** The stream is shuffled, and each
  of 30 drifts exchanges the values of 2-8 random features in the current
  window (a cyclic permutation of the columns). Drifts with fewer than two
  drifting features are discarded, which leaves 29.
* **B. Natural drift.** Covertype in its original, spatially ordered, record
  order: consecutive non-overlapping windows, 465 of 580 window pairs with
  2 <= k < 16 drifting features.
* **Prediction (Section 5.3).** The curve of one drift predicts the next one
  (100 consecutive pairs for B). Monitored features are drawn at random in
  each of 10 folds, and an unmonitored feature is predicted to drift when its
  score is >= 0.5. The ground truth is full monitoring with the same test.
  Baselines (Section 5.4): *reduced monitoring* (unmonitored features never
  drift) and *reduced random* (they drift with the share of drifting
  monitored features).
* **C. Online.** `dfr_dde(n_monitored = 7, test = "js", window_size = 1000,
  monitoring_step = 250)` on the first 200 000 records, each prediction
  checked against full monitoring of the window where it was confirmed.

## Results

**Lift curves** (`dde_lift_summary.csv`, `dde_lift_curves.png`). The mean
lift over drifts is dominated by thresholds with a handful of pairs (single
drifts reach a lift of 60), so the table pools the pairs of every drift: the
pooled lift is the number of drifting pairs over the number expected if drift
did not depend on correlation.

| threshold | 0 | 0.1 | 0.2 | 0.3 | 0.4 | 0.5 | 0.6 | 0.7 |
|---|---|---|---|---|---|---|---|---|
| A, induced | 1.00 | 1.06 | 1.14 | 1.15 | 1.12 | 1.08 | 1.16 | 0.76 |
| B, natural | 1.00 | 1.45 | 1.56 | 1.67 | 1.78 | 1.82 | 1.82 | 1.85 |

The induced drifts give a flat curve, as for Covertype in Fig. 3: the
exchanged features are drawn independently of their correlation. The natural
drifts give an increasing curve, so correlated features do drift together,
which is the effect the method relies on. It is weaker than for Insects
(lift 4.07 at 0.9, Table 3), with 16 features instead of 200.

**Prediction of the next drift** (`dde_prediction_summary.csv`,
`dde_prediction_f1.png`), with 7 of the 16 features monitored:

| setting | method | recall | precision | F1 |
|---|---|---|---|---|
| paper, Table 6 (Covertype) | DDE | 0.618 | 0.522 | 0.549 |
| A, induced | DDE | 0.492 | 0.908 | 0.583 |
| A, induced | reduced monitoring | 0.432 | 1.000 | 0.577 |
| A, induced | reduced random | 0.615 | 0.648 | 0.585 |
| B, natural | DDE | 0.624 | 0.824 | 0.637 |
| B, natural | reduced monitoring | 0.433 | 1.000 | 0.572 |
| B, natural | reduced random | 0.605 | 0.669 | 0.578 |
| C, online `dfr_dde` (121 drifts) | DDE | 0.912 | 0.503 | 0.625 |

* With natural drift, DDE has the best F1 from 4 to 13 monitored features
  and the best recall of the methods that keep a high precision, as in
  Tables 7 and 8. With 1-2 monitored features it ties reduced random.
* With induced drift the lift is flat, so DDE cannot beat the baselines (all
  three F1 within 0.01 at 7 features). DDE trades recall for precision
  there, but its F1 equals that of the baselines.
* The online detector reaches F1 0.625 and an AUC of 0.65 for the scores of
  the unmonitored features. It is of the same order as Table 6, with higher
  recall and the same precision; its curve is refitted at every drift, so it
  predicts more drift than the single-curve prediction.

## Differences from the paper

* The reference code is not available (see above).
* Covertype instead of Insects or Heartbeats, with feature selection and
  labelling chosen here.
* The paper does not define its exchange drift precisely; here it is a cyclic
  permutation of the chosen columns.
* `dfr_dde` confirms a drift `confirmation_delay` observations (half a
  window) after the first signal and takes the drifting set from the
  confirmation test. Features cross their thresholds at different times, and
  without the delay the drifting set of the first signal is incomplete.
* JS with the Evidently threshold (0.1) flags identical distributions as
  drifted when windows have a few hundred records; windows of 1000 are used
  here, and the documentation recommends KS or a higher threshold for small
  windows.
