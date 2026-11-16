# Validation: ensemble drift detector (CEDD and FEDD)

Checks `dfr_ensemble()` and `ensemble_vote()` against Giusti Tavares et al.
(2025), *Fuzzy-based ensemble method for robust concept drift detection in
multivariate time series* (IJCNN 2025), and its reference code. The link given
in the paper (<https://github.com/cefet-rj-dal/fedd>) is not public. The code
is in <https://github.com/cefet-rj-dal/reproduce>, folder `fedd/`:

* `core/4-ensemble-fuzzy.R`: the combination;
* `core/3-combination-analysis.R`: the evaluation of Table IV;
* `results.zip`: the detection series of the base detectors and of the
  ensembles on the 3W data set;
* `data/`: 3W samples of types 3 and 4.

The fuzzy scores use `har_fuzzify_detections_triangle()` of `harbinger`.

```sh
# from the package root (outside the container: git with schannel on Windows)
git clone --depth 1 --filter=blob:none --sparse https://github.com/cefet-rj-dal/reproduce data-real/fedd/reference
git -C data-real/fedd/reference sparse-checkout set fedd
# full 3W series of types 1, 2 and 4-8 (harbinger format) from cefet-rj-dal/united
for t in 1 2 4 5 6 7 8; do curl -L -o data-real/fedd/3w/full/oil_3w_Type_$t.RData \
  https://raw.githubusercontent.com/cefet-rj-dal/united/refs/heads/main/harbinger/oil_3w_Type_$t.RData; done

# inside the devcontainer
# arrow, only to read the reference RData files (arrow-backed vectors), in a private library
Rscript -e 'install.packages("arrow", lib = "data-real/fedd/rlib")'
Rscript development/validation/fedd/run_fedd.R > development/validation/results/fedd/run.log
Rscript development/validation/fedd/find_tolerance.R 7   # about 10 minutes
```

`data-real/` is ignored by git. `results.zip` is read in place, not extracted.
Results go to `development/validation/results/fedd/`.

## The method in the package

* **`dfr_ensemble(drifter_list, voting, threshold, tolerance, lookahead)`** is
  a drifter that runs its members on the stream and votes on their
  detections:
  * `voting = "efer"`: vote >= 1;
  * `voting = "majority"`: vote > n / 2;
  * `voting = "threshold"`: vote >= `threshold` (lambda).

  With `tolerance` <= 1 the ensemble is crisp (CEDD). With `tolerance` k > 1
  it is fuzzy (FEDD): a detection at d gives time t the score
  `(k - |t - d|) / k`, and each member's score is the largest over its
  detections.
* **`lookahead`:** the paper's fuzzy vote uses detections on both sides of t,
  so online the vote of t is decided at t + k - 1 (`lookahead = TRUE`, the
  default; `obj$events` holds the times decided). `lookahead = FALSE` is the
  causal variant, which uses only past detections and decides at once.
* **`ensemble_vote(detections, ...)`** applies the same vote to detection
  series computed beforehand, as the paper does. It is vectorised, and the
  tests check that it gives the same drifts as the step-by-step engine of
  `dfr_ensemble()` on random series for every rule and tolerance, with and
  without lookahead.
* **`dfr_multi_criteria()`** keeps its own `"fuzzy"` mode, which is one-sided
  and decays from each detection. It is not the paper's FEDD.

Points where the reference code is followed:

* **Strict majority:** vote > n / 2. The paper writes vote >= the ceiling of
  n / 2.
* **Score formula:** `(k - j) * (1 / k)` at distance j, as in `harbinger`. The
  paper's Equation 3 prints `|t - t'| / k`, which would score 0 at the
  detection itself.
* **Consumption after a fuzzy ensemble drift at t:**
  * the detections before t are forgotten;
  * every member with a positive score at t loses its first detection at or
    after t, even if it lies beyond the tolerance (online, its next detection
    is dropped when it arrives).

## 1. Combination against the reference files (`equivalence.csv`)

The detection series of the six base detectors selected in the paper (Table
III) are combined by `ensemble_vote()` and compared step by step with the
ensembles in `results.zip`, for 28 examples. In 4WELL1_3 one member series is
5 rows longer than the others, and the reference script also leaves this
example out ("Missing One").

| ensemble | examples | identical | detections (ours / reference) |
|---|---|---|---|
| CEDD EFER (lambda = 1) | 28 | 28 | 771 / 771 |
| CEDD MV | 28 | 28 | 0 / 0 |
| CEDD TV (lambda = 2) | 21 | 21 | 60 / 60 |
| FEDD MV (k = 4000) | 28 | 3 | 162 / 121 |
| FEDD TV (k = 4000, lambda = 4) | 21 | 3 | 71 / 48 |

The fuzzy reference files were not produced with k = 4000, the value given in
the paper. `find_tolerance.R` searches, for each file, the k (50 to 6000, step
50, and 10% of the series length) with which `ensemble_vote()` reproduces it
exactly (`reference_tolerance.csv`). Of the 49 fuzzy files, 22 are reproduced:

* 14 files that have detections are reproduced with k = 2000 (9 files), 4000
  (2), 1000 (1) or 10% of the length (2, a commented line of
  `4-ensemble-fuzzy.R`);
* 8 files have no detections and match any k.

This includes MV and TV files of the same example with the same k, and the
reference comments next to the example list ("Ok 1000", "Ok 2000",
"Ok 2500", ...). So where the inputs are the same, the combination, including
the consumption rule, gives exactly the reference output.

No k reproduces the other 27 files. For example, 1WELL2 would need k around
4201 and 2WELL3_2 k around 4007, so they were built from other member series
than the published ones. AEDD, an autoencoder, gives different detections on
every run.

## 2. Table IV (`table4.csv`, `table4_examples.csv`)

The evaluation follows `3-combination-analysis.R` with the installed
`harbinger` (2.1.707):

* **Metric:** SoftED with a time tolerance of 4000.
* **Events:** the first observation of the second and of the third class of
  each 3W series, only the first for the one-event examples.
* **Averages:** means over the examples, ignoring missing values.
* **Coverage:** the share of examples with a defined F1.
* **Quirks of the script kept:** its list repeats the key 3WELL14_2, so
  3WELL14_2 counts twice and 3WELL14_3 never. It names 1WELL6_1 "1WELL6",
  which matches no row. So there are 20 examples.

The fuzzy reference files index their rows from 1 and the others from the
member files' index; the comparison does the same.

Events come from the class labels where the series has them: 5 series, the
three of types 3 and 4 from the reference RData files and two type-5 series of
the `united` data. For the other 14 series they come from the change points of
the `harbinger`-format 3W data of `cefet-rj-dal/united`.

| method | precision | recall | F1 | coverage | paper |
|---|---|---|---|---|---|
| Page-Hinkley | 0.46 | 0.60 | 0.46 | 90% | 0.46 / 0.60 / 0.46 / 90% |
| KLDIST | 0.28 | 0.09 | 0.45 | 15% | 0.28 / 0.09 / 0.45 / 15% |
| MCDD | 0.38 | 0.65 | 0.42 | 90% | 0.38 / 0.65 / 0.41 / 91% |
| AEDD | 0.39 | 0.66 | 0.40 | 95% | 0.39 / 0.66 / 0.40 / 95% |
| KSWIN | 0.37 | 0.64 | 0.41 | 90% | 0.34 / 0.65 / 0.38 / 91% |
| ADWIN | 0.36 | 0.65 | 0.38 | 95% | 0.35 / 0.64 / 0.38 / 95% |
| CEDD TV (lambda = 2), ours = reference | 0.45 | 0.53 | 0.49 | 75% | 0.45 / 0.53 / 0.49 / 75% |
| CEDD EFER, ours = reference | 0.15 | 0.84 | 0.23 | 95% | 0.14 / 0.83 / 0.21 / 95% |
| CEDD MV, ours = reference | - | 0 | - | 0% | 0 / 0 / - / 0% |
| FEDD MV, reference files | 0.36 | 0.53 | 0.37 | 90% | 0.38 / 0.55 / 0.39 / 90% |
| FEDD MV, ours, k = 4000 | 0.33 | 0.54 | 0.34 | 90% | |
| FEDD TV (lambda = 4), reference files | 0.41 | 0.35 | 0.44 | 65% | 0.38 / 0.28 / 0.41 / 55% |
| FEDD TV (lambda = 4), ours, k = 4000 | 0.40 | 0.47 | 0.42 | 85% | |

What the table shows:

* **Base detectors and crisp ensembles:** the evaluation reproduces the
  paper's figures, coverage included, within 0.01, except KSWIN (+0.03 in
  precision and F1). The 2025 SoftED of `harbinger` may differ slightly from
  the version used for the paper.
* **Fuzzy ensembles:** neither the reference files nor `ensemble_vote()` with
  k = 4000 reproduce the paper exactly. The reference files mix tolerances
  (section 1), and the paper's rows come from yet another set of runs.
* **Research questions:** with the published member detections and k = 4000,
  the conclusions hold.
  * FEDD MV has coverage of 90% against 0% for CEDD MV (CEDD MV never
    detects).
  * CEDD TV has the best F1 (0.49).
  * FEDD TV (F1 0.42) is at or above every single detector except
    Page-Hinkley and KLDIST.

## Not reproduced

* The base detections themselves. They come from `run_experiments()` of a
  private `core.R`: normalisations, warm-up of 2000 observations and the
  members' parameters. `dfr_ensemble()` runs `heimdall` detectors online, but
  their detections on 3W are not those of the paper.
* The prequential analysis of Table III.
