# Validation: Student-Teacher Unsupervised Drift Detection (`dfr_studd`)

Checks `dfr_studd()` against Cerqueira, Gomes, Bifet and Torgo (2023), *STUDD:
a student-teacher method for unsupervised concept drift detection* (Machine
Learning 112, 4351–4378), and its code, <https://github.com/vcerqueira/studd>
(commit e3d25f5: `src/studd_batch.py`, `src/workflows.py`,
`experiments/main.py`).

```sh
# from the package root, on the host (the container has no SSL)
git -c http.sslBackend=schannel clone https://github.com/vcerqueira/studd data-real/studd/reference
git -c http.sslBackend=schannel clone https://github.com/scikit-multiflow/scikit-multiflow data-real/studd/skmultiflow
git -c http.sslBackend=schannel clone https://github.com/denismr/incremental-ks data-real/studd/incremental-ks  # c6bc988
mkdir -p data-real/studd/work/skmultiflow_053
for f in page_hinkley.py base_drift_detector.py; do
  git -C data-real/studd/skmultiflow show 0.5.3:src/skmultiflow/drift_detection/$f > data-real/studd/work/skmultiflow_053/$f
done
# extract incremental-ks/datasets/*.zip into data-real/studd/datasets
# inside the devcontainer
sh development/validation/studd/run_reference.sh 6 > development/validation/results/studd/reference_run.log
OPENBLAS_NUM_THREADS=1 Rscript development/validation/studd/run_studd.R 6 > development/validation/results/studd/run.log
```

`data-real/` is ignored by git. Results go to
`development/validation/results/studd/`. The reference runs take about 1.5 h
(the three Posture runs dominate, 85 min each) and `run_studd.R` about 3.3 h
on 6 cores.

## Reference code

scikit-multiflow is not installed and is not taken from PyPI.
`studd_reference.py` uses the class `STUDD` of `src/studd_batch.py` unchanged
and loads `page_hinkley.py` and `base_drift_detector.py` of scikit-multiflow
0.5.3 as they are. Two stand-ins replace the rest of the package:

* `BaseSKMObject`, an empty base class;
* `DataStream`, which returns the rows in order and slices like 0.5.3.

There is one more shim. The reference computes `int(model_yhat != std_model_yhat)`
on one-element arrays, which NumPy 2.5 rejects. The predictions are therefore
returned as an array view that supports `int()`, with the same values.

## Data

The data sets come from the incremental-ks repository cited by the paper:
AbruptInsects, Insects, Keystroke, Bike and Posture. The last column is the
class. Streams are truncated to 150 000 rows, as in the paper.

The settings follow `experiments/main.py` and `src/workflows.py`:

* random forests as teacher and student (`RF`/`RF`, Table 2);
* W = 500 for the small streams and 1000 for the others;
* Page-Hinkley with δ = 0.002 / 2;
* on each alarm, the teacher is retrained on the last W rows and the student on every row seen online.

The paper's text gives W = 2000. The ratios of labels in its Table 4 match
W = 1000, as in `main.py`.

## Checks

1. **Step-by-step equivalence.** The reference is run in trace mode with seed 1.
   Every `fit`/`predict` call of the teacher and of the student is recorded.
   `dfr_studd` is then run with models that replay these calls, using a port
   of the scikit-multiflow 0.5.3 Page-Hinkley test.

   | stream | fits | same fit rows/labels | same errors | alarms | κ (R / Python) |
   |---|---|---|---|---|---|
   | AbruptInsects | 6 / 6 | yes | yes | 2611, 4644 (both) | 0.5084 / 0.5084 |
   | Insects | 2 / 2 | yes | yes | none | 0.8093 / 0.8093 |
   | Keystroke | 2 / 2 | yes | yes | none | 0.8545 / 0.8545 |

2. **Page-Hinkley.** The package's `dfr_page_hinkley(delta = 0.001)` is
   compared with the 0.5.3 test on the same error streams.

   | stream | alarms, 0.5.3 port | alarms, `dfr_page_hinkley` | same |
   |---|---|---|---|
   | AbruptInsects | 2112, 4145 | 2111, 4150 | no |
   | Insects | none | none | yes |
   | Keystroke | none | none | yes |

   The alarms are online-row indices. The two tests use different statistics:
   * scikit-multiflow 0.5.3: `sum = max(0, α·sum + x − mean − δ)`;
   * `dfr_page_hinkley`: `sum − min(sum)`.

   They agree only when α = 1. `dfr_studd` accepts any univariate detector,
   and the default `dfr_page_hinkley` was not changed. Here the alarms differ
   by 1 and 5 rows.

3. **Data streams.** `dfr_studd` uses `cla_rf(ntree = 100, nodesize = 1, mtry = ⌊√p⌋)`.
   The reference uses scikit-learn's default random forests. Each is run with
   10 seeds (3 for Posture).

   | data set | κ paper | κ ref. | κ dfr_studd | alarms ref. | alarms dfr_studd | labels paper | labels ref. | labels dfr_studd | κ BL-st paper | κ BL-st ref. |
   |---|---|---|---|---|---|---|---|---|---|---|
   | AbruptInsects | 0.73 | 0.710 | 0.734 | 1.6 | 1.8 | 0.28 | 0.244 | 0.263 | 0.59 | 0.571 |
   | Insects | 0.81 | 0.812 | 0.811 | 0 | 0 | 0.09 | 0.094 | 0.094 | 0.81 | 0.812 |
   | Keystroke | 0.84 | 0.866 | 0.866 | 0 | 0 | 0.31 | 0.313 | 0.313 | 0.84 | 0.866 |
   | Bike | 0.32 | 0.009 | 0.062 | 0 | 0.3 | 0.06 | 0.058 | 0.075 | 0.31 | 0.009 |
   | Posture | 0.50 | 0.436 | 0.450 | 2.7 | 5.3 | 0.02 | 0.024 | 0.042 | 0.34 | 0.336 |

   Labels are the share of the stream used to train the teacher:
   (W + W·alarms) / rows. Mann-Whitney tests between `dfr_studd` and the
   reference over the seeds find no difference:
   * κ: p ≥ 0.39;
   * number of alarms: p ≥ 0.20.

   Values per seed are in `runs_dfr_studd.csv` and `runs_reference.csv`.

## Notes

* **Bike.** The reference matches neither the paper's STUDD κ nor its
  static baseline. The teacher learns from the first 1000 rows, which are
  winter rows and almost all "low", so it barely ever predicts "high". The
  paper's preprocessing of this data set is not in the repository (its
  `datasets_sample.pkl` was not unpickled).
* **Posture.** `dfr_studd` raises more alarms than the reference (5.3 against
  2.7), and its κ is close (0.45 against 0.44). Both are below the paper's
  0.50. The static baseline reproduces the paper (0.336 against 0.34).
