# Validation: Parallel Activations Drift Detector (`dfr_padd`)

Checks `dfr_padd()` against Komorniczak and Ksieniewicz (2024), *Unsupervised
Concept Drift Detection based on Parallel Activations of Neural Network*
(arXiv:2404.07776), and its code, <https://github.com/w4k2/padd>
(commit 7c349c4: `methods.py`, `e2.py`, `utils.py`, `analyze_e2.py`).

```sh
# from the package root, on the host (the container has no SSL)
git -c http.sslBackend=schannel clone https://github.com/w4k2/padd data-real/padd/reference
git -c http.sslBackend=schannel clone https://github.com/w4k2/stream-learn data-real/padd/stream-learn  # v0.9.2, 97498a9
# inside the devcontainer
python3 -I development/validation/padd/padd_reference.py data-real/padd/reference data-real/padd/stream-learn data-real/padd/work streams
python3 -I development/validation/padd/padd_reference.py data-real/padd/reference data-real/padd/stream-learn data-real/padd/work trace
python3 -I development/validation/padd/padd_reference.py data-real/padd/reference data-real/padd/stream-learn data-real/padd/work run \
  > development/validation/results/padd/reference_run.log
OPENBLAS_NUM_THREADS=1 Rscript development/validation/padd/run_padd.R 4 > development/validation/results/padd/run.log
```

`data-real/` is ignored by git. Results go to
`development/validation/results/padd/`. The full validation takes about
15 minutes.

## Data

The data are the synthetic streams of the paper's experiment 2 (`e2.py`). They
are built with `StreamGenerator` from stream-learn:

* 250 chunks of 200 observations each;
* 30, 60 or 90 features, of which 30 % are informative;
* 3, 5, 10 or 15 concept drifts;
* gradual (`concept_sigmoid_spacing = 5`) or sudden (999) drifts;
* 10 replications, with random states `np.random.seed(1410); randint(100, 10000, 10)`.

That gives 240 streams. stream-learn has no PyPI install here, so it is used
from its source tree; `wget`, which it imports only to download benchmarks, is
replaced by an empty module. The streams are written as float32. The
detectors use the settings of `e2.py`:

* `e = r = 12`, `s = 50`, 10 hidden units;
* gradual drifts: `alpha = 0.13`, threshold 0.26;
* sudden drifts: `alpha = 0.07`, threshold 0.19.

## Checks

1. **Metrics.** The drift detection errors of `utils.dderror` and
   `get_real_drfs` of `analyze_e2.py` are ported to R:
   * D1: mean distance from each detection to the nearest drift;
   * D2: mean distance from each drift to the nearest detection;
   * R: |drifts / detections − 1|.

   Applied to the detections stored in `res/exp1_comp_final.npy`, the port
   reproduces the PADD column of `tables/table_d1.txt`, `table_d2.txt` and
   `table_cnt_ratio.txt` (Figure 6 of the paper). The largest difference is
   0.0005, and the tables have 3 decimals.
2. **Step-by-step equivalence.** The unchanged `PADD` class is instrumented to
   record three things: the network weights, every sample index drawn by
   `np.random.choice` (replaced by the equivalent `randint` draw, which is
   asserted), and every t-test p-value. This is done on two streams:
   replication 0, 30 features, 10 drifts, gradual and sudden. `dfr_padd` is
   then given the same weights and indices.

   | stream | tests | max \|Δp\| | detections (R / Python) | same chunks |
   |---|---|---|---|---|
   | gradual | 35 856 | 4.5e-14 | 11 / 11 | yes |
   | sudden | 35 856 | 5.9e-14 | 11 / 11 | yes |
3. **Experiment 2.** Two runs are made on the 240 regenerated streams:
   `dfr_padd` with its own random draws (seeds 1000 + i), and the unchanged
   reference class with `np.random.seed(1410)`. Both are compared with the
   paper's values.

   | drift | D1 paper | D1 ref. | D1 dfr_padd | D2 paper | D2 ref. | D2 dfr_padd | R paper | R ref. | R dfr_padd |
   |---|---|---|---|---|---|---|---|---|---|
   | gradual | 7.82 | 7.11 | 7.45 | 11.09 | 9.17 | 11.05 | 0.217 | 0.220 | 0.238 |
   | sudden | 2.45 | 2.31 | 2.34 | 5.28 | 5.79 | 6.05 | 0.142 | 0.175 | 0.178 |

   The values are means over the 12 configurations of each drift type, each
   averaged over 10 replications. Per-configuration values are in
   `experiment2.csv`. The detection counts follow the number of drifts: about
   4, 5, 10 and 14 detections for 3, 5, 10 and 15 drifts.
4. `fit()` on the rows of a stream gives the detections of the chunk loop.

## Deviations

* **The paper's detections can't be reproduced exactly.** `e2.py` runs every
  detector on one global NumPy generator, so the samples PADD drew depend on
  the other methods. Even rerunning the unchanged reference class
  (`reference_detections.npy`) does not give the stored detections. Equality
  is therefore checked with the reference's own draws (check 2), and the
  rest statistically (check 3).
* **Package versions differ.** The streams come from stream-learn 0.9.2 with
  scikit-learn 1.9, which may not be the versions behind the paper.
  Differences between the paper and both reruns are of the same size as the
  differences between the paper's replications (e.g. sudden drifts with
  3 drifts and 60 features).
* **Undefined t-tests.** When the two samples are constant, SciPy returns
  NaN. `dfr_padd` treats that test as not significant, as the comparison
  `p < alpha` does in the reference.
