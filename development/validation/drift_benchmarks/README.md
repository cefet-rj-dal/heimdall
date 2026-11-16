# Validation: real vs. virtual drift benchmarks

Checks the `dsg_*` functions against the experiments of Hinder et al. (2026),
*Real vs. Virtual Drift: Creating Realistic Stream Learning Benchmarks*
(ESANN 2026), following the reference code
(<https://github.com/FabianHinder/Creating-Drift-Benchmarks>):
`Run_Benchmarks.py` for the experiment and `Analyze.ipynb` for Fig. 2.

```sh
# inside the devcontainer, from the package root
OPENBLAS_NUM_THREADS=1 Rscript development/validation/drift_benchmarks/run_benchmarks.R 500 8
```

Arguments: repetitions per source and drift type (500, as in the paper), cores,
optional comma-separated sources, optional output directory. The real-world
streams bundled with the reference code are downloaded to `data-real/`
(ignored by git). Results go to `development/validation/results/drift_benchmarks/`.
`OPENBLAS_NUM_THREADS=1` keeps the parallel workers from oversubscribing
the CPU with BLAS threads.

The run has two steps, so that the stream generator is the only part that
differs from the reference:

1. `run_benchmarks.R` generates the streams with the `dsg_*` functions and
   writes them to `streams/<source>.csv.gz`, along with the quality of each
   labelling model (`streams/<source>_quality.csv`).
2. It then calls `evaluate_models.py`, which fits the scikit-learn models of
   `Run_Benchmarks.py` with the same settings and scorer, writing
   `scores/<source>.csv`. It finishes with the analysis of `Analyze.ipynb`:
   CSV tables and the two panels of Fig. 2.

Both steps skip sources that are already done, so an interrupted run can be
resumed.

## Experiment

For each source and drift type (`v` or `r`), each repetition draws the stream
`---<type>-`, with 5 segments of 200 points:

| segment | role |
|---|---|
| 1-2 | training set "s" (400 points) |
| 3 | test set without drift |
| 4 | test set after the drift |
| 5 | extra drifted points; "s" + 5 is the extended training set "e" |

Logistic regression (`LogisticRegressionCV`), MLP (`MLPClassifier`), random
forest, linear SVM (`LinearSVC`) and k-NN are fitted with scikit-learn defaults
(`max_iter = 5000` where the reference sets it) and scored with the Matthews
correlation coefficient. Fig. 2
shows, per source, model and drift type, the share of repetitions where

* **(a)** MCC(test without drift) >= MCC(test after the drift), training on "s";
* **(b)** MCC(trained on "e") <= MCC(trained on "s"), on the test set without drift.

Sources, as in `Analyze.ipynb`:

* synthetic: Agrawal, Mixed, SEA and Sine, whose virtual drift comes from
  regional sub-sampling with k-means;
* real-world: Electricity, ForestCover and Weather with their natural real
  drift, and those plus CreditCard and Http with k-means label flips
  ("Flip").

## Differences from the reference code

* **No RandomTree source.** river's RandomTree generator has no R
  counterpart and was left out of the synthetic set.
* **Random forests instead of ExtraTrees.** The labelling model is always a
  random forest from `randomForest`, with the reference's leaf size: 10 for
  Electricity, 3 for the others.
* **Sources reused across repetitions.** Real-world sources are built 5 times
  (2 times for Http and CreditCard, whose forests take minutes to fit), and the
  repetitions are spread over them. The reference rebuilds the source in every
  repetition. Synthetic sources are rebuilt every time, as in the reference.
* **Only the five models of Fig. 2.** `Run_Benchmarks.py` also fits
  ExtraTrees, a decision tree and a perceptron, which the figure does not show.
* **Every time block gets a labelling rule.** `k` real blocks give `k + 1` time
  segments, and the reference builds labelling rules only for the first `k`
  of them. Here every segment gets one, which gives the `n x n` concepts the
  paper describes.
* **Built-in sources load their data.** `FileDataSource.__init__` in the
  reference has its `data` branch inverted, so the built-in real-world sources
  are created with `self.data = None` as committed. The implementation here
  loads the data as intended.

## Results

Run with 500 repetitions per source and drift type, 8 cores
(`results/drift_benchmarks/`: `fig2a.png`, `fig2b.png`, the per-source and
median CSVs, `streams_summary.csv` and `labelling_model_quality.csv`).

**Streams.** Every drift that was kept passed the MMD check of `dsg_drift`
(`verified` = `valid_streams` in `streams_summary.csv`). Mixed, Sine,
Electricity, Weather and the Flip versions of Electricity and Weather gave all
500 streams per type. Repetitions are lost when a drawn drift is too weak to be
verified: Agrawal 406 (r) / 432 (v), ForestCover 325 / 195,
ForestCoverFlip 329 / 234, HttpFlip 393 / 363. The labelling forests reach
OOB accuracies of 0.75 (WeatherFlip) to 0.98 (HttpFlip).

**Fig. 2a**: share of repetitions where the drift lowers the MCC; median over
sources.

| model | synthetic, real | synthetic, virtual | real-world, real | real-world, virtual |
|---|---|---|---|---|
| LR  | 0.97 | 0.68 | 0.57 | 0.68 |
| MLP | 0.99 | 0.78 | 0.57 | 0.61 |
| RF  | 1.00 | 0.92 | 0.60 | 0.62 |
| SVM | 0.96 | 0.66 | 0.57 | 0.63 |
| kNN | 0.97 | 0.93 | 0.54 | 0.71 |

**Fig. 2b**: share of repetitions where adding drifted points to the training
set does not help on the pre-drift test set.

| model | synthetic, real | synthetic, virtual | real-world, real | real-world, virtual |
|---|---|---|---|---|
| LR  | 0.91 | 0.68 | 0.47 | 0.48 |
| MLP | 0.85 | 0.55 | 0.48 | 0.47 |
| RF  | 0.97 | 0.40 | 0.46 | 0.42 |
| SVM | 0.86 | 0.63 | 0.39 | 0.50 |
| kNN | 0.85 | 0.57 | 0.42 | 0.42 |

**Comparison with the paper.** The qualitative pattern of Fig. 2 is reproduced:

* On synthetic sources, real drift almost always lowers the performance
  (0.96–1.00) and hurts training (0.85–0.97), clearly more than virtual drift,
  so the generated real drift does change the concept.
* On real-world sources the two drift types are much closer. Real drift lowers
  performance in only 54–60 % of the repetitions, with virtual drift slightly
  above it, and adding drifted data hurts in fewer than half of the
  repetitions for both types. Natural real-world concepts are smoother, so
  the "realistic" real drift is milder than the synthetic one, which is the
  paper's main point.
* HttpFlip and CreditCardFlip are the strongest real-world sources (0.58–0.97
  in 2a for real drift), and ForestCover the weakest, below 0.5 for some
  models in 2b.

The paper singles out the linear SVM as an exception; here it behaves like the
other models. The remaining differences in level are expected from the
deviations listed above, chiefly random forests instead of ExtraTrees as
labelling models, the missing RandomTree source, and sources reused across
repetitions.
