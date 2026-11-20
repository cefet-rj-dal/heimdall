# Validation: online Kolmogorov-Smirnov test (`greedyks`, `dfr_greedyks`)

Checks `greedyks()`, `ks_sf()` and `dfr_greedyks()` against Cardoso and
Galeno (2023), *Online evaluation of the Kolmogorov-Smirnov test on arbitrarily
large samples* (Journal of Computational Science), and its code:

* <https://github.com/moretoknow/driftwatch> (commit f1cdf2e), with `GreedyKS`
  and the stream detector;
* <https://github.com/moretoknow/greedyks_experiments> (commit 0647300), with
  the protocols of the experiments (`util.py`) and the call center notebook.

## What is implemented

| reference | heimdall |
|---|---|
| bucket sketch with `m` memory units, Fenwick trees, greedy D+ / D- | `greedyks(cdf, m, min_instances)`, `greedyks_update()`, `greedyks_statistic()` |
| p-value with `scipy.stats.kstwo.sf` | `greedyks_pvalue()`, `ks_sf()` (Simard and L'Ecuyer, ported from scipy `_ksstats.py`) |
| stream detector: reference sample, fitted `norm` / `expon` / `uniform`, grace period, restart after a drift | `dfr_greedyks(cdf, m, alpha, grace_period, reference_size)` |

## Differences from the paper

* The bucket size is `ceiling(f * m)`, as in the code. The paper writes the floor.
* For n > 140 and n d^2 < 2.2 the exact p-value is never below 0.0224. In
  that region, when alpha < 0.02, the detector uses the Pelz-Good
  approximation. The decision does not change, but the reported p-value can
  differ slightly from `kstwo.sf`. Use `exact = TRUE` in `greedyks_pvalue()`
  for the scipy value.
* The reference needs `fenwick`, `segment_tree` and `ddsketch`, which are not
  in the container. They are not downloaded from PyPI;
  `greedyks_reference.py` defines small stand-ins with the same behaviour
  (DDSketch is only used by baselines that are not reproduced here).
* The Technion server of the call center data was unreachable. The same files
  (444,448 records, as in the paper) were taken from the mirror of the
  University of Bern.
* The runs of the paper have no seed, so the effectiveness and error
  experiments only agree statistically. The `.pkl` files with the results
  of the authors were not loaded, because they are untrusted pickles.

## Results (`development/validation/results/greedyks/`)

* `units.csv`. The statistic D after each of 3,000 observations, for the
  normal, uniform and exponential distributions with m = 1 to 316 and
  `min_instances` 0 and 50, differs from the reference by at most 1.1e-16.
  The final indexes are the same and so are all the decisions (p <= 0.01).
  `ks_sf()` agrees with scipy within a relative 1.25e-11.
* `error_summary.csv` (Figures 2, 3 and 5). The absolute error of D against
  the exact KS has the same mean as the reference in all 36 settings
  (Wilcoxon p > 0.04), and is close to the values read off the plots.
* `effectiveness_summary.csv` (Tables 2-5). The premature, missed and mean
  delay counts of heimdall, of the reference and of the paper are of the same
  order in every setting (100 runs each).
* `callcenter_summary.csv` (Table 6 and Figure 7):
  * Table 6 is reproduced exactly: drift at index 1638 (1999-01-04 21:41:26),
    D = 0.039804, for both GreedyKS and the exact KS.
  * The weekly DTW, the mini-batches and the alarms are identical to the
    reference in all 53 weeks for m = 31, 100 and 316.

## Commands

```sh
# on the host (the container has no SSL)
git -c http.sslBackend=schannel clone https://github.com/moretoknow/driftwatch data-real/greedyks/reference
git -c http.sslBackend=schannel clone https://github.com/moretoknow/greedyks_experiments data-real/greedyks/experiments
# call center data (Bern mirror) in data-real/greedyks/callcenter/raw/*.txt

# inside the devcontainer, from the package root
python3 -I development/validation/greedyks/greedyks_reference.py data-real/greedyks/reference data-real/greedyks/work units
Rscript development/validation/greedyks/check_units.R data-real/greedyks/work development/validation/results/greedyks/units.csv
sh development/validation/greedyks/run_reference.sh 4 > development/validation/results/greedyks/reference_run.log
sh development/validation/greedyks/run_heimdall.sh 4 > development/validation/results/greedyks/heimdall_run.log
cp data-real/greedyks/work/*_reference.* development/validation/results/greedyks/
Rscript development/validation/greedyks/summarize.R development/validation/results/greedyks
```

`data-real/` is ignored by git.
