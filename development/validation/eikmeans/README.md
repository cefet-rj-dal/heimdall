# Validation: equal intensity k-means space partitioning (EI-kMeans)

Checks `eikmeans()`, `eikmeans_test()` and `dfr_eikmeans()` against Liu, Lu
and Zhang (2021), *Concept drift detection via equal intensity k-means space
partitioning* (IEEE Transactions on Cybernetics 51(6)), and its reference code
(<https://github.com/Anjin-Liu/TCYB2019-EIkMeansDriftDetection>:
`EIkMeans_lib.py`, `EIkMeans_Exp2.py`, `EIkMeans_Exp3.py` and the data
generators, with the original real-world data files).

```sh
# inside the devcontainer, from the package root
git clone https://github.com/Anjin-Liu/TCYB2019-EIkMeansDriftDetection data-real/eikmeans/reference

# 1. data from the reference generators and the reference detector on it
MPLBACKEND=Agg python3 -I development/validation/eikmeans/reference_eikmeans.py \
  data-real/eikmeans/reference data-real/eikmeans development/validation/results/eikmeans/reference
# 2. eikmeans() on the same data, comparison, error tables, 20 more data seeds
OPENBLAS_NUM_THREADS=1 Rscript development/validation/eikmeans/run_eikmeans.R 6 > \
  development/validation/results/eikmeans/run.log
```

`data-real/` is ignored by git. Results go to
`development/validation/results/eikmeans/`.

`reference_eikmeans.py` runs the reference code unchanged. It only captures
the p-value from scipy's `chi2_contingency`, moves the files of the Exp2
generator (which joins paths with Windows separators) into place, and replaces
`matplotlib.pyplot` by no-ops when matplotlib is missing (`fill_lambda` draws
a scatter plot that does not affect the results).

## 1. Equivalence with the reference code (`equivalence.csv`)

32 partitions: the six 2-D datasets of Experiment 2 at N = 2000, 3000, 4000
and 5000, and the eight real-world tests of the paper's Experiment 4. Each is
checked on 250 stationary and 250 drifted windows.

| check | result |
|---|---|
| number of bins | identical in all 32 (2 to 33 bins) |
| centroids | max difference 7e-13 |
| amplify-shrink coefficients | identical |
| training counts per bin | identical |
| p-values (16,000 windows) | max difference 4e-15 |
| drift decisions | 16,000 of 16,000 identical |

## 2. Experiment 2: 2-D synthetic data (Tables III-V)

Test windows of n = 200, alpha = 5%, k = 40. The partition is built on the
first 2000 training points and refilled with N points. Below: N = 2000, mean
over 20 data seeds drawn in R from the generator's distributions
(`exp2_seeds_summary.csv`), against the paper.

| dataset | Type-I (%) | paper | Type-II (%) | paper |
|---|---|---|---|---|
| 2d-U-mean | 5.06 | 4.79 | 46.5 | 45.00 |
| 2d-1G-mean | 5.02 | 4.90 | 36.8 | 40.84 |
| 2d-1G-var | 5.02 | 4.90 | 78.8 | 80.46 |
| 2d-1G-cov | 5.02 | 4.90 | 80.7 | 80.84 |
| 2d-2G-mean | 3.56 | 3.82 | 58.2 | 57.43 |
| 2d-4G-mean | 2.12 | 2.31 | 47.9 | 46.42 |

All are within one standard deviation across seeds (Type-I sd 1-2.3 points,
Type-II sd 4-15 points). With N = 3000-5000 (Table V) the Type-II errors
decrease slightly, by up to 2.5 points, and the Type-I errors stay at the same
level, as in the paper. On the single dataset of the reference generator
(seed 1, `errors_reference_data.csv`) the errors are noisier (Type-II 41-84%)
but have the same pattern.

## 3. Experiment 4: real-world data (Tables X-XI)

One draw of the reference generator (seed 1, 250 windows of each kind),
`errors_reference_data.csv`.

| test | bins | Type-I (%) | paper | Type-II (%) | paper |
|---|---|---|---|---|---|
| Higgs-Back | 31 | 4.0 | 6.34 | 82.8 | 87.16 |
| Back-Higgs | 29 | 19.2 | 9.09 | 69.6 | 74.02 |
| Sign-Back | 6 | 4.4 | 4.67 | 0.0 | 0.00 |
| Back-Sign | 2 | 2.4 | 4.06 | 0.0 | 0.00 |
| Arabic A-B | 12 | 4.0 | 4.40 | 11.6 | 12.92 |
| Arabic B-A | 9 | 4.8 | 4.45 | 30.0 | 12.10 |
| Localization | 18 | 0.0 | 0.00 | 0.0 | 62.00 |
| Insect | 24 | 3.2 | 2.81 | 27.6 | 12.37 |

The decisions are those of the reference code on this data (section 1).
The largest gaps from the paper are the Type-I error of Back-Higgs (19.2%
against 9.09%), and the Type-II errors of Arabic B-A, Insect and Localization
(0% against 62%). They come from the data draw (one seed of the generator
here; the paper does not give its seeds) or from differences between the code
used for the paper and the published one, not from the port.

## Code against paper

The implementation follows the reference code where it differs from the
paper:

* theta goes from 0 to 0.95 in steps of 0.05, not up to 1.5.
* The initial number of clusters is min(k, n_test / 5, N / 50).
* The greedy initialization uses the distance to the (N/K)-th nearest
  neighbour (the point itself included), and k-means starts from the first
  point of each group, not the group mean.
* The partition is built on the first 2000 training points.
* The chi-square test uses the Yates correction when there is one degree of
  freedom (scipy's default).
* k-means follows scikit-learn's Lloyd iteration: the stopping rule and the
  relocation of empty clusters.

Not reproduced: the other detectors of the comparison (kdq-tree, QuantTree,
PCA-SPLL, and others) and the classification experiments with drift
adaptation.
