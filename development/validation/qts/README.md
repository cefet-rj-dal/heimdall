# Validation: quadtree spatial mapping drift detector (QTS)

Checks `dfr_qts()` against Ramos, Castro, Coelho and Angelov (2024),
*Unsupervised Drift Detection Using Quadtree Spatial Mapping* (ESANN 2024),
and its reference code (<https://github.com/beraram/QTS-method>:
`QTS_method.py`, `QTree_plus.py` and the bundled datasets).

```sh
# inside the devcontainer, from the package root
git clone https://github.com/beraram/QTS-method <tmp>
mkdir -p data-real/qts/reference
cp <tmp>/Datasets/*.csv data-real/qts/ && cp <tmp>/*.py data-real/qts/reference/

# 1. the reference loop on each dataset (over 10 minutes for Electricity)
for d in Toy RBF Weather_Norm Electricity_Norm; do
  python3 -I development/validation/qts/reference_qts.py data-real/qts/reference \
    data-real/qts/$d.csv development/validation/results/qts/reference/$d
done
# 2. dfr_qts on the same data, comparison and Table 1 (about 2 minutes on 4 cores)
Rscript development/validation/qts/run_qts.R 4
# 3. classifier pipeline of Table 2 (a few minutes)
python3 -I development/validation/qts/evaluate_pipeline.py data-real/qts development/validation/results/qts 5
```

`data-real/` is ignored by git. Results go to
`development/validation/results/qts/`.

* `reference_qts.py` runs the detection loop of `QTS_method.py` without
  changes. Only skmultiflow's `DataStream`, which does not install on current
  Python, is replaced by a loop over the same data frame.
* `run_qts.R` prepares the data exactly as the reference does: pandas takes
  the first row as the header, the features are min-max scaled over the whole
  file with sklearn's arithmetic, and the first remaining row only sets the
  dimension of the tree. It then runs `dfr_qts()` with its defaults, which are
  the reference parameters (height 4, `W` = 500, `Rw` = `Sw` = `f'(Sw)` = 1500,
  `RD` = 10% of `W`, alpha = 1, beta = 3). Positions are the reference
  `n_samples`.

## Results

**Equivalence with the reference code** (`equivalence.csv`). On all four
datasets the occupancy curve of `dfr_qts()` is identical to the reference's at
every sample, and so are the detections:

| dataset | samples | detections (reference = dfr_qts) |
|---|---|---|
| Toy | 19 998 | 5039, 10047, 13735, 15427 |
| RBF | 19 998 | 10669 |
| Electricity | 45 310 | 18395, 26512, 32028, 36218 |
| Weather | 18 156 | 3542, 15531, 18030 |

**Table 1** (`table1.csv`). The true drifts were read from the data, since the
paper does not list them: the spread of Toy changes every 5000 samples and
the centres of RBF move at 10 000. Metrics use the paper's definition with a
delay range of 1000 samples.

| dataset | | recall | precision | F1 | detections | drifts |
|---|---|---|---|---|---|---|
| Toy | paper | 0.8443 | 0.9978 | 0.9147 | 4 | 3/3 |
| Toy | dfr_qts | 0.8290 | 0.9996 | 0.9063 | 4 | 3/3 |
| RBF | paper | 0.331 | 1 | 0.4974 | 1 | 1/1 |
| RBF | dfr_qts | 0.331 | 1 | 0.4974 | 1 | 1/1 |

RBF is reproduced exactly: one detection, 669 samples after the drift. Toy
has the same detections (all three drifts plus one false alarm at 13735). The
small differences in recall and precision come from the exact drift positions
and from how false positives are weighed, which the paper does not specify.

**Fig. 2** (`occupancy.png`). The Electricity occupancy oscillates around 300
and jumps to about 400 near sample 18 000, as in the paper; QTS signals it at
18395.

**Table 2** (`table2.csv`). Random forest (scikit-learn defaults), trained on
the first `n_train` samples and retrained on the `n_train` samples after each
detection; mean of 5 seeds. BL1 never retrains, and BL2 retrains periodically
as often as QTS. The paper does not give `n_train`, so three values are
reported:

| dataset | | QTS | BL1 | BL2 |
|---|---|---|---|---|
| Electricity | paper | 0.7318 | 0.6612 | 0.7034 |
| Electricity | n_train = 500 | 0.652 | 0.631 | 0.688 |
| Electricity | n_train = 1000 | 0.706 | 0.681 | 0.730 |
| Electricity | n_train = 2000 | 0.725 | 0.684 | 0.742 |
| Weather | paper | 0.7409 | 0.7409 | 0.7358 |
| Weather | n_train = 500 | 0.751 | 0.758 | 0.763 |
| Weather | n_train = 1000 | 0.765 | 0.769 | 0.773 |
| Weather | n_train = 2000 | 0.778 | 0.771 | 0.775 |

Retraining at the QTS detections always beats the static classifier on
Electricity (+2.1 to +4.1 points), as in the paper. Unlike the paper, the
periodic baseline BL2 is better than QTS on Electricity, by 1.7 to 3.6 points.
On Weather, as in the paper, the three are within about one point: QTS is the
best with `n_train = 2000` and slightly below the baselines with smaller
training sets. The pipeline details the paper leaves open (training size, what
the old classifier does while the new training set is collected, how K is
chosen) change these numbers by several points, so only the qualitative
comparison is meaningful. D3, OCDD and STUDD were not run.

## Differences from the reference code

* **Derivative rule.** The code compares `|mean(RD)|` with
  `mean(OD) + alpha * sd(OD)`, while the paper and its pseudocode compare
  `|mean(RD) - mean(OD)|` with `alpha * sd(OD)`. `dfr_qts()` uses the code's
  rule by default and offers the paper's with `derivative_rule = "paper"`. On
  these datasets the two differ only in the last Toy detection (15427 or
  15462).
* **Occupancy without the tree.** `dfr_qts()` counts the leaves from the cell
  counts of every level instead of rebuilding the tree at every sample; the
  test suite checks it against a port of `QTree_plus.Node.insert`, including
  ties on cell boundaries and duplicated points.
* **Scaling.** The reference min-max scales each dataset over the whole file
  before streaming. `dfr_qts()` takes the bounds as `lower` and `upper`
  (defaults 0 and 1), and values outside them fall in the outermost cells, as
  in the reference tree.
* **Window sizes.** The defaults are the code's (`W` = 500, 1500 for the
  occupancy windows). The paper recommends 1000 for most cases. With small
  windows and many cells the occupancy rarely changes and the derivative
  strategy reacts to noise, which the documentation points out.
