"""Outputs of the reference QuantTree code used to check the R port.

Usage: python3 -I reference_check.py <dir with the quanttree package> <output dir>

1. partitions/: for 5 seeds, training data, the leaves of
   QuantTreePartitioning (dimension, lower, upper, side of each bin) and the
   bins _find_bin gives to the training data and to new data.
2. thresholds.csv: the pre-computed thresholds of the total variation
   statistic shipped with the reference (all_distr_quanttree.pkl), at the
   target FPRs of the paper.
3. thresholds_nonuniform.csv: thresholds of QuantTreeThresholdStrategy for
   non-uniform bin probabilities (the case of calibrated MMQT modalities),
   simulated with the reference procedure.
"""

import os
import pickle
import sys

import numpy as np

sys.path.insert(0, sys.argv[1])
from quanttree.quanttree_src import QuantTreePartitioning, QuantTreeThresholdStrategy  # noqa: E402

out_dir = sys.argv[2]
os.makedirs(os.path.join(out_dir, "partitions"), exist_ok=True)

# 1. Partitionings
for seed in range(5):
    rng = np.random.default_rng(100 + seed)
    dim = [2, 3, 5, 8, 16][seed]
    bins = [4, 10, 16, 32, 64][seed]
    data = rng.normal(size=(1000 + 512 * seed, dim))
    data[:50, 0] = np.round(data[:50, 0], 1)  # ties
    new = rng.normal(0.2, 1.1, size=(3000, dim))
    np.random.seed(seed)
    part = QuantTreePartitioning(pi_values=bins)
    part.build_partitioning(data)
    prefix = os.path.join(out_dir, "partitions", f"seed{seed}_")
    np.savetxt(prefix + "data.csv", data, delimiter=",", fmt="%.17g")
    np.savetxt(prefix + "new.csv", new, delimiter=",", fmt="%.17g")
    np.savetxt(prefix + "leaves.csv", part.leaves.T, delimiter=",", fmt="%.17g")
    np.savetxt(prefix + "bins_data.csv", part.find_bin(data), fmt="%d")
    np.savetxt(prefix + "bins_new.csv", part.find_bin(new), fmt="%d")

# 2. Pre-computed thresholds (statistic in counts, as in the reference)
with open(os.path.join(sys.argv[1], "quanttree", "thresholds", "quanttree_thresholds",
                       "all_distr_quanttree.pkl"), "rb") as f:
    table = pickle.load(f)
rows = ["bins,n_train,nu,alpha,threshold"]
for (statistic, bins, n_train, nu), (thresholds, ecdf) in table.items():
    if statistic != "tv":
        continue
    for alpha in [0.005, 0.01, 0.02, 0.05]:
        rows.append(f"{bins},{n_train},{nu},{alpha},{thresholds[np.sum(ecdf <= 1 - alpha)]}")
with open(os.path.join(out_dir, "thresholds.csv"), "w") as f:
    f.write("\n".join(rows) + "\n")

# 3. Non-uniform bin probabilities, reference simulation
rows = ["case,bins,n_train,nu,alpha,threshold"]
np.random.seed(7)
cases = {
    "skewed16": (np.linspace(1, 3, 16) / np.linspace(1, 3, 16).sum(), 64 * 128, 128),
    "sparse8": (np.array([0.3, 0.25, 0.2, 0.1, 0.08, 0.04, 0.02, 0.01]), 32 * 128, 128),
}
for case, (pi, n_train, nu) in cases.items():
    strategy = QuantTreeThresholdStrategy(nu=nu, ndata_training=n_train, pi_values=pi, nbatch=20000,
                                          statistic_name="tv")
    stats = strategy.estimate_quanttree_sim()
    values = np.unique(stats)
    ecdf = np.array([np.sum(stats <= v) / stats.shape[0] for v in values])
    for alpha in [0.005, 0.01, 0.02, 0.05]:
        rows.append(f"{case},{len(pi)},{n_train},{nu},{alpha},{values[np.sum(ecdf <= 1 - alpha)]}")
    np.savetxt(os.path.join(out_dir, f"pi_{case}.csv"), pi, fmt="%.17g")
with open(os.path.join(out_dir, "thresholds_nonuniform.csv"), "w") as f:
    f.write("\n".join(rows) + "\n")
print("done")
