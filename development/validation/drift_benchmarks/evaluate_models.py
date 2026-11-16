"""Scores the models of Run_Benchmarks.py on the streams written by run_benchmarks.R.

Hinder et al. (2026), "Real vs. Virtual Drift: Creating Realistic Stream
Learning Benchmarks", https://github.com/FabianHinder/Creating-Drift-Benchmarks.
The models, their settings and the scorer are those of Run_Benchmarks.py,
restricted to the five models shown in Fig. 2.

usage: python3 evaluate_models.py <output dir> [cores]

Reads <output dir>/streams/<source>.csv.gz and writes
<output dir>/scores/<source>.csv; sources that already have scores are skipped.
"""
import os

for var in ("OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS"):
    os.environ[var] = "1"

import glob
import sys
import warnings
from multiprocessing import Pool

import numpy as np
import pandas as pd
from sklearn.base import clone
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegressionCV
from sklearn.metrics import get_scorer
from sklearn.neighbors import KNeighborsClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.svm import LinearSVC

MODELS = [
    ("kNN", KNeighborsClassifier()),
    ("RF", RandomForestClassifier()),
    ("SVM", LinearSVC(max_iter=5000)),
    ("LR", LogisticRegressionCV(max_iter=5000)),
    ("MLP", MLPClassifier(max_iter=5000)),
]
SCORER = get_scorer("matthews_corrcoef")


def score(model, x, y):
    try:
        return SCORER(model, x, y)
    except Exception:
        return np.nan


def evaluate(task):
    source, rep, drift_type, stream = task
    warnings.filterwarnings("ignore")
    stream = stream.sort_values("segment", kind="stable")
    features = [c for c in stream.columns if c not in ("rep", "drift_type", "segment", "y", "drift", "verified")]
    x = stream[features].to_numpy(dtype=float)
    y = stream["y"].to_numpy()
    train = {"s": np.r_[0:400], "e": np.r_[0:400, 800:1000]}
    test_n, test_d = np.r_[400:600], np.r_[600:800]
    rows = []
    for train_type, rows_train in train.items():
        for name, model in MODELS:
            model = clone(model)
            try:
                model.fit(x[rows_train], y[rows_train])
            except Exception:
                model = None
            rows.append({
                "source": source, "rep": rep, "drift_type": drift_type, "train_type": train_type, "model": name,
                "mcc_no_drift": score(model, x[test_n], y[test_n]),
                "mcc_drift": score(model, x[test_d], y[test_d]),
            })
    return rows


def main():
    out_dir = sys.argv[1]
    cores = int(sys.argv[2]) if len(sys.argv) > 2 else max(1, os.cpu_count() - 1)
    os.makedirs(os.path.join(out_dir, "scores"), exist_ok=True)
    for path in sorted(glob.glob(os.path.join(out_dir, "streams", "*.csv.gz"))):
        source = os.path.basename(path)[: -len(".csv.gz")]
        target = os.path.join(out_dir, "scores", source + ".csv")
        if os.path.exists(target):
            print(f"{source}: already scored", flush=True)
            continue
        streams = pd.read_csv(path)
        tasks = [(source, rep, drift_type, group) for (rep, drift_type), group in streams.groupby(["rep", "drift_type"])]
        with Pool(cores) as pool:
            rows = [row for part in pool.imap_unordered(evaluate, tasks) for row in part]
        pd.DataFrame(rows).to_csv(target + ".tmp", index=False)
        os.replace(target + ".tmp", target)
        print(f"{source}: {len(tasks)} streams scored", flush=True)


if __name__ == "__main__":
    main()
