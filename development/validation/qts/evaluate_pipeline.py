"""Classifier pipeline of Table 2 (Ramos et al., 2024, Section 3).

A random forest with scikit-learn defaults is trained on the first
`n_train` samples and predicts every later sample. With a drift detector, a
detection discards the classifier and a new one is trained on the next
`n_train` samples, which the old classifier still predicts while they arrive.
Baselines: BL1 never retrains; BL2 retrains every K samples, with K chosen so
that it retrains as often as QTS. The paper does not give `n_train`, so the
script reports several values.

Usage: python3 -I evaluate_pipeline.py <data dir> <results dir> [seeds=5]
Reads <results dir>/detections.csv (written by run_qts.R) and writes
<results dir>/table2.csv.
"""

import os
import sys

import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier

data_dir, out_dir = sys.argv[1], sys.argv[2]
n_seeds = int(sys.argv[3]) if len(sys.argv) > 3 else 5
datasets = {"Electricity": "Electricity_Norm", "Weather": "Weather_Norm"}
train_sizes = [500, 1000, 2000]
detections = pd.read_csv(os.path.join(out_dir, "detections.csv"))


def blocks(n, n_train, retrain_rows):
    """Splits rows n_train..n-1 into blocks served by one classifier each:
    (start, end, (train_start, train_end)). A trigger at row r trains a new
    classifier on rows r + 1 .. r + n_train, used from row r + n_train + 1;
    triggers while that training set is being collected are ignored."""
    trigger = sorted(r for r in retrain_rows if r >= n_train)
    out = []
    train = (0, n_train)
    start = n_train
    i = 0
    while start < n:
        # next swap: the first trigger at or after the current block start,
        # the new classifier starting n_train samples after it.
        while i < len(trigger) and trigger[i] < start - 1:
            i += 1
        if i < len(trigger):
            swap = min(trigger[i] + 1 + n_train, n)
            i += 1
        else:
            swap = n
        out.append((start, swap, train))
        train = (swap - n_train, swap)
        start = swap
    return out


def evaluate(X, y, retrain_rows, n_train, seed):
    correct = 0
    total = 0
    used = 0
    for start, end, (a, b) in blocks(len(y), n_train, retrain_rows):
        if end <= start:
            continue
        model = RandomForestClassifier(random_state=seed, n_jobs=1).fit(X[a:b], y[a:b])
        correct += int((model.predict(X[start:end]) == y[start:end]).sum())
        total += end - start
        used += 1
    return correct / total, used - 1


rows = []
for name, file in datasets.items():
    df = pd.read_csv(os.path.join(data_dir, file + ".csv")).astype(float)
    X = df.iloc[:, :-1].to_numpy()
    y = df.iloc[:, -1].to_numpy()
    n = len(y)
    for rule in ["code", "paper"]:
        found = detections[(detections.dataset == name) & (detections["rule"] == rule)]["sample"].to_numpy()
        # n_samples counts from 1 over the rows of the data frame.
        qts_rows = [int(s) - 1 for s in found]
        for n_train in train_sizes:
            qts_triggers = [r for r in qts_rows if r >= n_train]
            k = n // (len(qts_triggers) + 1) if qts_triggers else n
            methods = {
                "QTS": qts_rows,
                "BL1": [],
                "BL2": list(range(k, n, k))[:len(qts_triggers)],
            }
            for method, retrain_rows in methods.items():
                if rule == "paper" and method == "BL1":
                    continue
                scores = [evaluate(X, y, retrain_rows, n_train, seed) for seed in range(n_seeds)]
                rows.append({"dataset": name, "rule": rule, "n_train": n_train, "method": method,
                             "accuracy": float(np.mean([s[0] for s in scores])),
                             "accuracy_sd": float(np.std([s[0] for s in scores])),
                             "retrainings": scores[0][1]})
                print(rows[-1], flush=True)

pd.DataFrame(rows).to_csv(os.path.join(out_dir, "table2.csv"), index=False)
