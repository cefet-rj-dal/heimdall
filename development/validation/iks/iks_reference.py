"""Reference traces of the Incremental Kolmogorov-Smirnov test (pure Python
IKS.py / Treap.py of https://github.com/denismr/incremental-ks, commit c6bc988)
for the validation of heimdall's iks() / dfr_iks().

Usage (inside the devcontainer, from the package root):
  python3 -I development/validation/iks/iks_reference.py <incremental_ks_dir> <out_dir> units
  python3 -I development/validation/iks/iks_reference.py <incremental_ks_dir> <out_dir> timing

units   random sequences of insertions and removals in the two groups (distinct
        values; tied values without tie-breaking; tied values with random
        tie-breaking pairs, as IKSSW), with KS(), Kuiper() and Test(ca) after
        every operation, and scipy.stats.ks_2samp on the same samples; and a
        reference / sliding window stream as testing_single_stream_rnd_factor.py.
        Writes units.json (operations, tie values and statistics).
timing  the experiments of Figures 1 and 2 of the paper with the pure Python
        IKS (always growing samples up to 5000; sliding windows of 100..1000 over
        streams of 10000), for scale.
"""

import json
import os
import random
import sys
import time

import numpy as np
from scipy.stats import ks_2samp

iks_dir, out_dir, mode = sys.argv[1:4]
os.makedirs(out_dir, exist_ok=True)
sys.path.insert(0, os.path.join(iks_dir, "IncrementalKS", "Pure Python"))
from IKS import IKS  # noqa: E402


def run_sequence(case, rng, n_ops, ties, tiebreak, ca=1.95):
    iks = IKS()
    live = [[], []]
    ops, stats = [], []
    for _ in range(n_ops):
        # keep both groups the same size after every pair of operations
        for group in (0, 1):
            if live[group] and (len(live[group]) > 60 or rng.random() < 0.35):
                obs = live[group].pop(rng.randrange(len(live[group])))
                iks.Remove(obs, group)
                kind = "remove"
            else:
                v = round(rng.gauss(0.3 * group, 1), 1) if ties else rng.gauss(0.3 * group, 1)
                obs = (v, rng.random()) if tiebreak else v
                live[group].append(obs)
                iks.Add(obs, group)
                kind = "add"
            value, tie = (obs if tiebreak else (obs, 0.0))
            ops.append({"op": kind, "group": group, "x": value, "tie": tie})
        if len(live[0]) == len(live[1]) and live[0]:
            a = [o[0] if tiebreak else o for o in live[0]]
            b = [o[0] if tiebreak else o for o in live[1]]
            stats.append({"after": len(ops), "ks": iks.KS(), "kuiper": iks.Kuiper(), "test": bool(iks.Test(ca)),
                          "ks_2samp": float(ks_2samp(a, b).statistic)})
    return {"case": case, "ops": ops, "stats": stats}


if mode == "units":
    rng = random.Random(2016)
    cases = []
    for case, (ties, tiebreak) in enumerate([(False, False), (True, False), (True, True)]):
        for rep in range(3):
            cases.append(run_sequence(f"ties={ties} tiebreak={tiebreak} rep={rep}", rng, 400, ties, tiebreak))
    # Reference and sliding window (testing_single_stream_rnd_factor.py), W = 100.
    nrng = np.random.RandomState(7)
    initial = nrng.normal(0, 1, 100)
    stream = np.concatenate([nrng.normal(1.0 * (i % 2), 1, 100) for i in range(6)])
    iks = IKS()
    ops, stats = [], []
    sliding = []
    for val in initial:
        r0, r1 = rng.random(), rng.random()
        iks.Add((val, r0), 0)
        iks.Add((val, r1), 1)
        sliding.append((val, r1))
        ops += [{"op": "add", "group": 0, "x": float(val), "tie": r0}, {"op": "add", "group": 1, "x": float(val), "tie": r1}]
    for val in stream:
        old = sliding.pop(0)
        iks.Remove(old, 1)
        w = (float(val), rng.random())
        iks.Add(w, 1)
        sliding.append(w)
        ops += [{"op": "remove", "group": 1, "x": old[0], "tie": old[1]}, {"op": "add", "group": 1, "x": w[0], "tie": w[1]}]
        stats.append({"after": len(ops), "ks": iks.KS(), "kuiper": iks.Kuiper(), "test": bool(iks.Test(1.95)),
                      "ks_2samp": float(ks_2samp(initial, [s[0] for s in sliding]).statistic)})
    cases.append({"case": "reference and sliding window, W = 100", "ops": ops, "stats": stats})
    with open(os.path.join(out_dir, "units.json"), "w") as f:
        json.dump({"cases": cases, "ca_for_pvalue_0.001": IKS.CAForPValue(0.001)}, f)
    print("units:", len(cases), "cases,", sum(len(c["ops"]) for c in cases), "operations")

elif mode == "timing":
    rows = []
    rng = np.random.RandomState(1)
    for rep in range(3):
        a, b = rng.normal(size=5000), rng.normal(size=5000)
        iks = IKS()
        start = time.time()
        for i in range(5000):
            iks.Add((a[i], random.random()), 0)
            iks.Add((b[i], random.random()), 1)
            iks.KS()
        rows.append({"experiment": "growing", "size": 5000, "rep": rep, "seconds": time.time() - start})
        for w in range(100, 1001, 100):
            sa, sb = rng.normal(size=10000), rng.normal(size=10000)
            iks = IKS()
            wa, wb = [], []
            for i in range(w):
                ta, tb = (sa[i], random.random()), (sb[i], random.random())
                iks.Add(ta, 0)
                iks.Add(tb, 1)
                wa.append(ta)
                wb.append(tb)
            start = time.time()
            for i in range(w, 10000):
                iks.Remove(wa.pop(0), 0)
                iks.Remove(wb.pop(0), 1)
                ta, tb = (sa[i], random.random()), (sb[i], random.random())
                iks.Add(ta, 0)
                iks.Add(tb, 1)
                wa.append(ta)
                wb.append(tb)
                iks.KS()
            rows.append({"experiment": "sliding", "size": w, "rep": rep, "seconds": time.time() - start})
    with open(os.path.join(out_dir, "timing_reference.csv"), "w") as f:
        f.write("experiment,size,rep,seconds\n")
        for r in rows:
            f.write(f"{r['experiment']},{r['size']},{r['rep']},{r['seconds']}\n")
    print("timing:", len(rows), "rows")
