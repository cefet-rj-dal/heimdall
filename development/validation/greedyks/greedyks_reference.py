"""Reference runs of GreedyKS (driftwatch, commit f1cdf2e) for the validation
of heimdall's greedyks() / dfr_greedyks().

Usage (inside the devcontainer, from the package root):
  python3 -I development/validation/greedyks/greedyks_reference.py <driftwatch_dir> <out_dir> units
  python3 -I development/validation/greedyks/greedyks_reference.py <driftwatch_dir> <out_dir> \
      error <experiment> [processes]
  python3 -I development/validation/greedyks/greedyks_reference.py <driftwatch_dir> <out_dir> \
      effectiveness <experiment> [processes]
  python3 -I development/validation/greedyks/greedyks_reference.py <driftwatch_dir> <out_dir> \
      callcenter <raw_dir> [processes]

<driftwatch_dir> is the clone of https://github.com/moretoknow/driftwatch. Its
modules greedyks.py and prefix_sum_structs.py are used unchanged; the packages
they import and the container lacks (fenwick, segment-tree: PyPI) are replaced
by stand-ins with the same interface (FenwickTree(n).add(i, k) and
.prefix_sum(stop), the sum of the first `stop` entries; SegmentTree is not
used by the BIT structure). ReservoirSampling of various.py (the "Exact KS"
baseline when its size is the stream length) is loaded with an empty stand-in
for ddsketch, which it does not use.

The protocols follow util.py of https://github.com/moretoknow/greedyks_experiments
(commit 0647300), restricted to GreedyKS and Exact KS:
  units          kstwo.sf / kstwobign.sf on a grid; traces of GreedyKS (D after
                 every observation, detected_change) on seeded streams, which are
                 written for heimdall.
  error          single_process / eval_error_D (absolute error of D), 100 runs per
                 setting and distribution; <experiment> is sample_size, memo_units,
                 mean_diff or std_diff (absolute_error.ipynb).
  effectiveness  eval_effectiveness (drift after 10 % of the stream, test with
                 p <= 0.01 once more observations than memory units were seen),
                 effectiveness.ipynb settings.
  callcenter     eval_call_center (weekly samples, hourly mini-batch ground truth,
                 DTW between drift instants) for m = 31, 100, 316
                 (call_center_drift_detection.ipynb), and the drift point of
                 Table 6 (call_center_gof.ipynb).
Runs are seeded (numpy.random.seed(run index)); the paper's were not.
"""

import importlib
import itertools as it
import json
import math
import multiprocessing as mproc
import os
import sys
import types

import numpy as np
import scipy.stats as st

driftwatch_dir, out_dir, mode = sys.argv[1:4]
os.makedirs(out_dir, exist_ok=True)


class FenwickTree:
    """Stand-in for fenwick.FenwickTree (PyPI package `fenwick`)."""

    def __init__(self, n):
        self.n = n
        self.tree = [0] * (n + 1)

    def add(self, idx, k):
        i = idx + 1
        while i <= self.n:
            self.tree[i] += k
            i += i & -i

    def prefix_sum(self, stop):
        i, total = stop, 0
        while i > 0:
            total += self.tree[i]
            i -= i & -i
        return total


def stub(name, **attrs):
    module = types.ModuleType(name)
    module.__dict__.update(attrs)
    sys.modules[name] = module
    return module


stub("fenwick", FenwickTree=FenwickTree)
stub("segment_tree", SegmentTree=object)
stub("ddsketch")
stub("ddsketch.ddsketch")
stub("ddsketch.store")
stub("ddsketch.mapping")
package = stub("driftwatch")
package.__path__ = [os.path.join(driftwatch_dir, "src", "driftwatch")]
GreedyKS = importlib.import_module("driftwatch.greedyks").GreedyKS
ReservoirSampling = importlib.import_module("driftwatch.various").ReservoirSampling

np.seterr("raise", under="ignore")
distrib_types = ["normal", "uniform", "exp"]
num_reps = 100


def gen_test_distribs(mean_diff, std_diff, type_):
    # util.gen_test_distribs
    mean = std = 1
    if type_ == "normal":
        distrib = st.norm
        params = [[mean, std], [mean + mean_diff, std + std_diff]]
    elif type_ == "uniform":
        distrib = st.uniform
        params = [[mean - std * 12 ** .5 / 2, std * 12 ** .5],
                  [mean + mean_diff - (std + std_diff) * 12 ** .5 / 2, (std + std_diff) * 12 ** .5]]
    else:
        distrib = st.expon
        params = [[mean - std, std], [(mean + mean_diff) - (std + std_diff), std + std_diff]]
    return distrib(*params[0]), distrib(*params[1])


def args_gen(sample_size=(10 ** 4,), memo_units=(10 ** 2,), mean_diff=(0.1,), std_diff=(0.0,)):
    # util.args_gen (q_interval unused here), with the run index for the seed
    index = 0
    for nr in range(num_reps):
        for args in it.product(sample_size, memo_units, mean_diff, std_diff, distrib_types):
            yield (index,) + args
            index += 1


def error_task(args):
    index, ss, mu, md, sd, type_ = args
    np.random.seed(index)
    ref_distrib, alt_distrib = gen_test_distribs(md, sd, type_)
    sample = alt_distrib.rvs(ss)
    exact_D = st.ks_1samp(sample, ref_distrib.cdf).statistic
    gks = GreedyKS(ref_distrib, mu, exact_prob=True)
    for observation in sample:
        gks.add_element(observation)
    return {"run": index // (len(distrib_types)), "distrib": type_, "sample_size": ss, "memo_units": mu,
            "mean_diff": md, "std_diff": sd, "exact_D": exact_D, "greedyks_D": gks.get_D(),
            "error": abs(gks.get_D() - exact_D)}


def effectiveness_task(args):
    # util.eval_effectiveness (grace_period = None)
    index, ss, mu, md, sd, type_ = args
    np.random.seed(index)
    ref_distrib, alt_distrib = gen_test_distribs(md, sd, type_)
    sample = alt_distrib.rvs(ss)
    num_bins = mu
    drift_point = len(sample) // 10
    sample = sample[:int(len(sample) * .9)]
    stream = np.concatenate((ref_distrib.rvs(drift_point), sample))
    grace_period = num_bins
    methods = {"GreedyKS": GreedyKS(ref_distrib, num_bins, exact_prob=True),
               "Exact KS": ReservoirSampling(len(stream), ref_distrib)}
    respon = {m: float("nan") for m in methods}
    stop = {m: False for m in methods}
    for t, element in enumerate(stream):
        for m in methods:
            if not stop[m]:
                methods[m].add_element(element)
                if t >= grace_period and methods[m].detected_change():
                    respon[m] = t - drift_point
                    stop[m] = True
    return {"run": index // len(distrib_types), "distrib": type_, "sample_size": ss, "memo_units": mu,
            "mean_diff": md, "std_diff": sd, "greedyks": respon["GreedyKS"], "exact_ks": respon["Exact KS"]}


ERROR_EXPERIMENTS = {
    "sample_size": {"sample_size": tuple(int(v) for v in 10 ** np.arange(3, 6))},
    "memo_units": {"memo_units": tuple(int(v) for v in 10 ** np.arange(1, 4))},
    "mean_diff": {"mean_diff": tuple(10. ** np.arange(-3, 0))},
    "std_diff": {"std_diff": tuple(10. ** np.arange(-3, 0)), "mean_diff": (0,)},
}
EFFECTIVENESS_EXPERIMENTS = {
    "sample_size": {"sample_size": tuple(int(v) for v in (10 ** np.arange(3.5, 5, 0.5)).astype(int))},
    "memo_units": {"memo_units": tuple(int(v) for v in (10 ** np.arange(1, 2.5, 0.5)).astype(int))},
    "mean_diff": {"mean_diff": tuple(10. ** np.arange(-2, -0.5, 0.5))},
    "std_diff": {"std_diff": tuple(10. ** np.arange(-2, -0.5, 0.5)), "mean_diff": (0,)},
}


def write_csv(rows, path):
    keys = list(rows[0].keys())
    with open(path, "w") as f:
        f.write(",".join(keys) + "\n")
        for r in rows:
            f.write(",".join(repr(float(r[k])) if not isinstance(r[k], str) else r[k] for k in keys) + "\n")


def dtw_distance(s1, s2):
    """dtaidistance.dtw.distance without window or pruning: square root of the
    smallest sum of squared differences along a warping path."""
    s1 = np.asarray(s1, float)
    s2 = np.asarray(s2, float)
    if len(s1) == 0 or len(s2) == 0:
        return float("inf")
    cost = np.full((len(s1) + 1, len(s2) + 1), np.inf)
    cost[0, 0] = 0
    for i in range(1, len(s1) + 1):
        d = (s1[i - 1] - s2) ** 2
        for j in range(1, len(s2) + 1):
            cost[i, j] = d[j - 1] + min(cost[i - 1, j], cost[i, j - 1], cost[i - 1, j - 1])
    return math.sqrt(cost[-1, -1])


def load_call_center(raw_dir):
    # util.load_call_center_data: date (yymmdd) + vru_entry, all months, sorted
    import pandas as pd
    files = sorted(os.path.join(raw_dir, f) for f in os.listdir(raw_dir) if f.endswith(".txt"))
    dfs = [pd.read_csv(f, sep=r"\s+") for f in files]
    tss = [pd.to_datetime(d.date.astype(str) + d.vru_entry, format="%y%m%d%H:%M:%S") for d in dfs]
    return pd.concat(tss).sort_values()


def call_center_task(args):
    # util.eval_call_center, GreedyKS only, returning the drift instants
    ts_smp, num_bins = args
    resp = {"mini-batch": [], "GreedyKS": []}
    instance = None
    minibatch_expon = None
    full_batch = []
    time = 0
    for _, group in ts_smp.groupby([ts_smp.dt.year, ts_smp.dt.month, ts_smp.dt.day, ts_smp.dt.hour]):
        latest_hour_batch = (group[1:].values - group[:-1].values).astype(float) / 10 ** 9
        latest_hour_var = len(set(latest_hour_batch))
        latest_hour_expon = None
        full_batch = np.concatenate((full_batch, latest_hour_batch))
        if minibatch_expon is None or st.ks_1samp(full_batch, minibatch_expon.cdf).pvalue < 0.01:
            if latest_hour_var > 3:
                latest_hour_expon = minibatch_expon = st.expon(*st.expon.fit(latest_hour_batch))
                full_batch = latest_hour_batch
                resp["mini-batch"].append(time)
            else:
                minibatch_expon = None
        for element in latest_hour_batch:
            time += 1
            if instance is not None:
                instance.add_element(element)
                if instance.detected_change():
                    resp["GreedyKS"].append(time)
                    instance = None
        if latest_hour_var > 3 and instance is None:
            latest_hour_expon = latest_hour_expon or st.expon(*st.expon.fit(latest_hour_batch))
            instance = GreedyKS(latest_hour_expon, num_bins, exact_prob=True)
            for element in latest_hour_batch:
                instance.add_element(element)
    return resp


if mode == "units":
    # Distribution of the statistic.
    grid = []
    for n in [1, 2, 5, 10, 30, 80, 140, 141, 300, 1000, 5000, 20000, 100000, 150000]:
        for d in [1e-4, 0.3 / n, 0.5 / n, 0.7 / n, 1.0 / n, 1.5 / n, 0.01, 0.02, 0.03, 0.05, 0.08, 0.1, 0.15, 0.2,
                  0.3, 0.45, 0.5, 0.6, 0.8, 0.95, 1 - 0.5 / n, 1.0]:
            if 0 <= d <= 1:
                grid.append({"n": n, "d": d, "kstwo": float(st.kstwo.sf(d, n)),
                             "kstwobign": float(st.kstwobign.sf(d * n ** .5))})
        for z in [0.5, 1.0, 1.3, 1.48, 1.52, 1.6, 1.63, 1.7, 2.0]:
            d = z / n ** .5
            if d <= 1:
                grid.append({"n": n, "d": d, "kstwo": float(st.kstwo.sf(d, n)),
                             "kstwobign": float(st.kstwobign.sf(z))})
    # p-value at n d^2 = 2.2 (boundary of the region where heimdall's detector uses Pelz-Good)
    boundary = [{"n": n, "p": float(st.kstwo.sf((2.2 / n) ** .5, n))}
                for n in list(range(141, 2001)) + list(range(2000, 100001, 500))]
    # Traces of GreedyKS.
    traces = []
    rng = np.random.RandomState(2023)
    for case, (type_, m, min_instances, size) in enumerate(it.product(distrib_types, [1, 10, 31, 100, 316],
                                                                      [0, 50], [3000])):
        ref, alt = gen_test_distribs(0.1, 0.05, type_)
        stream = np.concatenate((ref.rvs(size // 3, random_state=rng), alt.rvs(size - size // 3, random_state=rng)))
        gks = GreedyKS(ref, m, min_instances=min_instances, exact_prob=True)
        D, drift, dn, dp = [], [], [], []
        for x in stream:
            gks.add_element(x)
            D.append(gks.get_D())
            dn.append(gks.dn)
            dp.append(gks.dp)
            drift.append(bool(gks.detected_change()))
        name = f"trace_{case}"
        np.ascontiguousarray(stream, dtype="<f8").tofile(os.path.join(out_dir, name + "_x.bin"))
        traces.append({"name": name, "distrib": type_, "m": m, "min_instances": min_instances,
                       "params": [float(v) for v in ref.args], "D": D, "dn": dn, "dp": dp, "drift": drift,
                       "final": gks.get_max_p_n_indexes()})
    with open(os.path.join(out_dir, "units.json"), "w") as f:
        json.dump({"grid": grid, "boundary": boundary, "traces": traces}, f)
    print("units:", len(grid), "grid points,", len(traces), "traces, min boundary p",
          min(b["p"] for b in boundary))

elif mode in ("error", "effectiveness"):
    experiment = sys.argv[4]
    processes = int(sys.argv[5]) if len(sys.argv) > 5 else None
    settings = (ERROR_EXPERIMENTS if mode == "error" else EFFECTIVENESS_EXPERIMENTS)[experiment]
    task = error_task if mode == "error" else effectiveness_task
    with mproc.Pool(processes) as pool:
        rows = pool.map(task, list(args_gen(**settings)), chunksize=1)
    write_csv(rows, os.path.join(out_dir, f"{mode}_{experiment}_reference.csv"))
    print(mode, experiment, len(rows), "runs")

elif mode == "callcenter":
    raw_dir = sys.argv[4]
    processes = int(sys.argv[5]) if len(sys.argv) > 5 else None
    ts_all = load_call_center(raw_dir)
    # Instants (seconds since 1999-01-01) for heimdall.
    seconds = (ts_all.values.astype("datetime64[s]") - np.datetime64("1999-01-01T00:00:00")).astype(float)
    np.ascontiguousarray(seconds, dtype="<f8").tofile(os.path.join(out_dir, "callcenter_seconds.bin"))
    samples = [g[1] for g in ts_all.groupby([ts_all.dt.isocalendar().week])]
    rows = []
    instants = {}
    for num_bins in (10 ** np.arange(1.5, 3, 0.5)).astype(int):
        with mproc.Pool(processes) as pool:
            results = pool.map(call_center_task, [(s, int(num_bins)) for s in samples], chunksize=1)
        for week, resp in enumerate(results):
            rows.append({"m": int(num_bins), "week": week, "n_minibatch": len(resp["mini-batch"]),
                         "n_greedyks": len(resp["GreedyKS"]),
                         "dtw": dtw_distance(resp["GreedyKS"], resp["mini-batch"])})
            instants[f"{num_bins}_{week}"] = resp
    write_csv(rows, os.path.join(out_dir, "callcenter_dtw_reference.csv"))
    with open(os.path.join(out_dir, "callcenter_instants_reference.json"), "w") as f:
        json.dump(instants, f)
    # Table 6 (call_center_gof.ipynb): reference fitted to 8:00-8:59 of the first work day.
    ts_first_week = samples[0]
    ts_from = ts_first_week[np.where(ts_first_week.dt.hour == 8)[0][0]:]
    td_from = (ts_from[1:].values - ts_from[:-1].values).astype(float) / 10 ** 9
    first_hour = next(iter(ts_from.groupby([ts_from.dt.year, ts_from.dt.month, ts_from.dt.day, ts_from.dt.hour])))[1]
    td_first = (first_hour[1:].values - first_hour[:-1].values).astype(float) / 10 ** 9
    ref_distrib = st.expon(*st.expon.fit(td_first))
    gks = GreedyKS(ref_distrib, 100, exact_prob=True)
    exact = ReservoirSampling(len(td_from), ref_distrib)
    found = {}
    for t, element in enumerate(td_from):
        for name, method in (("GreedyKS", gks), ("Exact KS", exact)):
            if name not in found:
                method.add_element(element)
                if t >= len(td_first) and method.detected_change():
                    found[name] = t
        if len(found) == 2:
            break
    table6 = {}
    for name, t in found.items():
        table6[name] = {"index": t, "time_at_index": str(ts_from.iloc[t]), "time_after": str(ts_from.iloc[t + 1]),
                        "D": float(st.ks_1samp(td_from[:t], ref_distrib.cdf).statistic)}
    table6["reference"] = {"loc": float(ref_distrib.args[0]), "scale": float(ref_distrib.args[1]),
                           "first_hour": len(td_first)}
    with open(os.path.join(out_dir, "callcenter_table6_reference.json"), "w") as f:
        json.dump(table6, f, indent=1)
    print(json.dumps(table6, indent=1))
    print("callcenter: records", len(ts_all), "weeks", len(samples))
