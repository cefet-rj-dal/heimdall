"""Streams and reference runs for the validation of dfr_padd() against
Komorniczak and Ksieniewicz (2024), "Unsupervised Concept Drift Detection based
on Parallel Activations of Neural Network", and its code
(https://github.com/w4k2/padd, methods.py and e2.py).

Usage (inside the devcontainer, from the package root):
  python3 -I development/validation/padd/padd_reference.py <padd_dir> <stream_learn_dir> <out_dir> <mode>

Modes:
  streams  the 240 streams of experiment 2 (e2.py): StreamGenerator of
           stream-learn (source tree, v0.9.2), 250 chunks of 200, 30/60/90
           features (30 % informative), 3/5/10/15 drifts,
           concept_sigmoid_spacing 5 (gradual) or 999 (sudden), random states
           np.random.seed(1410); np.random.randint(100, 10000, 10).
           Written as float32 to <out_dir>/streams/<name>.bin.
  run      the unchanged PADD class of methods.py on every stream, with the
           settings of e2.py (e = r = 12, s = 50, hidden 10; alpha 0.13 and
           threshold 0.26 for gradual drifts, 0.07 and 0.19 for sudden ones),
           on the float32 streams. np.random.seed(1410) once, as e2.py; e2.py
           also runs the other detectors on the same generator, so the draws
           (and the detections) are not those of res/exp1_comp_final.npy.
           Writes <out_dir>/reference_detections.npy (10, 3, 2, 4, 250).
  trace    two streams (replication 0, 30 features, 10 drifts, gradual and
           sudden) with the PADD class instrumented: the network weights,
           every sample drawn by np.random.choice (as 0-based indices) and
           every t-test p-value are recorded. np.random.choice(a, s) with
           replacement draws np.random.randint(0, len(a), s) and returns
           a[idx]; the instrumented version does exactly that and records idx.
"""

import json
import sys
from pathlib import Path

import numpy as np

N_CHUNKS, CHUNK_SIZE = 250, 200
FEATURES, DRIFTS, SPACINGS = [30, 60, 90], [3, 5, 10, 15], [5, 999]
ALPHAS, THRESHOLDS = [0.13, 0.07], [0.26, 0.19]


def random_states():
    np.random.seed(1410)
    return np.random.randint(100, 10000, 10)


def stream_name(rep, n_f, css, n_d):
    return f"r{rep}_f{n_f}_c{css}_d{n_d}"


def load(out, name, n_f):
    return np.fromfile(Path(out, "streams", f"{name}.bin"), dtype=np.float32).reshape(-1, n_f).astype(np.float64)


def streams(stream_learn_dir, out):
    sys.path.insert(0, stream_learn_dir)
    # strlearn imports wget (only used to download its benchmark data sets,
    # not by StreamGenerator); it is not installed, so an empty module stands in.
    import types
    sys.modules.setdefault("wget", types.ModuleType("wget"))
    from strlearn.streams import StreamGenerator
    Path(out, "streams").mkdir(parents=True, exist_ok=True)
    for rep, rs in enumerate(random_states()):
        for n_f in FEATURES:
            for css in SPACINGS:
                for n_d in DRIFTS:
                    stream = StreamGenerator(n_chunks=N_CHUNKS, chunk_size=CHUNK_SIZE, n_drifts=n_d, n_classes=2,
                                             n_features=n_f, n_informative=int(0.3 * n_f), n_redundant=0,
                                             n_repeated=0, concept_sigmoid_spacing=css, random_state=int(rs))
                    chunks = [stream.get_chunk()[0] for _ in range(N_CHUNKS)]
                    np.concatenate(chunks).astype(np.float32).tofile(Path(out, "streams",
                                                                          stream_name(rep, n_f, css, n_d) + ".bin"))
        print(f"replication {rep} (random state {rs}) written", flush=True)


def run(padd_dir, out):
    sys.path.insert(0, padd_dir)
    from methods import PADD
    np.random.seed(1410)
    _ = np.random.randint(100, 10000, 10)
    result = np.zeros((10, 3, 2, 4, N_CHUNKS))
    for rep in range(10):
        for f_id, n_f in enumerate(FEATURES):
            for c_id, css in enumerate(SPACINGS):
                for d_id, n_d in enumerate(DRIFTS):
                    x = load(out, stream_name(rep, n_f, css, n_d), n_f)
                    detector = PADD(alpha=ALPHAS[c_id], ensemble_size=12, n_replications=12, stat_proba=50,
                                    neck_width=10, th=THRESHOLDS[c_id])
                    for chunk in range(N_CHUNKS):
                        detector.process(x[chunk * CHUNK_SIZE:(chunk + 1) * CHUNK_SIZE])
                        result[rep, f_id, c_id, d_id, chunk] = detector._is_drift
        print(f"replication {rep}: {result[rep].sum(axis=-1).ravel()}", flush=True)
    np.save(Path(out, "reference_detections.npy"), result)


def trace(padd_dir, out):
    sys.path.insert(0, padd_dir)
    import methods
    from methods import PADD

    original_choice = np.random.choice
    original_ttest = methods.ttest_ind
    # Check that the instrumented choice draws what np.random.choice draws.
    np.random.seed(7)
    expected = original_choice(np.arange(1234.0), 50)
    np.random.seed(7)
    assert np.array_equal(np.arange(1234.0)[np.random.randint(0, 1234, 50)], expected)

    log = {"index": [], "p": []}

    def choice(a, size):
        idx = np.random.randint(0, len(a), size)
        log["index"].append(idx.tolist())
        return a[idx]

    def ttest_ind(a, b):
        stat, pval = original_ttest(a, b)
        log["p"].append(float(pval))
        return stat, pval

    traces = {}
    for c_id, css in enumerate(SPACINGS):
        name = stream_name(0, 30, css, 10)
        x = load(out, name, 30)
        np.random.seed(1410 + c_id)
        detector = PADD(alpha=ALPHAS[c_id], ensemble_size=12, n_replications=12, stat_proba=50, neck_width=10,
                        th=THRESHOLDS[c_id])
        log["index"], log["p"] = [], []
        np.random.choice, methods.ttest_ind = choice, ttest_ind
        detections = []
        try:
            for chunk in range(N_CHUNKS):
                detector.process(x[chunk * CHUNK_SIZE:(chunk + 1) * CHUNK_SIZE])
                detections.append(bool(detector._is_drift))
        finally:
            np.random.choice, methods.ttest_ind = original_choice, original_ttest
        traces[name] = {"alpha": ALPHAS[c_id], "threshold": THRESHOLDS[c_id],
                        "weights": [layer.tolist() for layer in detector.stack], "index": log["index"],
                        "p": log["p"], "detections": detections}
        print(f"{name}: {sum(detections)} detections, {len(log['p'])} tests", flush=True)
    json.dump(traces, open(Path(out, "trace.json"), "w"))


if __name__ == "__main__":
    padd_dir, stream_learn_dir, out, mode = sys.argv[1:5]
    if mode == "streams":
        streams(stream_learn_dir, out)
    elif mode == "run":
        run(padd_dir, out)
    else:
        trace(padd_dir, out)
