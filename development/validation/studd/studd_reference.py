"""Reference runs for the validation of dfr_studd() against Cerqueira, Gomes,
Bifet and Torgo (2023), "STUDD: a student-teacher method for unsupervised
concept drift detection" (Machine Learning 112, 4351-4378), and its code
(https://github.com/vcerqueira/studd, src/studd_batch.py and src/workflows.py).

Usage (inside the devcontainer, from the package root):
  python3 -I development/validation/studd/studd_reference.py <studd_dir> <skmultiflow_dir> <datasets_dir> \
      <out_dir> <mode> <dataset> <seed>

<studd_dir> is the clone of the reference repository; the class STUDD of
src/studd_batch.py is used unchanged. scikit-multiflow is not installed (and is
not taken from PyPI): <skmultiflow_dir> holds page_hinkley.py and
base_drift_detector.py of its release 0.5.3 (git show 0.5.3:...), loaded as
they are. Two small stand-ins replace the rest of scikit-multiflow:
  * skmultiflow.core.BaseSKMObject, an empty base class (only get_params and
    similar helpers, not used here, live in it);
  * skmultiflow.data.data_stream.DataStream, which returns the rows in order
    (next_sample slices X and y exactly as DataStream.next_sample of 0.5.3).

The data sets are those of the incremental-ks repository
(https://github.com/denismr/incremental-ks, datasets/), cited by the paper:
comma-separated, the last column is the class. Streams are truncated to
150000 rows, as in the paper. The settings are those of experiments/main.py
and src/workflows.py: random forests (scikit-learn defaults) as teacher and
student, W = 500 for the small streams and 1000 for the others, Page-Hinkley
with delta = 0.002 / 2, the teacher retrained on the last W observations and
the student on every observation seen online.

Modes:
  run    np.random.seed(seed); STUDD and the static baseline (BL-st). Writes
         <out_dir>/runs/<dataset>_<seed>.json: alarms (stream index), Cohen's
         kappa, accuracy, labels used, number of updates.
  trace  as run (STUDD only), recording every call to fit and predict of the
         teacher and of the student (rows, labels, predictions) and the
         mimicking error of every online row. Writes
         <out_dir>/traces/<dataset>_<seed>.json.
"""

import copy
import importlib.util
import json
import sys
import time
import types
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn import metrics
from sklearn.ensemble import RandomForestClassifier

SMALL = {"AbruptInsects", "Insects", "Keystroke"}
MAX_ROWS = 150000


def install_stubs(skmultiflow_dir):
    skmultiflow = types.ModuleType("skmultiflow")
    skmultiflow.__path__ = []
    core = types.ModuleType("skmultiflow.core")

    class BaseSKMObject:
        pass

    core.BaseSKMObject = BaseSKMObject
    data = types.ModuleType("skmultiflow.data")
    data.__path__ = []
    data_stream = types.ModuleType("skmultiflow.data.data_stream")

    class DataStream:
        def __init__(self, X, y):
            self.X = np.asarray(X)
            self.y = np.asarray(y).reshape(-1, 1)
            self.n_samples = self.X.shape[0]
            self.sample_idx = 0

        def prepare_for_use(self):
            self.sample_idx = 0

        def next_sample(self, batch_size=1):
            self.sample_idx += batch_size
            x = self.X[self.sample_idx - batch_size:self.sample_idx, :]
            y = self.y[self.sample_idx - batch_size:self.sample_idx, :].flatten()
            return x, y

        def has_more_samples(self):
            return (self.n_samples - self.sample_idx) > 0

    data_stream.DataStream = DataStream
    drift = types.ModuleType("skmultiflow.drift_detection")
    drift.__path__ = []
    sys.modules.update({"skmultiflow": skmultiflow, "skmultiflow.core": core, "skmultiflow.data": data,
                        "skmultiflow.data.data_stream": data_stream, "skmultiflow.drift_detection": drift})
    for name in ["base_drift_detector", "page_hinkley"]:
        full = f"skmultiflow.drift_detection.{name}"
        spec = importlib.util.spec_from_file_location(full, Path(skmultiflow_dir, f"{name}.py"))
        module = importlib.util.module_from_spec(spec)
        sys.modules[full] = module
        spec.loader.exec_module(module)


def load_dataset(datasets_dir, name):
    frame = pd.read_csv(Path(datasets_dir, f"{name}.data"), header=None).iloc[:MAX_ROWS]
    X = frame.iloc[:, :-1].astype(float)
    X.columns = [f"X{i}" for i in range(X.shape[1])]
    y = frame.iloc[:, -1].astype(str).values
    return X, y


LOG = {}


class OneElement(np.ndarray):
    """studd_batch.py computes int(model_yhat != std_model_yhat) on arrays of
    one element, a conversion NumPy 2.5 no longer allows (it was deprecated in
    NumPy 1.25). This view restores it; nothing else changes."""

    def __int__(self):
        return int(np.asarray(self).reshape(-1)[0]) if self.size == 1 else int(np.asarray(self))


class CompatForest(RandomForestClassifier):
    """RandomForestClassifier (scikit-learn defaults) whose predictions are OneElement views."""

    def predict(self, X):
        return super().predict(X).view(OneElement)


class TracedForest(CompatForest):
    """CompatForest that records its calls in LOG[role]."""

    def __init__(self, role="teacher", n_estimators=100):
        super().__init__(n_estimators=n_estimators)
        self.role = role

    def fit(self, X, y):
        LOG.setdefault(self.role, []).append({"call": "fit", "rows": int(len(y)), "labels": [str(v) for v in y]})
        return super().fit(X, y)

    def predict(self, X):
        prediction = super().predict(X)
        LOG.setdefault(self.role, []).append({"call": "predict", "rows": int(len(prediction)),
                                              "labels": [str(v) for v in np.asarray(prediction)]})
        return prediction


def main():
    studd_dir, skmultiflow_dir, datasets_dir, out, mode, name, seed = sys.argv[1:8]
    seed = int(seed)
    install_stubs(skmultiflow_dir)
    sys.path.insert(0, studd_dir)
    from src.studd_batch import STUDD
    from skmultiflow.drift_detection.page_hinkley import PageHinkley

    X, y = load_dataset(datasets_dir, name)
    W = 500 if name in SMALL else 1000
    delta = 0.002
    np.random.seed(seed)
    started = time.time()
    ucdd = STUDD(X=X, y=y, n_train=W)
    if mode == "trace":
        ucdd.initial_fit(model=TracedForest("teacher"), std_model=TracedForest("student"))
    else:
        ucdd.initial_fit(model=CompatForest(), std_model=CompatForest())
    result = ucdd.drift_detection_std(datastream_=ucdd.datastream, model_=ucdd.base_model,
                                      std_model_=ucdd.student_model, n_train_=ucdd.n_train, n_samples=W,
                                      delta=delta / 2, upd_model=True, upd_std_model=True, detector=PageHinkley)
    seconds = time.time() - started
    summary = {"dataset": name, "seed": seed, "rows": int(len(y)), "window": W, "alarms": result["alarms"],
               "kappa": metrics.cohen_kappa_score(result["preds"]["y"], result["preds"]["y_hat"]),
               "accuracy": metrics.accuracy_score(result["preds"]["y"], result["preds"]["y_hat"]),
               "samples_used": result["samples_used"], "n_updates": result["n_updates"], "seconds": seconds}
    if mode == "trace":
        Path(out, "traces").mkdir(parents=True, exist_ok=True)
        summary["log"] = LOG
        json.dump(summary, open(Path(out, "traces", f"{name}_{seed}.json"), "w"))
    else:
        static = ucdd.BL1_never_adapt(datastream_=ucdd.datastream, model_=ucdd.base_model)
        summary["kappa_static"] = metrics.cohen_kappa_score(static["preds"]["y"], static["preds"]["y_hat"])
        Path(out, "runs").mkdir(parents=True, exist_ok=True)
        json.dump(summary, open(Path(out, "runs", f"{name}_{seed}.json"), "w"))
    print(f"{name} seed {seed}: alarms {result['alarms']} kappa {summary['kappa']:.3f} "
          f"labels {summary['samples_used']} ({seconds:.0f} s)", flush=True)


if __name__ == "__main__":
    main()
