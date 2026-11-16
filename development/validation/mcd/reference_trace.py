"""Trace of the reference MCD-DD code for the step-by-step comparison with dfr_mcd().

Usage: python3 -I reference_trace.py <reference repo> <data .pkl> <output .json> <n_windows> \
           <win_size> <m> <hidden> <output> [float64|float32]

Runs the loop of main.py (test, then train, window by window) with the
reference MCD and Encoder classes, unchanged, for the first n_windows windows.
torch.randint and torch.normal are wrapped to record every draw (sample-set
indices and noise), so that dfr_mcd() can replay them. Recorded per window:
the draws of the test and of the training, the test distances, the threshold
and the encoder weights after the training; and the initial weights.
"""

import json
import sys

import numpy as np
import torch

repo, data_path, out_path = sys.argv[1], sys.argv[2], sys.argv[3]
n_windows, win_size, m, hidden, output = (int(v) for v in sys.argv[4:9])
dtype = sys.argv[9] if len(sys.argv) > 9 else "float64"
sys.path.insert(0, repo)
from encoder import Encoder  # noqa: E402
from mcd import MCD  # noqa: E402

if dtype == "float64":
    torch.set_default_dtype(torch.float64)
torch.manual_seed(1111)
np.random.seed(1111)

draws = []
original_randint, original_normal = torch.randint, torch.normal


def recording_randint(*args, **kwargs):
    value = original_randint(*args, **kwargs)
    draws.append(("sets", (value + 1).tolist()))
    return value


def recording_normal(*args, **kwargs):
    value = original_normal(*args, **kwargs)
    draws.append(("noise", value.reshape(-1, value.shape[-1]).tolist()))
    return value


torch.randint, torch.normal = recording_randint, recording_normal

dataset = torch.tensor(np.load(data_path, allow_pickle=True), dtype=torch.get_default_dtype())
slide = win_size // 10
model = Encoder(dataset.shape[1], hidden, output)
optimizer = torch.optim.Adam(model.parameters(), lr=0.005)
mcd = MCD(model, optimizer, 1, 10, m, 10, 1, 10, 0.1, 1, 0.95, "cpu")


def weights():
    p = dict(model.named_parameters())
    return {"W1": p["layers.0.weight"].tolist(), "b1": p["layers.0.bias"].tolist(),
            "W2": p["layers.2.weight"].tolist(), "b2": p["layers.2.bias"].tolist()}


trace = {"dtype": dtype, "win_size": win_size, "m": m, "hidden": hidden, "output": output,
         "init": weights(), "windows": []}
for i in range(n_windows):
    window = dataset[i * slide:i * slide + win_size]
    record = {"start": i * slide + 1}
    if i > 0:
        draws.clear()
        distances = mcd.test(window)
        record["test_sets"] = [d[1] for d in draws]
        record["distances"] = [float(d) for d in distances]
    draws.clear()
    threshold = mcd.train(window)
    sets = [d[1] for d in draws if d[0] == "sets"]
    noise = [d[1] for d in draws if d[0] == "noise"]
    record.update({"sets": sets[:10], "threshold_sets": sets[10:], "weak": noise[:10], "strong": noise[10],
                   "threshold": float(threshold), "weights": weights()})
    trace["windows"].append(record)
    print("window", i, "threshold", float(threshold), "distance", record.get("distances", [None])[-1], flush=True)

with open(out_path, "w") as f:
    json.dump(trace, f)
