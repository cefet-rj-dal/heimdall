"""Runs of the reference MCD-DD code (main.py) for the evaluation of Table 2.

Usage: python3 -I run_reference.py <reference repo> <data .pkl> <output .csv> <runs> <win_size> <m> <hidden> <output> [float32|float64]

Repeats the loop of main.py, unchanged, in float32 as published or in float64
(seed 1111 + run
before creating the encoder), and records, for every tested sub-window, the
index of its last observation (1-based), the MCD to the previous sub-window and
the threshold. main.py only prints the detections; the evaluation itself is not
in the repository.
"""

import sys

import numpy as np
import torch

repo, data_path, out_path = sys.argv[1], sys.argv[2], sys.argv[3]
runs, win_size, m, hidden, output = (int(v) for v in sys.argv[4:9])
dtype = sys.argv[9] if len(sys.argv) > 9 else "float32"
sys.path.insert(0, repo)
from encoder import Encoder  # noqa: E402
from mcd import MCD  # noqa: E402

torch.set_num_threads(1)
if dtype == "float64":
    torch.set_default_dtype(torch.float64)
dataset = torch.tensor(np.load(data_path, allow_pickle=True), dtype=torch.get_default_dtype())
slide = win_size // 10
n_windows = max(1, (len(dataset) - win_size) // slide + 1)
rows = ["run,observation,mcd,threshold"]
for run in range(runs):
    np.random.seed(1111 + run)
    torch.manual_seed(1111 + run)
    model = Encoder(dataset.shape[1], hidden, output)
    optimizer = torch.optim.Adam(model.parameters(), lr=0.005)
    mcd = MCD(model, optimizer, 1, 10, m, 10, 1, 10, 0.1, 1, 0.95, "cpu")
    threshold = 0
    for i in range(n_windows):
        window = dataset[i * slide:i * slide + win_size]
        if i > 0:
            distance = float(mcd.test(window)[-1])
            rows.append(f"{run + 1},{i * slide + win_size},{distance!r},{float(threshold)!r}")
        threshold = mcd.train(window)
    print("run", run + 1, flush=True)
with open(out_path, "w") as f:
    f.write("\n".join(rows) + "\n")
