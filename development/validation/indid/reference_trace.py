"""Trace of the reference InDiD code for the step-by-step comparison with indid().

Usage: python3 -I reference_trace.py <reference repo> <data dir> <output .json>

1. Losses (float64): utils/loss.py CPDLoss (T = 8) and torch.nn.BCELoss on
   random probabilities and labels (with and without changes, a change at the
   second step, a change near the end), with their gradients (autograd).
2. Training steps (float64): the core model of the 1-D experiment
   (core_models.BaseRnn(1, 4, 1, 0.5)) initialised with seed 102; three Adam
   steps (lr 0.001) with the InDiD loss (T = 32) and three with BCE, on batches
   of 64 training sequences drawn with torch.randperm. BaseRnn.forward casts
   its input to float32, so here the same nn.LSTM and nn.Linear modules of the
   model are run in float64 (lstm -> linear -> sigmoid, as BaseRnn.forward),
   with float64 as default dtype (loss.py builds torch.ones with the default).
   Recorded: batch indices, loss, gradients and weights after every step.
3. The same steps in float32 with the unchanged CPDModel.training_step and
   BaseRnn.forward; the probabilities on the test set after the steps, and the
   metrics of utils/metrics.py evaluation_pipeline (100 thresholds of test.py).
"""

import copy
import json
import os
import sys

import numpy as np
import torch

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ref_env  # noqa: E402

repo, data_dir, out_path = sys.argv[1], sys.argv[2], sys.argv[3]
ref = ref_env.load(repo)
CPDLoss = ref["loss"].CPDLoss


def tolist(t):
    return t.detach().double().cpu().numpy().tolist()


trace = {}

# 1. Losses ------------------------------------------------------------------------------
# loss.py creates its auxiliary tensors (torch.ones) with the default dtype, so the
# default is float64 in the float64 parts and float32 in the float32 part.
torch.set_default_dtype(torch.float64)
torch.manual_seed(1)
prob = torch.rand(8, 40, dtype=torch.float64) * 0.9 + 0.05
labels = torch.zeros(8, 40, dtype=torch.float64)
for row, change in [(1, 1), (2, 10), (3, 25), (4, 39), (5, 5), (6, 20)]:
    labels[row, change:] = 1
cases = {}
for name, fn in (("indid", CPDLoss(len_segment=8)), ("bce", torch.nn.BCELoss())):
    p = prob.clone().requires_grad_(True)
    value = fn(p, labels)
    value.backward()
    cases[name] = {"value": float(value), "grad": tolist(p.grad)}
trace["loss"] = {"prob": tolist(prob), "labels": tolist(labels), "T": 8, "cases": cases}


# 2-3. Training steps -------------------------------------------------------------------------
def load(part):
    meta = json.load(open(os.path.join(data_dir, f"synthetic_1D_{part}.json")))
    x = np.fromfile(os.path.join(data_dir, f"synthetic_1D_{part}_x.bin"), dtype="<f8").reshape(meta["shape"])
    y = np.fromfile(os.path.join(data_dir, f"synthetic_1D_{part}_y.bin"), dtype="<f8").reshape(meta["shape"][:2])
    return torch.tensor(x), torch.tensor(y)


x_train, y_train = load("train")
x_test, y_test = load("test")
args = {"experiments_name": "synthetic_1D", "learning": {"lr": 0.001, "batch_size": 64}, "num_workers": 0,
        "loss": {"T": 32}}
torch.set_default_dtype(torch.float32)
torch.manual_seed(102)
core = ref["core_models"].BaseRnn(input_size=1, hidden_dim=4, n_layers=1, drop_prob=0.5)
names = {"lstm.weight_ih_l0": "W_ih", "lstm.weight_hh_l0": "W_hh", "lstm.bias_ih_l0": "b_ih",
         "lstm.bias_hh_l0": "b_hh", "linear.weight": "W_out", "linear.bias": "b_out"}


def params(model):
    return {names[k]: tolist(v) for k, v in model.named_parameters()}


trace["init"] = params(core)
torch.manual_seed(7)
batches = [torch.randperm(700)[:64] for _ in range(3)]
trace["batches"] = [(b + 1).tolist() for b in batches]
trace["steps"] = {}
for loss_type in ("indid", "bce"):
    # float64
    torch.set_default_dtype(torch.float64)
    model = copy.deepcopy(core).double()
    loss_fn = CPDLoss(len_segment=32) if loss_type == "indid" else torch.nn.BCELoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=0.001)
    steps64 = []
    for b in batches:
        optimizer.zero_grad()
        out, _ = model.lstm(x_train[b])
        pred = model.activation(model.linear(out.contiguous().view(-1, 4))).view(len(b), -1)
        value = loss_fn(pred.squeeze(), y_train[b].squeeze())
        value.backward()
        grads = {names[k]: tolist(v.grad) for k, v in model.named_parameters()}
        optimizer.step()
        steps64.append({"loss": float(value), "grads": grads, "params": params(model)})
    # float32, unchanged CPDModel
    torch.set_default_dtype(torch.float32)
    model32 = ref["cpd_models"].CPDModel(loss_type, args, copy.deepcopy(core), None, None)
    optimizer = model32.configure_optimizers()
    steps32 = []
    for b in batches:
        optimizer.zero_grad()
        value = model32.training_step((x_train[b].float(), y_train[b].float()), 0)
        value.backward()
        optimizer.step()
        steps32.append({"loss": float(value), "params": params(model32.model)})
    model32.eval()
    with torch.no_grad():
        p_test = model32(x_test.float())
    from torch.utils.data import DataLoader, TensorDataset
    loader = DataLoader(TensorDataset(x_test.float(), y_test.float()), batch_size=64, shuffle=False)
    result, delay_dict, fp_delay_dict = ref["metrics"].evaluation_pipeline(
        model32, loader, ref_env.threshold_list(100), device="cpu", model_type="seq2seq", verbose=False)
    best_th, time_fa, delay, auc, conf, f1, cover, best_th_cover, max_cover = result
    trace["steps"][loss_type] = {"float64": steps64, "float32": steps32, "test_prob": tolist(p_test),
                                 "metrics": {"best_threshold": float(best_th), "time_to_fa": time_fa,
                                             "delay": delay, "auc": float(auc), "confusion": list(conf),
                                             "f1": f1, "cover": float(cover), "max_cover": float(max_cover)}}
    print(loss_type, "float64 losses", [round(s["loss"], 6) for s in steps64], "metrics", trace["steps"][loss_type]["metrics"],
          flush=True)

with open(out_path, "w") as f:
    json.dump(trace, f)
