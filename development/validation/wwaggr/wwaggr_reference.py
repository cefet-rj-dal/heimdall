"""Reference traces for the validation of the WWAggr implementation of heimdall
(dfr_wwaggr, wwaggr, cpd_calibrate, cpd_ensemble, tscp2, ts2vec and the
generalised LSTM of indid) against the code of Stepikin, Romanenkova and
Zaytsev (2025), https://github.com/stalex2902/wwagr-ensemble-cpd (commit
4b8e31d).

Usage (inside the devcontainer, from the package root):
  python3 -I development/validation/wwaggr/wwaggr_reference.py <reference_dir> <betacal_dir> <pot_dir> \
      <out_dir> units

The modules of the reference are imported unchanged. The packages the
container lacks are replaced as follows (nothing is taken from PyPI):
  * pytorch_lightning: a stand-in whose LightningModule is torch.nn.Module with
    a no-op log() and a cpu device (only these are used by the code below);
  * POT 0.9.5 (ot.wasserstein_1d): its pure-Python modules backend.py, utils.py
    and lp/solver_1d.py are loaded from <pot_dir> (tag 0.9.5); the compiled
    emd_wrap, which wasserstein_1d does not use, is an empty stand-in;
  * betacal 1.1.0: loaded from <betacal_dir> (https://github.com/betacal/python);
  * the video, dataset and logging modules imported by ensembles.py and
    evaluation_pipelines.py (av, pytorchvideo, torchvision, the datasets
    module): empty stand-ins, not used by the functions called;
  * numpy.trapz (removed in NumPy 2) is numpy.trapezoid.

Writes <out_dir>/units.json with the inputs and outputs of:
  aggregation   anchor_window_detector_batch (Wasserstein p = 1, 2, 3 and MMD
                rbf/multiscale; anchors start, prev, combined) and the mean,
                min, max, median of EnsembleCPDModel;
  calibration   BetaCalibration (abm, am, ab, a) on float32 scores;
  metrics       evaluation_pipeline (threshold search, F1 with margins, AUDC,
                covering) on precomputed scores;
  seq2seq       BaseRnn (two-layer LSTM; LayerNormLSTM before/after) with BCE
                and InDiD losses: probabilities, losses and gradients;
  tscp          TSCP_model.training_step (with and without batch norm):
                loss, gradients and running statistics;
  ts2vec        TS2Vec.shared_step (spectral norm) with the random crops,
                masks and dropout recorded: loss, gradients and power
                iteration vectors;
  optimizers    Adam with CosineAnnealingLR and AdamW with
                CosineAnnealingWarmRestarts, a few steps;
  predictions   get_repr_learning_output + post_process_output of TS-CP2 and
                TS2Vec models (step and alpha).
"""

import importlib.util
import json
import os
import sys
import types

import numpy as np
import torch

reference_dir, betacal_dir, pot_dir, out_dir, mode = sys.argv[1:6]
os.makedirs(out_dir, exist_ok=True)
np.trapz = np.trapezoid


def stub(name, **attrs):
    module = types.ModuleType(name)
    module.__dict__.update(attrs)
    sys.modules[name] = module
    return module


class LightningModule(torch.nn.Module):
    def log(self, *args, **kwargs):
        pass

    @property
    def device(self):
        return torch.device("cpu")


stub("pytorch_lightning", LightningModule=LightningModule, Trainer=object)
stub("pytorch_lightning.callbacks")
stub("pytorch_lightning.callbacks.early_stopping", EarlyStopping=object)
stub("pytorch_lightning.loggers", TensorBoardLogger=object)
stub("src.datasets")
stub("src.datasets.datasets", AllModelsOutputDataset=object, CPDDatasets=object)
stub("src.models.model_utils", get_models_list=None)

# POT 0.9.5 without its compiled extension.
ot = stub("ot")
ot.__path__ = [os.path.join(pot_dir, "ot")]
ot_lp = stub("ot.lp")
ot_lp.__path__ = [os.path.join(pot_dir, "ot", "lp")]
stub("ot.lp.emd_wrap", emd_1d_sorted=None, emd_c=None, check_result=None)
solver_1d = importlib.import_module("ot.lp.solver_1d")
ot.wasserstein_1d = solver_1d.wasserstein_1d

sys.path.insert(0, betacal_dir)
sys.path.insert(0, reference_dir)
from betacal import BetaCalibration  # noqa: E402
from src.baselines import prediction_utils, tscp, ts2vec_sn  # noqa: E402
from src.ensembles import distances  # noqa: E402
from src.ensembles.ensembles import EnsembleCPDModel  # noqa: E402
from src.loss.loss import CPDLoss  # noqa: E402
from src.metrics import evaluation_pipelines  # noqa: E402
from src.models import core_models  # noqa: E402

torch.set_default_dtype(torch.float64)


def tolist(t):
    if isinstance(t, torch.Tensor):
        t = t.detach().cpu().numpy()
    return np.asarray(t, dtype=np.float64).tolist()


def matrix(t):
    """A tensor as a list of rows of its 2-D row-major reshape (out, -1)."""
    t = t.detach()
    return tolist(t.reshape(t.shape[0], -1)) if t.dim() > 1 else tolist(t)


out = {}

# Aggregation --------------------------------------------------------------------------------------
rng = np.random.default_rng(1)
preds = torch.tensor(rng.uniform(0, 1, (10, 4, 40)) * np.linspace(0.2, 1, 40))
preds[:, :, 25:] += 0.3
preds = preds.clamp(0, 1)
aggregation = {"preds": tolist(preds), "cases": []}
for distance, p, kernel in [("wasserstein_1d", 1, "rbf"), ("wasserstein_1d", 2, "rbf"), ("wasserstein_1d", 3, "rbf"),
                            ("mmd", 1, "rbf"), ("mmd", 1, "multiscale")]:
    for anchor in ["start", "prev", "combined"]:
        for w in [1, 2, 3, 5]:
            if anchor == "combined" and w < 2:
                continue
            scores = distances.anchor_window_detector_batch(preds, w, distance=distance, p=p, kernel=kernel,
                                                            anchor_window_type=anchor)
            aggregation["cases"].append({"distance": distance, "p": p, "kernel": kernel, "anchor": anchor,
                                         "window": w, "scores": tolist(scores)})
ens = object.__new__(EnsembleCPDModel)
ens.predict_all_models = lambda inputs, step=1, alpha=1.0: preds
aggregation["mean"] = tolist(ens.predict(None)[0])
for name, q in [("min", 0.0), ("max", 1.0), ("median", 0.5), ("q30", 0.3)]:
    aggregation[name] = tolist(ens.get_quantile_predictions(None, q)[0])
out["aggregation"] = aggregation

# Calibration --------------------------------------------------------------------------------------
labels = (rng.uniform(0, 1, 2000) < 0.4).astype(np.float32)
scores = np.clip(rng.beta(2, 5, 2000) + 0.35 * labels, 0, 1).astype(np.float32)
scores[:5] = [0.0, 1.0, 1e-9, 0.5, 0.999999]
test = np.linspace(0, 1, 101).astype(np.float32)
calibration = {"scores": scores.tolist(), "labels": labels.tolist(), "test": test.tolist(), "cases": []}
for parameters in ["abm", "am", "ab", "a"]:
    cal = BetaCalibration(parameters)
    cal.fit(scores.reshape(-1, 1), labels)
    calibration["cases"].append({"parameters": parameters, "map": np.asarray(cal.calibrator_.map_, float).tolist(),
                                 "predictions": np.asarray(cal.predict(torch.tensor(test, dtype=torch.float32)),
                                                           float).tolist()})
# A case where the abm fit gives a negative coefficient (scores decreasing in the label).
cal = BetaCalibration("abm")
cal.fit((1 - scores).reshape(-1, 1), labels)
calibration["negative"] = {"map": np.asarray(cal.calibrator_.map_, float).tolist(),
                           "predictions": np.asarray(cal.predict(test), float).tolist()}
# The same fits with a tight tolerance of the logistic regression (scikit-learn's
# default tol = 1e-4 stops lbfgs before the maximum of the likelihood).
import functools  # noqa: E402
from betacal import beta_calibration as betacal_module  # noqa: E402
loose = betacal_module.LogisticRegression
betacal_module.LogisticRegression = functools.partial(loose, tol=1e-12, max_iter=100000)
calibration["tight"] = []
eps32 = float(np.finfo(np.float32).eps)  # in float64, clipped as the float32 scores are
s64 = np.clip(scores.astype(np.float64), eps32, 1 - eps32)
t64 = np.clip(test.astype(np.float64), eps32, 1 - eps32)
for parameters in ["abm", "am", "ab", "a"]:
    cal = BetaCalibration(parameters)
    cal.fit(s64.reshape(-1, 1), labels.astype(np.float64))
    calibration["tight"].append({"parameters": parameters, "map": np.asarray(cal.calibrator_.map_, float).tolist(),
                                 "predictions": np.asarray(cal.predict(t64), float).tolist()})
betacal_module.LogisticRegression = loose
out["calibration"] = calibration

# Metrics ------------------------------------------------------------------------------------------
n, length = 60, 30
change = rng.integers(3, 40, n)
lab = (np.arange(length)[None, :] >= change[:, None]).astype(np.float64)
prob = np.clip(0.15 + 0.6 * lab + rng.normal(0, 0.15, (n, length)), 0, 1)
loader = [((torch.tensor(prob[i:i + 16]), torch.zeros(len(prob[i:i + 16]), length)), torch.tensor(lab[i:i + 16]))
          for i in range(0, n, 16)]
thresholds = [-0.001] + list(1 / (1 + np.exp(-np.linspace(-5, 5, 50)))) + [1.001]
res = evaluation_pipelines.evaluation_pipeline(None, loader, thresholds, device="cpu", model_type="fake_ensemble",
                                               verbose=False, margin_list=[1, 2, 4])
(best_th, time_fa, delay, audc, conf, f1, cover, th_cover, max_cover), (th_margins, f1_margins), _, _ = res
out["metrics"] = {"prob": prob.tolist(), "labels": lab.tolist(), "thresholds": thresholds,
                  "best_threshold": best_th, "time_to_fa": time_fa, "delay": delay, "audc": audc,
                  "confusion": list(conf), "f1": f1, "cover": cover, "max_cover": max_cover,
                  "f1_margins": [f1_margins[m] for m in [1, 2, 4]]}

# Networks: the reference casts the inputs to float32 (BaseRnn, BaseTSCPEncoder and TSEncoder),
# so the networks run in single precision.
torch.set_default_dtype(torch.float32)

# Seq2seq ------------------------------------------------------------------------------------------
seq2seq = []
x = torch.tensor(rng.normal(0, 1, (6, 9, 3)), dtype=torch.float32)
lab = torch.tensor((np.arange(9)[None, :] >= np.array([3, 10, 5, 10, 2, 7])[:, None]).astype(np.float32))
for name, layer_norm, ln_type, n_layers in [("lstm2", False, None, 2), ("before", True, "before", 1),
                                            ("after", True, "after", 1)]:
    torch.manual_seed(3)
    net = core_models.BaseRnn(input_size=3, hidden_dim=4, n_layers=n_layers, drop_prob=0.0, layer_norm=layer_norm,
                              ln_type=ln_type)
    net.train()
    for loss_name, criterion in [("bce", torch.nn.BCELoss()), ("indid", CPDLoss(len_segment=4))]:
        net.zero_grad()
        p = net(x)
        loss = criterion(p.squeeze(), lab.squeeze())
        loss.backward()
        seq2seq.append({"name": name, "loss_type": loss_name,
                        "params": {k: matrix(v) for k, v in net.state_dict().items()},
                        "grads": {k: matrix(v.grad) for k, v in net.named_parameters()},
                        "p": tolist(p), "loss": float(loss)})
out["seq2seq"] = {"x": tolist(x), "labels": tolist(lab), "cases": seq2seq}

# TS-CP2 -------------------------------------------------------------------------------------------
tscp_cases = []
xb = torch.tensor(rng.normal(0, 1, (5, 10, 3)), dtype=torch.float32)
for batch_norm in [False, True]:
    args = {"experiments_name": "synthetic", "num_workers": 0,
            "model": {"c_in": 3, "nb_filters": 6, "kernel_size": 3, "nb_stacks": 2, "dilations": [1, 2],
                      "use_skip_connections": True, "use_batch_norm": batch_norm, "use_layer_norm": False,
                      "use_weight_norm": False, "dropout_rate": 0.0, "seq_len": 4, "n_steps": 3, "code_size": 4,
                      "window": 4, "window_1": 4, "window_2": 4},
            "learning": {"batch_size": 5, "lr": 1e-3, "decay_steps": 100}, "loss": {"temperature": 0.5}}
    torch.manual_seed(4)
    encoder = tscp.BaseTSCPEncoder(args)
    # Test weights: with the N(0, 0.05) initialisation of the reference, this small network has all the
    # ReLUs of its dense layers inactive (constant embeddings, zero gradients). Larger convolution weights,
    # biases away from the ReLU kink (zero-padded positions) and positive dense biases exercise it.
    with torch.no_grad():
        for m in encoder.modules():
            if isinstance(m, torch.nn.Conv1d):
                m.weight.normal_(0, 0.5)
                m.bias.normal_(0, 0.1)
            if isinstance(m, torch.nn.Linear):
                m.weight.normal_(0, 0.5)
                m.bias.fill_(0.1)
    model = tscp.TSCP_model(args, encoder, None, None)
    model.train()
    params = {k: matrix(v) for k, v in encoder.state_dict().items()}
    loss = model.training_step((xb, None), 0)
    loss.backward()
    tscp_cases.append({"batch_norm": batch_norm, "params": params,
                       "grads": {k: matrix(v.grad) for k, v in encoder.named_parameters() if v.grad is not None},
                       "after": {k: tolist(v) for k, v in encoder.state_dict().items() if "running" in k},
                       "loss": float(loss)})
    model.eval()
    if batch_norm:
        tscp_bn_model = model
    else:
        tscp_model = model
out["tscp"] = {"x": tolist(xb), "cases": tscp_cases}

# TS2Vec -------------------------------------------------------------------------------------------
log = {"randint": [], "masks": [], "drops": []}
real_randint = np.random.randint


def logged_randint(*args, **kwargs):
    value = real_randint(*args, **kwargs)
    log["randint"].append(np.asarray(value).tolist())
    return value


def logged_mask(B, T, p=0.5):
    mask = torch.from_numpy(np.random.binomial(1, p, size=(B, T))).to(torch.bool)
    log["masks"].append(mask.numpy().astype(int).tolist())
    return mask


class LoggedDropout(torch.nn.Module):
    def forward(self, x):
        if not self.training:
            return x
        keep = (torch.rand_like(x) >= 0.1).to(x.dtype)
        log["drops"].append(tolist(keep / 0.9))
        return x * keep / 0.9


ts2vec_sn.generate_binomial_mask = logged_mask
args = {"experiments_name": "synthetic", "num_workers": 0,
        "model": {"max_train_length": None, "window": 3, "window_1": 3, "window_2": 3},
        "learning": {"lr": 1e-3, "batch_size": 4, "weight_decay": 0.2}, "loss": {"temporal_unit": 0}}
torch.manual_seed(5)
np.random.seed(5)
encoder = ts2vec_sn.TSEncoder(input_dim=3, output_dim=4, hidden_dim=5, depth=2, spec_norm=True)
encoder.repr_dropout = LoggedDropout()
model = ts2vec_sn.TS2Vec(args, encoder, None, None)
model.train()
xs = torch.tensor(rng.normal(0, 2, (4, 11, 3)), dtype=torch.float32)
before = {k: matrix(v) for k, v in encoder.state_dict().items()}
np.random.randint = logged_randint
loss = model.shared_step((xs, None))
np.random.randint = real_randint
loss.backward()
out["ts2vec"] = {"x": tolist(xs), "params": before,
                 "grads": {k: matrix(v.grad) for k, v in encoder.named_parameters() if v.grad is not None},
                 "after": {k: tolist(v) for k, v in encoder.state_dict().items() if k.endswith("_u") or
                           k.endswith("_v")},
                 "loss": float(loss), "randint": log["randint"], "masks": log["masks"], "drops": log["drops"]}
model.eval()
ts2vec_model = model

# Optimizers ---------------------------------------------------------------------------------------
optim = {}
grads = [rng.normal(0, 1, 6) for _ in range(12)]
start = rng.normal(0, 1, 6)
for name, make_opt, make_sched in [
        ("adam_cosine", lambda w: torch.optim.Adam([w], lr=0.01),
         lambda o: torch.optim.lr_scheduler.CosineAnnealingLR(o, T_max=5)),
        ("adamw_restarts", lambda w: torch.optim.AdamW([w], lr=0.01, weight_decay=0.2),
         lambda o: torch.optim.lr_scheduler.CosineAnnealingWarmRestarts(o, T_0=3, T_mult=2))]:
    w = torch.nn.Parameter(torch.tensor(start, dtype=torch.float64))
    opt = make_opt(w)
    sched = make_sched(opt)
    path = []
    for epoch in range(4):
        for k in range(3):
            opt.zero_grad()
            w.grad = torch.tensor(grads[epoch * 3 + k], dtype=torch.float64)
            opt.step()
        sched.step()
        path.append(tolist(w))
    optim[name] = path
out["optimizers"] = {"start": start.tolist(), "grads": [g.tolist() for g in grads], "paths": optim}

# Predictions of the representation models ---------------------------------------------------------
xp = torch.tensor(rng.normal(0, 1, (3, 20, 3)), dtype=torch.float32)
xp[:, 12:, :] += 2.0
predictions = {"x": tolist(xp), "cases": []}
for name, model, w, max_pool in [("tscp", tscp_model, 4, False), ("tscp_bn", tscp_bn_model, 4, False),
                                 ("ts2vec", ts2vec_model, 3, True)]:
    for step, alpha in [(1, 1.0), (3, 0.3)]:
        with torch.no_grad():
            raw = prediction_utils.get_repr_learning_output(model, xp, w, w, step=step, max_pool=max_pool)
            processed = prediction_utils.post_process_output(raw, alpha=alpha)
        predictions["cases"].append({"model": name, "window": w, "step": step, "alpha": alpha,
                                     "scores": tolist(processed)})
predictions["models"] = {"tscp": {k: matrix(v) for k, v in tscp_model.model.state_dict().items()},
                         "tscp_bn": {k: matrix(v) for k, v in tscp_bn_model.model.state_dict().items()},
                         "ts2vec": {k: matrix(v) for k, v in ts2vec_model._net.state_dict().items()}}
out["predictions"] = predictions

with open(os.path.join(out_dir, "units.json"), "w") as f:
    json.dump(out, f)
print("UNITS_DONE")
