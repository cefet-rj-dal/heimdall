"""One training and evaluation run of the reference seq2seq model (train.py and
test.py of the reference, with Lightning replaced by ref_env.train_reference).

Usage: python3 -I run_reference.py <reference repo> <data dir> <output dir> <dataset> <loss> <seed>

dataset: synthetic_1D or synthetic_100D; loss: indid, bce or combined.
Settings of configs/<dataset>_seq2seq.yaml. fix_seeds(seed) before the model
is built (as train.py). The test set is the validation set of early stopping
(as CPDModel.val_dataloader). Writes <dataset>_<loss>_<seed>.json with the
metrics of utils/metrics.py evaluation_pipeline (100 thresholds of test.py),
the number of epochs and the test probabilities (<...>_prob.bin, float64).
"""

import json
import os
import sys
import time

import numpy as np
import torch
import yaml

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ref_env  # noqa: E402

repo, data_dir, out_dir, dataset, loss_type, seed = sys.argv[1:7]
seed = int(seed)
torch.set_num_threads(1)
ref = ref_env.load(repo)
os.makedirs(out_dir, exist_ok=True)
config = yaml.safe_load(open(os.path.join(repo, "configs", f"{dataset}_seq2seq.yaml")))


def load(part):
    meta = json.load(open(os.path.join(data_dir, f"{dataset}_{part}.json")))
    x = np.fromfile(os.path.join(data_dir, f"{dataset}_{part}_x.bin"), dtype="<f8").reshape(meta["shape"])
    y = np.fromfile(os.path.join(data_dir, f"{dataset}_{part}_y.bin"), dtype="<f8").reshape(meta["shape"][:2])
    return torch.utils.data.TensorDataset(torch.tensor(x).float(), torch.tensor(y).float())


train, test = load("train"), load("test")
args = {"experiments_name": dataset, "learning": config["learning"], "num_workers": 0, "loss": config["loss"]}
ref["cpd_models"].fix_seeds(seed)
core = ref["core_models"].BaseRnn(input_size=config["model"]["input_size"], hidden_dim=config["model"]["hidden_dim"],
                                  n_layers=config["model"]["n_layers"], drop_prob=config["model"]["drop_prob"])
stages = ["bce", "indid"] if loss_type == "combined" else [loss_type]
started = time.time()
epochs = []
for stage in stages:
    model = ref["cpd_models"].CPDModel(stage, args, core, train, test)
    history = ref_env.train_reference(model, train, test, epochs=config["learning"]["epochs"] // len(stages),
                                      patience=config["early_stopping"]["patience"],
                                      batch_size=config["learning"]["batch_size"])
    epochs.append(len(history))
seconds = time.time() - started
model.eval()
loader = torch.utils.data.DataLoader(test, batch_size=config["learning"]["batch_size"], shuffle=False)
with torch.no_grad():
    prob = torch.cat([model(x) for x, _ in loader]).double().numpy()
result, _, _ = ref["metrics"].evaluation_pipeline(model, loader, ref_env.threshold_list(100), device="cpu",
                                                  model_type="seq2seq", verbose=False)
best_th, time_fa, delay, auc, conf, f1, cover, best_th_cover, max_cover = result
name = f"{dataset}_{loss_type}_{seed}"
prob.astype("<f8").tofile(os.path.join(out_dir, name + "_prob.bin"))
with open(os.path.join(out_dir, name + ".json"), "w") as f:
    json.dump({"dataset": dataset, "loss": loss_type, "seed": seed, "epochs": epochs, "seconds": seconds,
               "time_to_fa": time_fa, "delay": delay, "auc": float(auc), "f1": f1, "cover": float(cover),
               "max_cover": float(max_cover), "threshold": float(best_th), "confusion": list(conf)}, f)
print(name, "epochs", epochs, "F1", round(f1, 4), "AUC", round(float(auc), 2), flush=True)
