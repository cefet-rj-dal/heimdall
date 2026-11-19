"""Reference runs of the WWAggr code on the Human Activity data, for the
validation of heimdall's WWAggr implementation.

Usage (inside the devcontainer, from the package root):
  python3 -I development/validation/wwaggr/har_reference.py <reference_dir> <betacal_dir> <hapt_dir> <out_dir> \
      dataset
  python3 -I development/validation/wwaggr/har_reference.py <reference_dir> <betacal_dir> <hapt_dir> <out_dir> \
      train <model> <ensemble> <member>

<model> is bce, indid, tscp or ts2vec; <ensemble> 1 to 3; <member> 0 to 9.

The reference modules (datasets.py, models, baselines, configs/human_activity_*)
are used unchanged, with stand-ins for the packages the container lacks and
the functions do not use: av and pytorchvideo (video data); pytorch_lightning
(LightningModule = torch.nn.Module with a no-op log() and a cpu device).

  dataset  HumanActivityDataset (train and test, seq_len 20) and the
           calibration split of evaluate_ensemble.py (train_test_split of the
           training set, test_size 0.3, random_state 42): writes
           har_dataset.npz.
  train    One member, as scripts/train_single_models.py: seed
           s + 10 (ensemble - 1), fix_seeds(seed), get_models_list(), then the
           training loop of Lightning's Trainer (max_epochs, a sanity check of
           two validation batches, the configured optimiser and scheduler
           stepped every epoch, gradient clipping as configured, validation on
           the test set every epoch, EarlyStopping with the configured monitor,
           min_delta and patience; the last model is kept). Then the scores of
           the test and calibration sequences, as evaluate_ensemble.py (seq2seq
           outputs; get_repr_learning_output + post_process_output with the
           configured step and alpha), and the beta calibration of
           calibrate_all_models_in_ensemble (BetaCalibration "abm" fitted on
           the float32 scores of the calibration split) applied to the test
           scores. Writes members/<model>_<ensemble>_<member>.npz.
"""

import json
import os
import sys
import time
import types

import numpy as np
import torch

reference_dir, betacal_dir, hapt_dir, out_dir, mode = sys.argv[1:6]
os.makedirs(os.path.join(out_dir, "members"), exist_ok=True)
torch.set_num_threads(1)


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
stub("av")
stub("pytorchvideo")
stub("pytorchvideo.transforms", ShortSideScale=object)

# torch 2.14 removed the deprecated `verbose` argument of the schedulers, which
# TS2Vec.configure_optimizers passes (verbose=True only prints the rates).
_WarmRestarts = torch.optim.lr_scheduler.CosineAnnealingWarmRestarts


class WarmRestarts(_WarmRestarts):
    def __init__(self, *args, verbose=None, **kwargs):
        super().__init__(*args, **kwargs)


torch.optim.lr_scheduler.CosineAnnealingWarmRestarts = WarmRestarts

sys.path.insert(0, betacal_dir)
sys.path.insert(0, reference_dir)
import yaml  # noqa: E402
from betacal import BetaCalibration  # noqa: E402
from sklearn.model_selection import train_test_split  # noqa: E402
from src.baselines import prediction_utils  # noqa: E402
from src.datasets.datasets import HumanActivityDataset  # noqa: E402
from src.models.model_utils import get_models_list  # noqa: E402
from src.utils.fix_seeds import fix_seeds  # noqa: E402

path = hapt_dir.rstrip("/") + "/"


def datasets():
    train = HumanActivityDataset(path_to_data=path, seq_len=20, train_flag=True)
    test = HumanActivityDataset(path_to_data=path, seq_len=20, train_flag=False)
    return train, test


def stack(dataset, indices=None):
    indices = range(len(dataset)) if indices is None else indices
    items = [dataset[i] for i in indices]
    return np.stack([np.asarray(x, float) for x, _ in items]), np.stack([np.asarray(y, float) for _, y in items])


def calibration_split(train):
    # evaluate_ensemble.py: train_test_split(train_dataset, test_size=0.3, random_state=seed=42).
    idx = np.arange(len(train))
    _, val_idx = train_test_split(idx, test_size=0.3, random_state=42)
    return val_idx


if mode == "dataset":
    train, test = datasets()
    x_train, y_train = stack(train)
    x_test, y_test = stack(test)
    val_idx = calibration_split(train)
    np.savez(os.path.join(out_dir, "har_dataset.npz"), x_train=x_train, y_train=y_train, x_test=x_test,
             y_test=y_test, val_idx=val_idx)
    for name, value in [("x_train", x_train), ("y_train", y_train), ("x_test", x_test), ("y_test", y_test),
                        ("val_idx", val_idx.astype(np.float64))]:
        np.ascontiguousarray(value, dtype="<f8").tofile(os.path.join(out_dir, f"{name}.bin"))
    print(json.dumps({"train": list(x_train.shape), "test": list(x_test.shape), "val": len(val_idx)}))
    sys.exit(0)

model_name, ens_num, member = sys.argv[6], int(sys.argv[7]), int(sys.argv[8])
bootstrap = model_name.endswith("_boot")
base_name = model_name.replace("_boot", "")
model_type = "seq2seq" if base_name in ("bce", "indid") else base_name
with open(os.path.join(reference_dir, "configs", f"human_activity_{model_type}.yaml")) as f:
    args = yaml.safe_load(f.read())
args["experiments_name"] = "human_activity"
args["model_type"] = model_type
args["num_workers"] = 0
args["loss_type"] = base_name if model_type == "seq2seq" else None

class Cached(torch.utils.data.Dataset):
    """The items of a dataset, computed once (HumanActivityDataset.__getitem__ indexes pandas at every call);
    same items in the same order, so the batches drawn by the DataLoader are unchanged."""

    def __init__(self, dataset):
        self.items = [dataset[i] for i in range(len(dataset))]

    def __len__(self):
        return len(self.items)

    def __getitem__(self, i):
        return self.items[i]


train_dataset, test_dataset = (Cached(d) for d in datasets())
full_train = train_dataset
if bootstrap:
    # train_single_models.py with boot_sample_size: torch.manual_seed(42), then torch.randint draws of the
    # training indices for the members of an ensemble. The draws of ensembles 2 and 3 there depend on the
    # random state left by the training of the previous ensemble; here they restart from 42 + 100 (e - 1).
    torch.manual_seed(42 + 100 * (ens_num - 1))
    for _ in range(member + 1):
        idxs = torch.randint(len(train_dataset), size=(len(train_dataset),))
    train_dataset = torch.utils.data.Subset(train_dataset, idxs)
seed = member + 10 * (ens_num - 1)
fix_seeds(seed)
model = get_models_list(args, train_dataset, test_dataset)[-1]

# Training loop of Lightning's Trainer.
configured = model.configure_optimizers()
if isinstance(configured, dict):
    optimizer, scheduler = configured["optimizer"], configured["lr_scheduler"]
else:
    optimizer, scheduler = configured, None
train_loader = model.train_dataloader()
val_loader = model.val_dataloader()
step_fn = model.shared_step if model_type == "ts2vec" else None


def validation_loss(limit=None):
    model.eval()
    total, count = 0.0, 0
    with torch.no_grad():
        for i, batch in enumerate(val_loader):
            if limit is not None and i >= limit:
                break
            loss = model.validation_step(batch, i)
            size = len(batch[0])
            total += float(loss) * size
            count += size
    return total / count


stopping = args["early_stopping"]
monitor = stopping["monitor"]
min_delta = abs(stopping.get("min_delta", 0.0))
patience = stopping["patience"]
grad_clip = args["learning"].get("grad_clip", 0.0)
validation_loss(limit=2)  # sanity check of Lightning (two validation batches)
best, wait, history = float("inf"), 0, []
start = time.time()
for epoch in range(args["learning"]["epochs"]):
    model.train()
    total, count = 0.0, 0
    for i, batch in enumerate(train_loader):
        loss = model.training_step(batch, i)
        optimizer.zero_grad()
        loss.backward()
        if grad_clip and grad_clip > 0:
            torch.nn.utils.clip_grad_norm_(model.parameters(), grad_clip)
        optimizer.step()
        size = len(batch[0])
        total += float(loss) * size
        count += size
    train_loss = total / count
    val_loss = validation_loss()
    if scheduler is not None:
        scheduler.step()
    history.append((epoch + 1, train_loss, val_loss))
    current = train_loss if monitor.startswith("train") else val_loss
    if current < best - min_delta:
        best, wait = current, 0
    else:
        wait += 1
        if wait >= patience:
            break
elapsed = time.time() - start

# Scores of the test and calibration sequences (evaluate_ensemble.py).
model.eval()


def scores(x):
    out = []
    with torch.no_grad():
        for i in range(0, len(x), 256):
            batch = torch.tensor(x[i:i + 256])
            if model_type == "seq2seq":
                out.append(model(batch).squeeze(-1).reshape(len(batch), -1).numpy())
            else:
                step, alpha = args["predictions"]["step"], args["predictions"]["alpha"]
                raw = prediction_utils.get_repr_learning_output(model, batch, model.window_1, model.window_2,
                                                                step=step, max_pool=(model_type == "ts2vec"))
                out.append(prediction_utils.post_process_output(raw, alpha=alpha).numpy())
    return np.concatenate(out)


x_test, y_test = stack(test_dataset)
val_idx = calibration_split(full_train)
x_val, y_val = stack(full_train, val_idx)
test_scores = scores(x_test).astype(np.float32)
val_scores = scores(x_val).astype(np.float32)
crop = y_val.shape[1] - val_scores.shape[1]
calibrator = BetaCalibration("abm")
calibrator.fit(val_scores.reshape(-1, 1), y_val[:, crop:].reshape(-1))
test_calibrated = calibrator.predict(torch.tensor(test_scores.reshape(-1))).reshape(test_scores.shape)
prefix = os.path.join(out_dir, "members", f"{model_name}_{ens_num}_{member}")
np.savez(prefix + ".npz", test=test_scores, test_calibrated=test_calibrated, history=np.array(history),
         elapsed=elapsed)
# Raw little-endian float64 [sequence, step] (C order) for R, and the shape.
np.ascontiguousarray(test_scores, dtype="<f8").tofile(prefix + "_test.bin")
np.ascontiguousarray(test_calibrated, dtype="<f8").tofile(prefix + "_test_calibrated.bin")
with open(prefix + ".json", "w") as f:
    json.dump({"shape": list(test_scores.shape), "epochs": len(history), "elapsed": elapsed}, f)
print(f"{model_name} ensemble {ens_num} member {member}: {len(history)} epochs, {elapsed:.0f} s")
