"""Loads the reference InDiD modules (utils/loss.py, core_models.py, cpd_models.py,
datasets.py, metrics.py) unchanged in an environment without the video and
Lightning packages.

The synthetic experiments do not use these packages; each missing one is
replaced by an empty stub module so that the imports at the top of the
reference files succeed:
* pytorch_lightning: LightningModule is torch.nn.Module with a no-op log();
  training is done by train_reference() below, which follows Lightning 1.6
  (see its docstring).
* av, pims, pytorchvideo, torchvision.transforms._transforms_video (video data),
  ruptures and tsai (other baselines), and utils.klcpd / utils.tscp (baselines
  imported by metrics.py, not used for seq2seq models).
"""

import importlib
import sys
import types

import numpy as np
import torch


def _stub(name, **attributes):
    module = types.ModuleType(name)
    for key, value in attributes.items():
        setattr(module, key, value)
    sys.modules[name] = module
    return module


class _LightningModule(torch.nn.Module):
    def log(self, *args, **kwargs):
        pass


def _missing(name):
    try:
        importlib.import_module(name)
        return False
    except Exception:
        return True


def load(repo):
    # metrics.py calls np.trapz, which NumPy 2 renamed np.trapezoid.
    if not hasattr(np, "trapz"):
        np.trapz = np.trapezoid
    _stub("pytorch_lightning", LightningModule=_LightningModule)
    dummy = type("Dummy", (), {"__init__": lambda self, *a, **k: None})
    for name, attributes in [("av", {}), ("pims", {"ImageSequence": dummy}),
                             ("pytorchvideo", {}), ("pytorchvideo.transforms", {"ShortSideScale": dummy}),
                             ("ruptures", {}), ("tsai", {}), ("tsai.models", {}),
                             ("tsai.models.TCN", {"TCN": dummy})]:
        if _missing(name):
            _stub(name, **attributes)
    if _missing("torchvision.transforms._transforms_video"):
        _stub("torchvision.transforms._transforms_video", CenterCropVideo=dummy, NormalizeVideo=dummy)
    if _missing("torchvision.datasets.video_utils"):
        _stub("torchvision.datasets.video_utils", VideoClips=dummy)
    sys.path.insert(0, repo)
    _stub("utils.klcpd")
    _stub("utils.tscp")
    import utils  # noqa: F401
    # The same order as train.py: datasets first (datasets and cpd_models import
    # each other).
    from utils import datasets
    from utils import core_models, cpd_models, loss, metrics
    return {"core_models": core_models, "cpd_models": cpd_models, "datasets": datasets, "loss": loss,
            "metrics": metrics}


def train_reference(model, train_dataset, test_dataset, epochs, patience, batch_size, seed=None):
    """Training of train.py with Lightning 1.6 replaced by a plain loop:
    Adam (CPDModel.configure_optimizers), shuffled train batches, one validation
    pass per epoch, early stopping on the validation loss (mean over the
    validation batches weighted by their size, as Lightning logs it) with
    min_delta 0 and the given patience; the last model is kept (as train.py)."""
    from torch.utils.data import DataLoader
    optimizer = model.configure_optimizers()
    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    val_loader = DataLoader(test_dataset, batch_size=batch_size, shuffle=False)
    best = float("inf")
    wait = 0
    history = []
    for epoch in range(epochs):
        model.train()
        for batch in train_loader:
            optimizer.zero_grad()
            loss_value = model.training_step(batch, 0)
            loss_value.backward()
            optimizer.step()
        model.eval()
        total, count = 0.0, 0
        with torch.no_grad():
            for batch in val_loader:
                n = batch[0].shape[0]
                total += float(model.validation_step(batch, 0)) * n
                count += n
        val_loss = total / count
        history.append(val_loss)
        if val_loss < best:
            best = val_loss
            wait = 0
        else:
            wait += 1
            if wait >= patience:
                break
    return history


def threshold_list(number=100):
    """Thresholds of test.py."""
    values = 1 / (1 + np.exp(-np.linspace(-5, 5, number)))
    return [-0.001] + list(values) + [1.001]
