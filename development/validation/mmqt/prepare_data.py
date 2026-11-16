"""Data of the MMQT validation (Stucchi et al., 2023, Section V-A).

Usage: python3 -I prepare_data.py <dir with the quanttree package> <cwru dir> <output dir>

1. synthetic.json: Gaussian modalities drawn with the generator of the
   reference repository (quanttree/utils.py, get_synthetic_modalities, which
   implements the CCM-inspired procedure of the paper): for every dimension d
   and number of modalities M, `runs` sets of M stationary and M post-change
   Gaussians with a minimum symmetric KL distance sKL0 between any two.
2. calibration.json: pairs of stationary Gaussians at an exact symmetric KL
   distance (compute_roto_translation), for Table IV.
3. cwru/<file>.csv: CWRU drive-end vibration, short-time Fourier transform
   over windows of 32 samples with 8 samples of overlap, absolute value of the
   first d = 8 coefficients.
"""

import glob
import json
import os
import sys

import numpy as np
import scipy.io
import scipy.signal

sys.path.insert(0, sys.argv[1])
from quanttree import utils  # noqa: E402

cwru_dir, out_dir = sys.argv[2], sys.argv[3]
os.makedirs(os.path.join(out_dir, "cwru"), exist_ok=True)


def gaussians(modes):
    return [{"mean": m[0].tolist(), "cov": m[1].tolist()} for m in modes]


# 1. Fig. 5-6 and Table III
runs = 20
skl0 = 1.0
synthetic = []
for dim in [4, 8, 16]:
    for n_modes in [2, 4, 6, 8, 10]:
        if dim == 8 and n_modes != 4:
            continue
        for run in range(runs):
            np.random.seed(1000 * dim + 10 * n_modes + run)
            stationary, change = utils.get_synthetic_modalities(nstat=n_modes, nchange=n_modes, dim=dim, dmin=skl0)
            synthetic.append({"dim": dim, "M": n_modes, "run": run, "skl0": skl0,
                              "stationary": gaussians(stationary), "change": gaussians(change)})
with open(os.path.join(out_dir, "synthetic.json"), "w") as f:
    json.dump(synthetic, f)

# 2. Table IV: two stationary modalities in R^8 at sKL in {1, 2, 4, 8}
calibration = []
for skl in [1, 2, 4, 8]:
    for run in range(30):
        np.random.seed(5000 + 100 * skl + run)
        g0 = utils.random_gaussian(8)
        rot, shift = utils.compute_roto_translation(g0, skl)
        g1 = utils.rotate_and_shift_gaussian(g0, rot, shift)
        calibration.append({"skl": skl, "run": run, "measured_skl": float(utils.symmetric_kullback_leibler(g0, g1)),
                            "stationary": gaussians([g0, g1])})
with open(os.path.join(out_dir, "calibration.json"), "w") as f:
    json.dump(calibration, f)

# 3. CWRU
for path in sorted(glob.glob(os.path.join(cwru_dir, "*.mat"))):
    name = os.path.splitext(os.path.basename(path))[0]
    mat = scipy.io.loadmat(path)
    keys = [k for k in mat if k.endswith("_DE_time")]
    own = [k for k in keys if k.startswith("X%03d" % int(name))]
    signal = mat[(own or keys)[0]].ravel()
    _, _, z = scipy.signal.stft(signal, nperseg=32, noverlap=8, boundary=None, padded=False)
    features = np.abs(z[:8, :]).T
    np.savetxt(os.path.join(out_dir, "cwru", name + ".csv"), features, delimiter=",", fmt="%.9g")
    print(name, (own or keys)[0], len(signal), features.shape)
print("done")
