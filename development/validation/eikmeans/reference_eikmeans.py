"""Runs the reference EI-kMeans code on the datasets of the paper.

Usage: MPLBACKEND=Agg python3 -I reference_eikmeans.py <reference repo> <data dir> <output dir>

1. Generates the data of Experiment 2 (Exp2_Syn_dataGenerator.py, the six 2-D
   datasets, N_train = 5000, n_test = 200, 250 test pairs, seed 1) and of
   Experiment 4 (Exp3_Rea_dataGenerator.py, five real-world datasets) with the
   reference generators, unchanged. The Exp2 generator joins paths with
   Windows separators, so on Linux its files are named "<dir>\\data_*.csv";
   they are renamed into <dir>/.
2. Runs EIkMeans_lib.EIkMeans as EIkMeans_Exp2.py (k = 40; partitioning built
   on N1 = 2000 training points and refilled with N2-N4 = 3000-5000) and
   EIkMeans_Exp3.py (k = 40, 60 for Localization), and records the partition
   (centroids, amplify coefficients, training counts) and the p-value and
   decision of every test window. The p-values are captured by wrapping
   scipy's chi2_contingency inside the module; the code is otherwise unchanged.
"""

import glob
import os
import shutil
import sys

import numpy as np

repo, data_dir, out_dir = sys.argv[1], sys.argv[2], sys.argv[3]
try:
    import matplotlib.pyplot  # noqa: F401
except ImportError:
    # EIkMeans_lib plots the bins in fill_lambda (plt.scatter, plt.show); the
    # plot has no effect on the results, so pyplot is replaced by no-ops.
    import types
    pyplot = types.ModuleType("matplotlib.pyplot")
    pyplot.scatter = pyplot.show = lambda *args, **kwargs: None
    sys.modules["matplotlib"] = types.ModuleType("matplotlib")
    sys.modules["matplotlib.pyplot"] = pyplot
sys.path.insert(0, repo)
sys.path.insert(0, os.path.join(repo, "Datasets"))
import EIkMeans_lib as eikm  # noqa: E402
import Exp2_Syn_dataGenerator as gen2  # noqa: E402
import Exp3_Rea_dataGenerator as gen3  # noqa: E402

os.makedirs(out_dir, exist_ok=True)
captured = []
original_chi2 = eikm.chi2_contingency


def recording_chi2(table):
    result = original_chi2(table)
    captured.append(result[1])
    return result


eikm.chi2_contingency = recording_chi2

# 1. Data -----------------------------------------------------------------------
exp2_dir = os.path.join(data_dir, "exp2")
exp2 = {"2d-U-mean": gen2.generate_2d_U_mean, "2d-1G-mean": gen2.generate_2d_1G_mean,
        "2d-1G-var": gen2.generate_2d_1G_var, "2d-1G-cov": gen2.generate_2d_1G_cov,
        "2d-2G-mean": gen2.generate_2d_2G_mean, "2d-4G-mean": gen2.generate_2d_4G_mean}
for name, generate in exp2.items():
    folder = os.path.join(exp2_dir, name)
    if os.path.exists(os.path.join(folder, "data_train.csv")):
        continue
    generate(folder=folder, r_seed=1, N_train=5000, n_test=200, test_num=250)
    for path in glob.glob(folder + "\\*"):
        shutil.move(path, os.path.join(folder, path.split("\\")[-1]))

exp3_dir = os.path.join(data_dir, "exp3")
original = os.path.join(repo, "Datasets", "OriginalDataFiles")
if not os.path.exists(os.path.join(exp3_dir, "5_Insects")):
    gen3.generate_Higgs(1, os.path.join(original, "1_Higgs", "data0"), os.path.join(original, "1_Higgs", "data1"),
                        os.path.join(exp3_dir, "1_Higgs") + "/", 2000, 1000, 250)
    gen3.generate_MiniBooNe(1, os.path.join(exp3_dir, "2_MiniBooNeParticle") + "/",
                            os.path.join(original, "2_MiniBooNe", "MiniBooNE_PID.txt"), 2000, 500, 250)
    gen3.generate_ArabicDigit(1, os.path.join(exp3_dir, "3_Arabic_Digit") + "/",
                              os.path.join(original, "3_Arabic_Digit", "ArabicDigit_Shuffled_With_Sex.csv"),
                              2000, 500, 250)
    gen3.generate_Localization(1, os.path.join(exp3_dir, "4_Localization") + "/",
                               os.path.join(original, "4_Localization", "ConfLongDemo_JSI.txt"), 3000, 600, 250)
    gen3.generate_Insects(1, os.path.join(exp3_dir, "5_Insects") + "/",
                          os.path.join(original, "5_Insects", "Insects.data"), 2000, 500, 250)


# 2. Reference runs ----------------------------------------------------------------
def load(path):
    # Some generators write space-separated files (np.savetxt with fmt="%s"
    # and no delimiter), which EIkMeans_Exp3.py reads with delimiter=",".
    with open(path) as f:
        comma = "," in f.readline()
    return np.loadtxt(path, dtype=float, delimiter="," if comma else None, ndmin=2)


def window(folder, kind, t):
    for name in (f"data_{kind}_{t}.csv", f"data_{kind.upper()}_{t}.csv"):
        path = os.path.join(folder, name)
        if os.path.exists(path):
            return load(path)
    raise FileNotFoundError(os.path.join(folder, f"data_{kind}_{t}.csv"))


def run(setting, folder, k, sizes=None, test_num=250):
    train = load(os.path.join(folder, "data_train.csv"))
    w0 = [window(folder, "w0", t) for t in range(test_num)]
    w1 = [window(folder, "w1", t) for t in range(test_num)]
    base = eikm.EIkMeans(k)
    base.build_partition(train[: (sizes or [len(train)])[0]], w0[0].shape[0])
    rows = []
    for n_train in (sizes or [len(train)]):
        model = base.get_copy()
        model.fill_lambda(train[:n_train])
        prefix = os.path.join(out_dir, f"{setting}_n{n_train}")
        np.savetxt(prefix + "_centers.csv", model.C, delimiter=",", fmt="%.17g")
        np.savetxt(prefix + "_amplify.csv", model.amplify_coe, delimiter=",", fmt="%.17g")
        np.savetxt(prefix + "_counts.csv", model.lambdas, delimiter=",", fmt="%d")
        for t in range(test_num):
            for kind, data in (("w0", w0[t]), ("w1", w1[t])):
                captured.clear()
                h = model.drift_detection(data, 0.05)
                rows.append(f"{setting},{n_train},{kind},{t},{float(captured[0])!r},{h}")
    print(setting, "bins:", len(base.C), flush=True)
    return rows


rows = ["setting,n_train,window,t,p_value,drift"]
for name in exp2:
    rows += run(name, os.path.join(exp2_dir, name), 40, sizes=[2000, 3000, 4000, 5000])
exp3 = [("Higgs-Back", "1_Higgs/back_higg", 40), ("Back-Higgs", "1_Higgs/higg_back", 40),
        ("Sign-Back", "2_MiniBooNeParticle/singal_backgr", 40), ("Back-Sign", "2_MiniBooNeParticle/backgr_singal", 40),
        ("Arabic-A-B", "3_Arabic_Digit/mix1_mix2", 40), ("Arabic-B-A", "3_Arabic_Digit/mix2_mix1", 40),
        ("Localization", "4_Localization/mix1", 60), ("Insect", "5_Insects/mix1", 40)]
for name, folder, k in exp3:
    rows += run(name, os.path.join(exp3_dir, folder), k)
with open(os.path.join(out_dir, "decisions.csv"), "w") as f:
    f.write("\n".join(rows) + "\n")
print("done")
