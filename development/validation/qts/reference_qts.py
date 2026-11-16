"""Runs the detection loop of the reference QTS code on one dataset.

The loop is the one of QTS_method.py (https://github.com/beraram/QTS-method),
copied without changes except for the parameters, which are arguments here,
the progress printing, which was removed, and the stream: skmultiflow's
DataStream, which does not install on current Python, is replaced by a loop
over the rows of the same scaled data frame. QTree_plus.py is imported from
the cloned repository.

Usage: python3 reference_qts.py <QTree_plus dir> <dataset.csv> <output prefix>
       [height=4] [window=500] [reference=1500]

Writes <prefix>_detections.csv (the 1-based sample positions of drift_found)
and <prefix>_occupancy.csv (data_QT without its two initial values).
"""

import sys

import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler

sys.path.insert(0, sys.argv[1])
from QTree_plus import Node as NDT  # noqa: E402

path, prefix = sys.argv[2], sys.argv[3]
height = int(sys.argv[4]) if len(sys.argv) > 4 else 4
QTS_win_size = int(sys.argv[5]) if len(sys.argv) > 5 else 500
win_tam_size = int(sys.argv[6]) if len(sys.argv) > 6 else 1500

# Dataset reading, as in QTS_method.py (the first row becomes the header).
df = pd.read_csv(path)
df = df.astype(float)
scaler = MinMaxScaler()
df.iloc[:, 0:df.shape[1] - 1] = scaler.fit_transform(df.iloc[:, 0:df.shape[1] - 1])
X_all = df.iloc[:, 0:df.shape[1] - 1].to_numpy()

# DataStream replacement: the first sample sets the dimension of the tree.
X_train = X_all[0:1]
mid = [0.5] * X_train[0].size
ndta = NDT(mid, 0.5, height)
n_samples = 1

drift_found = []
data_QT = [0, 1]
derivative_w = []
derivative_rd = 0
derivate_b = []
QTS_win = []
flag_dv_change = 'false'
flag_initial = 'true'
flag_ocup_change = 'false'
repete_flag = 'false'
threshold_dv = 0
cont_drift = 0
ocup_ref = 0
ocup_win = 0
ocup_dif = 0
std_ref = 0
std_win = 0
cont_ocup_drift = 0
ocup_win_nmean = 0
rho = 0.1
alpha = 1
beta = 3

for row in range(1, X_all.shape[0]):
    n_samples += 1
    X = X_all[row:row + 1]

    QTS_win.append(X[0])
    if len(QTS_win) > QTS_win_size:
        QTS_win = QTS_win[1:]

    ndta = NDT(mid, 0.5, height)
    for i, j in enumerate(QTS_win):
        ndta.insert(j)

    data_QT.append(ndta.number_leaf())

    if data_QT[-1] == data_QT[-2]:
        derivative_w.append(0)
    if data_QT[-1] > data_QT[-2]:
        derivative_w.append(1)
    if data_QT[-1] < data_QT[-2]:
        derivative_w.append(-1)

    attenuator = 1
    rd = int(rho * QTS_win_size)

    if (len(data_QT) > (QTS_win_size + win_tam_size)) and (flag_initial == 'true'):
        ocup_ref = np.mean(data_QT[-win_tam_size:])
        std_ref = np.std(data_QT[-win_tam_size:])
        flag_initial = 'false'

    if (len(data_QT) > (QTS_win_size + win_tam_size)):
        ocup_df = pd.DataFrame(data_QT[-win_tam_size:])
        ocup_win_df = ocup_df.ewm(span=win_tam_size).mean()
        ocup_win = ocup_win_df.iloc[win_tam_size - 1, 0]
        ocup_win_nmean = np.mean(data_QT[-win_tam_size:])
        std_win = np.std(data_QT[-win_tam_size:])

    if len(derivative_w) > win_tam_size:
        threshold_dv = (np.mean(derivative_w[-win_tam_size:-(rd - 1)]) + alpha * np.std(derivative_w[-win_tam_size:-(rd - 1)]))
        derivative_rd = (np.mean(derivative_w[-rd:]) * attenuator)
        derivate_b.append(derivative_rd)

    if (repete_flag == 'true'):
        if (abs(derivative_rd) > threshold_dv):
            drift_found.append(n_samples)

    if (repete_flag == 'false'):
        if (abs(derivative_rd) > threshold_dv) and (flag_dv_change == 'false'):
            drift_found.append(n_samples)
            flag_dv_change = 'true'

        if (flag_dv_change == 'true'):
            if cont_drift < (QTS_win_size + win_tam_size):
                cont_drift += 1
            else:
                cont_drift = 0
                flag_dv_change = 'false'
                ocup_ref = ocup_win_nmean
                std_ref = std_win

    if (flag_initial == 'false'):
        ocup_dif = ocup_win

    if ((ocup_dif > (ocup_ref + beta * std_ref)) or (ocup_dif < (ocup_ref - beta * std_ref))) and (flag_dv_change == 'false') and (flag_ocup_change == 'false'):
        drift_found.append(n_samples)
        flag_ocup_change = 'true'

    if flag_ocup_change == 'true':
        if cont_ocup_drift < win_tam_size:
            cont_ocup_drift += 1
        else:
            cont_ocup_drift = 0
            ocup_ref = ocup_win_nmean
            std_ref = std_win
            flag_ocup_change = 'false'

pd.DataFrame({"sample": drift_found}).to_csv(prefix + "_detections.csv", index=False)
pd.DataFrame({"occupancy": data_QT[2:]}).to_csv(prefix + "_occupancy.csv", index=False)
print(path, "samples:", n_samples, "detections:", drift_found)
