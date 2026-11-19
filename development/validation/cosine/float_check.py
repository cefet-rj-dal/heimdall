"""Origin of the differences between cosine_score() and evaluation.py.

The reference loop runs scipy's cosine() on float32 embeddings, which computes
in float32. Rerunning the same loop in float64 on the notebook sentences, and
comparing with the identity mean_i cos(x, e_i) = x/|x| . mean_i e_i/|e_i| used
by cosine_score(), separates float32 rounding from the method.

Usage (inside the devcontainer, from the package root):
  python3 -I development/validation/cosine/float_check.py data-real/smm/embeddings
"""

import json
import sys

import numpy as np
from scipy.spatial.distance import cosine

folder = sys.argv[1]
B = np.fromfile(f"{folder}/baseline_padded.bin").reshape(-1, 768)
X = np.fromfile(f"{folder}/notebook.bin").reshape(-1, 768)
reference = json.load(open(f"{folder}/reference_scores.json"))["sets"]["notebook"]["scores"]
loop64 = np.array([sum(1 - cosine(x, e) for e in B) / len(B) for x in X])
Bn = B / np.linalg.norm(B, axis=1, keepdims=True)
identity = (X @ Bn.mean(0)) / np.linalg.norm(X, axis=1)
print("float32 loop vs float64 loop", np.max(np.abs(loop64 - reference)))
print("float64 loop vs mean-direction identity", np.max(np.abs(loop64 - identity)))
