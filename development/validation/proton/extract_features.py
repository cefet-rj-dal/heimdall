"""Extracts the FLAIR outputs that PROTON consumes on the ophthalmology benchmark.

For every image of the benchmark of Ju et al. (MICCAI 2025), as used in Table 2
of the PROTON paper, writes one row with the l2-normalized image embedding
(e1..e512) and the zero-shot softmax probabilities over the two
in-distribution classes (p_normal, p_dr), computed with the vanilla class
names as prompts.

Sampling follows Ju et al.'s notebooks (seed 1): 150 random Normal and 150
random Diabetic retinopathy FIVES images (in-distribution), 150 Glaucoma and
150 AMD (semantic shift), 150 ultra-widefield images (covariate shift) and 150
ImageNet images (far-OOD).

Run inside the devcontainer, from the package root, after get_data.sh:
    python3 development/validation/proton/extract_features.py
"""

import pathlib
import random

import numpy as np
import pandas as pd
import torch
from PIL import Image

from flair import FLAIRModel

ROOT = pathlib.Path("data-real/proton")
OUTPUT = ROOT / "flair_features.csv"
PER_CLASS = 150
CLASS_NAMES = ["normal", "diabetic retinopathy"]

GROUPS = [
    # (group, shift, folder)
    ("id_normal", "id", ROOT / "fives" / "N"),
    ("id_dr", "id", ROOT / "fives" / "D"),
    ("sem_glaucoma", "semantic", ROOT / "fives" / "G"),
    ("sem_amd", "semantic", ROOT / "fives" / "A"),
    ("cov_uwf", "covariate", ROOT / "uwf"),
    ("far_imagenet", "far", ROOT / "imagenet"),
]


def sample_images(folder):
    images = sorted(p for p in folder.iterdir() if p.suffix.lower() in {".png", ".jpg", ".jpeg"})
    random.shuffle(images)
    return images[:PER_CLASS]


def main():
    random.seed(1)
    np.random.seed(1)
    torch.manual_seed(1)

    # caption="[CLS]" gives the bare class names as prompts. (Ju et al.'s README
    # suggests caption="", but FLAIR builds prompts with
    # caption.replace("[CLS]", class), so an empty caption would give every
    # class the same empty prompt.)
    model = FLAIRModel(from_checkpoint=True, weights_path=str(ROOT / "flair_resnet.pth"), caption="[CLS]")
    model.eval()

    with torch.no_grad():
        input_ids, attention_mask = model.preprocess_text(CLASS_NAMES)
        text = model.text_model(input_ids, attention_mask)  # (C, d), normalized
        scale = torch.clamp(model.logit_scale.data, 0, 4.6052).exp()

    rows = []
    for group, shift, folder in GROUPS:
        for path in sample_images(folder):
            image = np.array(Image.open(path).convert("RGB"))
            with torch.no_grad():
                embedding = model.vision_model(model.preprocess_image(image))  # (1, d), normalized
                probs = torch.softmax(embedding @ text.t() * scale, dim=-1)
            e = embedding[0].cpu().numpy()
            p = probs[0].cpu().numpy()
            row = {"group": group, "shift": shift, "file": path.name, "p_normal": p[0], "p_dr": p[1]}
            row.update({f"e{i + 1}": v for i, v in enumerate(e)})
            rows.append(row)
        print(f"{group}: {sum(r['group'] == group for r in rows)} images")

    pd.DataFrame(rows).to_csv(OUTPUT, index=False)
    print(f"wrote {OUTPUT}")


if __name__ == "__main__":
    main()
