#!/usr/bin/env bash
# Downloads the ophthalmology OOD benchmark of Ju et al. (MICCAI 2025), which
# PROTON uses for its main results (Table 2), and the FLAIR weights.
# Run inside the devcontainer, from the package root, after setup_env.sh:
#   bash development/validation/proton/get_data.sh
#
# Layout produced under data-real/proton/ (ignored by git):
#   fives/{N,D,G,A}/   FIVES images by class (Normal, Diabetic retinopathy,
#                      Glaucoma, AMD)
#   uwf/               DeepDRiD ultra-widefield fundus images (covariate shift)
#   imagenet/          ImageNet images shared by Ju et al. (far-OOD)
#   flair_resnet.pth   FLAIR weights
set -euo pipefail

ROOT=data-real/proton
mkdir -p "$ROOT"

# FIVES (Jin et al., 2022, CC BY 4.0): 800 images, 200 per class.
if [ ! -d "$ROOT/fives" ]; then
  [ -f "$ROOT/fives.rar" ] || wget -q -O "$ROOT/fives.rar" https://ndownloader.figshare.com/files/34969398
  echo "789c80dd5376a82063e27fa49192bac9  $ROOT/fives.rar" | md5sum -c -
  # unar cannot decode two entries of this (intact) archive, including
  # train/Original/447_G.png; the images are checked below and unreadable ones
  # are left out.
  [ -d "$ROOT/fives_raw" ] || unar -q -o "$ROOT/fives_raw" "$ROOT/fives.rar" || echo "unar reported errors; checking the images"
  python3 - "$ROOT" <<'EOF'
import pathlib, shutil, sys
from PIL import Image
root = pathlib.Path(sys.argv[1])
names = {"N": "N", "D": "D", "G": "G", "A": "A"}
for image in sorted((root / "fives_raw").rglob("*.png")):
    if "Original" not in image.parts:
        continue  # skip the vessel annotations
    label = image.stem.split("_")[-1].upper()
    if label not in names:
        continue
    try:
        with Image.open(image) as im:
            im.load()
    except OSError as error:
        print(f"skipping unreadable image {image.name}: {error}")
        continue
    target = root / "fives" / names[label]
    target.mkdir(parents=True, exist_ok=True)
    # train/ and test/ restart their numbering, so keep the split in the name.
    split = "test" if "test" in [p.lower() for p in image.parts] else "train"
    shutil.copy(image, target / f"{split}_{image.name}")
print({d.name: len(list(d.iterdir())) for d in sorted((root / "fives").iterdir())})
EOF
fi

# DeepDRiD ultra-widefield images (CC BY-SA 4.0), only that folder.
if [ ! -d "$ROOT/uwf" ]; then
  git clone -q --depth 1 --filter=blob:none --sparse https://github.com/deepdrdoc/DeepDRiD "$ROOT/deepdrid"
  git -C "$ROOT/deepdrid" sparse-checkout set ultra-widefield_images
  mkdir -p "$ROOT/uwf"
  find "$ROOT/deepdrid/ultra-widefield_images" -type f \( -iname '*.jpg' -o -iname '*.png' \) | while read -r f; do
    cp "$f" "$ROOT/uwf/$(echo "$f" | sed "s|$ROOT/deepdrid/ultra-widefield_images/||; s|/|_|g")"
  done
  echo "uwf images: $(ls "$ROOT/uwf" | wc -l)"
fi

# ImageNet subset shared by Ju et al. for the far-OOD setting (936 ILSVRC2012
# validation images). Google Drive limits how often its files can be
# downloaded; when it refuses, gdown stops and the images already fetched are
# kept. Running this script again resumes the download.
mkdir -p "$ROOT/imagenet_raw" "$ROOT/imagenet"
if [ "$(ls "$ROOT/imagenet_raw" | wc -l)" -lt 150 ]; then
  (cd "$ROOT" && gdown -q --continue --folder 1wIl7Pk0SXmZY5fhyWTtd4W6VQYP1FAg6 -O imagenet_raw) \
    || echo "Google Drive stopped the ImageNet download; keeping the files fetched so far"
fi
find "$ROOT/imagenet_raw" -type f \( -iname '*.jpeg' -o -iname '*.jpg' -o -iname '*.png' \) -exec cp {} "$ROOT/imagenet/" \;
echo "imagenet images: $(ls "$ROOT/imagenet" | wc -l)"

# FLAIR ResNet-50 weights: the Google Drive file used by the FLAIR package, or
# the Hugging Face mirror davanstrien/flair when Drive refuses the download.
if [ ! -f "$ROOT/flair_resnet.pth" ]; then
  if gdown -q 1l24_2IzwQdnaa034I0zcyDLs_zMujsbR -O "$ROOT/flair_weights.zip"; then
    python3 -c "import zipfile, sys; zipfile.ZipFile(sys.argv[1]).extractall(sys.argv[2])" "$ROOT/flair_weights.zip" "$ROOT"
  else
    python3 -c "
import shutil, sys
from huggingface_hub import hf_hub_download
shutil.copy(hf_hub_download('davanstrien/flair', 'flair_resnet.pth'), sys.argv[1])" "$ROOT/flair_resnet.pth"
  fi
  ls -la "$ROOT/flair_resnet.pth"
fi
