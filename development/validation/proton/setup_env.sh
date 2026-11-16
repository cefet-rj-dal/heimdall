#!/usr/bin/env bash
# Python environment needed to extract FLAIR features for the PROTON validation.
# Run inside the devcontainer, from the package root:
#   bash development/validation/proton/setup_env.sh
set -euo pipefail

# RAR extractor for the FIVES archive.
apt-get update -qq
apt-get install -y -qq --no-install-recommends unar git > /dev/null

# FLAIR (Silva-Rodriguez et al., 2025) and its dependencies, CPU only. torch is
# already provided by the image; torchvision must match it.
TORCH_VERSION=$(python3 -c "import torch; print(torch.__version__.split('+')[0])")
pip install -q --index-url https://download.pytorch.org/whl/cpu "torchvision" || pip install -q torchvision
pip install -q "transformers<5" gdown huggingface_hub pillow tqdm scikit-learn
pip install -q --no-deps "git+https://github.com/jusiro/FLAIR.git"
python3 -c "import torch, torchvision, transformers, flair; print('torch', torch.__version__, 'torchvision', torchvision.__version__, 'transformers', transformers.__version__)"
echo "torch base version: ${TORCH_VERSION}"
