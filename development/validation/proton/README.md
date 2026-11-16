# PROTON validation

Checks `dfr_proton()` against the ophthalmology benchmark of Das et al. (2026),
*PROTON: Prototype-Based Test-Time Online OOD Detection for Medical VLMs*
(arXiv:2606.20913). The authors' repository
(<https://github.com/GenMI-Lab/PROTON>) contains no code, so the
implementation follows the paper (Algorithm 1, Eqs. 1-4).

## Benchmark

The benchmark is the one by Ju et al. (MICCAI 2025,
<https://github.com/PyJulie/Medical-VLMs-OOD-Detection>), used by Table 2 of
the paper. The model is FLAIR (ResNet-50, d = 512) with frozen weights and the
vanilla class names `normal` and `diabetic retinopathy` as prompts.

| set | source | images |
|---|---|---|
| in-distribution | FIVES Normal + Diabetic retinopathy | 150 + 150 |
| semantic shift | FIVES Glaucoma + AMD | 150 + 150 |
| covariate shift | DeepDRiD ultra-widefield fundus | 150 |
| far-OOD | ImageNet subset shared by Ju et al. | 150 |

The paper does not name the source of the ultra-widefield images. DeepDRiD is
the ultra-widefield set used for covariate shift in Ju et al.'s benchmark.

## Steps

Inside the devcontainer, from the package root:

```sh
bash development/validation/proton/setup_env.sh        # unar, torchvision, transformers, gdown, FLAIR
bash development/validation/proton/get_data.sh         # ~2.5 GB into data-real/proton/
python3 development/validation/proton/extract_features.py   # FLAIR embeddings + probabilities
Rscript development/validation/proton/proton_flair.R        # Tables 1a, 2, 3
```

`proton_flair.R` interleaves the ID images with the OOD images of each shift
type into a stream with 70% ID (the paper's default), feeds it to
`dfr_proton()` with the paper's hyperparameters (gamma = 0.7, M = 100,
K_min = 5, alpha in [0.3, 0.7], sigma2_0 = 0.02), and reports AUROC, AUPR and
FPR@95 for MCM and PROTON, averaged over five stream orders. It also
reproduces the component ablation (Table 3a), the sensitivity to gamma and M
(Tables 3b and 3c) and the ID-fraction study (Table 1a). Tables and a figure
are written to `development/validation/results/`.

## Data actually used in the run reported below

* FIVES archive checked against figshare's MD5. `unar` cannot decode one
  entry of it (`train/Original/447_G.png`, truncated after extraction), so
  150 glaucoma images are drawn from the remaining 199.
* DeepDRiD has 256 ultra-widefield images; 150 are drawn at random.
* Google Drive refused most of Ju et al.'s ImageNet folder (936 images,
  "too many accesses"), so the far-OOD set has the **82** images that could
  be downloaded instead of 150. At 70% ID the far-OOD streams therefore hold
  191 ID images. Running `get_data.sh` again resumes the download.
* Google Drive also refused the FLAIR weights; they came from the Hugging
  Face mirror `davanstrien/flair` (SHA-256 `348db929...b579`, as recorded
  on the mirror; there is no official checksum to compare with).

## Results

Prompts: the bare class names (`caption="[CLS]"`). FLAIR zero-shot accuracy
on the ID images (Normal vs DR): 87.3%. AUROC, mean of 5 stream orders, 70% ID:

| shift | MCM | PROTON | gain | paper MCM | paper PROTON | paper gain |
|---|---|---|---|---|---|---|
| semantic | 65.6 | 66.8 | +1.2 | 53.9 | 62.7 | +8.8 |
| covariate | 63.6 | 52.6 | **-11.0** | 42.4 | 66.3 | +23.9 |
| far-OOD | 82.8 | 91.6 | **+8.8** | 76.4 | 84.5 | +8.1 |

Full tables, including the component ablation, the sensitivity to gamma and M,
and the ID fraction, are in `results/proton_table*.csv`. The far-OOD gain is
reproduced. The semantic gain is small, and on covariate shift PROTON is worse
than MCM, the opposite of the paper.

### Why the MCM baseline differs from the paper

The MCM baseline (computed from the same features) does not match the paper
either, and it depends heavily on the prompt template. Recomputing the
probabilities from the stored embeddings with other templates gives:

| template | sem / cov / far MCM AUROC |
|---|---|
| `[CLS]` (used) | 66.5 / 64.1 / 83.1 |
| `A fundus photograph of [CLS]` | 77.1 / 67.8 / 84.7 |
| `A fundus photograph of [CLS].` | 68.6 / 84.5 / 62.2 |
| `[CLS].` | 48.4 / 11.0 / 45.6 |

None matches the paper's 53.9 / 42.4 / 76.4. The source of Ju et al.'s
ultra-widefield images is not documented and may also differ from DeepDRiD.
So the absolute numbers cannot be matched; the comparison that matters is
PROTON against MCM on the same features.

### Why PROTON does not help on covariate shift here

* **The embedding geometry is as the paper describes.** With prototypes built
  from the labelled ID images, the prototype distance separates the
  ultra-widefield images perfectly (AUROC 100.0; far-OOD 99.9). Their median
  distance is 0.85, against 0.08 (Normal) and 0.21 (DR).
* **The confidence gate does not keep them out.** 92% of the ultra-widefield
  images have `max(p) >= 0.7`, and all of them are classified as diabetic
  retinopathy. They fill the DR bank and pull its prototype towards them,
  so in the stream their prototype distance falls to 0.1-0.3, like ID
  images. Meanwhile genuine DR images end up far from the contaminated
  prototype. The prototype distance alone then reaches only 53.9 AUROC.
* **The variance never rises above the reference.** The MCM variance is
  0.0096 on ID images and 0.0107 with the covariate images mixed in, both
  below sigma2_0 = 0.02, so alpha stays near 0.58 instead of moving towards
  the prototype distance.

This agrees with the sensitivity results. With gamma = 0.9 the bank is
starved and PROTON falls back to MCM (63.6). With a 90% ID stream there are
few contaminating images and PROTON reaches 89.5 on covariate shift. With
30% or 10% ID it falls well below MCM on every shift, whereas the paper reports
that PROTON never degrades below MCM. `results/proton_covariate_scores.png`
shows the in-stream scores and alpha.

## Notes

* Ju et al.'s README suggests `caption=""` in FLAIR for vanilla prompts, but
  FLAIR builds each prompt with `caption.replace("[CLS]", class)`, so an empty
  caption would give both classes the same empty prompt. `caption="[CLS]"`
  (the bare class name) is used instead.
* The paper does not specify how the stream is built for each shift type.
  Here each stream holds the ID images and the OOD images of one shift type
  at the paper's default ID fraction of 70%.
* The metric functions were checked against scikit-learn
  (`roc_auc_score`, `average_precision_score`, and FPR at the TPR closest to
  95% as in Ju et al.'s notebooks).
