# TS-CP2 change point model

Trains the unsupervised TS-CP2 model of Deldari et al. (2021), as
re-implemented in the WWAggr code: a temporal convolutional network
(TCN) embeds windows of `window` steps; it is trained with a contrastive
(InfoNCE-like) loss so that each history window is close to the window
that follows it and far from the other windows of the batch. The change
score of a step is 1 minus the cosine similarity of the embeddings of
the two consecutive windows that end at it (see
[`cpd_scores()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_scores.md)).

## Usage

``` r
tscp2(
  x,
  window = 4,
  nb_filters = 64,
  kernel_size = 5,
  nb_stacks = 2,
  dilations = c(1, 2, 4, 8),
  use_skip_connections = TRUE,
  batch_norm = FALSE,
  n_steps = 8,
  code_size = 4,
  temperature = 0.1,
  learning_rate = 0.001,
  decay_steps = 1000,
  batch_size = 8,
  epochs = 200,
  patience = 5,
  min_delta = 1e-04,
  step = 1,
  alpha = 1,
  seed = NULL
)
```

## Arguments

- x:

  Training sequences, in the formats accepted by
  [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md).
  Only the first `2 * window` steps of every sequence are used (a
  history window and the future window that follows it).

- window:

  Length of the history and future windows.

- nb_filters:

  Number of filters of the convolutions.

- kernel_size:

  Kernel size of the convolutions.

- nb_stacks, dilations:

  The TCN has `nb_stacks` stacks of residual blocks, one per dilation.

- use_skip_connections:

  Whether the skip connection of the last block is added to the output
  of the TCN.

- batch_norm:

  Whether the convolutions are followed by batch normalisation.

- n_steps, code_size:

  Sizes of the dense layers: `2 * n_steps`, `n_steps` and the embedding
  (`code_size`).

- temperature:

  Temperature of the contrastive loss.

- learning_rate:

  Learning rate of Adam, annealed by a cosine schedule of period
  `decay_steps` epochs.

- decay_steps:

  Period of the cosine annealing (`T_max`, in epochs).

- batch_size:

  Number of sequences per batch.

- epochs:

  Largest number of epochs.

- patience, min_delta:

  Early stopping on the training loss: training stops after `patience`
  epochs whose loss is not below the best one minus `min_delta`.

- step, alpha:

  Prediction settings: the similarity is computed every `step` steps,
  and the scores are smoothed by an exponential moving average with
  factor `alpha` (1: none).

- seed:

  Seed of the private random stream (initial weights and order of the
  batches). `NULL` draws one from the session.

## Value

A `tscp2` object with the parameters, the batch normalisation
statistics, the settings and the training loss of every epoch.

## Details

The model follows the reference code
(<https://github.com/stalex2902/wwagr-ensemble-cpd>,
`src/baselines/tscp.py` and `prediction_utils.py`):

- Every residual block is a causal dilated convolution, ReLU, a second
  one, ReLU, plus the input (through a 1x1 convolution when the number
  of channels changes), then ReLU. Convolution weights are initialised
  from N(0, 0.05) with zero biases.

- The window `[window, channels]` is reshaped (not transposed) to
  `[channels, window]` before the TCN, as
  `x.reshape(batch, -1, seq_len)` of the reference.

- The loss of a batch of N pairs is the binary cross-entropy (target 1)
  of the logits
  `sum_i exp(s_ii / temperature) / sum_j exp(s_ij / temperature)`, where
  `s_ij` is the cosine similarity of history i and future j.

- The scores are `1 - max(s, 0)` (Appendix B of the WWAggr paper).

The dropout of the reference (rate 0 in its configurations) and its
layer and weight normalisation options (not functional there) are not
implemented. The network, its gradients and Adam are written in R. The
defaults are those of the Human Activity configuration of the reference
(`configs/human_activity_tscp.yaml`).

## References

Deldari, S., Smith, D. V., Xue, H. and Salim, F. D. (2021). Time series
change point detection with self-supervised contrastive predictive
coding. *Proceedings of The Web Conference 2021*, 3124-3135.
<doi:10.1145/3442381.3449903>

Stepikin, A., Romanenkova, E. and Zaytsev, A. (2025). WWAggr: A window
Wasserstein-based aggregation for ensemble change point detection. *2025
IEEE International Conference on Data Mining (ICDM)*, 1535-1544.
<doi:10.1109/ICDM65498.2025.00163>

## See also

[`cpd_scores()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_scores.md),
[`cpd_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md),
[`dfr_wwaggr()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_wwaggr.md).

## Examples

``` r
set.seed(1)
x <- array(stats::rnorm(16 * 12 * 2), c(16, 12, 2))
model <- tscp2(x, window = 4, nb_filters = 4, dilations = c(1, 2), n_steps = 4, code_size = 3,
               epochs = 3, seed = 1)
model
#> TS-CP2 model: windows of 4 steps, 2 feature(s), embedding of size 3, 3 epoch(s)
```
