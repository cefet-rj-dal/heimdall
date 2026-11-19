# SN-TS2Vec change point model

Trains the unsupervised SN-TS2Vec model of Bazarova et al. (2024), as
used in the WWAggr code: the TS2Vec encoder of Yue et al. (2022) (a
linear input layer and residual blocks of dilated convolutions with
GELU) with spectral normalisation of the convolutions, trained with the
hierarchical contrastive loss of TS2Vec on two overlapping random crops
of every sequence. The change score of a step is 1 minus the cosine
similarity of the max-pooled embeddings of the two consecutive windows
that end at it (see
[`cpd_scores()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_scores.md)).

## Usage

``` r
ts2vec(
  x,
  window = 4,
  output_dim = 32,
  hidden_dim = 128,
  depth = 8,
  spectral_norm = TRUE,
  mask = c("binomial", "continuous", "all_true"),
  temporal_unit = 0,
  max_train_length = NULL,
  learning_rate = 0.001,
  weight_decay = 0.2,
  batch_size = 128,
  epochs = 40,
  patience = 40,
  validation = NULL,
  step = 1,
  alpha = 1,
  seed = NULL
)
```

## Arguments

- x:

  Training sequences, in the formats accepted by
  [`indid()`](https://cefet-rj-dal.github.io/heimdall/reference/indid.md).

- window:

  Length of the history and future windows of the scores.

- output_dim, hidden_dim:

  Sizes of the embedding and of the hidden channels.

- depth:

  Number of hidden residual blocks (one more maps to `output_dim`).

- spectral_norm:

  Whether the convolutions are spectrally normalised (largest singular
  value at most 1).

- mask:

  Masking of the input during training: `"binomial"` (each step kept
  with probability 0.5), `"continuous"` (5 masked segments of 10 % of
  the length) or `"all_true"` (no masking).

- temporal_unit:

  Smallest level of the hierarchy with a temporal loss.

- max_train_length:

  If set, longer training sequences are cut at a random offset to this
  length.

- learning_rate, weight_decay:

  Settings of AdamW; the learning rate follows a cosine schedule with
  warm restarts (periods of 10 epochs, then 1000).

- batch_size:

  Number of sequences per batch.

- epochs:

  Largest number of epochs.

- patience:

  Early stopping on the validation loss (only with validation data).

- validation:

  Optional validation sequences.

- step, alpha:

  Prediction settings, as in
  [`tscp2()`](https://cefet-rj-dal.github.io/heimdall/reference/tscp2.md).

- seed:

  Seed of the private random stream (weights, crops, masks, dropout,
  order of the batches). `NULL` draws one from the session.

## Value

A `ts2vec` object with the parameters, the power iteration vectors, the
settings and the losses of every epoch.

## Details

The model follows the reference code
(<https://github.com/stalex2902/wwagr-ensemble-cpd>,
`src/baselines/ts2vec_sn.py` and `prediction_utils.py`):

- Spectral normalisation divides a weight by its largest singular value
  when it exceeds 1. The singular value is estimated by one power
  iteration at every forward pass in training (15 at initialisation), as
  `torch.nn.utils.parametrizations.spectral_norm`.

- Training masks the hidden representation of the input steps, and
  applies dropout (0.1) to the output.

- The window embedding is the maximum over its steps.

The network, its gradients and AdamW are written in R. The defaults are
those of the Human Activity configuration of the reference
(`configs/human_activity_ts2vec.yaml`).

## References

Bazarova, A., Romanenkova, E. and Zaytsev, A. (2024). Normalizing
self-supervised learning for provably reliable change point detection.
*2024 IEEE International Conference on Data Mining (ICDM)*, 21-30.

Yue, Z., Wang, Y., Duan, J., Yang, T., Huang, C., Tong, Y. and Xu, B.
(2022). TS2Vec: Towards universal representation of time series.
*Proceedings of the AAAI Conference on Artificial Intelligence*, 36(8),
8980-8987.

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
x <- array(stats::rnorm(8 * 12 * 2), c(8, 12, 2))
model <- ts2vec(x, window = 3, output_dim = 4, hidden_dim = 6, depth = 2, epochs = 2, seed = 1)
model
#> SN-TS2Vec model: windows of 3 steps, 2 feature(s), embedding of size 4, 2 epoch(s)
```
