# InDiD change point detection model

Trains the sequence-to-sequence change point detector of Romanenkova et
al. (2022): a recurrent network (an LSTM layer and a linear layer with
sigmoid output) reads a sequence and gives, at every time step, the
probability that a change has already happened. The model is trained on
labelled sequences with the InDiD loss, a differentiable approximation
of the detection delay after a change and of the time to a false alarm
before it; with the binary cross-entropy (BCE); or with both in turn
(`combined`).

## Usage

``` r
indid(
  x,
  labels,
  loss = c("indid", "bce", "combined"),
  hidden_size = 4,
  T = 32,
  learning_rate = 0.001,
  batch_size = 64,
  epochs = 200,
  patience = 10,
  validation = NULL,
  seed = NULL,
  n_layers = 1,
  dropout = 0,
  layer_norm = c("none", "before", "after")
)
```

## Arguments

- x:

  Training sequences: an array `[sequence, time, feature]`, a matrix
  `[sequence, time]` (one feature) or a list of matrices
  `[time, feature]` of the same size.

- labels:

  Labels of the training sequences, a matrix `[sequence, time]` that is
  0 before the change and 1 from the change on (all 0 when the sequence
  has no change).

- loss:

  `"indid"` (default), `"bce"` or `"combined"` (BCE for half of the
  epochs, then InDiD for the other half).

- hidden_size:

  Size of the LSTM hidden state.

- T:

  Length of the segment after the change used by the delay part of the
  InDiD loss.

- learning_rate:

  Learning rate of Adam.

- batch_size:

  Number of sequences per batch.

- epochs:

  Largest number of epochs (for `"combined"`, split in two).

- patience:

  Epochs without a lower validation loss before training stops (early
  stopping, only with validation data).

- validation:

  Optional validation data, `list(x = , labels = )`.

- seed:

  Seed of the private random stream (initial weights and order of the
  batches). `NULL` draws one from the session.

- n_layers:

  Number of LSTM layers.

- dropout:

  Dropout rate on the outputs of every LSTM layer but the last (training
  only), as `torch.nn.LSTM`.

- layer_norm:

  `"none"` (default, `torch.nn.LSTM`), or the one-layer LSTM with layer
  normalisation of the WWAggr code (`LayerNormLSTM`): `"before"` or
  `"after"` the linear map of every gate.

## Value

An `indid` object with the network (`params`), the settings and the
training and validation losses of every epoch (`history`).

## Details

The model and the training follow the reference code
(<https://github.com/romanenkova95/InDiD>: `utils/loss.py`,
`utils/core_models.py` (`BaseRnn`), `utils/cpd_models.py`, `train.py`
and the `configs/`):

- The InDiD loss of a sequence with its first change at position `c` is
  `alpha * D(p[c:(c + T - 1)]) - D(p[1:(c - 1)])`, and `-D(p)` without a
  change, where
  `D(p) = sum_t t p_t prod_{i < t} (1 - p_i) + (n + 1) prod_i (1 - p_i)`
  is the expected stopping time on a segment of `n` steps. The loss of a
  batch is the mean over its sequences. `alpha` is `2 * B / T`, where
  `B` is the number of sequences in the batch: the reference uses
  `len(prob)` of the batch matrix, so `alpha` depends on the batch size,
  not on the sequence length.

- BCE is `torch.nn.BCELoss` (mean over all time steps, logarithms
  clamped at -100).

- The LSTM has one layer (the dropout of the reference configs has no
  effect with one layer). The weights are initialised as in torch:
  uniform within `+-1/sqrt(hidden_size)`.

- Adam with the defaults of torch, shuffled batches, and early stopping
  on the validation loss (mean over the validation batches weighted by
  their size, min_delta 0) as Lightning; the model of the last epoch is
  kept, as `train.py`. The reference validates on its test set.

The network, the gradients (backpropagation through time) and Adam are
written in R. The defaults are those of the 1-D SyntheticNormal
experiment (`configs/synthetic_1D_seq2seq.yaml`).

The architectures of the WWAggr code (`BaseRnn` of
<https://github.com/stalex2902/wwagr-ensemble-cpd>,
`src/models/core_models.py`), used for the BCE and InDiD members of the
ensembles of
[`cpd_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/cpd_ensemble.md),
are available through `n_layers`, `dropout` and `layer_norm`. In
`LayerNormLSTM`, every gate (forget, input, output, cell) has a linear
map of the input and one of the hidden state, each with its own layer
normalisation (before the map, on the input or hidden state, or after
it); the linear maps are initialised as `torch.nn.Linear`. Models with
more than one layer or with layer normalisation are trained with the
automatic differentiation of the package.

## References

Romanenkova, E., Stepikin, A., Morozov, M. and Zaytsev, A. (2022).
InDiD: Instant disorder detection via a principled neural network.
*Proceedings of the 30th ACM International Conference on Multimedia*,
3152-3162. <doi:10.1145/3503161.3548182>

## See also

[`indid_predict()`](https://cefet-rj-dal.github.io/heimdall/reference/indid_predict.md)
for the probabilities of change of new sequences and
[`dfr_indid()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_indid.md)
to monitor a stream.

## Examples

``` r
# Sequences of 32 steps around 1; half of them move to 6 at a random step.
set.seed(1)
make <- function(n) {
  change <- sample(2:32, n, replace = TRUE)
  change[seq_len(n / 2)] <- 33
  x <- t(vapply(change, function(c) stats::rnorm(32, ifelse(seq_len(32) < c, 1, 6)), numeric(32)))
  labels <- t(vapply(change, function(c) (seq_len(32) >= c) * 1, numeric(32)))
  list(x = x, labels = labels)
}
train <- make(200)
model <- indid(train$x, train$labels, T = 8, learning_rate = 0.02, epochs = 30, seed = 1)
test <- make(4)
round(indid_predict(model, test$x)[, 25:32], 2)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,] 0.01 0.03 0.07 0.24 0.02 0.05 0.03 0.01
#> [2,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> [3,] 0.00 0.01 0.01 0.01 0.00 0.00 0.80 0.96
#> [4,] 0.97 0.98 0.98 0.98 0.98 0.99 0.98 0.98
test$labels[, 25:32]
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]    0    0    0    0    0    0    0    0
#> [2,]    0    0    0    0    0    0    0    0
#> [3,]    0    0    0    0    0    0    1    1
#> [4,]    1    1    1    1    1    1    1    1
```
