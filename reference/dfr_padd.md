# Parallel Activations Drift Detector (PADD)

Unsupervised batch drift detector of Komorniczak and Ksieniewicz (2024).
The data stream is processed in chunks of `chunk_size` observations,
each passed through a randomly initialised, never trained neural network
(one hidden ReLU layer of `hidden` units, `outputs` ReLU outputs and a
softmax, weights drawn from N(0, 0.1)). For every output, `replications`
Student t-tests compare a sample of `sample_size` activations of the
current chunk with a sample of the activations stored since the last
drift (both drawn with replacement). A drift is reported when more than
`threshold * outputs * replications` tests are significant at level
`alpha`; the stored activations are then cleared. Since it monitors the
inputs only, it is used for **virtual concept drift**.

## Usage

``` r
dfr_padd(
  target_feat = NULL,
  chunk_size = 200,
  alpha = 0.05,
  threshold = 0.17,
  outputs = 30,
  replications = 35,
  sample_size = 75,
  hidden = 10,
  seed = NULL,
  weights = NULL
)
```

## Arguments

- target_feat:

  Names of the columns to be monitored. `NULL` (the default) monitors
  every numeric column.

- chunk_size:

  Number of observations of a chunk (the batches of the paper, 200 in
  its experiments).

- alpha:

  Significance level of the t-tests.

- threshold:

  Fraction of significant tests above which a drift is reported.

- outputs:

  Number of outputs of the network (`e`).

- replications:

  Number of t-tests per output (`r`).

- sample_size:

  Size of the samples drawn for every test (`s`).

- hidden:

  Number of units of the hidden layer.

- seed:

  Seed of the network weights and of the samples. `NULL` draws it from
  the global generator when the detector is created; the global random
  state is left unchanged.

- weights:

  Optional fixed network: a list with the hidden layer
  (`(n_features + 1) x hidden`, the last row is the bias) and the output
  layer (`hidden x outputs`). By default it is drawn on the first chunk.

## Value

`dfr_padd` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds, on the last observation of every chunk, the fraction of
significant tests (`significant`), and `obj$weights` the network.

## Details

The defaults are those of the reference implementation
(<https://github.com/w4k2/padd>, `methods.py`). The paper's experiments
use `outputs = 12`, `replications = 12`, `sample_size = 50`, and
`alpha = 0.07, threshold = 0.19` for sudden drifts or
`alpha = 0.13, threshold = 0.26` for gradual ones.

As in the reference, the first chunk is only stored; after a drift the
stored activations are replaced by those of the drifting chunk; the
t-test is the pooled-variance (Student) test, and a test whose two
samples are constant (undefined p-value) is not significant. The drift
is reported on the last observation of the chunk. Rows with missing
values are dropped.
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
clears the stored activations and keeps the network.

## References

Komorniczak, J. and Ksieniewicz, P. (2024). Unsupervised concept drift
detection based on parallel activations of neural network.
arXiv:2404.07776.

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
# 12 chunks of 200 observations; the mean of 3 of the 10 features moves at chunk 7.
x <- matrix(stats::rnorm(2400 * 10), ncol = 10)
x[1201:2400, 1:3] <- x[1201:2400, 1:3] + 1
detector <- dfr_padd(outputs = 12, replications = 12, sample_size = 50,
                     alpha = 0.07, threshold = 0.19, seed = 1)
model <- fit(detector, as.data.frame(x))
which(!is.na(model$drifter_output$significant) & model$drifter_output$significant > 0.19)
#> [1] 1400
```
