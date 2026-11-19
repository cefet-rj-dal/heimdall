# Cosine similarity drift detector for embeddings

Monitors a stream of embeddings (for instance BERT sentence embeddings
of the texts sent to an NLP model) with the custom Model Monitor of
Nigenda et al. (2022): every observation is scored by its average cosine
similarity with the baseline embeddings
([`cosine_score()`](https://cefet-rj-dal.github.io/heimdall/reference/cosine_score.md)),
the scores are averaged over monitoring windows of `window_size`
observations, and a drift is reported when the average falls below
`threshold`. Since it monitors the distribution of the inputs, it is
used for **virtual concept drift**.

## Usage

``` r
dfr_cosine(
  target_feat = NULL,
  reference = NULL,
  n_reference = 1000,
  window_size = 1,
  threshold = 0.5
)
```

## Arguments

- target_feat:

  Names of the columns holding the embedding. `NULL` (the default) uses
  every numeric column.

- reference:

  Optional baseline embeddings (a matrix or data frame with the
  monitored columns), such as the embeddings of the training texts. When
  `NULL`, the first `n_reference` observations of the stream form the
  baseline.

- n_reference:

  Size of the baseline taken from the stream when `reference` is `NULL`.

- window_size:

  Number of observations whose scores are averaged before the comparison
  with `threshold` (the monitoring window, an hour of traffic in the
  paper). With `1`, every observation is checked on its own, as the
  reference code does.

- threshold:

  A window whose average score is below it is a drift (0.5 in the
  reference code).

## Value

`dfr_cosine` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds the score of every observation (`score`) and the average of the
window on its last observation (`window_score`).

## Details

The windows do not overlap; the drift is reported on the last
observation of the window. Observations with missing values or zero norm
are skipped.
[`reset_state()`](https://cefet-rj-dal.github.io/heimdall/reference/reset_state.md)
keeps a baseline given in `reference` and empties the window; a baseline
taken from the stream is forgotten, and the next `n_reference`
observations form a new one.

## References

Nigenda, D., Karnin, Z., Zafar, M. B., Ramesha, R., Tan, A., Donini, M.
and Kenthapadi, K. (2022). Amazon SageMaker Model Monitor: A system for
real-time insights into deployed machine learning models. In
*Proceedings of the 28th ACM SIGKDD Conference on Knowledge Discovery
and Data Mining*, 3671-3681. <doi:10.1145/3534678.3539145>

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
baseline <- matrix(stats::rnorm(2000, mean = 2), ncol = 20)
# From row 21 half of the coordinates change sign.
flipped <- matrix(stats::rnorm(400, mean = 2), ncol = 20) %*% diag(rep(c(1, -1), each = 10))
stream <- as.data.frame(rbind(matrix(stats::rnorm(400, mean = 2), ncol = 20), flipped))
detector <- dfr_cosine(reference = baseline, window_size = 10, threshold = 0.5)
model <- fit(detector, stream)
model$drifter_output[!is.na(model$drifter_output$window_score), ]
#>          score  window_score
#> 10  0.78239783  0.8035939636
#> 20  0.65578924  0.7993399742
#> 30 -0.03978513 -0.0370861591
#> 40 -0.07524117  0.0001798928
```
