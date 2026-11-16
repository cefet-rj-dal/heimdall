# PROTON: prototype-based test-time online OOD detection

Online out-of-distribution (OOD) detector for the outputs of a frozen
classifier with an embedding space, such as a medical vision-language
model (Das et al., 2026). Each observation is one input scored by the
model: its embedding and its class probabilities. PROTON keeps a bank of
the most recent confidently classified embeddings of each class, scores
each input by its cosine distance to the nearest class prototype, and
fuses that distance with the Maximum Concept Matching (MCM) score,
`-max(p)`, using a weight driven by the running variance of the MCM
score. It needs no training data, labels or model changes.

## Usage

``` r
dfr_proton(
  embedding_cols,
  prob_cols,
  score_col = NULL,
  gamma = 0.7,
  bank_size = 100,
  k_min = 5,
  alpha_min = 0.3,
  alpha_max = 0.7,
  sigma2_0 = 0.02,
  threshold = NULL
)
```

## Arguments

- embedding_cols:

  Names of the columns holding the embedding.

- prob_cols:

  Names of the columns holding the class probabilities (one per
  in-distribution class), in the same order for every observation.

- score_col:

  Optional column with another base OOD score (higher means more OOD),
  used instead of the MCM score `-max(p)`. The probabilities are still
  used by the confidence gate.

- gamma:

  Confidence gate: an embedding enters the bank of its predicted class
  only when `max(p) >= gamma`.

- bank_size:

  Maximum number of embeddings kept per class (`M`); the oldest one is
  dropped first.

- k_min:

  Number of embeddings every class needs before the prototype distance
  is used. Until then the score is the base score alone.

- alpha_min, alpha_max:

  Range of the weight of the base score in the fusion.

- sigma2_0:

  Reference variance of the base score on in-distribution data.

- threshold:

  Optional threshold on the fused score. When given, an observation with
  a score above it is reported as a drift (an OOD input); when `NULL`
  (default) no drift is reported and the scores are only exposed through
  `drifter_output`.

## Value

`dfr_proton` object

## Details

For each observation \\x_t\\, with \\\ell_2\\-normalized embedding
\\e_t\\ and probabilities \\p_t\\ (Algorithm 1 of the paper):

1.  the base score \\S\_{MCM} = -\max_c p\_{t,c}\\ updates its running
    variance \\\sigma^2_t\\ (Welford's algorithm);

2.  once every class bank holds `k_min` embeddings, the prototype
    distance is \\S\_{proto} = 1 - \max_c e_t^\top \mu_c\\, where
    \\\mu_c\\ is the normalized mean of the bank of class \\c\\;

3.  the weight is \\\alpha_t = \alpha\_{max} - \mathrm{sigmoid}(100
    (\sigma^2_t - \sigma^2_0)) (\alpha\_{max} - \alpha\_{min})\\ and the
    score is \\S_t = \alpha_t S\_{MCM} + (1 - \alpha_t) S\_{proto}\\;
    before calibration \\S_t = S\_{MCM}\\;

4.  finally, when \\\max_c p\_{t,c} \ge \gamma\\, \\e_t\\ enters the
    bank of the predicted class.

A variance that rises above the in-distribution reference (for instance
when confidently misclassified, covariate-shifted inputs arrive) moves
the weight towards the prototype distance.

Rows with a missing embedding or probability value are skipped.
`obj$drifter_output` holds, for the rows of the last
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), the fused
`score`, the `base_score`, the `proto_score`, the weight `alpha`, the
running `variance`, whether the detector was `calibrated`, the predicted
`class` and whether the embedding was `enqueued`.

## References

Das, A., Wasalathilaka, N., Lu, Y., Dukre, A., Mahapatra, D., Khan, S.,
and Razzak, I. (2026). PROTON: Prototype-Based Test-Time Online OOD
Detection for Medical VLMs. arXiv:2606.20913.

Ming, Y., Cai, Z., Gu, J., Sun, Y., Li, W., and Li, Y. (2022). Delving
into out-of-distribution detection with vision-language representations.
*Advances in Neural Information Processing Systems*, 35, 35087-35102.

## Examples

``` r
library(daltoolbox)
library(heimdall)

# Two in-distribution classes in a 3-dimensional embedding space, followed
# by covariate-shifted inputs: they are classified as confidently as the
# in-distribution ones, but their embeddings lie elsewhere.
set.seed(1)
make_rows <- function(n, center, p1) {
  e <- sweep(matrix(stats::rnorm(3 * n, sd = 0.05), n), 2, center, "+")
  data.frame(e1 = e[, 1], e2 = e[, 2], e3 = e[, 3], p1 = p1, p2 = 1 - p1)
}
stream <- rbind(
  make_rows(50, c(1, 0, 0), 0.9),
  make_rows(50, c(0, 1, 0), 0.1),
  make_rows(20, c(0, 0, 1), 0.9)
)

detector <- dfr_proton(embedding_cols = c("e1", "e2", "e3"), prob_cols = c("p1", "p2"))
detector <- fit(detector, stream)
tail(detector$drifter_output[, c("score", "base_score", "proto_score", "alpha")])
#>          score base_score proto_score     alpha
#> 115 -0.3297136       -0.9   0.7402567 0.6523188
#> 116 -0.3537017       -0.9   0.6712624 0.6523188
#> 117 -0.3339388       -0.9   0.7281044 0.6523188
#> 118 -0.3465035       -0.9   0.6919657 0.6523188
#> 119 -0.3306199       -0.9   0.7376500 0.6523188
#> 120 -0.3682943       -0.9   0.6292911 0.6523188
```
