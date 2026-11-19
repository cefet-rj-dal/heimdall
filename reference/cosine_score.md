# Average cosine similarity to a baseline of embeddings

Scores every new observation (an embedding vector, such as the sentence
embedding of a text) by its average cosine similarity with the
embeddings of a baseline, the data drift check of the custom Amazon
SageMaker Model Monitor for NLP models of Nigenda et al. (2022):
\$\$s(x) = \frac{1}{N}\sum\_{i=1}^{N} \frac{x \cdot e_i}{\\x\\
\\e_i\\}.\$\$ Inputs far from the baseline distribution get low scores.

## Usage

``` r
cosine_score(reference, x)
```

## Arguments

- reference:

  The baseline embeddings: a numeric matrix or data frame, one row per
  embedding.

- x:

  The embeddings to score, with the same columns (or a single embedding
  as a numeric vector).

## Value

A numeric vector with the score of every row of `x`.

## Details

The average of the cosine similarities equals the inner product of the
normalised input with the mean of the normalised baseline embeddings, so
scoring costs one inner product per input instead of one per baseline
embedding (the reference code loops over the baseline). Baseline
embeddings with zero norm are left out, and an input with zero norm gets
`NA` (the cosine is undefined; the reference code gets `NaN`).

## References

Nigenda, D., Karnin, Z., Zafar, M. B., Ramesha, R., Tan, A., Donini, M.
and Kenthapadi, K. (2022). Amazon SageMaker Model Monitor: A system for
real-time insights into deployed machine learning models. In
*Proceedings of the 28th ACM SIGKDD Conference on Knowledge Discovery
and Data Mining*, 3671-3681. <doi:10.1145/3534678.3539145>

## See also

[`dfr_cosine()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_cosine.md)
to monitor a stream.

## Examples

``` r
set.seed(1)
baseline <- matrix(stats::rnorm(2000, mean = 1), ncol = 20)
near <- matrix(stats::rnorm(100, mean = 1), ncol = 20)
far <- matrix(stats::rnorm(100, mean = -1), ncol = 20)
cosine_score(baseline, near)
#> [1] 0.5292560 0.2974946 0.3483357 0.4821827 0.5267879
cosine_score(baseline, far)
#> [1] -0.5477422 -0.3954581 -0.3608128 -0.4182917 -0.4938171
```
