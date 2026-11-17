# LLR2 drift detector

Fully unsupervised drift detector of Sojo, Bielza and Larrañaga (2027).
A semiparametric Bayesian network
([`spbn()`](https://cefet-rj-dal.github.io/heimdall/reference/spbn.md))
is learned on a baseline of the stream. For every node `i`, the
statistic `Lambda_i = mean log f(x_i | pa_i)` over the most recent
sliding window minus the same mean over a fixed reference window is
compared with the chi-square distribution with one degree of freedom,
`p_i = 1 - F(2 |Lambda_i|)`; a drift is reported when some `p_i < alpha`
(or, with `fdr = TRUE`, when the Benjamini-Hochberg procedure rejects
some node at level `alpha`: LLR2-BH). The nodes that drift are reported,
and the network is updated.

## Usage

``` r
dfr_llr2(
  target_feat = NULL,
  n_baseline = 4096,
  window_size = 250,
  step = 50,
  alpha = 0.05,
  fdr = FALSE,
  update = c("parameter", "parameter_sliding", "sliding", "none"),
  structure = NULL,
  node_types = NULL,
  max_parents = 4,
  folds = 5
)
```

## Arguments

- target_feat:

  Names of the columns to be monitored. `NULL` (the default) monitors
  every numeric column.

- n_baseline:

  Size of the baseline: the first `n_baseline - window_size`
  observations learn the network (`W_l`) and the last `window_size` form
  the reference window (`W_r`).

- window_size:

  Size of the sliding and reference windows (`n_samples` in the paper).

- step:

  Stride of the sliding window: the test is run on observations whose
  position in the stream is a multiple of `step`.

- alpha:

  Significance level.

- fdr:

  `TRUE` for the Benjamini-Hochberg correction across nodes (LLR2-BH).

- update:

  Update after a drift: `"parameter"` (default; the CPDs become the
  weighted geometric mean of CPDs fitted on the sliding window and on
  the learning window, Eq. 11-13), `"parameter_sliding"` (CPDs refitted
  on the sliding window), `"sliding"` (structure and CPDs learned again
  on the sliding window) or `"none"`.

- structure, node_types, max_parents, folds:

  Passed to
  [`spbn()`](https://cefet-rj-dal.github.io/heimdall/reference/spbn.md)
  for the baseline network.

## Value

`dfr_llr2` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds, for every observation, its log-likelihood under the network in
use (`loglik`) and, on tested observations, the p-value of every node
(`p_<node>`); `obj$drifts` lists the drifts with their nodes, and
`obj$network` the current network.

## Details

After a drift the reference window is emptied; the first full sliding
window after the drift becomes the new reference window (its mean
log-likelihood under the updated network) and tests resume at the next
stride. This reproduces the spacing of the detections of the reference
results (at least `window_size + step` observations between two drifts).

The `"parameter"` update follows Section 4.3: for every node, the CPD
fitted on the sliding window (target, weight
`eta = window_size / |W_l|`) is combined with the CPD fitted on the
learning window (source, weight `1 - eta`); if the risk `|L_T| - |L_S|`
(cross-validated log-likelihood of the sliding window under the target
CPD against its log-likelihood under the source) is not positive, the
source is dropped (`eta = 1`). The structure is kept; the paper also
relearns the structure around the drifted nodes by hill climbing with
transfer learning (HC-TL), which is not implemented. Its ablation (Fig.
12) shows that detection is driven by the statistic, with equivalent
results for the parameter and sliding updates.

## References

Sojo, R., Bielza, C. and Larrañaga, P. (2027). A fully unsupervised
concept drift detector via Bayesian networks for machine condition
monitoring. *Expert Systems with Applications*, 332, 133634.
<doi:10.1016/j.eswa.2026.133634>

## See also

[`spbn()`](https://cefet-rj-dal.github.io/heimdall/reference/spbn.md)

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
n <- 3000
a <- stats::rnorm(n)
b <- a + stats::rnorm(n, sd = 0.5)
b[2001:n] <- b[2001:n] + 2
detector <- dfr_llr2(n_baseline = 1000, window_size = 200, step = 50)
model <- fit(detector, data.frame(a, b))
model$drifts
#> [[1]]
#> [[1]]$observation
#> [1] 2100
#> 
#> [[1]]$nodes
#> [1] "b"
#> 
#> [[1]]$lambda
#>        b 
#> -3.40449 
#> 
#> [[1]]$p_value
#>           b 
#> 0.009070055 
#> 
#> 
```
