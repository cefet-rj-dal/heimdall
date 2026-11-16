# Ensemble drift detector (CEDD and FEDD)

Combines the detections of several drift detectors by voting (Giusti
Tavares et al., 2025). At every time step the vote is the sum of the
members' scores, and the ensemble reports a drift when the vote passes
the voting rule:

- `"efer"` (early find, early report): vote \>= 1;

- `"majority"`: vote \> n / 2 (n members);

- `"threshold"`: vote \>= `threshold`.

With `tolerance` k \<= 1 the scores are crisp: 1 when the member detects
a drift at that time step, 0 otherwise (crisp ensemble drift detector,
CEDD). With k \> 1 they are fuzzy (fuzzy ensemble drift detector, FEDD).
A detection at time d gives time t the score `1 - |t - d| / k` when
`|t - d| < k`. A member's score is the largest over its detections. So
detections that are close in time but not simultaneous can still add up
to a majority.

## Usage

``` r
dfr_ensemble(
  drifter_list,
  voting = c("majority", "efer", "threshold"),
  threshold = NULL,
  tolerance = 0,
  lookahead = TRUE
)
```

## Arguments

- drifter_list:

  Named list of drifters (`dist_based` or `mv_dist_based`) run in
  parallel on the stream.

- voting:

  Voting rule: `"majority"` (default), `"efer"` or `"threshold"`.

- threshold:

  Vote needed by the `"threshold"` rule (lambda).

- tolerance:

  Time tolerance k of the fuzzy scores; `0` (default) gives the crisp
  ensemble.

- lookahead:

  With fuzzy scores, `TRUE` (default) uses detections on both sides of a
  time step, as in the paper. The vote of time t is then known only when
  t + k - 1 has arrived, so drifts are reported k - 1 observations after
  the time they refer to (`obj$events` holds those times). `FALSE` uses
  only the detections up to the current observation and reports at once.

## Value

`dfr_ensemble` object. After
[`fit()`](https://rdrr.io/pkg/daltoolbox/man/fit.html), `drifter_output`
holds, on every observation, the decision of each member, the time step
whose vote was decided (`time`) and the vote (`vote`). `obj$events`
holds the time steps of the ensemble drifts, including those decided
when the stream ended.

## Details

The combination follows the reference code
(<https://github.com/cefet-rj-dal/reproduce>,
`fedd/core/4-ensemble-fuzzy.R`, with the triangular fuzzification of
`harbinger`):

- The majority rule is strict, vote \> n / 2 (the paper writes vote \>=
  the ceiling of n / 2).

- After a fuzzy ensemble drift at time t, the detections before t are
  forgotten. Each member with a positive score at t also loses its first
  detection at or after t, or its next detection if none has arrived
  yet. So one detection does not raise a second ensemble drift.

- The scores are `(k - j) * (1 / k)` at distance j, as in `harbinger`.
  The paper's Equation 3 prints `|t - t'| / k`, which would give 0 at
  the detection itself.

Members with a `target_feat` receive that column of each row, the other
univariate members the row mean, and multivariate members the row (as
[`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md)).
The members are not reset by the ensemble. The paper's members are also
run as separate detectors, each with its own warm-up and normalisation,
before the vote.

[`ensemble_vote()`](https://cefet-rj-dal.github.io/heimdall/reference/ensemble_vote.md)
applies the same voting to detection series computed beforehand.

## References

Giusti Tavares, L., Lima, J., Melo, M., Chen, C., Garibaldi, J.,
Scatena, G. S., Costa, A. H. R., Gomi, E. S., Salles, R., Pacitti, E.,
Santos, I., Siqueira, I. G., Carvalho, D., Coutinho, R., Porto, F. and
Ogasawara, E. (2025). Fuzzy-based ensemble method for robust concept
drift detection in multivariate time series. *2025 International Joint
Conference on Neural Networks (IJCNN)*.
<doi:10.1109/IJCNN64981.2025.11228919>

## See also

[`ensemble_vote()`](https://cefet-rj-dal.github.io/heimdall/reference/ensemble_vote.md),
[`dfr_multi_criteria()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_multi_criteria.md).

## Examples

``` r
library(daltoolbox)
library(heimdall)
set.seed(1)
data <- data.frame(serie = c(stats::rnorm(500), stats::rnorm(500, 3)))
members <- list(ph = dfr_page_hinkley(), kswin = dfr_kswin(), adwin = dfr_adwin())
# Crisp majority: the members must agree on the same observation.
crisp <- fit(dfr_ensemble(members, voting = "majority"), data)
crisp$events
#> integer(0)
# Fuzzy majority with a tolerance of 50 observations.
fuzzy <- fit(dfr_ensemble(members, voting = "majority", tolerance = 50), data)
fuzzy$events
#> [1] 507
```
