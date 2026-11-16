# Ensemble vote over detection series

Applies the voting of
[`dfr_ensemble()`](https://cefet-rj-dal.github.io/heimdall/reference/dfr_ensemble.md)
to detection series computed beforehand, one column per detector (crisp
ensemble with `tolerance` \<= 1, fuzzy ensemble otherwise). This is how
the paper combines its detectors.

## Usage

``` r
ensemble_vote(
  detections,
  voting = c("majority", "efer", "threshold"),
  threshold = NULL,
  tolerance = 0,
  lookahead = TRUE
)
```

## Arguments

- detections:

  Logical (or 0/1) matrix or data frame, one row per time step and one
  column per detector.

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

A logical vector with the ensemble drifts, with attribute `vote` (the
vote of every time step, evaluated with the detections left after the
earlier ensemble drifts).

## Examples

``` r
detections <- matrix(FALSE, 100, 3)
detections[c(40, 43, 47), 1:3] <- diag(3) == 1
# No crisp majority: the detections are not simultaneous.
which(ensemble_vote(detections, "majority"))
#> integer(0)
# A fuzzy majority with a tolerance of 10 time steps.
which(ensemble_vote(detections, "majority", tolerance = 10))
#> [1] 39
```
