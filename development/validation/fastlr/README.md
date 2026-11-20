# Validation: fast likelihood-based change point detection (`fastlr_split`, `dfr_fastlr`)

Checks `fastlr_split()` and `dfr_fastlr()` against Tatti (2019), *Fast
likelihood-based change point detection* (ECML PKDD 2019,
doi:10.1007/978-3-030-46150-8_39, arXiv:2301.08892).

The paper points to its code at <https://version.helsinki.fi/dacs/>. That
server could not be reached (connection refused or timeout), and no other
copy was found. The implementation therefore follows the paper only.

## What is implemented

| paper | heimdall |
|---|---|
| border indices / blocks with increasing proportions, kept by the pool-adjacent-violators merge (Section 4) | `.fastlr_append()`, `.fastlr_blocks()` |
| ChangeInc on the sequence and on the flipped sequence (Problem 1) | `.fastlr_change()` |
| FindCands (Alg. 1), FindCands' (Alg. 2), FindSegment (Prop. 2), FindChange (Alg. 3), the (1 - eps) guarantee | `.fastlr_find_change()`, `.fastlr_find_segment()`, `fastlr_split(x, eps)` |
| `eps = 0`: every border index tested (the exact solution) | `fastlr_split(x, 0)` |
| stream detector: change declared when the score exceeds `tau + log(n)`, window restarted after the change | `dfr_fastlr(tau, eps)` |

`tests/testthat/test-fastlr.R` checks the following:

* The blocks satisfy the definition of border indices.
* With `eps = 0`, the score equals the brute-force optimum over every split.
* With `eps` = 0.1, 0.5 and 0.9, the score lies within [(1 - eps) OPT, OPT],
  and no more points are tested than with `eps = 0`.

## Data

* The only real dataset of the paper is `network2` (Amit et al., 2019, Palo
  Alto Networks). It is available only on request by e-mail, so it is not
  used.
* The validation uses the synthetic sequences of Section 7: Ind, Step, Slope
  and Hill (see the header of `run_fastlr.R`). They are generated with fixed
  seeds.

## Results (`development/validation/results/fastlr/`)

**`tau_step.csv` (Figures 1a and 1b, Step, eps = 0).** All 19 true changes
are found for every tau.

| tau | changes declared | mean delay | paper |
|---|---|---|---|
| 0.5 | 46,251 | 1.7 | 46,366 changes, delay 1.4 |
| 1 | 117 | 15.6 | about 170 changes, delay about 13 |
| 6 | 19 | 24.4 | 19 changes, delay about 24.3 |
| 10 | 19 | 33.8 | 19 changes, delay about 30 |

**`eps_synthetic.csv` (Figures 1c and 2, tau = 6).** Changes declared: none
on Ind, the 19 true changes on Step, and 61 on Slope (60 at eps = 0.7).

* g / OPT:
  * The minimum ratio is always at least 1 - eps, and it falls roughly
    linearly in eps (0.26 to 0.42 at eps = 0.9).
  * The average ratio is at least 0.98 (paper: above 0.97).
* Delay on Step: 24.4 at eps <= 0.5, 25.5 at eps = 0.9 (paper: about 24.3,
  rising to about 27).
* Tested change points per entry, as total over total
  (`total_tests_per_n`, eps = 0):

  | sequence | heimdall | paper |
  |---|---|---|
  | Ind | 2.2e-4 | about 3e-4 |
  | Step | 3.1e-3 | about 3e-3 |
  | Slope | 8.3e-3 | about 6e-3 |

  The mean of the per-entry ratios (`tests_per_n`) is larger, because it is
  dominated by the short windows right after each change.
* Candidates of FindCands per block (`total_cands_per_k`): 0.99 at
  eps = 0.1, 0.86 to 0.89 at eps = 0.5, and 0.58 to 0.67 at eps = 0.9. The
  paper says "roughly by 1/2 for the larger values of eps". The windows here
  have few blocks (about 16 on Step, both directions together), which limits
  the pruning.

**`hill.csv` (Figure 3, 100,000 to 1,000,000 entries, no change declared).**

* Candidates of FindCands per block at 1,000,000 entries, which decrease
  with the length, as in the paper:

  | eps | mean of the per-entry ratios | total / total | paper (Figure 3c) |
  |---|---|---|---|
  | 0.1 | 0.63 | 0.55 | about 0.5 |
  | 0.5 | 0.32 | 0.25 | about 0.25 |
  | 0.9 | 0.19 | 0.14 | about 0.15 |

* The number of blocks grows slowly: 68 at 100,000 entries, 143 at
  1,000,000.

## Running time

The running times are not comparable with the paper (C++ against R).

* With `eps = 0`, heimdall scores all the blocks in one vectorised R call,
  while `eps > 0` runs the loops of Algorithms 1 to 3 in interpreted R.
* As a result, `eps = 0` is the fastest setting here:
  * Step: 126 s for `eps = 0`, 423 to 469 s for `eps > 0`.
  * Hill, 1,000,000 entries: 611 s for `eps = 0`, 8,443 s for `eps = 0.1`,
    2,786 s for `eps = 0.9`.
* The time of `eps = 0` is linear in the length here, not quadratic as in
  Figure 3a. With so few blocks, the cost of each step is almost constant.
* Among `eps > 0`, the time decreases with eps as in the paper. The ratio to
  `eps = 0.1` at 1,000,000 entries is 0.51 for `eps = 0.5` and 0.33 for
  `eps = 0.9`. The number of tested candidates, which does not depend on the
  language, follows the paper.

## Commands

```sh
# inside the devcontainer, from the package root
Rscript development/validation/fastlr/run_fastlr.R tau development/validation/results/fastlr 8
Rscript development/validation/fastlr/run_fastlr.R eps development/validation/results/fastlr 8
Rscript development/validation/fastlr/run_fastlr.R hill development/validation/results/fastlr 8
```
